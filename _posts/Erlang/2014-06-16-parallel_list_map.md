---
layout: post
category : Erlang
tagline:
tags : [Erlang, 并发, 列表]
title: 【Erl代码片段】简易的并行版lists:map/2
---

Joe在书上示范了一个并行化的`lists:map`:

```erlang
pmap(F, L) ->
    S = self(),
    %% make_ref() returns a unique reference
    %% we'll match on this later
    Ref = erlang:make_ref(),
    Pids = map(fun(I) ->
                spawn(fun() -> do_f(S, Ref, F, I) end)
                end, L),

    %% gather the results
    gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
    receive
        {Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].
```

其实里面的Ref是没什么必要的, Pid本身就是不重复的,
可以用列表解析改简短一点:

```erlang
pmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end
    || Pid <- [spawn(fun() -> Parent ! {self(), (catch F(X))} end) || X <- L]].
```

如果不在乎返回值的次序, 可以用霸爷的版本: <http://blog.yufeng.info/archives/401>,
不过他没有在`F(X)`前加`catch`, 下面的代码中加上了.
他在论坛中解释说不加 `catch` 是为了"fail fast"[(链接)](http://www.iteye.com/topic/365175),
不过这并不完全成立, 因为`F(X)`是在新建的进程中执行的, 而且没有link,
出错只会使调用者无限等待, 并不见得特别好.

```erlang
upmap(F, L) ->
    Parent = self(),
    Ref = make_ref(),
    [receive {Ref, Result} -> Result end
    || _ <- [spawn(fun() -> Parent ! {Ref, (catch F(X)} end) || X <- L]].
```

Tsung里有一个限制产生进程数的版本:
<https://github.com/processone/tsung/blob/master/src/tsung/ts_utils.erl#L874>
如下:

```erlang
pmap(F, L, NProcs) ->
    pmap(F, L, NProcs,"").

pmap(F, L, NProcs, Res) when length(L) > NProcs->
    {Head, Tail} = lists:split(NProcs,L),
    Parent = self(),
    lists:foldl(fun(X, Acc) -> spawn(fun() -> Parent ! {self(), F(X), Acc} end), Acc+1  end, 0, Head),
    NewRes = wait_result(NProcs,[]),
    pmap(F,Tail, NProcs, Res ++ NewRes);

pmap(F, L, _NProcs, Acc) ->
    Acc ++ pmap(F,L).

wait_result(0, Res)->
    {_Ids, RealRes} = lists:unzip(lists:keysort(1, Res)),
    RealRes;
wait_result(Nprocs, Res)->
    receive
        {_Pid, Result, Id} ->
            NewRes = Res ++ [{Id, Result}],
            wait_result(Nprocs-1, NewRes)
    end.
```

这代码看起来并不怎么好, 子进程发送的 `{self(), F(X), Acc}`
中`self()`是没有用的, 底下的 `wait_result/2` 也没有对它进行匹配,
合理来说这地方应该是一个 `Ref` 才对.
而且代码中先是 `Res ++ [{Id, Result}]`, 然后又做 `keysort`, 并不太必要.

其实用现成的 `pmap/2` 去实现就可以了.

```erlang
pmap(F, L, NProcs) ->
    pmap(F, L, NProcs, []).

pmap(F, L, NProcs, Res) when length(L) > NProcs->
    {Head, Tail} = lists:split(NProcs,L),
    pmap(F, Tail, NProcs, [pmap(F, Head)|Res]);

pmap(F, L, _NProcs, Res) ->
    Res1 = [pmap(F, L)|Res],
    lists:append(lists:reverse(Res1)).
```

由于创建进程和消息往返都有开销, 选择性消息接收会有遍历消息队列的开销,
只有当F是一个耗时的操作, 这些并行化的map才会比较划算.
