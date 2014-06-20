---
layout: post
category : Erlang
tagline:
tags : [Erlang, 时间]
title: 【Erl代码片段】start_timer/3 发送的消息中的TimerRef的用途
---

Erlang中有两个很相似的延迟发送消息的函数, `send_after/3` 和 `start_timer/3`,
区别仅在于前者返回`Msg`, 后者返回`{timeout, TimerRef, Msg}`.
后者的这个 `TimerRef` 有什么用呢?

[坚强2002的博客](http://www.cnblogs.com/me-sa/archive/2012/03/16/erlang-timer.html)给出了例子:

> 来自代码erl5.8.2\lib\stdlib-1.17.2\src\gen_fsm.erl

```erlang
%% Returns Ref, sends event {timeout,Ref,Msg} after Time
%% to the (then) current state.
start_timer(Time, Msg) ->
    erlang:start_timer(Time, self(), {'$gen_timer', Msg}).

%% Returns Ref, sends Event after Time to the (then) current state.
send_event_after(Time, Event) ->
    erlang:start_timer(Time, self(), {'$gen_event', Event}).

%% Returns the remaing time for the timer if Ref referred to
%% an active timer/send_event_after, false otherwise.
cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            receive {timeout, Ref, _} -> 0
            after 0 -> false
            end;
        RemainingTime ->
            RemainingTime
    end.
```

代码十分清楚, 在取消定时器后, 可以利用这个`Ref`把消息队列中未处理的消息也删掉.

类似的用法在`demonitor/1`也可以看到. 取消掉一个monitor, 可以使用类似的方法,
把消息列队中的未处理的`'DOWN'`消息删掉:

```erlang
demonitor(MonitorRef),
receive
    {_, MonitorRef, _, _, _} ->
        true
after 0 ->
        true
end
```

但是这样的代码不用自己写, Erlang自带的 `demonitor(MonitorRef, [flush])`
就等价于上面的代码.

为什么`demonitor`提供了`flush`参数, 而`cancel_timer`没有呢?
因为 `monitor/2` 产生的消息只能发给调用它进程,
而 `start_timer` 产生的消息可以发给任意进程.
由于Erlang只允许读取本进程的消息队列, 不能干涉别的进程,
所以 `cancel_timer` 就不提供 `flush` 参数, 避免误解.

不过, 类似的 `unlink/1` 却也没有相应的 `flush` 参数可以用, 要自己刷消息队列:

```erlang
unlink(Id),
receive
    {'EXIT', Id, _} ->
        true
after 0 ->
        true
end
```

呵呵.
