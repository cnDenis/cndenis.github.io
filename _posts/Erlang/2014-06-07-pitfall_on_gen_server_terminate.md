---
layout: post
category : Erlang
tagline:
tags : [Erlang, ets, 并发]
title: 【Erlang】gen_server terminate的坑
---

gen_server进程结束时, 会调用`terminate`函数, 但这并不是在任何情况下都成立的.

当gen_server进程主动关闭时, 也就是在回调函数 `handle_xxx` 中返回 `{stop...}`
的时候, `terminate` 是必然被调用的.

当gen_server进程处于监控树中, 被其监控进程关闭时, 情况就不一样了.
只有在这个进程设置了捕获退出信号, 即 `process_flag(trap_exit, true)`,
并且其开启选项中设置了关闭超时时间, 而不是 `brutal_kill`, `terminate`才会被执行.

换句话说, **不捕获退出信号的gen_server被其监控进程关闭时, 会直接死掉,
不执行`terminate`函数!**

看一下 `gen_server` 的源代码关于消息处理的部分:

```erlang
decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
    case Msg of
    {system, From, Req} ->
        sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                  [Name, State, Mod, Time], Hib);
    {'EXIT', Parent, Reason} ->
        terminate(Reason, Name, Msg, Mod, State, Debug);
    _Msg when Debug =:= [] ->
        handle_msg(Msg, Parent, Name, State, Mod);
    _Msg ->
        Debug1 = sys:handle_debug(Debug, fun print_event/3,
                      Name, {in, Msg}),
        handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.
```

可以看出, 当它的父进程执行 `erlang:exit(Pid, Reason) `时,
也就是给它发送 `{'EXIT', Parent, Reason}`,
gen\_server会调用 `terminate` 并结束自己.
gen\_server源代码中并没有设置 `trap_exit`, 也就是默认是不捕获退出信号的,
如果用户不自己设置, 进程一旦接收到 `{'EXIT', Parent, Reason}` 就会立即退出,
没有机会运行 `terminate`.

从这段代码也可以看出, 设置 `trap_exit` 后, 用户并不需要而且没办法在 `handle_info`
里处理来自监控树的父进程的退出信号, gen_server在这个消息在到达 `handle_info`
之前就已经处理了, 直接调用了 `terminate`.

因此, 要确保监控树中的gen_server在进程结束时执行 `terminate`,
需要设置 `process_flag(trap_exit, true)`,
并且要在启动参数中, 设置足够长的结束等待时间.

参考:

* [9 Erlang pitfalls you should know about](http://mazenharake.wordpress.com/2010/10/31/9-erlang-pitfalls-you-should-know-about/), 其中第7条讲的就是这个问题, 有试验代码.

* [Erlang OTP之terminate 深入分析](http://www.qingliangcn.com/2010/08/erlang-otp%E4%B9%8Bterminate-%E6%B7%B1%E5%85%A5%E5%88%86%E6%9E%90/), 庆亮写的分析, 也有试验代码.
其中对于 `simple_one_for_one` 的描述不尽正确, 在目前的R16B中,
`simple_one_for_one` 的子进程信息是存在 `#state.dynamics` 中的,
只要处理时间足够, 关闭监控树是会正常关闭所有子进程的.
