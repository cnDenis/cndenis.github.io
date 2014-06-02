---
layout: post
category : Erlang
tagline:
tags : [Erlang, 代码片段, proplists]
title: 【Erl代码片段】更快的proplists:get_value/3
---

在cowboy的[cowboy_protocol.erl](https://github.com/extend/cowboy/blob/master/src/cowboy_protocol.erl)
文件中看到如这样一下函数:

    ```erlang
    %% Faster alternative to proplists:get_value/3.
    get_value(Key, Opts, Default) ->
        case lists:keyfind(Key, 1, Opts) of
            {_, Value} -> Value;
            _ -> Default
        end.
    ```

做了一下测试, 考虑到proplist一般都不长, 用一个10个元素的列表进行测试,
结果这个函数比 `proplists:get_value/3` 快5倍, 对于更长的列表差别更大.

    bench:get_value/3
    Single Process:   13035909 call per sec,   16777216 times in  1287 ms

    proplists:get_value/3
    Single Process:    2273335 call per sec,    4194304 times in  1845 ms

速度差别原因主要是, `keyfind` 是BIF, 用C实现的,
而 `proplists:get_value/3` 是erlang实现的,
另外 `proplists:get_value/3` 把 atom 当作 {atom, true} 处理,
逻辑比 `keyfind` 复杂一些

这里另外一个测试和讨论:
<http://www.ostinelli.net/erlang-listskeyfind-or-proplistsget_value/>,