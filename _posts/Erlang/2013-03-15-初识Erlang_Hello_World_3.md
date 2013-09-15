---
layout: post
category : Erlang
tagline:
tags : [Erlang, HelloWorld]
title: 初识Erlang，Hello World（3）
update: 2013-9-13
---

今天不只要对World说Hello，还要对别人说Hello

    hw(Who) ->
        io:format("Hello ~p ~n", [Who]).

平平无奇的一个函数，对Who说Hello

如果要对一组人说Hello，可以这么做：

    hw10(L) ->
        [hw(X) || X <- L].

    hw10() ->
        L = ["ali", "bob", "cat"],
        hw10(L).

上面是用了列表解析的方法。

也可以用lists:map，效果和上面是一样的

    hw11(L) ->
        lists:map(fun hw/1, L).

    hw11() ->
        L = ["ali", "bob", "cat"],
        hw11(L).

另一个很相似的做法是lists:foreach：

    hw14(L) ->
        lists:foreach(fun hw/1, L).

    hw14() ->
        L = ["ali", "bob", "cat"],
        hw14(L).

这两种做法有什么区别呢？看看输出：

    2> helloworld:hw11().
    Hello "ali"
    Hello "bob"
    Hello "cat"
    [ok,ok,ok]

    10> helloworld:hw14().
    Hello "ali"
    Hello "bob"
    Hello "cat"
    ok

虽然map和foreach都是把同一个函数作用在列表的每一个元素上，
map是会生成一个新的列表来存储结果并返回，（由于hw/1的值就是ok，所以会得到包含3个ok的列表）
而foreach仅会返回ok。

不使用Lists模块的话，可以这么做：

    hw12([H|T]) ->
        hw(H),
        hw12(T);
    hw12([]) ->
        ok.

    hw12() ->
        L = ["ali", "bob", "cat"],
        hw12(L).

这是Erlang中最常见的Head|Tail写法了吧。

也可以用case改写上面的函数

    hw13(L) ->
        case L of
            [] -> ok;
            [H|T] -> hw(H),
                     hw13(T)
        end.

    hw13() ->
        L = ["ali", "bob", "cat"],
        hw13(L).

#### 更新(2013年9月13日)

如果数组很大, `lists:foreach/1`占用的内存会比`lists:map/1`少一些,
因为不需要生成一个同样大小的数组进行返回. 因此, 如果不需要返回值的话,
应该用`lists:foreach/1`, 或者`lists:foldl/3`, 或是自己写尾递归.
