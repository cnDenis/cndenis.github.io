---
layout: post
category : Erlang
tagline:
tags : [Erlang, HelloWorld]
title: 初识Erlang，Hello World (2)
update: 2013-9-13
---

昨天费了老大劲终于把Hello World打出来了，今天我们继续。

## 说N遍Hello World，递归的使用

Hello World说一遍怎么够呢，说N遍吧。代码如下：

    -module(helloworld).
    -export([hw3/1]).
    % 打印N遍Hello world
    hw3(N) when N > 0 ->
        io:format("Hello World ~n"),
        hw3(N-1);

    hw3(0) -> ok.

这有什么好坑的呢：

    hw3(N) when N > 0 ->

when后面是一个guard，仅在它为true时函数会被执行。

保护元所能使用的语句是受限的，不能用自定义的函数。

    io:format("Hello World ~n"),
    hw3(N-1);

这一个函数体有多条语句，中间需要用逗号隔开

    hw3(0) -> ok.

hw3/1这个函数有几个函数子句，根据参数的模式识别和when保护元的值来选择使用函数体。

不同的函数体之间使用分号，最后一个末尾使用句点。

啥都不干用什么好呢？[stackoverflow](http://stackoverflow.com/questions/1126755/in-erlang-is-there-a-way-to-create-an-empty-function)
上说用ok. 那就用ok吧。

## 再说N遍Hello World，使用case

如果不想用多个函数子句的话，可以用case

    hw5(N) ->
        case N of
            X when X > 0 ->
                io:format("Hello World ~n"),
                hw5(N-1);
            0 -> ok
        end.

case的语法是：

    case 条件表达式 of ->
        模式1 -> 语句段1;
        模式2 -> 语句段2;
        ....
        模式N -> 语句段N
    end

继续一行一行看：

    case N of

N是条件表达式，也就是说运行case中的哪一条子句会根据N的值来决定。

只有一个子句会被执行，这点跟C语言不一样。

而且如果全部子句的模式匹配都失败的话，会抛出错误。所以要考虑是否使用catch all的语句。

条件表达式与子句之间是用of来分隔的，注意不要顺手打冒号了。

    X when X > 0 ->

模式后面可以接when，也可以不用。

这一行是这么运行的：首先是对X和N进行模式匹配，即`X=N`，匹配是成功的，因为
变量X没有赋值，所以X绑定到N，然后由于保卫元`X>0`为true，所以执行该子句。

    io:format("Hello World ~n"),
    hw5(N-1);

注意模式与语句段之间是用 `->` 分开的，中间的case语句段需要以分号结束

    0 -> ok

递归结束条件，不可忘记的。最后一个case子句，也就是*end之前不需要加标点*。

    end.

case语句需要以end结束，后面这个句点是表示hw5整个函数的结束，不是case语法的一部分


## 继续说Hello World，使用if

if可以视为没有条件表达式和of的case

    hw6(N) ->
        if
            N > 0 ->
                io:format("Hello World ~n"),
                hw5(N-1);
            N == 0 ->
                ok
        end.

if的语法格式是：

    if
        保护元1 -> 语句1;
        保护元2 -> 语句2;
        .......
        保护元N -> 语句N
    end

if会执行第一个保护元为真的语句。

要注意的东西跟case没什么区别，就不啰嗦了。


## 继续Hello World，使用列表解析

这似乎是最简洁的做法了

    hw7(N) ->
        [hw() || _ <- lists:seq(1, N) ].

这句话换相当于Python中的 [hw() for _ in range(1, N + 1)]。

意思是把（1，2，3...N）中的每一个值都装到变量_之后，运行hw()。由于hw中不调用
seq中产生的值，所以拿_这样的一个坑来装。

`lists:seq`是产生一个连接序列的数组，与Python中的range不同的是，它是既包头又包
尾的，也是就说产生（1，2，3...N）。


## 继续Hello World，山寨for

Erlang最令人觉得奇怪的是它没有循环语句，不过这不妨碍我们直接山寨一个。

    for(I, N, F) when I < N ->
        F(),
        for(I + 1, N, F);
    for(N, N, F) ->
        F().

    hw8(N) ->
        for(1, N, fun hw/0).

    hw9(N) ->
        for(1, N, fun() -> hw() end).

这个山寨的for用了高阶函数的概念，它的F参数是是一个函数。注意`hw8/1`和`hw9/1`中
的写法，我碰壁了好久，后来问了程序老大才解决的。

`hw8/1`是把函数名当参数后用，必须要使用fun关键词，而且要指明函数的目数，
这与Python不一样。Python一个函数名只能绑定一个函数，给出名字就唯一确定了绑定
对象。而Erlang允许同名不同目的函数，所以要指明目数。

`hw9/1`则是产生一个匿名函数作为参数，注意匿名函数的写法。以fun作为函数名，
其余跟一般函数没什么区别。注意fun后面的括号不要漏掉，即使没有参数。

#### 更新(2013年9月13日)

函数匹配是有顺序的, 像这篇最开头的`hw/3`, 如果把`hw3(0)`写在前面,
就可以不需要`when N > 0`了. 但这么做代码并不健壮, 遇上 N<0 就会出死循环.
因此, 还是加上`when N > 0`吧.




