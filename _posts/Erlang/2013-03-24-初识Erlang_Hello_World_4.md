---
layout: post
category : Erlang
tagline:
tags : [Erlang, HelloWorld]
title: 初识Erlang，Hello World（4）
update: 2013-9-13
---

前几节的Hello world都是自言自语，今天试试对着别人说。首先是在本机上各个进程之间，
然后是在网络上不同机器之间。

## 进程间的消息：单向接收

Erlang里可以很方便地创建进程，这种进程是超轻量级的，运行于erlang虚拟机内部的，
而与操作系统的进程管理无关的（也就是说你在任务管理器中只会见到一个Erlang进程），
Erlang内的进程创建开销很小，创建一个进程所需的时间仅为微秒级，内存消耗也很少，
一台机上运行数以千计的进程也没问题。

进程之间使用消息进行通讯。示例如下：

    hw15srv() ->
        receive
            {name, Name} ->
                io:format("Hello ~p ~n", [Name]),
                hw15srv()
        end.

    hw15() ->
        Pid = spawn(fun hw15srv/0),
        Pid ! {name, "alice"},
        Pid ! {name, "Bob"}.


`hw15/0`首先是使用`spawn`命令启动了一个`hw15srv/0`进程，。这个命令会返回
所启动的进程的pid，也就是进程的标识符。然后使用`!`向它发送消息。

`spawn`命令有几种用法，对于上例，以下几种方式都可以

    spawn(fun hw15srv/0)
    spawn(fun() -> hw15srv() end)
    spawn(?MODULE, hw15srv, [])

要注意的是，第三种用法（也叫MFA法，即模块、函数、参数法）进行调用的话，
这个被调用的函数（即示例中的`hw15srv`) *必须要export出来*，即便是写在同一个文件中。


`hw15srv/0`的作用是接收其他进程给他发的消息，并显示出来。接收消息使用`receive`，
格式有点象case，先是对收到的消息进行模式匹配，如果匹配的话，该项`->`后的语句。

发送消息的进程和接收消息的进程需要协调好消息的格式，这段程序中，消息是一个元组，
元组的第一个元素是原子`name`，（一般习惯中erlang中的元组的第一个元素用一个原子
作元组的命名），第二个元素是一个字符串。

`hw15srv/0`用的是所谓尾递归服务器的写法，语句段的*最后一行*写上递归函数名，
象这样的服务器一定要符合尾递归的要求，不然的话运行一段时间服务器就会把内存塞爆。

## 进程间的消息：相互对话

上一段程序中，`hw15srv/0`是只收不发的，别人对它说什么，它都没有响应。如果需要
有回应的话，可以这么写：

    hw16srv() ->
        receive
            {From, {name, Name}} ->
                io:format("Server receive: ~p ~n", [Name]),
                From ! {self(), {reply, "Welcome, " ++ Name}},
                hw16srv()
        end.

    hw16() ->
        Pid = spawn(fun hw16srv/0),
        Sid = self(),
        Pid ! {Sid, {name, "alice"}},
        receive
            {Pid, {reply, Msg}} ->
                io:format("Client receive: ~p ~n", [Msg])
        end.

用到的关键字和上一段程序是一样的：创建进程`spawn`，发送消息`!`，接收消息`receive`，
区别仅在于消息的格式。本段程序在消息的前面加上客户端进程的pid，让服务端进程知道
消息是由哪个进程发过来的。Erlang似乎没有获知消息来源的机制，因此需要手工把发送方
的pid写到消息中，方便接收方使用。

在`hw16/0`中，使用了`self()`来获得本进程的pid，然后本进程pid填到消息中，发给
服务端，然后等待服务端发过来的消息，收到后就显示出来。

`hw16srv/0`中则是接收消息，然后提取对方发过来的名字，在前面加上"welcome"，然后
发回给客户端。

## 进程间的消息：RPC

RPC是远程过程调用的缩写，也就是让另一个地方的程序帮你执行，然后把结果返回给你。
Erlang中的发送请求和等待回应可以封装为一个rpc/2的函数：

    rpc(Pid, Request) ->
        Pid ! {self(), Request},
        receive
            {Pid, Response} ->
                Response
        end.

    hw16b() ->
        Pid = spawn(fun hw16srv/0),
        {reply, Reply} = rpc(Pid, {name, "alice"}),
        io:format("Client receive: ~p ~n", [Reply]).

rpc/2把发送请求和等待回应包装起来，用起来更方便了

## 进程间的消息：引用

上面的rpc有个两个问题：第一个问题，是当传入的第一个参数不是一个pid，而是一个注
册了的进程名，信息虽然也能发出去，返回的信息却是pid而不是注册名，无法匹配
`{Pid, Response}` ，因而函数会永远等待下去而不返回。第二个问题，如果本线程给远
方线程发送了几个请求，对方发回了几个回复，那就搞不清哪个回复是对应哪个请求的了。
解决这两个问题的方法是使用一个“引用”，如下：


    rpc2(Pid, Request) ->
        Ref = make_ref(),
        Pid ! { {self(), Ref}, Request},
        receive
            {Ref, Reply} ->
                Reply
        end.

    hw18srv() ->
        receive
            { {From, Ref}, {name, Name}} ->
                io:format("Server receive: ~p ~n", [Name]),
                From ! {Ref, {reply, "Welcome, " ++ Name}},
                hw16srv()
        end.

    hw18() ->
        Pid = spawn(fun hw18srv/0),
        {reply, Reply} = rpc2(Pid, {name, "alice"}),
        io:format("Client receive: ~p ~n", [Reply]).

`rpc2/2`就用了 `make_ref()` 来生成一个唯一的“引用”，`make_ref/0`生成的引用几乎
是不会重复的，文档上的说明是要2E82次调用之后才会发生重复，这实际上是不可能的。

（在这里bs一下《Erlang编程指南》这本书的中文翻译，译得像机翻也就算了，错误还不少，
像这个82在那本书上就被写成了28，我看书时还在怀疑，现代计算机进行2E28次运算，
不就是几秒的事，这make_ref到底行不行。（在211页））


#### 更新(2013年9月13日)

像上面写的那个rpc函数是不带超时选项的, 这在实际代码中有可能会永远都不返回,
程序就卡住了, 要尽量避免这种情况, 可以在后面加上 `after` 子句, 超时不候.






