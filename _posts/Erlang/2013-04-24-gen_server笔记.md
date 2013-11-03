---
layout: post
category : Erlang
tagline:
tags : [Erlang, gen_server]
title: gen_server笔记
update: 2013-9-13
---

gen_server是erlang的OTP框架中最常用的“行为模式”了吧，至少几本erlang教材都是首
先介绍这个。

<strike>
这东西用来做什么的呢？或者说，为什么要用这东西呢？由于我接触这东西不过几天，
理解尚非常粗浅，就我看来，用gen_server有以下几个好处：

* *面向对像，封装数据与方法*。gen_server内部需要维护一个状态State，并提供各种函数
给别人调用，就类似于其他语言中的“类”一样，把数据和方法封装在一起，防止数据被非法
改动。

* *简化调用，屏蔽通讯*。erlang中如果要自己实现远程过程调用（RPC）的话，需要自己
定义消息格式，自己编写封装与解包的代码，还要处理各种异常问题，这些gen_server
都帮我们做好了，只需要像平常使用函数一样直接调就用行了，省时省力还不出错。

* *热代码替换等高级功能*。 热代码替换是erlang大力宣传的一项特色功能，不停机维护
在生产上是非常美妙的事。不过这是高级功能，初学暂时用不上，书上也没多讲。
</strike>

## gen_server模板

gen_server是可以使用模板来写的，如下：

```erlang
    -module().

    -behaviour(gen_server).

    %% API
    -export([]).

    %% gen_server callbacks
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
           terminate/2, code_change/3]).

    -define(SERVER,?MODULE).

    start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    init([]) ->
        {ok, State}.

    handle_call(_Request, _From, State) ->
        {reply, Reply, State}.

    handle_cast(_Msg, State) ->
        {noreply, State}.

    handle_info(_Info, State) ->
        {noreply, State}.

    terminate(_Reason, _State) ->
        ok.

    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
```

含义如下：

-module 是模块名，就是文件名。

-behaviour 是指定“行为模式”，在这里就是gen_server，作用在于用于检查这个模块是否
实现gen_server的所有接口。也就是 `init/1`, `handle_call/3`, `handle_cast/2`,
`handle_info/2`, `terminate/2`, `code_change/3`这六个函数。缺少的话编译器就会
报错。

-export 有两个，第一个里面写的是API，也就是供别人调用的接口，第二个是gen_server
的接口，就是上面说的那六个。其实要写到同一个export里也可以，不过分开写比较清楚。

接下来就是各API函数，一般来说就是封装、调用各用回调函数的包装函数。

再往后就是六个回调函数的具体实现。回调函数是负责具体干活的。

大概可以做这么个比喻吧，如果整个模块是个昆虫，API是就头，gen_server就是身子，而
回调函数是六条腿。你告诉头说要去哪里，腿就运动起来，整条虫子就跑起来了。身子是
把各条腿粘起来，否则单独的腿只能在原地抽筋，哪都去不了。

## 启动服务器

用来启动服务器的有`start/3`,`start/4`,`start_link/3`,`start_link/4`这四个函数。
使用这些start函数之后，就会产生一个新的进程，也就是一个gen_server服务器。这些
start函数的正常情况下返回值是`{ok,Pid}`，`Pid`就是这个新进程的进程号。
带link与不带的区别在于是否跟父进程建立链接，换种说法是，新启动的进程死掉后，
会不会通知启动他的进程（父进程）。

start函数可以四个参数`(ServerName, Module, Args, Options)`：

* 第一个参数`ServerName`是服务名，
是可以省掉的。具有相同服务名的模块在一个节点中只能启动一次，重复启动会报错，为
`{error, {already_started, Pid}}`。具有服务名的服务进程可以使用服务名来调用，
没有服务名的只能通过进程号pid来调用了。通常有名字的服务进程会使用模块名做为
服务名，即上面模板中定义的宏`-define(SERVER, ?MODULE)`，然后在需要使用服务名的
地方填入`?SERVER`.

* 第二个参数`Module`是模块名，一般而言API和回调函数是写在同一个文件里的，所以就用
`?MODULE`，表示本模块的模块名。

* 第三个参数`Args`是回调函数`init/1`的参数，会原封不动地传给`init/1`。

* 第四个参数`Options`是一些选项，可以设置debug、超时等东西。

start是对应的回调函数是`init/1`，一般来说是进行服务器启动后的一些初始化的工作，
并生成初始的状态State，正常返回是{ok, State}。这个State是贯穿整个服务器，
并把所有六个回调函数联系起来的纽带。它的值最初由`init/1`生成，
此后可以由三个handle函数修改，每次修改后又要放回返回值中，
供下一个被调用的handle函数使用。
如果`init/1`返回`ignore`或`{stop, Reason}`，则会中止服务器的启动。

有一点细节要注意的是，API函数和回调函数虽然习惯上是写在同一个文件中，但执行函数
的进程却通常是不一样的。在上面的模板中，`start_link/0`中使用`self()`的话，显示
的是调用者的进程号，而在`init/1`中使用`self()`的话，显示的是服务器的进程号。

## 使用服务器

三个handle开头的回调函数对应着三种不同的使用服务器的方式。如下：

    gen_server:call     -------------   handle_call/3
    gen_server:cast     -------------   handle_cast/2
    用！向服务进程发消息-------------   handle_info/2

call是有返回值的调用；cast是无返回值的调用，即通知；而直接向服务器进程发的
消息则由handle_info处理。

### call

call是有返回值的调用，也是所谓的同步调用，进程会在调用后一直等待直到回调函数返回为止。
它的函数形式是 `gen_server:call(ServerRef, Request, Timeout) -> Reply`，

* 第一个参数`ServerRef`是被调用的服务器，如果是服务器名，或是服务器的pid。
* 第二个参数`Request`会直接传给回调函数`handle_call`
* 最后一个参数`Timeout`是超时，是可以省略的，默认值是5秒。

在多节点的情况下，还有机会使用到`multi_call`，用来向各节点上的同具有相同注册名
的服务进程发起调用。（这个函数在文档上的表述有点让人难以理解，详见
[这里](http://erlang.2086793.n4.nabble.com/Question-about-gen-server-multi-call-2-td2104905.html)）

call是用来指挥回调函数`handle_call/3`干活的。具体形式为
`handle_call(Request, From, State)`。

* 第一个参数`Request`是由call传进来的，是写程序时关注和处理的重点。
* 第二个参数`From`是gen_server传进来的，是调用的来源，也就是哪个进程执行了call。
`From`的形式是`{Pid, Ref}`，`Pid`是来源进程号，而`Ref`是调用的标识，每一次调用
都不一样，用以区别。有了Pid，在需要向来源进程发送消息时就可以使用，但由于call
是有返回值的，一般使用返回值传递数据就好。
* 第三个参数`State`是服务器的状态，这是由init或是其他的handle函数生成的，可以根据需要进
行修改之后，再放回返回值中。

call对应的回调函数`handle_call/3`在正常情况下的返回值是`{reply, Reply, NewState}`，
`Reply`会作为call的返回值传递回去，`NewState`则会作为服务器的状态。
另外还可以使用`{stop, Reasion, State}`中止服务器运行，这比较少用。

使用call要小心的是，**两个服务器进程不能互相call**，不然会死锁。

### cast

cast是没有返回值的调用，一般把它叫做通知。它是一个“异步”的调用，调用后会直接收到
`ok`，无需等待回调函数执行完毕。

它的形式是`gen_server:cast(ServerRef, Request)`。参数含义
与call相同。由于不需要等待返回，所以没必要设置超时，没有第三个参数。

在多节点的情况下，可以用`abcast`，向各节点上的具有指定名字的服务进程发通知。
（奇怪的是为啥为不叫`multi_cast`，明明长得跟`multi_call`很像的）

cast们对应的回调函数是`handle_cast/2`，具体为：`handle_cast(Msg, State)`。
第一个参数是由cast传进去的，第二个是服务器状态，和call类似，不多说。

`handel_cast/2`的返回值通常是`{noreply, NewState}`，这可以用来改变服务器状态，
或是`{stop, Reason, NewState}`，这会停止服务器。通常来说，停止服务器的命令用
cast来实现比较多。

### 原生消息

原生消息是指不通过call或cast，直接发往服务器进程的消息，有些书上译成“带外消息”。
比方说网络套接字socket发来的消息、别的进程用!发过来的消息、跟服务器建立链接的进程死掉了，
发来`{'EXIT', Pid, Why}`等等。一般写程序要尽量用API，不要直接用!向服务器进程发消息，
但对于socket一类的依赖于消息的应用，就不得不处理原生消息了。

原生消息使用`handle_info/2`处理，具体为`handle_info(Info, State)`。其中Info是
发过来的消息的内容。回复和`handle_cast`是一样的。

# 停止服务器

上面介绍的handle函数返回{stop,...}，就会使用回调函数`terminate/2`进行扫尾工作。
典型的如关闭已打开的资源、文件、网络连接，打log做记录，通知别的进程“我要死啦”，
或是“信春哥，满血复活”：利用传进来的状态State重新启动服务器。

最简单的就是啥都不干，返回ok就好了。

--------------

参考资料和资源：

[gen_server的文档](http://www.erlang.org/doc/man/gen_server.html)

[OTP设计原则中gen_server的介绍](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)

#### 更新(2013年9月13日)

虽然笔记跟抄书没啥区别, 但抄一遍总会加强记忆的, 所以还是贴出来.

call 操作会阻塞, 而且有死锁的可能性, 用起来要小心.
