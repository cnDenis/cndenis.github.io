---
layout: post
category : Erlang
tagline:
tags : [Erlang, supervisor, OTP]
title: OTP监控树笔记
---

这是Learn You Some Erlang中的[Building an Applications with Otp](http://learnyousomeerlang.com/building-applications-with-otp)的笔记.

在文中, OTP被戏称为Onion Theory Platform, 监控树像洋葱一样, 一层又一层.

OTP的哲学是工人-监工, 工作进程做错事就让它死掉, 监督进程再开启新的工作进程来干活.
这样做有个让人觉得很麻烦的问题: 工作进程死掉了, 它的状态怎么办? 文中进行了讨论.
状态可以分成三类:

* 静态的状态. 这从配置文件读回来就可以, 没什么问题.

* 动态但可重新获取的状态. 就是那些可以利用初始状态计算而来的东西, 重新算就行了.

* 动态且不可重新获取的状态. 比如说用户的输入, 外部事件等等.

如果工作进程只有前两类状态, 用监工的方法很容易的处理, 死了重启, 恢复状态就可以了.

第三类的状态需要放到不允许出错的地方(error kernel), 其实就是用try...catch包起来.

作者示范了一个进程池的设计.

所谓进程池, 就是可以容纳一定数量的进程同时运行, 当同时运行的进程数达到限制时,
把工作放入队列进行排队, 等待有空位时再开始新的任务.

做这么一个进程池, 需要有一个服务进程来计数同时运行的工作进程数,
以及保存准备等待运行的队列.

那用这个服务进程来监控工作进程好不好呢? 可以, 但缺点是服务进程任务太重,
而且和现有的监督者进程功能重复.

一个比较好的方案是, 用一个监督进程`worker_sup`监视所有工作进程,
这个监督进程和进程池服务进程一起放到上一层的监督进程`ppool_sup`下, 如图:

<img src=http://learnyousomeerlang.com/static/img/ppool_sup.png>

如果让所有进程池都放在同一个`ppool_sup`下, 当一个进程池频繁出错的话,
这个`ppool_sup`会被关掉, 所有进程池都会被关掉, 这是很不好的事情.
所以要为每个进程池单独开一个监控进程. 变成这样:

<img src=http://learnyousomeerlang.com/static/img/ppool_supersup.png>

底下的具体实现部分就不记了.

实现这么看起来并不太复杂的功能, 从顶层到worker, 总共四层, 不愧是洋葱理论平台.