---
layout: post
category : Erlang
tagline:
tags : [Erlang, HelloWorld]
title: 初识Erlang，Hello World (1)
update: 2013-9-13
---

这两天开始玩Erlang，咱们从Hello World开始吧。

先是要到官网<http://www.erlang.org/download.html>下载安装包，安装上，设置好系
PATH，然后打开命令行，输入`erl`，如果看到类似于

    Eshell V5.10.1  (abort with ^G)
    1>

的话，就表示Erlang安装成功。这东西叫做Erlang的Shell。

然后就是最经典的Hello World：

随便用一个文本编辑器输入：

    -module(helloworld).
    -export([hw/0]).

    hw() -> io:format("Hello World! ~n").

保存为helloworld.erl，然后在命令行下进入该文件所在目录，进入Erlang Shell，输
入`>`后的命令：

    1> c(helloworld).
    {ok,helloworld}
    2> helloworld:hw().
    Hello World!
    ok

Yeah，Hello World完成！（我会告诉你我用了20分钟干这事么，下面是各种坑的介绍）

    % 首先，Erlang的注释符是百分号哦
    -module(helloworld).
    % -module是必要的，这可不是Bash或Python，上来就可以Echo或Print
    % 然后，helloworld 必须与文件名相同（除了不用加.erl）
    % 然后，行末的句点是必须的，虽然好不显眼

    -export([hw/0]).
    % -export也是必须的，这是用来说明哪些函数可以在外部被调用
    % 括号中必须是一个列表，换言之，加方括号
    % hw是函数名，斜杠后面是参数的数目，两者总是组合指明一个函数。
    % 参数数目不同的，即使同名，也是两个不同的函数
    % 还是记得句尾要加点

    hw() -> io:format("Hello World! ~n").
    % 这个是函数了
    % 函数名前面不用任何修饰，类似于其他语言中的Fun，def之类在这里是不必要的
    % 函数名是一个“原子”，换言之就是一个以小写字母开头的单词，可后继大小写字母下划线和@号
    % 或是你足够蛋疼的话，可以用单引号括起来的任意东西，比方说 '*&^%hw2'
    % 还有，不要拿fun当函数名，这个词另有用途
    % 函数名后面加括号，如果有参数，写在括号中
    % 括号后是 -> 这不是C语言中的指针，而是表示后面是函数的内容了
    % io:format可以当print，其实就是io模块里的format函数
    % 看清楚模块名和函数名之间是冒号，别弄成其他语言中的点
    % 双引号中是字符串，Erlang中的字符串实质上是数组
    % ~n是换行，用波浪线转义
    % 最后还是别忘记了是句点

这个erl文件的坑说完了，然后是Erlang Shell中的。

想用erl xxxxx.erl来执行是徒劳的，这货不是sh也不是python也不是pl，这么做你会
进入Erlang Shell。

然后先是要`c(helloworld).`编译，记得不要忘记句点。然后同一个目录下就会出现一个
同名的beam文件。

然后键入`helloworld:hw().`，回车没反应？又忘记句点了，补上，Hello World终于蹦
出来了。
