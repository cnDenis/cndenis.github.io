---
layout: post
category : Web
tagline:
tags : [Apache]
title: Apache启动时报443端口被占用的处理方法
---

下午在自己机上装了Apache2.4, 启动时说443端口被占了:

    (OS 10048)通常每个套接字地址(协议/网络地址/端口)只允许使用一次。  : AH00072: mak
    e_sock: could not bind to address [::]:443
    (OS 10048)通常每个套接字地址(协议/网络地址/端口)只允许使用一次。  : AH00072: mak
    e_sock: could not bind to address 0.0.0.0:443
    AH00451: no listening sockets available, shutting down

杀掉占用443的那个程序当然可以, 另一种方法是把443改成另外的不常用端口:
如[这篇文章](http://congjl2002.iteye.com/blog/1497640)介绍的,
还有一种方法是直接关闭Apache的SSL模块, 反正在我自己的机上用不着.

打开`httpd.conf`, 找到加载ssl_module的那一行, 加#号注释掉就好了:

    # LoadModule ssl_module modules/mod_ssl.so

