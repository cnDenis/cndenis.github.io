---
layout: post
category : Python
tagline:
tags : [Python, pygraphviz]
title: 在Windows下安装pygraphviz
---

用 pip 或是 easy_install 在Windows 下安装 pygraphviz，
会出现`OSError: Error locating graphviz.`的错误。
解决方法如下：

1.  先下载并安装 Graphviz 。下载地址 <http://www.graphviz.org/Download_windows.php>。

1.  在 <http://pypi.python.org/pypi/pygraphviz/> 下载 pygraphviz 原代码，解压。

1.  修改 setup.py ,

找到

    library_path=
    include_path=

修改为

    library_path=r"C:\Program Files\Graphviz 2.28\lib\release\lib"
    include_path=r"C:\Program Files\Graphviz 2.28\include\graphviz"


其中 `C:\Program Files\Graphviz 2.28` 改为 Graphviz 的安装地址。

1.  修改 agraph.py , 在函数 `def_run_prog` 中找到:

    `cmd=' '.join([runprog,args])`

修改为：

    `cmd=' '.join(['"%s"' % runprog,args])`

1.  在命令行中执行

    `setup.py install`

安装完成。

出处： <https://networkx.lanl.gov/trac/ticket/117>，原出处对`cmd`行的修改是有错误的，这里修正了。

