---
layout: post
category : Python
tagline:
tags : [Python, Tk]
title: 利用tkFileDialog.askopenfilenames获取多个文件名
---

用Python写小脚本，要选择文件，最简单的方式莫过于使用内置的Tk的控件了。如果需要同时选择多个文件，可以使用`tkFileDialog.askopenfilenames`，如下：

```py
    import tkFileDialog
    fns = tkFileDialog.askopenfilenames(filetypes=[("text file", "*.txt"),("all","*.*")])
    print(repr(fns))
```

但问题是，在Python 2.6以后的版本，`tkFileDialog.askopenfilenames`返回的居然是字符串，类似于这样：

```py
    u'C:/Python27/LICENSE.txt C:/Python27/NEWS.txt C:/Python27/README.txt'
```

如果每个文件的路径中都没有空格，用`fps.split()`就可以分解字符串得到文件名的列表，如果文件名或文件路径中包含空格，这个字符串将很不好解析。解决方法如下：

```py
    import Tkinter
    import tkFileDialog
    import os

    master = Tkinter.Tk()
    master.withdraw()  # 不显示界面主窗口

    fnstr = tkFileDialog.askopenfilenames(filetypes=[("text file", "*.txt"),("all","*.*")])

    fns = master.tk.splitlist(fnstr) # 把多个文件名字符串分割成元组

    print(repr(fns))
```

这样得到的就是元组了，

```py
    ('C:/Python27/LICENSE.txt', 'C:/Python27/NEWS.txt', 'C:/Python27/README.txt')
```

然后就可以用`for`得到每个文件名了。

如果文件名中包含中文，`Tkinter.Tk.tk.splitlist`分割完后的文件名可能会不能用，需要先把文件名`encode`后传进去，分割完再`decode`。代码如下：

```python
    import sys
    sysencode = sys.getfilesystemencoding()
    fnsen = fnstr.encode(sysencode)
    fnss = master.tk.splitlist(fnsen)
    fns = [unicode(f, encoding=sysencode) for f in fnss]
```

这样就可以了。

以上内容参考了：[Parsing the results of askopenfilenames()?](http://stackoverflow.com/questions/4116249/parsing-the-results-of-askopenfilenames) 和 [tkinter - askopenfilenames returns string instead of tuple in windows 2.6.1 release](http://bugs.python.org/issue5712)

