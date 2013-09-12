---
layout: post
category : Tools
tagline:
tags : [Sublime]
title: 我的 Sublime Text2 配置
update: 2013-9-11
---

Sublime Text 2是最近很火的一个代码编辑器，我也跟风玩了一下，果然很好用。方便的插件管理器加上丰富的插件，可以很容易把它调教得很顺手。几乎每项功能都可以用鼠标和键盘两种方式来实现，方便各种习惯的人。Sublime Text 2 的介绍与推荐网上已经有很多，这里记一下我安装的包和配置。

<strike>
[Package Control](http://wbond.net/sublime_packages/package_control)

包管理器是必备的，新下载的Sublime Text 2 第一个装的肯定是这个，有了它，装其他的包就很方便了。

安装方式有两种，第一种是在线下载安装：在 Sublime Text 2 中按下`` ctrl+` ``（就是大键盘数字1左边的那个键），拷贝以下命令到窗口下部的终端中，

    import urllib2,os; pf='Package Control.sublime-package'; ipp=sublime.installed_packages_path(); os.makedirs(ipp) if not os.path.exists(ipp) else None; urllib2.install_opener(urllib2.build_opener(urllib2.ProxyHandler())); open(os.path.join(ipp,pf),'wb').write(urllib2.urlopen('http://sublime.wbond.net/'+pf.replace(' ','%20')).read()); print 'Please restart Sublime Text to finish installation'

回车运行，重启 Sublime Text 2，即可。另一种方法是手动安装，看[这里](http://wbond.net/sublime_packages/package_control/installation)，个人觉得不太必要，在没有网络的环境下，包管理器也没什么用了。

在安装完包管理器之后，只要按下`ctrl+shift+p`，输入`ip`，选择“Package Control: Install Package”，然后输入要安装的包的名称，就可以在线安装了。
</strike>
(因为这东西在后台更新曾经导致我的Sublime完全崩溃, 已经弃用)

2. [Soda](https://github.com/buymeasoda/soda-theme/)

    这是个主题，也就是Sublime自身的皮肤，比自带的要漂亮一些。在包管理器中装上之后，打开配置文件`Preferences` -> `Settings - User`，加上一行`"theme": "Soda Light.sublime-theme"`或者 `"theme": "Soda Dark.sublime-theme"`。前面一个是亮色主题，后面一个是暗色主题。我喜欢暗色，看起来比较有黑客的调调。

3. [Made of code](http://madeofcode.com/posts/29)

    这个是代码高亮方案。包管理器中没有这个方案，官网似乎要翻墙才能上，国内可以在[这里](https://github.com/kumarnitin/made-of-code-tmbundle/tree/master/Themes)下载。把下载到的`Made of Code.tmTheme`放到Sublime Text 2的安装目录下的`Data\Packages\Color Scheme - Default`文件夹，然后就可以在`Preferences` -> `Color Scheme`选择`made of code`。

4. [Markdown Build](https://github.com/erinata/SublimeMarkdownBuild)
5. [MarkDown preview](https://github.com/revolunet/sublimetext-markdown-preview)

    这两个是写Markdown必备的。可以在包管理器中安装。装完之后，写作Markdown时（右下角显示语法为Markdown），可以按`ctrl+b`，直接就会生成HTML，并在浏览器中显示。

6. [SideBarEnhancements](https://github.com/titoBouzout/SideBarEnhancements)

    这是用来增强左边的侧边栏。左侧边栏可以在`View` -> `Side Bar` -> `Show Side Bar`中打开，可以用`Project` -> `Add Folder to Project...`往侧边栏加入常用的文件夹。装完这个插件，侧边栏的右键菜单会多一些功能，挺实用的。

7. [SublimeLinter](https://github.com/SublimeLinter/SublimeLinter)

    这是用来在写代码时做代码检查的。可以在包管理器中安装。写Python程序的话，它还会帮你查代码是否符合[PEP8](http://www.python.org/dev/peps/pep-0008/)的要求。有问题有代码会出现白框，点击时底下的状态栏会提示出什么问题。

8. [Python PEP8 Autoformat](https://bitbucket.org/StephaneBunel/pythonpep8autoformat)

    这是用来按PEP8自动格式化代码的。可以在包管理器中安装。如果以前写程序不留意的话，用SublimeLinter一查，满屏都是白框框，只要装上这个包，按`ctrl+shift+r`，代码就会按PEP8要求自动格式化了，一屏的白框几乎都消失了。

9. [gbk](https://github.com/akira-cn/sublime-gbk)

    听说Sublime Text 2读GBK编码的文件会乱码，所以我早早就把这个给装上了，没见过乱码，不知管用不。


10. [Bracket Highlighter](https://github.com/facelessuser/BracketHighlighter)

    这是用来做括号匹配高亮的，可以在包管理器中安装。Sublime Text 2自带的括号匹配只有小小的一横线，太不显眼了，这个可以让高亮变成大大的一坨，不过我觉得它大得会盖住光标了。*(现在已经不用了)*

11. [Terminal](http://wbond.net/sublime_packages/terminal)

    这是用来在当前文件所在位置打开终端的。可以在包管理器中安装。对于Windows用户，安装完后，要先在`Preferences` -> `Package Setting` -> `Terminal` -> `Settings - Default`里，设置`"terminal": "cmd",`。（如果喜欢用ipython的话，也可以改为ipython）之后只要按下`ctrl+shift+t`，即可在当前文件位置打开命令行窗口。

1. [sublime-markdown-extended](https://github.com/jonschlinkert/sublime-markdown-extended)

    这个是用来支持放在Markdown文件头部的yaml信息的代码高亮, 便于使用jekyll写Blog.

1. [Alignment](https://github.com/wbond/sublime_alignment)

    用来做代码对齐的, 最主要是做等号对齐. 我的设置把默认快捷键取消掉, 自定义为 `{ "keys": ["alt+a"], "command": "alignment" }`, 按`alt+a`进行等号对齐.

1. [Goto Documentation](https://github.com/kemayo/sublime-text-2-goto-documentation)

    这是用来快速查文档的, 我的设置把默认快捷键取消掉, 自定义为 `{"keys": ["f1"], "command": "goto_documentation"}`, 按F1就可以跳转到光标位置的文件.

    我把这个插件Fork出来改了几行, 添加了对 Erlang 的支持, 在[这里](https://github.com/cnDenis/sublime-text-2-goto-documentation)

1. [SublimeJEDI](https://github.com/srusskih/SublimeJEDI)

    这个似乎是目前ST2上最好用的Python代码补全插件, 不过它的快捷键默认设置比较坑爹, 我全部取消掉, 具体设置看我的 GIST: [键盘设置](https://gist.github.com/cnDenis/6525289) 和 [鼠标设置](https://gist.github.com/cnDenis/6525329)

1. [SublimeTmpl](https://github.com/kairyou/SublimeTmpl)

    新建文件时的模板, [我的GIST](https://gist.github.com/cnDenis/6525289)里有在不同语法环境下打开不同模板文件的设置. 对于经常要在多种语言下工具的人比较方便


我还安装了另外的一些包，还没怎么用，这里就不详细介绍了：

* [KeymapManager](https://github.com/welefen/KeymapManager) 插件快捷键管理，快捷键`ctrl+alt+k`
* [INI file Syntax Highlighting](https://github.com/clintberry/sublime-text-2-ini)  ini文件代码高亮
* [ApacheConf.tmLanguage](https://github.com/colinta/ApacheConf.tmLanguage) Apache配置文件代码高亮
* [sublime-phpsnippets](https://github.com/stuartherbert/sublime-phpsnippets)

* [jQuery](https://github.com/SublimeText/jQuery) jQuery
* [Tag](https://github.com/SublimeText/Tag) HTML标签补完、格式化

还有一些我装了又删掉的包，不是说它们做得不好，而是我不喜欢或是在我的机上莫明运行不能，不折腾了。

* [Pep8Lint](https://github.com/dreadatour/Pep8Lint) PEP8检查，其实用SublimeLinter检查就够了
* [SublimeCodeIntel](https://github.com/Kronuz/SublimeCodeIntel) 代码补全，这东西上百兆的巨大体积就不说了，问题是在我的比较大的代码文件上工作不正常，不知怎么办。反正Sublime Text 2本身也自带一点代码补完功能，这个不用也罢了。

Sublime Text 2 我最常用快捷键是以下几个：

* `ctrl+shift+p` 基本上啥功能都在里面了
* `ctrl+r` 快速定位到指定类/函数/标题
* `ctrl+g` 快速跳转到某一行号，在debug时很常用
* `ctrl+b` build
* `ctrl+shift+t` 在当前位置打开终端（需安装terminal）
* `ctrl+shift+r` 按PEP8格式化代码（需安装Python PEP8 Autoformat）

还有 `ctrl+s`、`ctrl+a`、`ctrl+z`、`ctrl+x`、`ctrl+c`、`ctrl+v`这些和其他软件一样，就不用多说了

Sublime Text 2 的配置文件的修改可以参考[Sublime Text 2 设置文件详解](http://www.feelcss.com/sublime-text-2-settings.html)。默认其实已经挺好，我是在`Preferences` -> `Setting User`加了这几行：

    "default_line_ending": "unix",
    "rulers":[78],
    "translate_tabs_to_spaces": true

分别是用来设定行尾换行符、显示长行标尺和按tab输出空格。

另外有一个很实用的设置，在括号中使用Enter跳出括号，在`Preferences` -> `Key Bindings User`中加上

    [
        {"keys": ["enter"], "command": "move", "args": {"by": "characters", "forward": true}, "context":
            [
                { "key": "following_text", "operator": "regex_contains", "operand": "^[)\\]\\>\\'\\\"\\ %>\\}\\;\\,]", "match_all": true },
                { "key": "preceding_text", "operator": "not_regex_match", "operand": "^.*\\{$", "match_all": true  },
                { "key": "auto_complete_visible", "operator": "equal", "operand": false }
            ]
        }
    ]

暂时先这么多，以后再补充吧。

参考：

* [Sublime Text 2 入门及技巧](http://lucifr.com/2011/08/31/sublime-text-2-tricks-and-tips/)
* [配置Sublime Text使用Markdown，语法高亮,生成HTML](http://timewilltell.me/node/15)
* [一些必不可少的Sublime Text 2插件](http://www.qianduan.net/essential-to-sublime-the-text-2-plugins.html)
* [Sublime Text 2 设置文件详解](http://www.feelcss.com/sublime-text-2-settings.html)
* [sublime 怎样跳出括号？](http://ruby-china.org/topics/4824)

## 更新:

现在已经不用 Package Control 了, 其实 ST2 的插件多数都在 Github上, 自己上去找插件, 放到 Packages 文件夹里就好, 用 Github 进行升级和管理, 比什么都好.

这篇文章是1月份写的, 用到现在, 我的 ST2 重装过一两回, 之前主要是用来写Python, 现在还要兼顾 Erlang/PHP/JavaScript/HTML, 还有多了这么一些插件:

