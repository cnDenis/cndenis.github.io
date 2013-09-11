---
layout: post
category : Python
tagline:
tags : [Python, BeautifulSoup]
title: 用Python解析HTML，BeautifulSoup使用简介
---

Beautiful Soup，字面意思是美好的汤，是一个用于解析HTML文件的Python库。主页在 <http://www.crummy.com/software/BeautifulSoup/> ， 下载与安装无需啰嗦，这里就介绍一下它的使用吧。

-------------

##装汤——Making the Soup

首先要把待解析的HTML装入BeautifulSoup。BeautifulSoup可以接受文件句柄或是字符串作为输入：

    from bs4 import BeautifulSoup
    fp = open("index.html")
    soup1 = BeautifulSoup(fp)
    soup2 = BeautifulSoup("<html>data</html>")

------------

##汤料——Soup中的对象

### 标签（Tag）
标签对应于HTML元素，也就是应于一对HTML标签以及括起来的内容（包括内层标签和文本），如：

    soup = BeautifulSoup('<b class="boldest">Extremely bold</b>')
    tag = soup.b

soup.b就是一个标签，soup其实也可以视为是一个标签，其实整个HTML就是由一层套一层的标签组成的。

### 名字（Name）
名字对应于HTML标签中的名字（也就是尖括号里的第一项）。每个标签都具有名字，标签的名字使用`.name`来访问，例如上例中，

    tag.name == u'b'
    soup.name == u'[document]'。

### 属性（Atrriutes）
属性对应于HTML标签中的属性部分（也就是尖括号里带等号的那些）。标签可以有许多属性，也可以没有属性。属性使用类似于字典的形式访问，用方括号加属性名，例如上例中，

    tag['class'] ==  u'boldest'

可以使用.attrs直接获得这个字典，例如，

    tag.attrs == {u'class': u'boldest'}

### 文本（Text）
文本对应于HTML中的文本（也就是尖括号外的部分）。文件使用`.text`来访问，例如上例中，

    tag.text ==  u'Extremely bold'

------------

##找汤料——Soup中的查找

解析一个HTML通常是为了找到感兴趣的部分，并提取出来。BeautifulSoup提供了`find`和`find_all`的方法进行查找。`find`只返回找到的第一个标签，而`find_all`则返回一个列表。因为查找用得很多，所以BeautifulSoup做了一些很方便的简化的使用方式：

    tag.find_all("a")  #等价于 tag("a")
    tag.find("a") #等价于 tag.a

因为找不到的话，find_all返回空列表，`find`返回`None`，而不会抛出异常，所以，也不用担心 `tag("a")` 或 `tag.a` 会因为找不到而报错。限于python的语法对变量名的规定，`tag.a` 的形式只能是按名字查找，因为点号.后面只能接变量名，而带括号的形式 `tag()` 或 `tag.find()` 则可用于以下的各种查找方式。

查找可以使用多种方式：字符串、列表、键-值（字典）、正则表达式、函数

* 字符串：  字符串会匹配标签的名字，例如 `tag.a` 或 `tag("a")`

* 列表：  可以按一个字符串列表查找，返回名字匹配*任意*一个字符串的标签。例如 `tag("h2", "p")`

* 键-值： 可以用`tag(key=value)`的形式，来按标签的属性查找。键-值查找里有比较多的小花招，这里列几条：

1. class
`class`是Python的保留字，不能当变量名用，偏偏在HTML中会有很多 `class=XXX` 的情况，BeautifulSoup的解决方法是加一下划线，用 `class_` 代替,如 `tag(class_=XXX)`。
2. True
当值为True时，会匹配所有带这个键的标签，如 `tag(href=True)`
3. text
text做为键时表示查找按标签中的文本查找，如 `tag(text=something）`

* 正则表达式：  例如 `tag(href=re.compile("elsie"))`

* 函数：  当以上方法都行不通时，函数是终极方法。写一个以单个标签为参数的函数，传入 `find` 或 `find_all` 进行查找。如

        def fun(tag):
            return tag.has_key("class") and not tag.has_key("id")
        tag(fun) # 会返回所有带class属性但不带id属性的标签

------------

##再来一碗——按文档的结构查找

HTML可以解析成一棵标签树，因此也可以按标签在树中的相互关系来查找。

* 查找上层节点：`find_parents()` 和 `find_parent()`

* 查找下一个兄弟节点：`find_next_siblings()` 和 `find_next_sibling()`
* 查找上一个兄弟节点：`find_previous_siblings()` 和 `find_previous_sibling()`

以上四个都只会查同一父节点下的兄弟

* 查找下层节点：其实上面说的find和find_all就是干这活的

* 查找下一个节点（无视父子兄弟关系） `find_all_next()` 和 `find_next()`
* 查找上一个节点（无视父子兄弟关系） `find_all_previous()` 和  `find_previous()`

以上的这些查找的参都和`find`一样，可以搭配着用。

------

##看颜色选汤——按CSS查找


用 `.select()`方法，看 <http://www.crummy.com/software/BeautifulSoup/bs4/doc/#css-selectors>

##一些小花招
* BeautifulSoup 可以支持多种解析器，如lxml, html5lib, html.parser. 如：`BeautifulSoup("<a></b>", "html.parser")`

* BeautifulSoup 用于解析xml的话，目前只支持lxml，需要自己另行安装。

具体表现可参考 <http://www.crummy.com/software/BeautifulSoup/bs4/doc/#differences-between-parsers>

* BeautifulSoup 在解析之前会先把文本转换成unicode。BeautifulSoup会优先考虑使用页面的charset定义。如果遇上网页的charset定义与实际使用的不符，就可能会产年乱码。可以用 `from_encoding` 指定编码，如：` BeautifulSoup(markup, from_encoding="gb18030")`

* soup.prettify()可以输出排列得很好看的HTML文本，遇上中文的话可以指定编码使其显示正常，如 `soup.prettify("gbk")`
