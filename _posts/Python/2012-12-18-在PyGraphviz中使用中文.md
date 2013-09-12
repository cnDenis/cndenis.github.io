---
layout: post
category : Python
tagline:
tags : [Python, PyGraphviz, 中文]
title: 在 PyGraphviz 中使用中文
---

在 PyGraphviz 使用中文要注意的：

* 汉字要使用 unicode ，例如：`u"汉字"`

* 添加节点时要指定中文的字体，例如 `fontname=SimSun`

示例如下：

```python
    import pygraphviz as pg

    G = pg.AGraph()

    G.add_node(u"经理", fontname="`Microsoft YaHei", shape="rect", style="rounded", fontsize=18) #雅黑
    G.add_node(u"秘书", fontname="SimHei") #黑体
    G.add_node(u"小兵", fontname="SimSun") #宋体
    G.add_node(u"小卒", fontname="Kaiti") #楷体

    G.add_edge(u"经理", u"秘书")
    G.add_edge(u"经理", u"小兵")
    G.add_edge(u"秘书", u"小卒")

    G.draw('test_pygraphviz.png', format='png', prog='neato')
```

输出效果如下：

<img src="http://farm9.staticflickr.com/8486/8283114699_a3c32a6287_m.jpg" width="187" height="240" alt="test_pygraphviz">

附几个常用的中文字体的英文名：

* 黑体：SimHei
* 宋体：SimSun
* 仿宋：FangSong
* 楷体：KaiTi
* 微软雅黑体：Microsoft YaHei

更多字体英文名可参考： <http://www.aoao.org.cn/blog/2008/03/fonts-chinese/>



