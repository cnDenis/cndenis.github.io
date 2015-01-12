---
layout: post
category : Erlang
tagline:
tags : [Erlang, lists, all, any]
title: 【Erlang】lists:all/2 和 lists:any/2 对于空列表运算结果是啥?
---

先说结果, 无论F是什么, 结果都是这样:

```erlang
    lists:all(F, []) = true
    lists:any(F, []) = false
```

换成其它语言也一样.

### lists:all(F, []) = true

从逻辑上, 假如 A 和 B 是两个列表, 那么:

    all(F, A ++ B) = all(F, A) andalso all(F, B)

显然, 如果一个大列表全部都是true, 把大列表分割成两个子列表后,
每个子列表中所有元素也都是true.

Erlang中, A = A ++ [], 所以

    all(F, A ++ []) = all(F, A) andalso all(F, [])

只有 all(F, []) = true 才能保证上式恒成立.

### lists:any(F, []) = false

类似的,

    any(F, A ++ B) = any(F, A) orelse all(F, B)

因此,

    any(F, A ++ []) = any(F, A) orelse all(F, [])

只有当 all(F, []) = false 才可以保证上式恒成立.

--------------------

参考:

<http://stackoverflow.com/questions/11979683/why-python-built-in-all-function-returns-true-for-empty-iterables>