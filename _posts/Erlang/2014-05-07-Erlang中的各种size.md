---
layout: post
category : Erlang
tagline:
tags : [Erlang]
title: Erlang数据项的size
---

Erlang中数据项(Erlang Term)的所占据的空间大小可以用各种size来获取.

* 其中对于所有数据项都通用的有: `erlang:external_size/1`, `erts_debug:size/1`, `erts_debug:flat_size/1`

* 适用于二进制串有: `erlang:size/1`, `erlang:byte_size/1`, `erlang:bit_size/1`

* 适用于元组的有: `erlang:size/1`, `erlang:tuple_size/1`

### erlang:external_size/1

[erlang:external_size/1](http://erlang.org/doc/man/erlang.html#external_size-1) 用来获取erlang数据项的外部格式([External Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html))*最大*大小. 所谓外部格式, 就是用`term_to_binary/1`
打包形成的格式, 如果要把数据项从一台节点发到另一个节点, 或是持久化装到数据库中,
就需要打包成外部格式. 因此, 这个函数计算的传输erlang数据项的IO开销, 可以用来计算网络带宽或磁盘空间的占用量. 文档上说如下关系始终成立:

    > Size1 = byte_size(term_to_binary(Term)),
    > Size2 = erlang:external_size(Term),
    > true = Size1 =< Size2.
    true

也就是说用`external_size/1`函数算出来的空间需求可能会大于实际用`term_to_binary/1`
打包后的体积, 但这对于进行空间预算是没有问题的.

这个函数还有带多一个参数的版本[erlang:external_size/2](http://erlang.org/doc/man/erlang.html#external_size-2), 可以指打包的格式的版本, 目前版本之间只是对浮点数的处理有点不一样.

### erts_debug:flat_size/1 和 erts_debug:size/1

`erts_debug:size/1` 和 `erts_debug:flat_size/1` 都是不在正式文档中的函数,
可以用来计算erlang数据项在内存中所需要空间. 各种数据项的空间占用可以在这里找到:
<http://www.erlang.org/doc/efficiency_guide/advanced.html#id68912>. 这两个函数区别在于,
在具有共享内存的数据结构中, `erts_debug:size/1`只计算一次共享的数据大小,
而`erts_debug:flat_size/1`则会重复计算.

这是erlang源代码中的例子:

    %% size(Term)
    %%  Returns the size of Term in actual heap words. Shared subterms are
    %%  counted once.  Example: If A = [a,b], B =[A,A] then size(B) returns 8,
    %%  while flat_size(B) returns 12.

文档中有另外一个例子: <http://www.erlang.org/doc/efficiency_guide/processes.html>

总的来说, `erts_debug:size/1`是erlang数据项在内存中所占用的空间大小,
`erts_debug:flat_size/1`是同一节点内, 跨进程移动数据项(包括ETS操作)所需要拷贝的数据大小.

### erlang:tuple_size/1

作为自动导入的BIF, 一般不用写模块名`erlang:`. 这个函数功能很简单,
计算元组所包含的元素个数, 对于元组T来说, `size(T)` 就等于`tuple_size(T)`,
后者指明了数据类型, 运行时微微快一点.

### erlang:byte_size/1 和 erlang:bit_size/1

这两个也是自动导入, 一般不用写模块名. 它们是用来计算二进制串(binary)或是比特串(bitstring)所占的空间的. 对于二进制串B来说, `size(B)` 等于 `byte_size(B)`.


