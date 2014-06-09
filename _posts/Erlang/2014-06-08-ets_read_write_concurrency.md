---
layout: post
category : Erlang
tagline:
tags : [Erlang, ets, 并发]
title: 【Erlang】ETS的并发性能调节选项
---

ETS在创建表时, 有 `write_concurrency` 和 `read_concurrency` 这两个选项,
可以用以优化ETS的并发性能.

`write_concurrency` 默认设置是 `false`, 当表进行写操作时,
整个表会被锁起来, 直到写完为止. 把这个值设置为 `true`, 可以优化并发写性能,
表中的不同项可以同时进行读写, 代价是会消耗一些内存, 并且会降低并发读的性能.
但是对于 `order_set` 类型的表, 目前的Erlang版本下是不受这个选项影响的.

`read_concurrency` 默认设置也是 `false`, 当设置为 `true`,
并且Erlang运行在多核机器上且开启SMP支持时, 并发读的性能会提升.
代价是读写切换变得更慢.

这两个选项是不会对操作的原子性和独立性有影响的,
也就是说这两项设置**只改变性能, 不影响逻辑**.

显然, `write_concurrency` 适用于读少写多的表,
`read_concurrency` 适用于读多写少的表, 以及读写交替少的表
(即读一大堆数据后再写一大堆, 读取操作较少被写入操作打断).

这两个选项是可以同时开启的, 但额外的内存消耗会更大.

测试一下, 分别对默认, 开启`read_concurrency`, 开启`write_concurrency` 这三种设置,
测试 `insert`, `lookup`, `update_counter` 三种操作

    bench for default ets
    ets:insert/2
    Single Process:    5664151 call per sec,    8388608 times in  1481 ms
      1000 Process:    2448296 call per sec,    4096000 times in  1673 ms
    ets:lookup/2
    Single Process:    6096372 call per sec,    8388608 times in  1376 ms
      1000 Process:    9481481 call per sec,   16384000 times in  1728 ms
    ets:update_counter/3
    Single Process:    5821379 call per sec,    8388608 times in  1441 ms
      1000 Process:     105872 call per sec,     128000 times in  1209 ms

    bench for read_concurrency ets
    ets:insert/2
    Single Process:    5418997 call per sec,    8388608 times in  1548 ms
      1000 Process:    2493000 call per sec,    4096000 times in  1643 ms
    ets:lookup/2
    Single Process:    4981358 call per sec,    8388608 times in  1684 ms
      1000 Process:   10944555 call per sec,   16384000 times in  1497 ms
    ets:update_counter/3
    Single Process:    5769331 call per sec,    8388608 times in  1454 ms
      1000 Process:    1935727 call per sec,    2048000 times in  1058 ms

    bench for write_concurrency ets
    ets:insert/2
    Single Process:    4371343 call per sec,    8388608 times in  1919 ms
      1000 Process:    2617252 call per sec,    4096000 times in  1565 ms
    ets:lookup/2
    Single Process:    4507580 call per sec,    8388608 times in  1861 ms
      1000 Process:    8062992 call per sec,    8192000 times in  1016 ms
    ets:update_counter/3
    Single Process:    4412734 call per sec,    8388608 times in  1901 ms
      1000 Process:    1889298 call per sec,    2048000 times in  1084 ms

可以看出, 开启并发性能优化的选项, 对于多进程的`update_counter`的性能有很大帮助.

