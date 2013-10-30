---
layout: post
category : Web
tagline:
tags : [SQL]
title: MySQL中 REPLACE 与 INSERT ON DUPLICATE KEY UPDATE 的区别
---

MySQL中, 要一步实现 "如果没有则插入, 如果存在则替换" 这样的操作, 有两种方式,
`REPLACE` 和 `INSERT ... ON DUPLICATE KEY UPDATE`. 这两种方法是有区别的.

它们之间的共同点是, 要求表中有 `UNIQUE` 或 `PRIMARY KEY` 的列,
不然没法判断是否需要替换, 这两都会变成和普通 `INSERT` 一样了.

不同之处在于, 如果存在重复的主键值, `REPLACE` 执行的是**先删除, 后插入**.
这样做一个后果是会导致索引碎片化, 降低效率. 更重要的是,
这条命令是无法进行部分更新的, 命令中没有赋值的列, 都会被设为**默认值**!

`REPLACE` 的语法示例如下:

```sql
REPLACE INTO
  ratings
SET
  quote_id = 100,
  user_id = 200,
  score = 10
```

`INSERT ... ON DUPLICATE KEY UPDATE` 进行的操作是, 如果存在重复的主键值,
则执行`UPDATE`子句的操作, 这是可以保留未赋值列的原值的. 如果没有重复主键,
它就是普通的 `INSERT`.

示例如下:

```sql
INSERT INTO
  ratings
SET
  quote_id = 100,
  user_id = 200,
  score = 10
ON DUPLICATE KEY UPDATE
  score = 10
```

本文参考了:
[MySQL REPLACE vs INSERT ON DUPLICATE KEY UPDATE](http://thomashunter.name/blog/mysql-replace-vs-insert-on-duplicate-key-update/)
