---
layout: post
category : Erlang
tagline:
tags : [Erlang, 代码片段, 字符串]
title: 【Erl代码片段】cowboy的一处大小写处理代码
---

在cowboy的库cowlib中的[cow_inline.hrl](https://github.com/extend/cowlib/blob/master/include/cow_inline.hrl)文件看到一堆很长的宏:

```erlang
-define(INLINE_LOWERCASE(Function, Rest, Acc),
    $A -> Function(Rest, << Acc/binary, $a >>);
    $B -> Function(Rest, << Acc/binary, $b >>);
    $C -> Function(Rest, << Acc/binary, $c >>);
    $D -> Function(Rest, << Acc/binary, $d >>);
    $E -> Function(Rest, << Acc/binary, $e >>);
    $F -> Function(Rest, << Acc/binary, $f >>);
    $G -> Function(Rest, << Acc/binary, $g >>);
    $H -> Function(Rest, << Acc/binary, $h >>);
    $I -> Function(Rest, << Acc/binary, $i >>);
    $J -> Function(Rest, << Acc/binary, $j >>);
    $K -> Function(Rest, << Acc/binary, $k >>);
    $L -> Function(Rest, << Acc/binary, $l >>);
    $M -> Function(Rest, << Acc/binary, $m >>);
    $N -> Function(Rest, << Acc/binary, $n >>);
    $O -> Function(Rest, << Acc/binary, $o >>);
    $P -> Function(Rest, << Acc/binary, $p >>);
    $Q -> Function(Rest, << Acc/binary, $q >>);
    $R -> Function(Rest, << Acc/binary, $r >>);
    $S -> Function(Rest, << Acc/binary, $s >>);
    $T -> Function(Rest, << Acc/binary, $t >>);
    $U -> Function(Rest, << Acc/binary, $u >>);
    $V -> Function(Rest, << Acc/binary, $v >>);
    $W -> Function(Rest, << Acc/binary, $w >>);
    $X -> Function(Rest, << Acc/binary, $x >>);
    $Y -> Function(Rest, << Acc/binary, $y >>);
    $Z -> Function(Rest, << Acc/binary, $z >>);
    C -> Function(Rest, << Acc/binary, C >>)
).
```

底下还有2参数,3参数...10参数的版本.

找到一个调用的地方看看: [cowboy_protocol.erl](https://github.com/extend/cowboy/blob/master/src/cowboy_protocol.erl)

```erlang
parse_hd_name(<< C, Rest/bits >>, S, M, P, Q, V, H, SoFar) ->
case C of
    $: -> parse_hd_before_value(Rest, S, M, P, Q, V, H, SoFar);
    $\s -> parse_hd_name_ws(Rest, S, M, P, Q, V, H, SoFar);
    $\t -> parse_hd_name_ws(Rest, S, M, P, Q, V, H, SoFar);
    ?INLINE_LOWERCASE(parse_hd_name, Rest, S, M, P, Q, V, H, SoFar)
end.
```

还是看不明白, 用`c(cowboy_protocol, ['P'])`查看展开的宏, 上面的代码变成了:

```erlang
parse_hd_name(<<C,Rest/bits>>, S, M, P, Q, V, H, SoFar) ->
    case C of
        $: ->
            parse_hd_before_value(Rest, S, M, P, Q, V, H, SoFar);
        $  ->
            parse_hd_name_ws(Rest, S, M, P, Q, V, H, SoFar);
        $\t ->
            parse_hd_name_ws(Rest, S, M, P, Q, V, H, SoFar);
        $A ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$a>>);
        $B ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$b>>);
        $C ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$c>>);
        $D ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$d>>);
        $E ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$e>>);
        $F ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$f>>);
        $G ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$g>>);
        $H ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$h>>);
        $I ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$i>>);
        $J ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$j>>);
        $K ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$k>>);
        $L ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$l>>);
        $M ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$m>>);
        $N ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$n>>);
        $O ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$o>>);
        $P ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$p>>);
        $Q ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$q>>);
        $R ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$r>>);
        $S ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$s>>);
        $T ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$t>>);
        $U ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$u>>);
        $V ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$v>>);
        $W ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$w>>);
        $X ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$x>>);
        $Y ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$y>>);
        $Z ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,$z>>);
        C ->
            parse_hd_name(Rest, S, M, P, Q, V, H, <<SoFar/binary,C>>)
    end.
```

嗯, 这样就明白多了, 就是进行程序的逻辑处理同时, 把大写字母转换成小写.

测试一下这种写法的效率, 把一个大写的二进制串转成小写.
Erlang没有原生的对二进制串做大小写转换的方法, 所以要先转列表, 转小写后再转回来.

```erlang
%% 先转列表
lower_by_list(B) ->
    list_to_binary(string:to_lower(binary_to_list(B))).

%% inline的方法
lower_by_inline(B) ->
    lower_by_inline(B, <<>>).
lower_by_inline(<<C, R/bits>>, Acc) ->
    case C of
        ?INLINE_LOWERCASE(lower_by_inline, R, Acc)
    end;
lower_by_inline(<<>>, Acc) ->
    Acc.
```

对于1K字节的大写二进制串, 运行结果如下:

    len: 1024
    bench:lower_by_list/1
    Single Process:      29925 call per sec,      32768 times in  1095 ms

    bench:lower_by_inline/1
    Single Process:      25090 call per sec,      32768 times in  1306 ms

如果单是做大小写转换, 这么做并不比先转列表再转回来快, 不过是可以省一点内存.

想起Erlang文档里的一句话: Do not guess about performance
[出自这里](http://www.erlang.org/documentation/doc-5.2/doc/efficiency_guide/profiling.html). 这里用了相当复杂的方法完成了一个简单的工作,
或许是针对项目需求极度优化的结果, 如果不顾场合照搬, 那就是给自己添麻烦了.