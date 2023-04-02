---
title: Erlang极简学习笔记<01>——启程篇
author: shixiongfei
date: 2019-03-31 21:24:00
categories: 编程
tags: [编程, Erlang]
---

- Erlang是一门函数式编程语言。Erlang的核心特征是容错，并发只是容错这个约束下的一个副产品

- 对于同样的参数，函数永远要返回同样的值，这个概念称为引用透明性(referential transparency)

- Erlang采用了actor模型，每个actor都是虚拟机中的一个独立进程

- Erlang中只能创建出相互之间完全没有共享、只能通过消息通信的actor（进程）。每次消息交互都是显示的、可追踪的和安全的

- Erlang不关心输入的是浮点数还是整数，算术运算符对两种类型都有效

```erlang
2 + 15.
49 * 100.
5 / 2.
```

- 如果需要整除或者余数，要使用 `div` 和 `rem`

```erlang
5 div 2.
5 rem 2.
```

- 算术计算遵循标准优先级规则

```erlang
50 * 100 - 4999.
```

- 以不为10的基数表示整数，需要以 `Base#Value` 这种形式输入数值(Base必须在2到36之间)

```erlang
2#101010.
8#0677.
16#AE.
```

- 变量必须以大写字符开始，一个变量只能绑定一次，并且改变任何变量的值都是绝对不允许的

```erlang
One = 1.
Two = One + One.
```

- 变量还可以以下划线(_)开始，但是依照惯例，这种变量仅用于不想关心其值的情况

- `=` 操作符的行为表现其实是模式匹配（pattern matching）

```erlang
2 = 1 + 1.
```

- 在eshell中可以使用 `f(Variable).` 函数删除一个变量，可以使用 `f().` 删除所有变量。这两个函数专门用来辅助代码实验，所以只能在shell中使用

- 原子以小写字符开始，如果不以小写字符开头或者其中包含有除字母、数字、下划线以及@符号之外的其他符号，那么必须被放到两个单引号(')之间

```erlang
atom.
atom@erlang.
'Atoms can be cheated!'.
atom = 'atom'.
```

- 每个原子都会被引用到一个原子表中，原子表不被垃圾回收，原子表会消耗内存(32位系统4字节，64位系统8字节)，系统最大原子个数1048577个

- 不要动态创建原子，在实际开发中应确保所有原子都是手动输入的

- 有些原子是保留字，这些原子不能使用

```erlang
after、and、andalso、band、begin、bnot、bor、bsl、bsr、bxor、case、catch、cond、div、end、fun、if、let、not、of、or、orelse、query、receive、rem、try、when 和 xor
```

- 布尔操作符 `and` 和 `or` 对操作符两边的参数都会去求值。如果想要一个短路操作符(只在必要时，才去求右值)，可以使用 `andalso` 和 `orelse`

```erlang
false or true.
true xor false.
not false.
not (true and true).
```

- Erlang使用 `=:=` 和 `=/=` 来做相等性和不等性测试

```erlang
5 =:= 5.
1 =:= 0.
1 =/= 0.
5 =:= 5.0.
```

- Erlang做比较时，会区分浮点数和整数。如果不想区分可以使用 `==` 和 `/=`

```erlang
5 == 5.0.
5 /= 5.0.
```

- 根据经验，你应该始终使用 `=:=` 和 `=/=` ，只有在明确知道确实不需要精确相等性时才换成 `==` 和 `/=`

- 其他比较操作符： `<` (小于)、 `>` (大于)、 `>=` (大于等于)和 `=<` (小于等于)。 **注意最后一个！**

```erlang
1 < 2.
1 >= 1.
1 =< 1.
```

- 比较操作中，数据类型之间的大小顺序是： `number < atom < reference < fun < port < pid < tuple < list < bit string`

- Erlang元组书写形式： `{Element1, Element2, ..., ElementN}` 。获取元组中元素可以使用模式匹配

```erlang
Point = {3, 4}.
{X, Y} = Point.
```

- Erlang列表的书写形式： `[Element1, Element2, ..., ElementN]` ，列表可以混合放入多种数据类型

- Erlang中没有字符串数据类型。字符串就是列表

```erlang
[97, 98, 99].
```

- `++` 操作符可以把列表粘合起来， `--` 操作符可以从列表中删除元素。 `++` 和 `--` 操作符都是右结合的，这意味着多个 `++` 或 `--` 操作是从右向左进行的

```erlang
[1, 2, 3] ++ [4, 5].
[1, 2, 3, 4, 5] -- [1, 2, 3].

[1, 2, 3] -- [1, 2] -- [3].
[1, 2, 3] -- [1, 2] -- [2].
```

- 列表第一个元素称为头(head)，剩余部分称为尾(tail)。可以用内建函数(BIF)获取他们

```erlang
hd([1, 2, 3, 4]).
tl([1, 2, 3, 4]).
```

- BIF通常是一些不能用纯Erlang实现的函数，因此会用C或者其他任何实现Erlang的语言编写。也有一些BIF本可以用Erlang实现，但是为了让这些常用的操作性更高，就用C实现了。例如： `length(List)`

- 可以使用模式匹配来分离列表的头部和尾部

```erlang
[Head|Tail] = [1, 2, 3, 4].
```

- 构建列表还可以使用 `|` 操作符， `|` 操作符又称为 `cons` 操作符（构造器），仅凭 `cons` 操作符和值就能构建出任何列表

- 任何列表都可以使用这个形式构建： `[Term1| [Term2| [...| [TermN]]]]`

- 列表推导式（list comprehension）可以用来构建或修改列表。和其他列表操作方式相比，它们会让程序既短小又易于理解

- 列表推导式的形式： `NewList = [Expression || Pattern <- List, Condition1, Condition2, ..., ConditionN]` ，其中 `Pattern <- List` 部分称为生成器表达式

```erlang
[2*N || N <- [1, 2, 3, 4]].
```

- 列表推导式中还可以使用具有布尔返回值的操作来增加约束条件

```erlang
[X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0].
```

- 列表推导式还可以同时使用多个生成器表达式。下面这个表达式会执行 `1+3` ,  `1+4` ,  `2+3` ,  `2+4` 这4个操作

```erlang
[X+Y || X <- [1, 2], Y <- [3, 4]]
```

- 所以列表表达式更通用的形式应该是： `NewList = [Expression || GeneratorExp1, GeneratorExp2, ..., GeneratorExpN, Condition1, Condition2, ..., ConditionN]`

- 列表表达式也可以把生成器表达式和模式匹配放一起当成过滤器来使用

```erlang
Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}].
FoggyPlace = [X || {X, fog} <- Weather].
```

- Erlang中的位语法会把二进制数据用 `<<` 和 `>>` 括起来，并把数据分隔成多个易理解的区段，区段之间用逗号分隔。每一段都是一个二进制位序列（不一定要在字节边界上，尽管默认情况下会这样）

```erlang
Color = 16#F09A29.
Pixel = <<Color:24>>.
```

- 同样可以使用模式匹配从二进制数据中提取内容

```erlang
Pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>.
<<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.

<<R:8, G:8, B:8>> = <<Pixel:24>>.
```

- 如果只想取第一个颜色，可以使用Erlang的语法糖 `Rest/binary` 。 `Rest/binary` 是一种特别的表示法，它可以将剩余的二进制数据都放到Rest变量中。所以 `<<Pattern, Rest/binary>>` 和 `[Head|Tail]` 是非常相似的

```erlang
<<R:8, Rest/binary>> = Pixels.
```

- 一个二进制区段的描述，Erlang允许有多种方式

```erlang
Value
Value:Size
Value/TypeSpecifierList
Value:Size/TypeSpecifierList
```

- 其中如果没有指定TypeSpecifierList，那么Size的单位永远是位

- TypeSpecifierList由下面的一个或多个组成，之间用连字符(-)分隔

  - 类型： `integer` ,  `float` ,  `binary` ,  `bytes` ,  `bitstring` ,  `bits` ,  `utf8` ,  `utf16` ,  `utf32`

    - 如果不指定类型，Erlang默认使用 `integer` 类型。其中 `bytes` 是 `binary` 的缩写， `bits` 是 `bitstring` 的缩写

  - 符号类型： `signed` ,  `unsigned`

    - 默认是 `unsigned` ，只有当类型为 `integer` 时，这个字段才有意义

  - 字节序： `big` ,  `little` 和 `native`

    - 字节序默认是 `big` ，因为它是网络协议编码中使用的标准字节序。只有当类型为 `integer` ,  `utf16` ,  `utf32` 或者 `float` 时，字节序才有意义

  - 单位：写成 `unit:Integer` 这样的形式

    - 单位指的是每个区段的大小。取值范围为1`256。对于 `integer` ,  `float` 和 `bitstring` 类型来说，单位默认值是1；对于 `binary` 类型，单位默认值为8。utf系列无需定义unit

    - **注意：** Size和单位的乘积等于要提取的区段中的位数，并且必须能被8整除。单位的大小通常用来确保字节对齐

      ```erlang
      <<25:4/unit:8>>.
      <<25:2/unit:16>>.
      <<25:1/unit:32>>.
      ```

    - Erlang会接受 `<<25:Size/unit:Unit>>` 的形式，并用Size乘以Unit算出这个值所需的空间，空间的大小要能被8整除

      ```erlang
      <<X1/unsigned>> == <<-44>>.
      <<X2/signed>> = <<-44>>.
      <<X3/integer-signed-little>> = <<-44>>.
      <<N:8/unit:1>> = <<72>>.
      <<N/integer>> = <<72>>.
      <<Y:4/little-unit:8>> = <<72, 0, 0, 0>>.
      ```

- Erlang提供了标准的二进制操作： `bsl` （按左移位）， `bsr` （按右移位）， `band` ， `bor` ， `bxor` 以及 `bnot`

```erlang
2#00100 = 2#00010 bsl 1.
2#00001 = 2#00010 bsr 1.
2#10101 = 2#10001 bor 2#00101.
```

- 二进制字符串在语言中的实现方式，和用列表实现的字符串使用的方式完全一样，只是在空间使用上更加高效

- 普通字符串与链表类似，二进制字符串更像C语言中的数组

- 二进制字符串的形式： `<<"This is a binary string!">>` 。其缺点是，它的模式匹配和操作方法没有列表这么简单。因此，通常当所存储的文本无需频繁操作或空间效率是个实际问题时，才会使用二进制字符串

- 二进制推导式（binary comprehension）之于位语法就等同于列表推导式之于列表：在处理二进制数据时，可以让代码短小精悍

```erlang
<< <<X>> || <<X>> <= <<1, 2, 3, 4, 5>>, X rem 2 == 0 >>.
```

- 语法层面的变化就是：标准列表推导式中的 `<-` 变成了 `<=` （用于二进制生成器），使用的是二进制数据（<<>>），而不是列表数据（[]）

- 相比之前的代码，这里有个更强大的用法

```erlang
Pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>.
RGB = [ {R, G, B} || <<R:8, G:8, B:8>> <= Pixels ].
<< <<R:8, G:8, B:8>> || {R, G, B} <- RGB >>.
```

- 如果生成器返回的是二进制数，那么就必须要对结果二进制数中的元素给出明确的二进制类型定义

```erlang
<< <<Bin/binary>> || Bin <- [<<3, 7, 5, 4, 7>>] >>.
```

- 默认情况下Erlang会认为放入二进制数中以及二进制数中提取的值都是整数（8位无符号）

- 还可以在二进制推导式中使用二进制生成器

```erlang
<< <<(X+1)/integer>> || <<X>> <= <<3, 7, 5, 4, 7>> >>.
```
