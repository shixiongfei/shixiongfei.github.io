---
title: Erlang极简学习笔记<03>——函数篇
author: shixiongfei
date: 2019-04-20 00:28:00
categories: 编程
tags: [编程, Erlang]
---

- Erlang中定义函数时可以使用模式匹配，形式如下：

```erlang
function(X) ->
  Expression;
function(Y) ->
  Expression;
function(_) ->
  Expression.
```

- 其中的每一条函数声明都被称作一个函数子句（function clause）。函数子句之间必须用分号 `;` 分隔，所有函数子句一起形成一个完整的函数定义，最后一个函数子句必须以句点 `.` 结尾

- 在Erlang中变量的值永远不能改变！

- 如果给一个已经绑定的变量赋值，除非这个新值和变量原有的值相同，否则就会引发错误

- 在函数头中可以使用 `=` 操作符，这样可以在匹配元组内部元素（ `{X, Y}` ）的同时，匹配整个元组

```erlang
point(Point = {X, Y}) ->
  io:format("Point: ~p~n", [Point]),
  io:format(" X: ~p, Y: ~p~n", [X, Y]).
```

- 卫语句是附加在函数头中的语句，能够让模式匹配更具表达力

- 卫表达式有一条基本规则，想要成功，它必须返回 `true` 。如果返回了 `false` 或者抛出了异常，就表明卫语句失败

```erlang
old_enough(X) when X >= 16, X =< 104 -> true;
old_enough(_) -> false.
```

- 在卫表达式中，逗号（ `,` ）的作用和操作符 `andalso` 类似，分号（ `;` ）和 `orelse` 类似

```erlang
wrong_age(X) when X < 16; Y > 104 -> true;
wrong_age(_) -> false.
```

- 函数可以带有任意多个由逗号分隔的卫表达式，这些卫表达式必须都是成功，整个卫语句才能通过

- 卫语句除了使用比较和布尔操作，还可以使用算术运算符( `A*B/C >= 0` )和判断数据类型的函数( `is_integer/1` ,  `is_atom/1` )

- 卫语句有一个缺点，考虑到副作用方面的原因，卫语句中不能使用用户自定义函数

- `if` 语句又称为卫模式(guard pattern)， `if` 语句的作用和卫语句类似，和卫语句的语法也一样，但是它在函数子句头之外使用

- Erlang中的 `if` 和其他大多数语言中见到的 `if` 不同，和那些 `if` 语句相比，Erlang版本的 `if` 像个怪物

```erlang
-module(what_the_if).
-export([heh_fine/0]).

heh_fine() ->
  if 1 =:= 1 ->
    works
  end,
  if 1 =:= 2; 1 =:= 1 ->
    works
  end,
  if 1 =:= 2, 1 =:= 1 ->
    fails
  end.
```

- Erlang中每个表达式都必须有返回值， `if` 表达式也不例外

- 当Erlang无法让卫表达式成功时就会崩溃

- 其他语言中的 `else` 在Erlang会使用 `true`

```erlang
oh_god(N) ->
  if N =:= 2 -> might_success;
    true -> always_does %%这是Erlang if 的 else!
  end.
```

- Erlang的 `if` 表达式可以有多个卫语句。再次强调！任何表达式都必须要有返回值

```erlang
help_me(Animal) ->
  Talk = if Animal == cat -> "meow";
    Animal == beef -> "mooo";
    Animal == dog -> "bark";
    Animal == tree -> "bark";
    true -> "fgdadfgna"
  end,
  {Animal, "says" ++ Talk ++ "!"}.
```

- Erlang中没有像null值（如：Lisp中的 `nil` ，C中的 `NULL` 或者Python中的 `None` ）这样的东西

- `case...of` 表达式就像函数头，可以使用复杂的模式匹配，还可以使用卫语句

```erlang
beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
      'favorable';
    {kelvin, N} when N >= 293 , N =< 318 ->
      'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 ->
      'favorable in the US';
    _ ->
      'avoid beach'
  end.
```
