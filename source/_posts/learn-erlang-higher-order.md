---
title: Erlang极简学习笔记<06>——高阶函数篇
author: shixiongfei
date: 2019-05-12 23:50:00
categories: 编程
tags: [编程, Erlang]
---

- 所有函数式语言都具有一个重要的特性：把自定义函数作为参数传递给另一个函数。这个函数参数会被绑定到一个变量上，在函数内部可以像使用其他变量一样使用这个变量。如果一个函数的参数是以这种方式传过来的其他函数，则称之为高阶函数(higher-order function)

- 高阶函数是一种强有力的抽象手段，也是Erlang提供的众多出色工具中需要熟练掌握的一个

- 函数可以携带并且可以作为参数传递给高阶函数的思想起源于数学，主要来自Lambda演算

- 本质上，纯Lambda演算中的所有东西都是函数，就连数字、操作符和列表也都是函数。因为所有东西都被表示成函数，所以函数必须能接收其他函数作为参数，还必须能用函数来操作函数

```erlang
-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().
```

```erlang
  Eshell
  > c(hhfuns).
  > hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
```

- `Module:Function/Arity` 告诉VM去使用这个指定的函数，并把这个函数绑定到一个变量上

```erlang
increment([]) -> [];
increment([H|T]) -> [H+1 | increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1 | decrement(T)].
```

```erlang
Eshell
> c(hhfuns).
> L = [1,2,3,4,5].
> hhfuns:increment(L).
> hhfuns:decrement(L).
```

- 上面2个函数做的事情大致相同：循环遍历列表，在每个元素上应用一个函数（ `+` 或 `-` ），然后再调用自身

```erlang
map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

incr(X) -> X+1.
decr(X) -> X-1.
```

```erlang
Eshell
> c(hhfuns).
> L = [1,2,3,4,5].
> hhfuns:map(fun hhfuns:incr/1, L).
> hhfuns:map(fun hhfuns:decr/1, L).
```

- 我们将流程中相同的部分抽取出来放在一个单独的函数 `map/2` 中，这个函数接收另外一个函数作为参数

- `map/2` 是一个更聪明的抽象，每当我们想把一个函数应用于列表中的每个元素上时，只需以这个函数为参数调用 `map/2` 即可

- 匿名函数（anonymous functions）又称funs，是在行间定义的一种特殊函数，无需给其取名，从而解决了函数作为参数的那些麻烦问题

- 正常函数能做的事情，匿名函数基本上也都可以完成，除了不能递归调用自己。语法如下：

```erlang
fun (Args1) ->
  Expression1, Expression2, ..., ExpressionN;
  (Args2) ->
  Expression1, Expression2, ..., ExpressionN;
  (Args3) ->
  Expression1, Expression2, ..., ExpressionN
end
```

- 函数式编程可以对非常低层次的代码进行抽象。因此可以完全忽略像循环这种基本概念，从而聚焦在做什么上，而不是怎么做

```erlang
Eshell
> hhfuns:map(fun(X) -> X+1 end, L).
> hhfuns:map(fun(X) -> X-1 end, L).
```

- 可以把函数的作用域想象成存放所有变量及这些变量对应值的地方。例如 `base(A) -> B = A+1` ，`A` 和 `B` 都是 `base/1` 函数作用域的一部分，这意味着在 `base/1` 中的任何地方都可以引用 `A` 和 `B` ，任何地方也包括匿名函数

```erlang
base(A) ->
  B = A + 1,
  F = fun() -> A * B end,
  F().
```

- 这个例子中，`A` 和 `B` 仍然在 `base/1` 的作用域范围之内，所以函数 `F` 可以访问到它们。这是因为 `F` 继承了 `base/1` 的作用域

- 不管匿名函数在哪里，这个被继承的作用域会一直跟随着它，即使把这个匿名函数传递给另外的一个函数

- 如果函数具有多个参数，但是其中有一个参数一直保持不变，那么此时就很适合使用匿名函数来携带状态

```erlang
Eshell
> Base = 2.
> PowerOfTwo = fun(X) -> math:pow(Base, X) end.
> hhfuns:map(PowerOfTwo, L).
```

- 闭包指的是可以让函数引用到它所携带的某些环境（作用域中的值部分）。换句话说，当匿名函数、作用域的概念以及可以携带变量的能力结合在一起时，闭包就出现了

- Erlang标准库已经提供了许多基于列表的抽象 [参见文档](http://erlang.org/doc/man/lists.html)

```erlang
lists:map/2, lists:filter/2, lists:foldl/3, lists:foldr/3
all/2, any/2, dropwhile/2, takewhile/2, partition/2,
flatten/1, flatlength/1, flatmap/2, merge/1,
nth/2, nthtail/2, split/2, zip/2, unzip/1 ...
```
