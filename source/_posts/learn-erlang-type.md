---
title: Erlang极简学习笔记<04>——类型篇
author: shixiongfei
date: 2019-04-27 23:56:00
categories: 编程
tags: [编程, Erlang]
---

- Erlang是动态类型语言。所有错误都在运行时被捕获，在编译代码时，对于可能导致失败的问题，编译器并不总会给出警告

- 绝大多数语言和类型系统都旨在写出没有错误的程序，但是Erlang却认为错误肯定会发生

- Erlang在语言中提供了一些特性，基于这些特性可以很容易对错误进行平滑处理，并且不会造成不必要的停机时间。所以Erlang的动态类型系统不是程序可靠性和安全性的障碍

- 动态类型是实现代码热加载的最简单方法。如果在静态类型系统中要实现热加载其难度要远远高于动态类型

- Erlang同时还是一个强类型语言。弱类型语言会在不同的数据项之间做隐式的类型转换

  ```erlang
  6 + "1"
  ** exception error: bad argument in an arithmetic expression
  in operator +/2
  call as 6 + "1"
  ```

- 和许多语言一样，Erlang也可以通过强制转换的方式改变数据的类型

- 大多数的类型转换都不能用Erlang直接实现，所以这些操作都是BIF提供的

- 所有转换函数的命名都采用 `TypeA_to_TypeB` 这样的形式，都在erlang模块中实现

  ```erlang
  erlang:list_to_integer("54").
  erlang:integer_to_list(54).
  erlang:list_to_float("54.32").
  erlang:atom_to_list(true).
  erlang:list_to_binary("hi there").
  erlang:binary_to_list(<<"hi there">>).
  ```

- Erlang语言的一个小缺陷：因为函数命名采用 `Type_to_Type` 这样的形式，所以每当语言中增加一个新的数据类型时，OTP团队就需要加入一整套BIF转换函数

- 注意：BIF函数 `binary_to_term/2` 和 `binary_to_term/1` 对数据反序列化的方式一样。它们之间的最大的区别是 `binary_to_term/2` 的第二个参数是一个选项列表。如果传入的是 `[safe]` ，那么如果二进制数据中含有未知的原子或匿名函数，就将不被解码，因为这有可能会耗尽节点的内存或者隐藏着一个安全风险。如果要解码的数据可能是不安全的，那么请使用 `binary_to_term/2` 而不是 `binary_to_term/1`

- Erlang有一些专门负责检测数据类型的函数，它们接收一个参数，如果参数的数据类型正确，就返回 `true` ，否则，就返回 `false` 。它们是为数不多的、可以在卫表达式中使用的函数，也称为类型检测BIF

  ```erlang
  erlang:is_atom(false).
  erlang:is_integer(123).
  erlang:is_list("45.67").
  erlang:is_binary(<<"hi there">>).
  ```

- Erlang中没有类似 `type_of(X) -> Type` 的函数来获取数据项的类型。因为Erlang只针对正确的情况编程，你的程序只需要处理你所期望的情况，对于除此之外的其他情况，都应该尽快的抛出异常

- 如果提供了一个 `type_of(X)` 的函数，就会怂恿人们在代码中写出条件分支

  ```erlang
  my_func(Exp) ->
    case type_of(Exp) of
      binary -> Expression1;
      list -> Expression2
    end.
  ```

- 在Erlang中应该在函数头中指定所期望的数据类型来进行分支处理，而不是根据某个类似 `type_of(X)` 的函数所返回的数据类型来做不同处理。下面这种形式更符合Erlang语言内在的声明性

  ```erlang
  my_func(Exp) when is_binary(Exp) -> Expression1;
  my_func(Exp) when is_list(Exp) -> Expression2.
  ```

- 所有可以在卫表达式中使用的函数中，类型检测BIF几乎占了一大半。其余的也都是BIF，只是不用于类型检测

- Erlang的数据结构看起来似乎很有限，但是一般来讲，仅用列表和元组就足以构建其他复杂的数据结构了
