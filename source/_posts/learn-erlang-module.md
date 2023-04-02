---
title: Erlang极简学习笔记<02>——模块篇
author: shixiongfei
date: 2019-04-12 22:06:00
categories: 编程
tags: [编程, Erlang]
---

- 模块（module）是一个具有名字的文件，其中包含一组函数。Erlang中的所有函数都必须定义在模块中

- Erlang模块中BIF函数和其他函数不同，在启动Erlang时，它们会被自动引入

- 模块中的其他所有函数都必须用 `Module:Function(Arguments)` 这样的形式调用

  ```erlang
  lists:seq(1, 4).
  ```

- 编写模块时，可以定义两种东西：函数(function)和属性(attribute)

- 属性是元数据，用来描述模块自身，如模块的名字、外部可见的函数、模块的作者等

- 所有模块属性都采用 `-Name(Attribute).` 的形式

- `-module(Name).` 这个属性永远是文件的第一个属性（也是第一条语句），其中 `Name` 是一个原子

- 注意！ `-module` 属性中定义的模块名必须和模块文件的名字一致。如果名字不一致，模块将无法编译

- `.erl` 是标准的Erlang源文件扩展名

- `-export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).` 用来定义模块中的哪些函数可以被其他模块调用

- `Atity` 是函数的元数，表示这个函数可以接收的参数个数

- 不同元数可以使用相同的函数名

- `add(X, Y)` 和 `add(X, Y, Z)` 就是不同的函数，可以分别表示为 `add/2` 和 `add/3`

- 函数定义的语法遵循 `Name(Args) -> Body.` 这样的形式。 `Name` 必须是一个原子， `Body` 可以是一个或者多个用逗号分隔的Erlang表达式，函数以一个句点结束

- 注意！Erlang的没有 `return` 关键字，函数中最后一个表达式的执行结果会被自动作为返回值

- Erlang中只有单行注释，注释以 `%` 起始

- 在Erlang社区中，概括性注释通常使用3个百分号(%%%)，独立行中的注释使用2个百分号(%%)，代码之后的行内注释使用单个百分号(%)

- `-import(Module, [Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).` 用来引入模块

- 注意！引入模块会降低代码的可读性，所以Erlang社区强烈反对在代码中使用 `-import` 属性

  ```erlang
  -module(useless)
  -export([add/2, hello/0, greet_and_add_two/1])

  add(A, B) ->
    A + B.

  %% io:format/1 是标准的文本输出函数
  hello() ->
    io:format("Hello, world!~n").

  greet_and_add_two(X) ->
    hello(),
    add(X, 2).
  ```

- Erlang代码会被编译成字节码，这样VM就能执行它了

- 在命令行中调用Erlang编译器

  ```erlang
  erlc useless.erl
  ```

- 如果在shell或者模块中，可以像这样编译代码

  ```erlang
  compile:file(Filename)
  ```

- 还有一种方法，在开发代码时经常使用，就是在shell中编译： `c()`

- 默认情况下，shell只会在它的启动目录和标准库中去查找文件

- `cd/1` 函数专门用于Erlang shell，可以更换shell当前目录，这样寻找文件方便些

  ```erlang
  cd("/path/to/where/you/saved/the-module/").
  c(useless).
  ```

- 代码编译成功后会产生一个 `.beam` 文件

- Erlang中的函数和表达式必须要有返回值

- Erlang提供了很多编译选项，用来对一个模块的编译方式进行控制。例如常用的：

    - -debug_info

      - 调试器、代码覆盖率统计以及静态分析之类的Erlang工具都使用模块中的调试信息来完成工作。建议这个编译选项一直开启

    - -{outdir,Dir}

      - 默认情况下，Erlang编译器会将 `.beam` 文件放置到当前目录。可以用这个选项指定编译文件的存放路径

    - -export_all

      - 这个选项会让编译器忽略文件中已定义的 `-export` 模块属性，把文件中所有函数都导出。通常用在测试和开发阶段

    - -{d,Macro} 和 {d,Macro,Value}

      - 这个选项定义了一个可以在模块中使用的宏，其中Macro是个原子。这个选项在单元测试中用得最多，因为它能确保模块中的测试函数只在明确需要时才会被创建和导出。如果元祖中没有定义第三个元素， `Value` 会被默认设置为 `true`

    - shell中使用编译选项

      ```erlang
      compile:file(useless, [debug_info, export_all]).
      c(useless, [debug_info, export_all]).
      ```

    - 还可以在模块内部通过模块属性来定义编译选项

      ```erlang
      -compile([debug_info, export_all]).
      ```

- 可以使用 `hipe` 模块来编译成本地码，据说可以提速20%

  ```erlang
  hipe:c(Module, OptionsList).
  ```

- 在shell中调用 `c(Module, [native]).` 也能编译成本地码

- Erlang的宏和C语言的 `#define` 语句类似，主要用来定义简短的函数和常量

- Erlang中的宏是通过模块属性来定义的

  ```erlang
  -define(MACRO, some_value).
  ```

- 然后你就可以在模块的任意函数中使用宏 `?MACRO` 了，这个宏在代码编译前会被替换成 `some_value`

- 函数宏的定义方法类似

  ```erlang
  -define(sub(X,Y), X-Y).
  ```

- 调用函数宏和调用其他宏一样就行了

  ```erlang
  ?sub(23, 47).
  ```

- Erlang中有一些预定义的宏：

    - `?MODULE` 会被替换成当前模块的名字，是一个原子；
    - `?FILE` 会被替换成当前文件的名字，是一个字符串；
    - `?LINE` 会被替换成该宏所在的代码行的行号；

- 和C语言一样，检测某个宏是否已经在代码中定义使用属性 `-ifdef(MACRO).` 、 `-else` 和 `-endif`

  ```erlang
  -ifdef(DEBUGMODE).
  -define(DEBUG(S), io:format("dbg: " ++ S)).
  -else.
  -define(DEBUG(S), ok).
  -endif.
  ```

- Erlang同样可以实现条件编译

  ```erlang
  -ifdef(TEST).
  my_test_function() ->
    rum_some_tests().
  -endif.
  ```

- 编译模块时定义指定宏

  ```erlang
  c(Module, [{d, 'DEBUGMODE'}, {d, 'TEST'}]).
  ```

- 模块属性是描述模块自身的元数据。编译一个模块时，编译器会提取出大部分模块属性并把它们保存在 `module_info/0` 函数中

  ```erlang
  useless:module_info().
  ```

- 可以使用 `module_info/1` 函数来获取一些特定信息

  ```erlang
  useless:module_info(attributes).
  ```

- 如果你在module中增加了 `-author("An Erlang Champ").` ，那么它会出现在和 `vsn` 同样的区段中

- `vsn` 是一个自动生成的唯一值，用来区分代码的不同版本。它通常用在代码热加载以及某些发布管理工具中

- 可以通过在模块中增加 `-vsn(VersionNumber)` 属性来自行指定一个 `vsn` 值

- 关于模块设计一定要牢记：一定要避免环形依赖。如果模块B调用了模块A，那么模块A就不应该再去调用模块B。这样的依赖关系最终会导致代码难以维护

- 事实上，如果代码依赖于太多的模块，即使它们之间并不构成环形依赖，也会让代码变的难以维护
