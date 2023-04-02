---
title: Erlang极简学习笔记<07>——错误和异常篇
author: shixiongfei
date: 2019-05-20 23:19:00
categories: 编程
tags: [编程, Erlang]
---

- Erlang是一门具有两种主要范型的语言：函数式和并发

- Erlang闻名于世的是其并发部分的内容：actor模型、数百万个并发进程、监督树等

- 虽然Erlang中提供了处理函数式代码中错误的方法，但是你时常听到的却是任其崩溃（let it crash）。这种错误处理机制位于Erlang语言的并发部分

- 错误有很多种：编译期错误、逻辑错误以及运行时错误

- 编译期错误通常是一些语法错误

- 常见编译期出错消息以及可能的解决办法，可供参考
  
  - module.beam: Module name 'madule' dose not match file name 'module'
    - 在-module属性中输入的模块名和文件名不匹配

  - ./module.erl:2: Warning: function some_function/0 is unused
    - 函数没有被导出，或者它的调用者使用了错误的名字或参数个数
    - 还可能是之前编写的函数现在不再需要了
    - 请检查代码！

  - ./module.erl:2: function some_function/1 undefined
    - 函数不存在。在-export属性中或者函数定义时写错了函数名或参数个数
    - 当某个函数无法编译时也会显示这个错误，这通常是因为语法错误
    - 例如，忘记了用句点结束函数定义

  - ./module.erl:5: syntax error before: 'SomeCharacterOrWord'
    - 出现这个错误的原因有许多
    - 常见原因有括号、元组缺少结束符号，或者表达式结束符错误（例如，在case语句的最后一个分之结束时使用了逗号）
    - 其他原因包括代码中使用了保留原子
    - 或者在不同编码之间不正确地转换了unicode字符

  - ./module.erl:5: syntax error before:
    - 这条出错消息的信息含量要少于上一条
    - 通常在行结束符不正确时出现这条错误
    - 它是上一条出错消息的特例，要保持警惕

  - ./module.erl:5: Warning: this expression will fail with a 'badarith' exception
    - 虽然Erlang是动态类型语言，但是别忘了它是强类型的。
    - 出现这个错误消息，是由于编译器聪明地发现有一个算术表达式会失败（如 `llama+5` ）
    - 不过对于更复杂的类型错误，编译器是无法发现的

  - ./module.erl:5: Warning: variable 'Var' is unused
    - 定义了一个变量但是从来没有使用过它
    - 这可能是代码bug造成的，因此请仔细检查编写的代码
    - 对于其他情况，可以把变量名改成 `_` ，或者如果觉得名字能让代码更易读，那么可以在变量名前增加一个下划线前缀

  - ./module.erl:5: Warning: a term is constructed, but never used
    - 在某个函数中，做了某些事情，如构建了一个列表或者定义了元组或匿名函数，但是没有把它绑定到变量或者返回
    - 这个警告表明你做了一些无用的工作或者犯了错误

  - ./module.erl:5: head mismatch
    - 函数可能会有多个头定义，每个头的参数个数可能不同
    - 别忘了不同的参数个数意味着不同的函数，并且不能把不同参数个数的函数头交叉定义
    - 同样地，当把一个函数的定义放到另外一个函数的头子句之间时，也会导致这个错误

  - ./module.erl:5: Warning: this clause cannot match because a previous clause at line 4 always matches
    - 在模块中定义函数时，把一个特定的子句放在了一个匹配一切的子句之后
    - 此时编译器会给出警告，告知永远不会执行到这个子句

  - ./module.erl:9: variable 'A' unsafe in 'case' (line 5)
    - 在 `case ... of` 的某个分支内定义了一个变量，但是在这个分支外面使用了这个变量
    - 这被认为是不安全的
    - 如果想使用这种变量，最好这样做： `MyVar = case ... of`

- 这个列表基本上覆盖了当前所有的编译期错误。错误有很多，并且通常来说，最困难的工作往往是找到导致其他函数发生大量级联错误的那个错误

- 在解决编译错误时，最好按照报告的顺序进行，这样可以避免被那些实际上根本不是错误的错误所误导

- 逻辑错误最难被发现和调试。逻辑错误通常是由程序员引入的：条件语句(如 `if` 和 `case` )的分支没能考虑到所有情况、在本该使用除法的地方使用了乘法等。它们不会让程序崩溃，但是会导致隐蔽的数据错误以及不及期望的程序行为

- 解决逻辑错误时，常常只能依靠自己。不过Erlang中有不少工具，可以给你提供帮助，这些工具包括测试框架、TypEr和Dialyzer工具以及跟踪调试模块

- 在这里，我们将关注那些会导致程序崩溃的错误，因为它们就出现在事发地，不会传播到相离错误发生地很远的其他代码中。这基本上就是“任其崩溃”的思想起源

- 运行时错误非常具有破坏性，因为它们会导致程序崩溃

- 下面是Erlang中一些常见的运行时错误以及产生这些错误的代码示例

1. 函数子句错误

    - 发生函数子句(function clause)错误最可能的原因是，函数的所有卫语句或者所有模式匹配都失败了

      ```erlang
      > lists:sort(ffffffff).
      ** exception error: no function clause matching lists:sort(ffffffff) (lists.erl, line 480)
      ```

2. case子句错误

    - 当忘记一个特定情况、传入的数据类型错误或者需要一个匹配一切的子句时，会发生case子句(case clause)错误

      ```erlang
      > case "Unexpected Value" of
      >   expected_value -> ok;
      >   other_expected_value -> 'also ok'
      > end.
      ** exception error: no case clause matching "Unexpected Value"
      ```

3. if子句错误

    - if子句(if clause)错误的原因和case子句类似。当Erlang找不到一个可以求值为true的分支时，会引发这个错误

      ```erlang
      > if 2 > 4 -> ok;
      >    0 > 1 -> ok
      > end.
      ** exception error: no true branch found when evaluating an if expression
      ```

4. 不正确匹配错误

    - 当模式匹配失败时，就会出现不正确匹配(bad match)错误。这通常意味着你试图进行不可能的模式匹配、对一个变量进行二次绑定或者在 `=` 操作符两边放置了不想等的东西

      ```erlang
      > [X, Y] = {4, 5}.
      ** exception error: no match of right hand side value {4,5}
      ```

5. 不正确参数错误

    - 不正确参数(bad argument)错误和函数子句错误类似，因为它们都和使用不正确的参数调用函数有关

      ```erlang
      > erlang:binary_to_list("heh, already a list").
      ** exception error: bad argument
          in function  binary_to_list/1
              called as binary_to_list("heh, already a list")
      ```

6. 未定义函数错误

    - 当调用了一个不存在的函数时，会发生未定义函数(undefined function)错误。要确保函数从模块中导出了，并且参数个数正确，仔细检查函数的名字和模块的名字，保证输入正确。当模块不在Erlang的搜索路径上时，也会发生这个错误。默认情况下，Erlang的搜索路径是当前目录。可以通过 `code:add_patha("/some/path/")` 或者 `code:add_pathz("some/path")` 把路径增加到搜索列表中。如果还不能解决问题，看看模块是否被编译过

      ```erlang
      > lists:random([1, 2, 3]).
      ** exception error: undefined function lists:random/1
      ```

7. 不正确算术计算错误

    - 当试图进行不正确的算术计算时，会发生不正确算术计算(bad arithmetic)错误，如除0或者在原子和数值之间进行算术计算

      ```erlang
      > 5 + llama.
      ** exception error: an error occurred when evaluating an arithmetic expression
          in operator  +/2
              called as 5 + llama
      ```

8. 不正确函数错误

    - 导致不正确函数(bad function)错误最常见的原因是把变量当成函数使用，但是变量的值并不是函数

      ```erlang
      > hhfuns:add(one,two).
      ** exception error: bad function one
          in function hhfuns:add/2 (hhfuns.erl, line 7)
      ```

9. 不正确元数错误

    - 不正确元数(bad arity)错误是不正确函数错误的特殊情况。当使用高阶函数时，给它们传递的参数个数多于或少于实际参数个数时会出现这个错误

      ```erlang
      > F = fun(_) -> ok end.
      #Fun<erl_eval.6.128620087>
      > F(a, b).
      ** exception error: interpreted function with arity 1 called with two arguments
      ```

10. 系统限制错误

    - 出现系统限制错误的原因有很多，下面是一些常见的：

      - 进程太多
      - 原子太长
      - 函数参数个数太多
      - 原子太多
      - 连接的节点数太多

- 要想监控代码的执行并防止逻辑错误，引发运行时崩溃，从而尽早定位问题往往是一种好方法

- 在Erlang中有3种异常：出错(error)、退出(exit)和抛出(throw)

- 出错异常：调用 `erlang:error(Reason)` 会结束当前进程的执行，如果你捕获了这个异常，那么其中会包括最后几次函数调用的栈跟踪和参数列表。出错异常(error exception)会引发运行时错误。当代码对当前情况无法处理时，可以使用出错异常结束执行

  1. 不适合出错异常的情形

      - 在进行查询时，模块不一定总能找到一个特定的数据。此时，让调用者去处理这个未知的结果是合理的。调用者可以使用一个默认值、插入一个新值或者做其他处理。在这种情况下，返回 `{ok, Value}` 元组或者一个像 `undefined` 这样的原子，要比引发错误异常更为合适

  2. 自定义错误

      - 出错异常并不局限于Erlang语言所提供的那些。你可以自定义出错异常

        ```erlang
        > erlang:error(badarith).
        ** exception error: an error occurred when evaluating an arithmetic expression
        > erlang:error(custom_error).
        ** exception error: custom_error
        ```

- 退出异常：Erlang中有两种退出异常(exit exception)

- 内部退出：这种异常在调用函数 `exit/1` 时触发，会导致当前进程结束执行
- 外部退出：这种异常在调用函数 `exit/2` 时触发，在多进程环境中使用

- 抛出异常：当期望程序员处理所发生的异常时，可以抛出(throw)异常。与错误异常和退出异常不同，抛出异常并没有“让进程崩溃！”的意思，只是为了改变控制流

```erlang
> throw(permission_denied).
** exception throw: permission_denied
```

- 注意：如果使用了 `throw` ，并期望程序员去处理它们，在抛出这些异常的模块中对其进行文档描述是一种很好的做法

- 在深度递归中，抛出异常可以用于非局部返回(nonlocal return)。一般来说，在用抛出异常进行非局部返回时，尽量把它限制在一个模块中，这样调试起来就比较容易。这样做还可以让你在更改模块内部实现时无需更改接口

- 抛出异常、出错异常和退出异常都可以被处理。处理方式是使用 `try...catch` 表达式

- `try...catch` 是一种表达式的求值方法，它可以在处理成功情况的同时处理出现的错误

```erlang
try Expression of
  SuccessfulPattern1 [Guards] ->
    Expression1;
  SuccessfulPattern2 [Guards] ->
    Expression2
catch
  TypeOfError:ExceptionPattern1 ->
    Expression3;
  TypeOfError:ExceptionPattern2 ->
    Expression4
end.
```

- `try` 和 `of` 之间的 `Expression` 是被保护的对象。这意味着，发生在这个表达式中的任何异常都会被捕获到

- `try...of` 和 `catch` 之间的模式和表达式的行为方式与 `case...of` 完全一样。它们不会被保护，在其中允许出现模式匹配、变量绑定以及卫语句

- 在catch部分中，可以针对不同的异常类型把 `TypeOfError` 替换卫 `error` 、 `throw` 或者 `exit` 。如果没有提供任何类型，会默认为 `throw` 类型

```erlang
-module(exceptions).
-compile(export_all).

throws(F) ->
  try F() of
    _ -> ok
  catch
    Throw -> {throw, caught, Throw}
  end.

errors(F) ->
  try F() of
    _ -> ok
  catch
    error:Error -> {error, caught, Error}
  end.

exits(F) ->
  try F() of
    _ -> ok
  catch
    exit:Exit -> {exit, caught, Exit}
  end.
```

```erlang
Eshell
> c(exceptions).
{ok,exceptions}
> exceptions:throws(fun() -> throw(thrown) end).
{throw,caught,thrown}
> exceptions:errors(fun() -> erlang:error("Die!") end).
{error,caught,"Die!"}
> exceptions:exits(fun() -> exit(goodbye) end).
{exit,caught,goodbye}
```

- `try...catch` 之后可以增加一个子句 `after` ， `after` 子句始终会执行

```erlang
try Expression of
  Pattern -> Expr1
catch
  Type:Exception -> Expr2
after
  Expr3
end.
```

- `after` 子句和很多其他语言中的 `finally` 等价。无论是否出现错误， `after` 部分中的表达式都一定会运行。不过， `after` 子句不会返回任何值。因此其中通常会运行带有副作用的代码。例如：无论是否发生异常，都要确保打开的文件被关闭，是这个子句的标准应用之一

- 事实上，`try` 和 `of` 之间可以多个表达式。当使用了多个表达式时，我们可能就不太会在意返回值了。因为 `of` 部分就变得没什么用了，我们可以直接把它去掉

```erlang
try
  talk(),
  _Knight = "None shall pass!",
  _Doubles = [N*2 || N <- lists:seq(1, 200)],
  throw(up),
  _WillReturnThis = tequila
catch
  Exception:Reason -> {caught, Exception, Reason}
end.
```

- Erlang中 `catch` 关键字可以捕获所有类型的异常，也能返回正常结果

```erlang
catcher(X,Y) ->
  case catch X/Y of
    {'EXIT', {badarith, _}} -> "uh oh";
    N -> N
  end.
```

```erlang
Eshell
> c(exceptions).
{ok,exceptions}
> exceptions:catcher(3,3).
1.0
> exceptions:catcher(6,3).
2.0
> exceptions:catcher(6,0).
"uh oh"
```

- 不过 `catch` 和 `=` 之间存在冲突，这是由语言中操作符的优先级造成的。唯一避免冲突的办法就是把 `catch` 放到括号中

```erlang
> X = catch 4+2.
* 1: syntax error before: 'catch'
> X = (catch 4+2).
6
```
