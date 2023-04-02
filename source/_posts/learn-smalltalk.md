---
title: Smalltalk学习记录
author: shixiongfei
date: 2020-01-09 23:46:00
categories: 编程
tags: [编程, Smalltalk, Pharo]
---

## 执行代码

Do it(执行代码) `Ctrl + D` / `Cmd + D` ，用于执行表达式

Print it(打印结果) `Ctrl + P` / `Cmd + P` ，用于打印表达式结果

Inspect it(对象检查器) `Ctrl + I` / `Cmd + I` ，用于查看对象内部结构

## 数值类型

1, 2, 100, 2/3 ...这些都是数字，数字可以响应很多消息用于计算各种数学表达式

```smalltalk
2.

20 factorial.

1000 factorial / 999 factorial.

(1/3).

(1/3) + (4/5).

(1/3) asFloat.

1 class.

1 class maxVal class.

(1 class maxVal + 1) class.
```

## 字符类型

字符使用 `$` 操作符

```smalltalk
$A.

$A class.

$B charCode.

Character cr.

Character space.
```

我们可以打印256个Ascii字符

```smalltalk
Character allByteCharacters.
```

## 字符串类型

字符串是一个字符集合，使用 `'` 单引号将字符集合括起来即可

```smalltalk
'ProfStef'.

'ProfStef' size.

'abc' asUppercase.

'Hello World' reverse.
```

我们可以使用 `at:` 消息来访问字符串中的任意字符

```smalltalk
'ProfStef' at: 1.
```

字符串拼接可以使用 `,` 操作符

```smalltalk
'ProfStef', ' is cool'.
```

## 符号类型

符号是保证全局唯一的字符串。只有一个 `#Symbol` 符号对象，但是可以有多个 `'Symbol'` 字符串对象

(Message == returns true if the two objects are the SAME)

```smalltalk
'ProfStef' asSymbol.

#ProfStef asString.

(2 asString) == (2 asString).

(2 asString) asSymbol == (2 asString) asSymbol.
```

## 数组类型

静态数组在解析期间被创建

```smalltalk
#(1 2 3).

#(1 2 3 #(4 5 6)) size.

#(1 2 4) isEmpty.

#(1 2 3) first.

#('hello' 'World') at: 2 put: 'Pharo'; yourself.
```

动态数组在执行期间被创建

```smalltalk
{ (2+3) . (6*6) }.

{ (2+3) . (6*6) . 'hello', ' Stef'} size.
```

## 消息

消息会被发送给对象，在Smalltalk中有三种类型的消息：一元 `unary` 、二元 `binary` 和关键字 `keyword`

### 一元 `Unary` 消息的形式： `anObject aMessage`

  ```smalltalk
  1 class.

  false not.

  Time now.

  Date today.

  Float pi.
  ```

### 二元 `Binary` 消息的形式： `anObject + anotherObject`

  ```smalltalk
  3 * 2.

  Date today + 3 weeks.

  false | false.

  true & true.

  true & false.

  10 @ 100.

  10 <= 12.

  'ab', 'cd'.

  Date today < Date yesterday.
  ```

### 关键字 `Keyword` 消息的形式： `anObject akey: anotherObject akey2: anotherObject2`

  ```smalltalk
  4 between: 0 and: 10.
  ```

  这个是将 `between:and:` 消息发送给数字 `4`

  ```smalltalk
  1 max: 3.

  Color r:1 g:0 b:0.
  ```

  这里消息 `r:g:b:` 是由 `Color` 类实现的，我们也可以写成下面这种形式

  ```smalltalk
  Color
      r:1
      g:1
      b:0.
  ```

## 消息优先级

一元消息会最先执行，接着执行二元消息，最后才会执行关键字消息

- 一元 `Unary` > 二元 `Binary` > 关键字 `Keywords`

```smalltalk
2 + 3 squared.

2 raisedTo: 3 + 2.
```

在具有相似优先级的消息之间，表达式从左到右执行

```smalltalk
-3 abs negated reciprocal.
```

可以使用括号更改求值顺序

```smalltalk
(2 + 3) squared.

(2 raisedTo: 3) + 2.
```

传统的数学优先规则不适用于Smalltalk

```smalltalk
2 * 10 + 2.
```

这里消息 `*` 被发送到 2，2 回答 20，然后 20 接收消息 `+`

```smalltalk
2 + 2 * 10.

2 + (2 * 10).

8 - 5 / 2.

(8 - 5) / 2.

8 - (5 / 2).
```

记住，所有消息都遵循一个简单的从左到右的优先规则，没有例外

## `;` 运算符

`;` 是级联运算符，在向同一个接收者发送消息时非常有用

```smalltalk
Transcript show: 'hello'.
Transcript show: 'Pharo'.
Transcript cr.
```

这段代码等价于下面的写法

```smalltalk
Transcript
    show: 'hello';
    show: 'Pharo' ;
    cr.
```

## 块

块 `Blocks` 是匿名方法，可以存储到变量中并在需要的时候才执行

块被包含在一对方括号中： `[]`

```smalltalk
[:x | x+2].
```

这里有一个块，功能是对传入的参数做加2操作(这里的参数被命名为 `x` )，这里并不会真的执行块

```smalltalk
[:x | x+2] value: 5.
```

当我们想执行块的时候，需要向块发送 `value` 消息

```smalltalk
[:x | x+2] value: 10.

[:x :y| x + y] value: 3 value: 5.
```

块可以分配给变量，并延迟执行

通过 `| b |` 来声明变量 `b`，并使用 `：=` 操作符来为变量赋值

```smalltalk
|b|
b := [:x | x+2].
b value: 12.
```

## 条件分支

条件语句只是给布尔对象发送消息

```smalltalk
1 < 2
    ifTrue: [100]
    ifFalse: [42].
```

这里的消息是 `ifTrue:ifFalse:`

```smalltalk
3 > 10
    ifTrue: [Transcript show: 'Maybe there''s a bug ....']
    ifFalse: [Transcript show: 'No: 3 is less than 10'].
```

## 循环

循环是高级集合迭代器，作为常规方法实现

```smalltalk
to:do:
to:by:do:
```

标准循环写法：

```smalltalk
1 to: 100 do: [:i | Transcript show: i asString; cr].

1 to: 100 by: 3 do: [:i | Transcript show: i asString; cr].

100 to: 0 by: -2 do: [:i | Transcript show: i asString; cr].
```

消息 `do:` 被发送到集合对象( `Array` 、 `Set` 、 `OrderedCollection` )，为每个元素计算块

```smalltalk
#(11 38 3 -2 10) do: [:each |
    Transcript show: each printString; cr].
```

除了 `do:` 之外，还有许多其他好用的迭代器

```smalltalk
#(11 38 3 -2 10) collect: [:each | each abs].

#(11 38 3 -2 10) collect: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each odd].

#(11 38 3 -2 10) select: [:each | each > 10].

#(11 38 3 -2 10) reject: [:each | each > 10].

#(11 38 3 -2 10)
    do: [:each | Transcript show: each printString]
    separatedBy: [Transcript show: '.'].
```

## 对象

对象是一个类的实例。通常，我们将 `new` 消息发送给类来创建一个对象实例

```smalltalk
SimpleButtonMorph new
    label: 'A nice button';
    openCenteredInWorld.
```

如果向类发送 `allInstances` 消息，类将回应一个包含该类所有实例的数组

```smalltalk
SimpleButtonMorph allInstances size.
```

## 反射

你可以在运行时检查和更改系统

查看 `True` 类的方法 `ifFalse:ifTrue:` 的源码

```smalltalk
(True>>#ifFalse:ifTrue:) definition.
```

也可以查看注释

```smalltalk
(True>>#ifFalse:ifTrue:) comment.
```

使用 `respondsTo` 检查是否能够响应消息

```smalltalk
ProfStef respondsTo: #goToNextLesson.
```

删除一个方法

```smalltalk
ProfStef class removeSelector: #goToNextLesson.
```

## Pharo环境

Pharo是一个完整的环境，拥有诸如：窗口、文本、数字、日期、颜色、点等等。与其他编程语言相比，您可以更直接地与对象交互

每个对象都理解消息 `inspect` ，因此，你将获得一个Inspector窗口，该窗口可以显示有关对象的详细信息

```smalltalk
Date today inspect.
```

## 调试Debugger

调试器可能是Smalltalk环境中最著名的工具。一旦发生异常它将立即打开。

下面的代码将打开消息堆栈上的调试器，选择 `PharoSyntaxTutorial>>dividetwoyzero`

```smalltalk
PharoSyntaxTutorial new divideTwoByZero.
```
