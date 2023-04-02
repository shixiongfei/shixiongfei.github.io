---
title: Ruby极速入门
author: shixiongfei
date: 2019-06-07 22:42:00
categories: 编程
tags: [编程, Ruby]
---

因为工作关系需要用到Ruby和Rails，于是在端午假期里花了点时间快速的学习了下。这里做一个简单的记录。

## 基础数据类型

在Ruby中，所有的一切都是对象。每个对象都有一个唯一的对象标识符(object identifier，缩写为object ID)

### 整数类型(numbers)

整数是 `Fixnum` 类和 `Bignum` 类的对象。 `Fixnum` 对象可以容纳比本机字节长少一位的整数。当一个 `Fixnum` 超过这个范围时，它将会自动转换成 `Bignum` 对象， `Bignum` 对象的表示范围仅受可用内存大小的限制。如果 `Bignum` 对象的操作结果可以用 `Fixnum` 表示，那么结果将以 `Fixnum` 类型返回

整数由一个可选的符号标记、一个可选的进制指示符( `0` 代表八进制， `0d` 代表十进制， `0x` 代表十六进制， `0b` 代表二进制)和一个相应进制的字符串组成。数字串中的下划线会被忽略

```ruby
123456                  => 123456               # Fixnum
0d123456                => 123456               # Fixnum
123_456                 => 123456               # Fixnum -忽略下划线
-543                    => -543                 # Fixnum -负数
0xaabb                  => 43707                # Fixnum -十六进制
0377                    => 255                  # Fixnum -八进制
-0b10_1010              => -42                  # Fixnum -二进制(负数)
123_456_789_123_456_789 => 123456789123456789   # Bignum
```

### 浮点数类型(floats)

一个带有小数点或指数的数字字面量被认为是 `Float` 对象， `Float` 对象和本机上的 `double` 数据类型大小一样。小数点的前后都必须至少要有一个数字。像 `1.e3` 这样的串将试图调用 `Fixnum` 类的 `e3` 方法

```ruby
12.34                   => 12.34
-0.1234e2               => -12.34
1234e-2                 => 12.34
```

### 字符串(strings)

Ruby提供了多种机制来生成字面量字符串。每种机制都产生 `String` 类型的对象。不同机制的区别在于如何分隔字符串以及字面量内容中会进行哪些替换

单引号引起来的字符串字面量执行的替换最少。例如 `'stuff'` 和 `%q/stuff/` ，这两者都会将 `\\` 转换为单个反斜杠， `\'` 转换为单引号。所有其他的反斜杠都不会进行转换

```ruby
'hello'                     => hello
'a backslash \'\\\''        => a backslash '\'
%q/simple string/           => simple string
%q(nesting (really) works)  => nesting (really) works
%q no_blanks_here ;         => no_blanks_here
```

双引号字符串，例如 `"stuff"` ， `%Q/stuff/` 和 `%/stuff/` 还执行额外的替换

```ruby
a = 123
"\123mile"                  => Smile
"Say \"Hello\""             => Say "Hello"
%Q!"I said 'nuts'," I said! => "I said 'nuts'," I said
%Q{Try #{a+1}, not #{a-1}}  => Try 124, not 122
%<Try #{a+1}, not #{a-1}>   => Try 124, not 122
"Try #{a+1}, not #{a-1}"    => Try 124, not 122
%{#{ a=1; b=2; a+b }}       => 3
```

### 区间(ranges)

区间使用表达式 `expr..expr` 和 `expr...expr` 来构建 `Range` 对象。两个点的形式是闭合区间(包括右端的值)，而三个点的形式是半开半闭的(不包括右端的值)

```ruby
> (1..5).each {|n| puts n}
1
2
3
4
5
> (1...5).each {|n| puts n}
1
2
3
4
```

### 数组(arrays)

数组类的字面量是在方括号间由逗号分隔的一连串对象引用组成。尾部逗号将被忽略。数组也可以用简写形式 `%w` 和 `%W` 来构成

```ruby
arr = [ :fred, 10, 3.14, "this is a string", "pebbles", ]
    => [:fred, 10, 3.14, "this is a string", "pebbles"]

arr = %w( fred wilma barney betty great\ gazoo )
    => ["fred", "wilma", "barney", "betty", "great gazoo"]

arr = %w( Hey!\tIt is now -#{Time.now}-)
    => ["Hey!\\tIt", "is", "now", "-\#{Time.now}-"]

arr = %W( Hey!\tIt is now -#{Time.now}-)
    => ["Hey!\tIt", "is", "now", "-2019-06-09 11:35:50 +0800-"]
```

数组对象在方括号之间提供索引便可以访问单个元素，Ruby的数组索引从零开始

```ruby
a = [1, 'cat', 3.14]    => 三个元素数组
a[0]                    => 1
a[2] = nil              => 设置第三个元素
a                       => [1, "cat", nil]
```

### 散列表(hashes)

Ruby的Hash字面量是由花括号中的键/值对列表构成，由逗号分隔键/值对，键和值之间由 `=>` 序列分隔。尾部的逗号将被忽略。并不要求一个具体的散列表中的所有 `key` 和 `value` 必须为相同类型。散列表中的键必须能够响应 `hash` 消息并返回一直散列码(hash code)，且键对应的散列码不能改变。散列表中的键也必须能用 `eql?` 来比较

```ruby
colors = {
  "red"   => 0xf00,
  "green" => 0x0f0,
  "blue"  => 0x00f,
}
```

散列表也使用与数组相同的方括号表示法来进行索引

```ruby
colors["red"]           => 3840
colors["white"] = 0xfff => 4095
colors                  => {"red"=>3840, "green"=>240, "blue"=>15, "white"=>4095}
```

### 符号(symbols)

Ruby的符号是一个对应字符串(通常是一个名字)的标识符。可以通过在名字前加一个冒号来构造该名字的符号，也可以在任意字符串字面量前加一个冒号来创建该字符串的符号。在双引号的字符串会发生替换。不管程序如何使用名字，一个具体的名字或者字符串总是产生同样的符号

```ruby
:Object
:my_variable
:"Ruby rules"
a = 'cat'
:'catsup'       => :catsup
:"#{a}sup"      => :catsup
:'#{a}sup'      => :"\#{a}sup"
```

其他语言称这个过程为 `interning` ，而称符号为原子

### 正则表达式(regular expressions)

正则表达式的字面量是 `Regexp` 类型的对象。可以通过调用 `Regexp.new` 构造函数显示创建正则表达式，也可以使用字面量形式 `/pattern/` 和 `%r{pattern}` 隐式创建

```ruby
/pattern/
/pattern/options
%r{pattern}
%r{pattern}options
Regexp.new('pattern' [, options])
```

一旦有了正则表达式对象，可以使用 `Regexp#match(string)` 或匹配操作符 `=` (肯定匹配)和 `!` (否定匹配)对字符串进行匹配

```ruby
name = "Fats Waller"
name =~ /a/             => 1
name =~ /z/             => nil
/a/ =~ name             => 1
```

匹配操作符是有副作用的，它们会设置一些Ruby的变量。 `$&` 得到与模式匹配的那部分字符串， `` $` `` 得到匹配之前的那部分字符串，而 `$'` 得到匹配之后的那部分字符串

```ruby
$&                      => a            模式匹配的字符串
$`                      => F            模式匹配之前的字符串
$'                      => ts Waller    模式匹配之后的字符串
```

## 变量与常量

Ruby名字用来引用常用、变量、方法、类和模块。命名规则是第一个字符必须是大写字母、小写字母或下划线，后面跟任意个命名用字符：大写字母、小写字母、下划线或者数字的任意组合

### 局部变量名(local variable)

由小写字母后跟命名用字符组合。通常来说，使用下划线而不是 `camelCase` 来书写含多个词的名字

```ruby
three_two_one = 321
```

### 实例变量名(instance variable)

以一个 `@` 开始，后跟一个名字。在 `@` 后用小写字母是个不错的做法

```ruby
@name   @_    @size
```

### 类变量名(class variable)

以 `@@` 开始，后跟一个名字

```ruby
@@name  @@_   @@size
```

### 常量名(const variable)

以一个大写字母开始，后跟多个命名用字符。类名字和模块名字都是常量，所以遵循常量的命名传统。习惯上，常量对象引用一般由大写字母和下划线组成，而类和模块名字是 `MixedCase`

```ruby
module Math
  ALMOST_PI = 22.0/7.0
end

class BigBlob
end
```

### 全局变量名(global variable)

由美元符 `$` 后跟命名用字符组成。全局变量在程序中任何地方都可以修改，一般不推荐使用全局变量

```ruby
$temp = "this is a global variable"
```

### 多重赋值

Ruby可以对一组变量同时赋值。如果变量前加上 `*` ，则Ruby会将未分配的值封装为数组赋值给该变量

```ruby
a, b, c = 1, 2, 3
a, b, *c = 1, 2, 3, 4, 5    => a=1, b=2, c=[3,4,5]
a, *b, c = 1, 2, 3, 4, 5    => a=1, b=[2,3,4], c=5
```

## 条件判断

### 条件与真假

在Ruby中，只有 `false` 和 `nil` 为假，除此以外的所有值都代表真。另外，在Ruby中还有个约定俗成的规则，为了使程序容易理解，返回真假的方法都要以 `?` 结尾

### 逻辑运算符

Ruby中的逻辑运算符有 `&&` 、 `||` 和 `!` ，同时还有意思相同，但优先级略低的逻辑运算符 `and` 、 `or` 、 `not` 。通常前者用于逻辑判断，后者用于流程控制

条件1与条件2必须都为真，表达式返回真

```ruby
条件1 && 条件2
条件1 and 条件2
```

条件1或条件2其中一个为真，表达式则返回真

```ruby
条件1 || 条件2
条件1 or 条件2
```

取相反条件，返回条件表达式的相反逻辑

```ruby
!条件
not 条件
```

### 比较操作符

Ruby的语法定义了比较操作符 `==` 、 `===` 、 `<=>` 、 `<` 、 `<=` 、 `>` 、 `>=` 、 `=~` 。所有这些操作符都是由方法来实现的。习惯上，Ruby也使用标准方法 `eql?` 和 `equal?` 。其中 `==` 和 `=` 都有相反的形式 `!=` 和 `!~`

### if语句

if语句是最基本的条件判断语句，形式如下：

```ruby
if 条件1 then
  处理1
elsif 条件2 then
  处理2
elsif 条件3 then
  处理3
else
  处理4
end
```

可以省略 `then`

### unless语句

unless语句的用法刚好与if相反，形式如下：

```ruby
unless 条件 then
  处理1
else
  处理2
end
```

可以省略 `then`

### if修饰符与unless修饰符

`if` 与 `unless` 可以像下面这样写在希望执行的代码后面

```ruby
puts "a > b" if a > b
```

在使用修饰符写法时，要注意程序的易读性

### 三元运算符

三元运算符是 `if...else...` 的简写形式

```ruby
条件 ? 处理1 : 处理2
```

### case语句

当比较对象只有一个，根据这个对象值的不同，执行不同的处理时，使用case语句会使程序更简单，更便于理解

```ruby
case 比较对象
when 值1 then
  处理1
when 值2 then
  处理2
when 值3 then
  处理3
else
  处理4
end
```

可以省略 `then`

## 循环

### times方法

times方法可以轻松实现单纯执行一定次数的处理

```ruby
循环次数.times do
  希望循环的处理
end
```

`do...end` 部分可以用 `{...}` 代替

```ruby
循环次数.times {
    希望循环的处理
}
```

使用times方法时也可以获知块中当前的循环次数

```ruby
5.times do |i|
  puts "第 #{i} 次的循环"
end
```

### for语句

for并不是方法，而是Ruby提供的循环控制语句。形式如下：

```ruby
for 变量 in 开始值..结束值 do
  希望循环的处理
end

for 变量 in 对象 do
  希望循环的处理
end
```

可以省略 `do`

```ruby
sum = 0
for i in 1..5
  sum = sum + i
end
puts sum

names = ["awk", "Perl", "Python", "Ruby"]
for name in names
  puts name
end
```

### while语句

不管哪种类型的循环，while语句都可以胜任，while语句的结构如下：

```ruby
while 条件 do
  希望循环的处理
end
```

可以省略 `do`

```ruby
i = 1
while i < 3
  puts i
  i += 1
end
```

### until语句

until是与while相对的语句，until语句的结构与while语句完全一样，只是条件判断刚好相反，不满足条件时才执行循环处理

```ruby
until 条件 do
  希望循环的处理
end
```

可以省略 `do`

```ruby
sum = 0
i = 1
until sum >= 50
  sum += i
  i += 1
end
puts sum
```

### each方法

each方法是将对象集合中的对象逐个取出，这与for语句循环取出数组元素非常相似

```ruby
对象.each do |变量|
  希望循环的处理
end

对象.each {|变量|
  希望循环的处理
}
```

在Ruby内部，for语句是用each方法来实现的。因此可以使用each方法的对象，同样也可以指定为for语句的循环对象

```ruby
sum = 0
(1..5).each do |i|
  sum = sum + i
end
puts sum
```

### loop方法

还有一种循环的方法，没有终止循环的条件，只是不断执行循环处理

```ruby
loop do
  希望循环的处理
end
```

程序不小心执行了死循环，可以使用 `Ctrl + C` 来强行终止程序

```ruby
loop do
  puts "Ruby"
end
```

### 循环控制

`break` 终止处理，跳出循环

`next` 跳到下一次循环

`redo` 在相同的条件下重复刚才的处理。一般很少使用

## 方法

### 调用

简单的方法调用

```ruby
对象.方法名(参数1, 参数2, ..., 参数n)
```

对象也被称为接收者(receiver)。在面向对象的世界中，调用方法被称为“向对象发送消息(message)”，调用的结果就是“对象接收(receive)了消息”。也就是说，方法的调用就是几个参数连同消息一起发送给对象的过程

带块的方法调用

```ruby
对象.方法名(参数, ...) do |变量1, 变量2, ...|
  块内容
end

对象.方法名(参数, ...) { |变量1, 变量2, ...|
  块内容
}
```

`|~|` 中指定的变量称为块变量。在执行块的时候，块变量由方法传到块内部

```ruby
5.times do |i|
  puts i
end
```

### 定义

一般方法的定义

```ruby
def 方法名(参数1, 参数2, ..., 参数n)
  希望执行的处理
end
```

我们可以用 `return` 语句指定方法的返回值

```ruby
return 值
```

通常方法的最后一个表达式结果就会成为方法的返回值，这时可以省略 `return` 语句

通过 `yield` 关键字我们可以定义带块的方法

```ruby
def myloop
  while true
    yield
  end
end

num = 1
myloop do
  puts "num is #{num}"
  break if num > 10
  num *= 2
end
```

可以通过 `*变量名` 的形式来定义参数个数不确定的方法。同样可以用 `*数组` 的形式来指定参数

```ruby
def foo(*args)
  args
end

p foo(1, 2, 3)      => [1, 2, 3]
p foo(*[4, 5, 6])   => [4, 5, 6]
```

定义带关键字参数的方法

```ruby
def 方法名(参数1: 参数1的值, 参数2: 参数2的值, ...)
  希望执行的处理
end
```

如果没有默认值的话，参数的值可以省略。我们还可以使用 `**args` 的形式来接收未定义的参数

```ruby
def volume(x:, y: 2, z: 4, **args)
  [x, y, z, args]
end

p volume(x: 2, y: 3)        => [2, 3, 4, {}]
p volume({x: 3, k: 5})      => [3, 2, 4, {:k=>5}]
```

## 类和对象

### 类和继承

类表示对象的种类，Ruby中的对象一定都属于某个类。通过扩展已定义的类来创建新类称为继承。

```ruby
class 类名 [< 父类名]
  类的定义
end
```

类的首字母必须大写。类中的 `initialize` 方法比较特别。使用 `new` 方法生成新的对象时， `initialize` 方法会被调用，同时 `new` 方法的参数也会被原封不动地传给 `initialize` 方法

```ruby
class HelloWrold
  def initialize(myname = "Ruby")
    @name = myname
  end
end

bob = HelloWrold.new("Bob")
ruby = HelloWrold.new
```

### 存取器

在Ruby中，从对象外部不能直接访问实例变量或对实例变量修改，需要通过方法来访问对象内部。Ruby为我们提供了简便的定义方法 `attr_reader` 、 `attr_writer` 、 `attr_accessor`

`attr_reader :name` 只读(定义name方法)

`attr_writer :name` 只写(定义name=方法)

`attr_accessor :name` 读写(同时定义以上两个方法)

### 类方法

方法的接收者就是类本身的方法称为类方法

```ruby
class << 类名
  方法定义
end

def 类名.方法名(参数, ...)
end
```

下面展示四种定义方法

```ruby
class << HelloWorld
  def hello(name)
    puts "#{name} said hello."
  end
end

HelloWorld.hello("John")        => John said hello.

class HelloWorld
  class << self
    def hello(name)
      puts "#{name} said hello."
    end
  end
end

HelloWorld.hello("Allen")       => Allen said hello.

def HelloWorld.hello(name)
  puts "#{name} said hello."
end

HelloWorld.hello("Amber")       => Amber said hello.

class HelloWorld
  def self.hello(name)
    puts "#{name} said hello."
  end
end

HelloWorld.hello("Chloe")       => Chloe said hello.
```

### 类变量和常量

类变量是该类所有实例的共享变量，这一点与常量类似

```ruby
class HelloWorld
  Version = "1.0"
  @@count = 0

  def self.count
    @@count
  end

  def initialize(myname = "Ruby")
    @name = myname
  end

  def hello
    @@count += 1
    puts "Hello, world. I am #{@name}."
  end
end

bob = HelloWorld.new("Bob")
alice = HelloWorld.new("Alice")
ruby = HelloWorld.new

p HelloWorld.count      => 0
bob.hello
alice.hello
ruby.hello
p HelloWorld.count      => 3
```

### 限制方法调用

`public` 以实例方法的形式向外部公开该方法

`private` 只能使用缺省接收者的方式调用该方法

`protected` 在同一个类中时可将该方法作为实例方法调用

```ruby
class Point
  attr_accessor :x, :y
  protected :x=, :y=

  def initialize(x=0.0, y=0.0)
    @x, @y = x, y
  end

  public

  def swap(other)
    tmp_x, tmp_y = @x, @y
    @x, @y = other.x, other.y
    other.x, other.y = tmp_x, tmp_y

    return self
  end
end

p1 = Point.new(1.0, 2.0)
p2 = Point.new(3.0, 4.0)

p [p1.x, p1.y]
p [p2.x, p2.y]

p1.swap(p2)

p [p1.x, p1.y]
p [p2.x, p2.y]
```

### alias与undef

有时我们希望给已经存在的方法设置别名，这种情况下就需要使用 `alias` 方法

```ruby
alias 别名 原名
alias :别名 :原名
```

`undef` 用于删除已定义的方法

```ruby
undef 方法名
undef :方法名
```

### 模块

模块是Ruby的特色功能之一

```ruby
module
  模块定义
end
```

模块不能拥有实例，模块不能被继承。如果希望把方法作为模块函数公开给外部使用，就需要用到 `module_function` 方法

```ruby
module HelloModule
  Version = "1.0"

  def hello(name)
    puts "Hello, #{name}"
  end

  module_function :hello
end

p HelloModule::Version      => 1.0
HelloModule.hello("Alice")  => Hello, Alice

include HelloModule
Version                     => 1.0
hello("Alice")              => Hello, Alice
```

使用 `Mix-in` 就能将模块混合到类中。在定义类时使用 `include` ，模块中的方法、常量就能被类使用

```ruby
module M
  def meth
    "meth"
  end
end

class C
  include M
end

c = C.new
p c.meth        => meth
```

## 错误与异常处理

### 异常处理

Ruby异常是 `Exception` 或其子类的对象，标准写法如下：

```ruby
begin
  可能会发生异常的处理
rescue
  发生异常时的处理
end
```

Ruby中异常及其相关信息都是被作为对象来处理的。在 `rescue` 后指定变量名，可以获得异常对象

```ruby
begin
  可能会发生异常的处理
rescue => 引用异常对象的变量
  发生异常时的处理
end
```

异常对象方法有 `class` 异常种类、 `message` 异常信息和 `backtrace` 异常发生的位置信息

### 后处理

不管是否发生异常都希望执行的处理，在Ruby中可以用 `ensure` 关键字来定义

```ruby
begin
  可能会发生异常的处理
rescue => 变量
  发生异常时的处理
ensure
  不管是否发生异常都要执行的处理
end
```

### 重试

在 `rescue` 中使用 `retry` 后， `begin` 以下的处理会再重新执行一遍

```ruby
begin
  io = File.open(file)
rescue
  sleep 10
  retry
end
```

### rescue修饰符

与 `if` 修饰符、 `unless` 修饰符一样， `rescue` 也有对应的修饰符

```ruby
表达式1 rescue 表达式2
```

如果表达式1中发生异常，表达式2的值就会成为整个表达式的值

```ruby
n = Integer(val) rescue 0
```

### 异常补充

如果异常处理的范围是方法的整个处理，也就是说整个方法内的程序都用 `begin...end` 括起来的话，就可以省略 `begin` 以及 `end` ，直接书写 `rescue` 与 `ensure` 部分的程序

```ruby
def foo
  方法处理
rescue => 变量
  异常处理
ensure
  后处理
end

class Foo
  类定义
rescue => 变量
  异常处理
ensure
  后处理
end
```

当存在多个种类的异常，且需要按异常的种类分别进行处理时，可以用多个 `rescue` 来分开处理

```ruby
begin
  可能发生异常的处理
rescue Exception1, Exception2 => 变量
  异常处理1
rescue Exception3 => 变量
  异常处理2
rescue
  以上异常之外的异常处理
end
```

### 主动抛出异常

`raise 信息` - 抛出 `RuntimeError` 异常，并在新生成的异常对象中设置作为信息的字符串

`raise 异常类` - 抛出指定的异常

`raise 异常类, 信息` - 抛出指定的异常，并在新生成的异常对象中设置作为信息的字符串

`raise` - 在 `rescue` 外抛出 `RuntimeError` 。在 `rescue` 中调用时，会再次抛出最后一次发生的异常( `$!` )

## 块(block)

块(block)就是在调用方法时能与参数一起传递的多个处理的集合

### 控制块的执行

同循环一样，块也使用 `break` 、 `next` 和 `redo` 作为流程控制

### 将块封装为对象

如上所述，在接收块的方法中执行块时，可以使用 `yield` 关键字。而Ruby还能把块当作对象处理。把块当对象处理后，就可以在接收块的方法之外的其他地方执行块，或者把块交给其他方法执行

把块当对象操作时，我们需要用到 `Proc` 对象。定义 `Proc` 对象的典型方法是，调用 `Proc.new` 这个带块的方法。在调用 `Proc` 对象的 `call` 方法之前，块中定义的程序不会被执行

```ruby
hello = Proc.new do |name|
  puts "Hello, #{name}"
end

hello.call("World")
hello.call("Ruby")
```

在定义方法时，如果末尾的参数使用 `&参数` 的形式，Ruby就会自动把调用方法时传进来的块封装为Proc对象

```ruby
def total(from, to, &block)
  result = 0
  from.upto(to) do |num|
    if block
      result += block.call(num)
    else
      result += num
      end
  end
  return result
end

p total(1, 10)                      => 55
p total(1, 10) {|num| num ** 2}     => 385
```

### 作用域

块内部的命名空间和块外部是共享的。在块外部定义的局部变量，在块中也可以继续使用。嗯！简而言之，这可以让Ruby轻松实现函数式的闭包。

```ruby
def counter
  cnt = 0
  Proc.new do
    cnt += 1
  end
end

c1 = counter
c2 = counter

c1.call         => 1
c1.call         => 2
c1.call         => 3
c2.call         => 1
c2.call         => 2
```

## 资源

[Ruby官网](https://www.ruby-lang.org)

[Ruby文档](https://ruby-doc.org)
