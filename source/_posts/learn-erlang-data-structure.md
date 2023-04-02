---
title: Erlang极简学习笔记<08>——常用数据结构篇Erlang极简学习笔记<08>——常用数据结构篇
author: shixiongfei
date: 2019-06-11 22:32:00
categories: 编程
tags: [编程, Erlang]
---

- 记录(record)是一种拼凑物。他们是在语言实现完毕后临时添加上去的，因此使用起来有些不方便。但是，如果数据结构比较小，并且想直接通过名字去访问属性字段，那么使用记录还是很合适的。记录的这种使用方式和C语言中的结构很像

- 记录以模块属性的形式声明

```erlang
-module(records).
-compile(export_all).

-record(reboot, {name,
                  type=industrial,
                  hobbies,
                  details=[]})

first_rebot() ->
    #rebot{
        name="Mechatron",
        type=handmade,
        details=["Moved by a small man inside"]
    }.
```

```erlang
Eshell
> c(records).
> records:first_robot().
```

- Erlang的记录只是元组之上的语法糖

- Erlang shell提供了一条命令 `rr(Module)` ，可以加载 `Module` 中定义的记录

- 读取记录字段值的方法有两种：专用的点语法以及模式匹配

- 点语法提取记录的值。语法不是很漂亮，因为记录在本质上就是元组的缘故

```erlang
Eshell
> rr(records).
> Crusher = #rebot{name="Crusher", hobbies=["Crushing people", "petting cats"]}.
> Crusher#rebot.hobbies.
["Crushing people","petting cats"]
```

- 可以在函数头中对它们进行模式匹配，也可以在卫语句中。实际编码中，我们不需要去匹配元组中的所有元素，甚至在编写函数时也不必知道元组中到底有多少元素。我们只需匹配我们用到的字段即可

```erlang
-record(user, {id, name, group, age}).

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

adult_section(U = #user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.
```

```erlang
Eshell

> c(records).
> rr(records).
> records:admin_panel(#user{id=1, name="fred", group=admin, age=96}).
"fred is allowed!"
> records:admin_panel(#user{id=2, name="you", group=users, age=66}).
"you is not allowed"
> records:adult_section(#user{id=21, name="Bill", group=users, age=72}).
allowed
> records:adult_section(#user{id=22, name="Noah", group=users, age=13}).
forbidden
```

- 更新记录的语法有些特殊，其实这只是一种为了调用底层 `erlang:setelement/3` 函数的编译技巧

```erlang
repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman" | Details]},
    {repaired, NewRob}.
```

```erlang
Eshell
> c(records).
> rr(records).
> records:repairman(#robot{name="Ulbert", hobbies=["trying to have fellings"]}).
{repaired,#robot{name = "Ulbert",type = industrial,
                  hobbies = ["trying to have fellings"],
                  details = ["Repaired by repairman"]}}
```

- 如果记录很有用，但是又想避免重复定义记录，那么可以使用头文件在多个模块之间共享记录定义，这是Erlang程序员常用的方法。Erlang的头文件和C中的类似

```erlang
%% 这是一个.hrl头文件
-record(included, {
    some_field,
    some_default = "yeah!",
    unimaginative_name
}).
```

- 要在模块中包含这个头文件，只需要增加一行 `-include(...).`

```erlang
-include("records.hrl").

%% 增加测试函数
included() -> #included{some_field="Some Value"}.
```

```erlang
Eshell
> c(records).
> records:included().
{included,"Some Value","yeah!",undefined}
```

- Erlang提供了多种键/值存储结构，有适合小数据量存储的有序字典( `orddict` )，还有适合大数据量存储的字典( `dict` )和通用平衡书( `GB` 树)

- 有序字典( `orddict` )所存储的数据项必须是严格的 `{Key, Value}` 形式，每个键都只能出现一次

- 键/值存储常用的CRUD(创建、读取、更新和删除)、 `orddict:store/3` 、 `orddict:find/2` 、 `orddict:fetch/2` 以及 `orddict:erase/2` ，可以使用 `orddict:new/0` 或者 `orddict:from_list/1` 创建一个有序字典

- 一般来讲，对于小于75个元素的数据量来说，有序字典在复杂度和效率之间做到了很好的平衡。如果超过了这个数据量，应该切换到其他的键/值存储

- 字典( `dict` )的接口和有序字典( `orddict` )完全一致： `dict:store/3` 、 `dict:find/2` 、 `dict:fetch/2` 以及 `dict:erase/2` 。字典中也具有有序字典的所有其他函数，如 `dict:map/2` 和 `dict:fold/2`

- `GB` 树是由 `gb_trees` 模块实现的，其中包含的函数要比 `dict` 中多，在数据结构的使用上提供了更多的直接控制手段

- `gb_trees` 有两种主要的工作模式：一种针对彻底了解自己数据的情况(称之为智能模式)，还有一种针对不能对数据数据做假设的情况(称之为简单模式)

- 在简单模式中，数据操作函数为 `gb_trees:enter/2` 、 `gb_trees:lookup/2` 和 `gb_trees:delete_any/2`

- 在智能模式中，相应的函数为 `gb_trees:insert/3` 、 `gb_trees:get/2` 、 `gb_trees:update/3` 和 `gb_trees:delete/2`

- 还有一个 `gb_trees:map/2` 函数，它和 `lists:map/2` 等价，不过操作的对象是树

- 简单模式函数相比智能模式函数存在劣势的原因在于 `GB` 树是平衡树，每当插入一个新元素(或者删除一批元素时)，树都需要平衡自己。这需要时间和内存。智能模式函数默认已知晓键值在树中的存在情况。有了这种假设，就可以略过所有的安全检查，从而达到更快的执行速度

- 集合是值唯一的一组元素，集合之间可以进行比较和其他操作——判断元素是否属于两个集合、不属于任何一个集合或者只在其中一个集合中等

- Erlang中主要有个4个集合处理模块

- `ordsets` 模块集合被实现为一种有序列表。它们主要适用于小集合，是最慢的一种集合，不过它的实现是所有集合中最简单、最容易理解的一种。该模块的一些标准函数有 `ordsets:new/0` 、 `ordsets:is_element/2` 、 `ordsets:add_element/2` 、 `ordsets:del_element/2` 、 `ordsets:union/1` 和 `ordsets:intersection/1`

- `sets` 模块实现使用的底层数据结构和 `dict` 使用的类似。 `sets` 模块的接口和 `ordsets` 完全一样，不过支持的数据规模要大一些。和 `dict` 一样， `sets` 更擅长读密集型的处理，如：检查某个元素是否在集合中

- `gb_sets` 模块的底层实现结构是一颗 `GB` 树，和 `gb_trees` 模块的使用类似。 `gb_sets` 和 `sets` 的关系与 `gb_trees` 和 `dict` 的关系一样：在非读取操作方面， `gb_sets` 要更快一些，提供的控制手段也更多一些。 `gb_sets` 在实现了和 `sets` 、 `ordsets` 同样接口的同时，还增加了其他一些函数。和 `gb_trees` 一样， `gb_sets` 中也分智能模式函数和简单模式函数，也有迭代器以及对最小值和最大值的快速访问函数

- `sofs` 是用来创建集合的集合。这个模块使用有序列表实现，这个列表被放置在一个包含元数据信息的元组中。如果想完全控制集合和集合之间的关系，强制集合类型或者有其他类似要求时，可以使用这个模块。当使用数学意义上的集合概念，而又不仅仅是值唯一的元素时，这个模块非常有用

- 来自Erlang/OTP团队的Bjorn Gustavsson，同时也是Wings3D项目的程序员，建议在大多数情况下尽量使用 `gb_sets` ，当需要一种清晰的表示，想在自己的代码中操作这种表示时，使用 `ordsets` ，当需要 `=:=` 操作符时，使用 `sets`

- 有向图( `directed graph` )是一个和数学关系密切的数据结构。Erlang中提供两个实现模块： `digraph` 和 `digraph_utils`

- `digraph` 模块主要实现了有向图的构造和修改功能——操作边和顶点、寻找路径和环等

- `digraph_utils` 模块则实现了图的遍历功能(后序和前序)、环、树形图以及树性质检测，寻找邻居顶点等功能

- 由于有向图和集合论关系密切，因此 `sofs` 模块中包含了一些函数，用来在集合族和有向图之间进行双向转换

- `queue` 模块实现了一个双向、先进先出(FIFO)的队列( `queue` )

- 队列的实现方式是：使用了两个列表，分别用来进行元素的快速入队和出队。因为单个列表无法同时在两端做到高效的元素增加和删除，所以 `queue` 模块采用了这样的实现思路：如果有两个列表，就用一个列表来增加元素，另一个列表用来删除元素。于是，一个列表变成了队列的一端，可以向其增加元素，另一个列表变成了队列的另一端，可以从中取出元素。当第二个列表为空时，可以把第一个列表反转，并把它变成提取元素的新列表。对于队列生存期间的所有操作的平均性能来说，这种实现是比较高效的

- `queue` 模块中的函数被分为3组具有不同复杂性的接口(或者API)

  - 原始API中包含了队列实现的基本函数，包括创建空队列的 `new/0` 、插入新元素的 `in/2` ，以及移除元素的 `out/1` 。其中也包含有像把队列换成列表、反转队列、检查某个特定值是否在队列中之类的函数

  - 扩展API主要增加了一些内省能力和灵活性。可以用它来进行一些诸如在不移除第一个元素的情况下查看队列的头元素( `get/1` 或者 `peek/1` )、直接移除元素而不关心它的值( `drop/1` )之类的操作。虽然这些函数并不是队列的必要操作，但是总体来说它们还是很有用的

  - Okasaki API有点奇怪。它的实现来自Chris Okasaki的Purely Functional Data Structures(Cambridge University Press, 1999)一书。这个API所提供的操作函数和其他API中的类似，不过，其中有些函数名是反着写的，整个实现也有些奇怪。除非有特殊的原因，否则不要使用这个API

- Erlang的顺序(函数式)编程至此已经全部介绍完毕。接下去的内容才真正开始我们的Erlang之旅——并发与进程
