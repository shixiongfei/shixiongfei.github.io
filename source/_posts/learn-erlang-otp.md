---
title: Erlang极简学习笔记<10>——OTP篇
author: shixiongfei
date: 2019-08-14 00:19:00
categories: 编程
tags: [编程, Erlang]
---

- Erlang的巨大优势一部分来自于其并发和分布式特性，还有一部分来自其错误处理能力，OTP框架则是第三部分

- 在一个服务器框架中，我们通常需要解决的问题有：进程（服务）命名、超时配置、调试信息、非期望消息处理、代码热加载、特殊错误的处理、公共回复代码、服务器关闭的处理、保证服务器和监督者的配合等。自己动手解决这些问题是一件有风险的事情，很幸运，Erlang/OTP已经在 `gen_server` 行为中解决了所有这些问题

- OTP的 `gen_server` 行为会要求提供一些进程的初始化和结束、基于消息发送的同步和异步处理以及其他任务的处理函数

- `init/1` 函数负责初始化服务器的状态，并完成服务器需要的所有一次性任务。这个函数可以返回 `{ok, State}` 、 `{ok, State, TimeOut}` 、 `{ok, State, hibernate}` 、 `{stop, Reason}` 以及 `ignore`

- 常规的 `{ok, State}` 返回值无需解释，只要记住 `State` 会直接传给进程的主循环，并作为进程的状态一直保存在那里就行了。

- 当期望服务器在某个时间期限之前能收到一条消息时，可以使用 `TimeOut` 变量。如果到期没有收到任何消息，那么会给服务器发送一条特殊的消息（原子 `timeout` ），可以在 `handle_info/2` 中处理这条消息。很少会在产品代码使用这个选项，因为不能总是知道会收到哪条消息，而任意一条消息都会重置计时器。通常，更好的方法是使用 `erlang:start_timer/3` 之类的函数，可以获得更好的处理控制

- 如果确实觉得进程在很长一段时间内不会有什么消息要处理，并且担心内存问题，那么可以在元组中使用 `hibernate` 原子。一般来讲， `hibernate` 选项会缩减进程的状态，直到它收到一条消息，不过会多耗费些处理能力。如果在是否使用 `hibernate` 选项时存有疑惑，就说明不太需要它

- 如果在初始化的过程中出现了错误，可以返回 `{stop, Reason}`

- 注意！当执行 `init/1` 函数时，创建服务器的进程会被阻塞。这是因为它在等待一条“就绪”消息，这条消息由 `gen_server` 模块自动发送以确认一切正常

- `handle_call/2` 函数用于处理同步消息。它有3个参数： `Request` 、 `From` 以及 `State`

- 在 `gen_server` 中，有8种不同的返回值可供选择，这些返回值都是元组形式

  - `{reply, Reply, NewState}`
  - `{reply, Reply, NewState, TimeOut}`
  - `{reply, Reply, NewState, hibernate}`
  - `{noreply, NewState}`
  - `{noreply, NewState, TimeOut}`
  - `{noreply, NewState, hibernate}`
  - `{stop, Reason, Reply, NewState}`
  - `{stop, Reason, NewState}`

- 这些返回值中， `TimeOut` 和 `hibernate` 的工作方式和 `init/1` 中的一样。 `Reply` 中的内容会被原封不动地发回给调用服务器的进程

- 共有3种不同的 `noreply` 选项，当使用 `noreply` 时，服务器的通用部分会认为你将自己发送回应消息，可以调用 `gen_server:reply/2` 发送回应

- 在绝大部分情况下，只需要使用 `reply` 元组。不过有些情况确实需要使用 `noreply` ，例如，希望由另一个进程来替你发送回应，或者想先发送一条确认消息（“嗨！我收到消息了！”），然后继续处理（处理完无需回应）。如果这是所需要的场景，那么只能使用 `gen_server:reply/2` ，否则，调用会超时然后崩溃

- `handle_cast` 函数用于异步消息的处理，它的参数是： `Message` 和 `State` 。和 `handle_call/3` 类似，其中也可以进行任何处理。不过它只能返回 `noreply` 元组

  - `{noreply, NewState}`
  - `{noreply, NewState, TimeOut}`
  - `{noreply, NewState, hibernate}`
  - `{stop, Reason, NewState}`

- `handle_info` 函数用于处理和接口不相容的消息。它和 `handle_case/2` 非常类似，事实上，返回值也完全一样。它们之间的区别在于，这个回调函数只用来处理直接通过 `!` 操作符发送的消息，以及如 `init/1` 中 `timeout` 、监控器通知或者 `EXIT` 信号之类的特殊消息

- 当上面3种 `handle_something` 函数返回形如 `{stop, Reason, NewState}` 或者 `{stop, Reason, Reply, NewState}` 的元组时，会调用 `terminate/2` 函数。它有两个参数： `Reason` 和 `State` ，分别对应 `stop` 元组中的同名字段

- 当父进程（创建服务器的进程）死亡时，也会调用 `terminate/2` 函数，不过这只会发生在 `gen_server` 捕获了退出信号的时候

- 如果在调用 `terminate/2` 时，原因不是 `normal` 、 `shutdown` 或者 `{shutdown, Term}` ，那么OTP框架会把这当成故障，并会把进程的状态、故障原因、最后收到的消息等记入日志。这让调试变得更加容易，可以帮助你快速定位问题

- 这个函数和 `init/1` 正好相反，因此所有在 `init/1` 中做的动作都应该在 `terminate/2` 中有对应的取消动作

- `code_change` 函数用于代码升级，它的调用形式是 `code_change(PreviousVersion, State, Extra)` 。其中，变量 `PreviousVersion` 在升级时是版本数据项本身，在降级时是 `{down, Version}` 。 `State` 变量中保存着服务器当前的所有状态数据，可以对其进行转换

- 假如我们一开始使用一个有序字典来存储所有数据。一段时间之后，有序字典变得越来越慢，我们决定用常规字典把它替换掉。为了避免进程在接下来的调用中崩溃，可以在这个函数中安全地进行数据结构的转换。所要做的就是用 `{ok, NewState}` 返回新的状态

- `gen_server` 的调用与回调关系

  - `gen_server:start/3-4`  <->  `YourModule:init/1`
  - `gen_server:start_link/3-4`  <->  `YourModule:init/1`
  - `gen_server:call/2-3`  <->  `YourModule:handle_call/3`
  - `gen_server:cast/2`  <->  `YourModule:handle_cast/2`

- 还有其他几个回调函数如 `handle_info/2` 、 `terminate/2` 和 `code_change/3` ，这些回调函数主要处理一些特殊情况

- `gen_fsm` 行为和 `gen_server` 有点类似，因为 `gen_fsm` 是 `gen_server` 行为的一个专用版本。它们之间最大的区别在于， `gen_fsm` 中不再处理 `call` 消息和 `cast` 消息，而是处理同步和异步事件

- FSM中的 `init` 函数和通用服务器中使用的 `init/1` 完全一样，除了返回值多一些，可接受的返回值为： `{ok, StateName, Data}` 、 `{ok, StateName, Data, Timeout}` 、 `{ok, StateName, Data, hibernate}` 以及 `{stop, Reason}` 。 `stop` 元组的工作原理和 `gen_server` 中的完全一样， `hibernate` 和 `Timeout` 的语义也保持不变

- `StateName` 是一个新出现的变量。 `StateName` 是原子类型，表示下一个被调用的回调函数

- 函数 `StateName/2` 和 `StateName/3` 是占位名字，由你来决定它们的内容

- 假设 `init/1` 函数返回元组 `{ok, sitting, dog}` ，这意味着FSM会处于sitting状态

- 在上面的FSM中， `init/1` 函数的返回值表明我们该处于 `sitting` 状态。当 `gen_fsm` 进程收到一个事件时，函数 `sitting/2` 或者 `sitting/3` 会被调用。对于异步事件，会调用 `sitting/2` 函数，对于同步事件，会调用 `sitting/3` 函数

- 函数 `sitting/2` (或者一般的说， `StateName/2` )有两个参数：一个是 `Event` ，作为事件发送来的实际消息；一个是 `StateData` ，调用携带的数据

- `sitting/2` 函数可以返回以下几种元组

  - `{next_state, NextStateName, NewStateData}`
  - `{next_state, NextStateName, NewStateData, Timeout}`
  - `{next_state, NextStateName, hibernate}`
  - `{stop, Reason, NewStateData}`

- 函数 `sitting/3` 的参数与此类似，只是在 `Event` 和 `StateData` 之间多了一个 `From` 参数。 `From` 参数和 `gen_fsm:reply/2` 的用法与 `gen_server` 中的完全一样

- 函数 `StateName/3` 可以返回如下元组

  - `{reply, Reply, NextStateName, NewStateData}`
  - `{reply, Reply, NextStateName, NewStateData, Timeout}`
  - `{reply, Reply, NextStateName, NewStateData, hibernate}`
  - `{next_state, NextStateName, NewStateData}`
  - `{next_state, NextStateName, NewStateData, Timeout}`
  - `{next_state, NextStateName, NewStateData, hibernate}`
  - `{stop, Reason, Reply, NewStateData}`
  - `{stop, Reason, NewStateData}`

- 注意，这些函数数量不受限制，只要被导出就行。元组中的原子 `NextStateName` 决定了下一次会调用哪个函数

- 无论当前在哪个状态中，全局事件都会触发一个特定反应。由于这类事件在每个状态中都会以同样的方式处理，因此 `handle_event/3` 回调函数正好满足需要。这个函数的参数和 `StateName/2` 类似，不过它在中间多了一个参数 `StateName(handle_event(Event, StateName, Data))` ，这个参数表明了收到事件时所处的状态。它的返回值和函数 `StateName/2` 一样

- 回调函数 `handle_sync_event/4` 和 `StateName/3` 的关系与 `handle_event/2` 和 `StateName/2` 的关系一样。这个函数处理同步全局事件，参数和所返回的元组种类都和 `StateName/3` 一样

- 通过向FSM发送事件所使用的函数，我们可以知道一个事件是全局的还是针对某个特定状态的。被 `StateName/2` 函数处理的异步事件是用函数 `gen_fsm:send_event/2` 发送的，而被 `StateName/3` 函数处理的同步事件是用函数 `gen_fsm:sync_send_event/2-3` 发送的（第三个可选的参数是超时）

- 两个对等的用来发送全局事件的函数为： `gen_fsm:send_all_state_event/2` 和 `gen_fsm:sync_send_all_state_event/2-3`

- FSM中 `code_change` 函数的工作方式和 `gen_server` 中的完全一样，只是多处一个额外的状态参数，如： `code_change(OldVersion, StateName, Data, Extra)` ，并且返回的元组格式为 `{ ok, NextStateName, NewStateData }`

- 同样的，FSM中 `terminate` 的行为和通用服务器中也类似， `terminate(Reason, StateName, Data)` 函数做多额工作应该和 `init/1` 相反

- `gen_event` 行为与 `gen_server` 以及 `gen_fsm` 有很大的不同，它根本不需要实际启动一个进程。之所以不需要进程是因为它的工作方式是“接受一组回调函数”

- 简单来讲， `gen_event` 行为运行这个接受并调用回调函数的事件管理器进程，而你只需要提供包含这些回调函数的模块即可。这意味着你无需关心事件分派，只需按照事件管理器要求的格式放置回调函数即可。所有的事件管理就自然都有了，你只需提供应用特定的东西

- `init` 和 `terminate` 函数与我们前面看到的 `gen_server` 和 `gen_fsm` 行为中的类似。 `init/1` 函数接收列表参数，返回 `{ok, State}` 。在 `init/1` 中创建的东西，要在 `terminate/2` 函数中有对应释放操作

- `handle_event(Event, State)` 函数可以说是 `gen_event` 回调模块的核心函数。和 `gen_server` 中的 `handle_cast/2` 一样， `handle_event/2` 函数也是异步的。不过，它的返回值有所不同：

  - `{ok, NewState}`
  - `{ok, NewState, hibernate}` ，让事件管理器进程进入休眠状态，直到收到下一个事件
  - `remove_handler`
  - `{swap_handler, Args1, NewState, NewHandler, Args2}`

- 返回值 `{ok, NewState}` 元组的含义和 `gen_server:handle_cast/2` 函数中的一样。它只更新自己的状态，不做任何回应。

- 返回 `{ok, NewState, hibernate}` 则会使整个事件管理器进入休眠状态。记住，事件处理器和其他管理器运行在同一个进程中

- 返回 `remove_handler` 则会导致事件处理器从事件管理器中删除。当某个事件处理器知道自己已经完成工作并且无其他任务时，可以使用这个返回值

- 最后一个返回值 `{swap_handler, Args1, NewState, NewHandler, Args2}` ，它移除当前事件处理器并用一个新的替代它。事件管理器首先调用 `CurrentHandler:terminate(Args1, NewState)` 函数，并移除当前的事件处理器。接着调用 `NewHandler:init(Args2, ResultFromTerminate)` 函数，添加新的事件处理器。

- 所有事件都是通过 `gen_event:notify/2` 函数触发的，和 `gen_server:cast/2` 一样，它也是异步的。还有另外一个函数 `gen_event:sync_notify/2` ，它是同步的。由于 `handle_event/2` 是异步的，这里的同步指的是，当所有事件处理器都收到这个事件并且处理完毕后， `sync_notify` 函数才会返回。在那之前，事件管理器会一直阻塞调用进程，不予响应

- `handle_call` 函数和 `gen_server` 的 `handle_call` 回调函数类似，不同之处在于，它可以返回 `{ok, Reply, NewState}` 、 `{ok, Reply, NewState, hibernate}` 、 `{remove_handler, Reply}` 以及 `{swap_handler, Reply, Args1, NewState, Handler2, Args2}` 。使用 `gen_event:call/3-4` 函数就可以发起该调用

- `handle_info` 回调和 `handle_event` 回调非常相似（有着同样的返回值和含义），唯一的不同在于， `handle_info` 只处理带外消息，如退出信号或使用 `!` 操作符直接向事件管理器进程发送消息。它的使用场景和 `gen_server` 以及 `gen_fsm` 中的 `handle_info` 的使用场景类似

- `code_change` 函数的工作方式和 `gen_server` 中的一样，不过它仅仅针对单独的事件处理器。它的参数为： `OldVsn` 、 `State` 、 `Extra` ，分别表示版本号、当前事件处理器的状态、最后这个参数—— `Extra` ，目前可以不用关心。这个方法只要返回 `{ok, NewState}` 即可

- 在所有OTP中，监督者是最容易使用和理解的一个，但也是最难设计好的一个。在Erlang中应该把所有东西都监督起来，此外，监督机制还可以让你以恰当的顺序终止应用

- 当想终止一个应用时，只需去终止虚拟机中最顶层的那个监督者（调用 `init:stop/1` 函数就可以了）。然后这个监督者会接着要求它的子进程停止运行。如果某个子进程也是一个监督者，它会做同样的事情

- 如果没有用树形结构来组织所有的进程，那么很难让VM以井然有序的方式终止。当然，有时也会出现进程因某种原因被卡住而不能正常终止的情况。此时，监督者可以强行杀死这个进程。

- 监督者使用起来很简单，我们只需要提供一个回调函数： `init/1` 。麻烦的地方在于这个函数的返回值非常复杂。下面是一个返回值的例子：

  ```erlang
  {ok, {{one_for_all, 5, 60},
        [{fake_id,
          {fake_mod, start_link, [SomeArg]},
          permanent,
          5000,
          worker,
          [fake_mode]},
         {other_id,
          {event_manager_mod, start_link, []},
          transient,
          infinity,
          worker,
          dynamic}]}}
  ```

- 重启策略 `RestartStrategy` 的值可以为：

  - `one_for_one` ，当被监督的进程都是独立的、互不相关的，或者即便这些进程重启后丢失了自己的状态，也不会对其他进程产生影响时，可以使用 `one_for_one` 策略
  - `one_for_all` ，当所有工作者进程都受同一个监督者监督，且这些工作者进程必须互相依赖才能正常工作时，就使用这个策略
  - `rest_for_one` ，如果一个进程死了，那么所有在这个进程之后启动的进程（依赖于该进程）都将被重启，反之不然
  - `simple_one_for_one` ，这个类型的监督者只监督一种子进程，当希望以动态的方式向监督者中增加子进程（当需要一个新的子进程时，向它发起请求，就能得到一个），而不是静态启动子进程时，可以使用这种策略

- `RestartStrategy` 元组中剩余的两个变量是 `MaxRestart` 和 `MaxTime` 。他们的意思是，如果在 `MaxTime` （以秒为单位）指定的时间内，重启次数超过了 `MaxRestart` 指定的数字，那么监督者会放弃重启并终止所有子进程，然后自杀，永远停止运行

- 子进程规格说明可以描述成： `{ChildId, StartFunc, Restart, Shutdown, Type, Modules}`

- `ChildId` 只是监督者内部使用的一个名称

- `StartFunc` 是一个元组，用来指定子进程的启动方式，它采用了标准的 `{M, F, A}` 格式。注意！这里的启动函数是OTP兼容的，在执行时会和调用者进程链接在一起，这一点非常重要

- `Restart` 指定了监督者在某个特定的子进程死后的处理方式，它可以取如下3个值：

- `permanent` : 不管发生什么，一个永久进程都要被重启
- `temporary` : 指的是那种绝对不应该被重启的进程
- `transient` : 介于上述两种进程之间。如果被正常终止了，就不会被重启。如果异常死亡，就会被重启

- `Shutdown` 当要求最顶层的监督者终止时，它会对每个子进程调用 `exit(ChildPid, shutdown)` 。如果这个子进程是一个工作者进程并且捕获了退出信号，那么就会调用自己的 `terminate` 函数；否则，进程死掉就行了。如果是一个监督者子进程收到了 `shutdown` 信号，它会用同样的方式将这个信号转发给它的子进程

- 子进程规格说明中的 `Shutdown` 值用来指定终止的超时期限，它可以设置一个确定的终止超时，可以是多少毫秒，也可以是 `infinity` 。如果指定时间过去了，进程还没有死，那么进程会被 `exit(Pid, kill)` 强行杀死

- 如果对子进程并不在意，不设定超时等待时间时，子进程死亡也没啥影响，那么可以将 `Shutdown` 设置成原子 `brutal_kill` 。使用 `brutal_kill` 会调用 `exit(Pid, kill)` 杀死子进程，此时，退出是即时的，子进程也无法捕获这个退出信号

- `Type` 字段可以让监督者知道子进程是一个监督者(supervisor)(实现了 `supervisor` 或者 `supervisor_bridge` 行为)还是一个工作者(worker)(任何其他OTP进程)

- `Modules` 是一个列表，其中只有一个元素：子进程行为使用的回调模块名。有一个例外情况：事先无法知道回调模块的标示符（如事件管理器中的事件处理器模块）。此时， `Modules` 的值要设置成 `dynamic` ，这样，在使用其他高级特性（如发布）时，整个OTP系统才能知道去找谁

- 动态监督使用 `one_for_one` 、 `rest_for_one` 或者 `one_for_all` 策略把工作者进程加入监督者中时，除了该进程的 `pid` 和其他一些信息外，还会向监督者持有的一个列表中增加子进程规格说明。在以后重启子进程或者执行其他任务时，会使用这份子进程规格说明。基于这种工作方式，相应的接口定义如下：

  - `start_child(SupervisorNameOrPid, ChildSpec)`  向列表中增加一个子进程规格说明，并且用该规格说明启动一个子进程
  - `terminate_child(SupervisorNameOrPid, ChildId)`  终止或者强行杀死( `brutal_kills` )指定的子进程。子进程的规格说明仍然保留在监督者中
  - `restart_child(SupervisorNameOrPid, ChildId)`  使用子进程规格说明重启子进程
  - `delete_child(SupervisorNameOrPid, ChildId`  删除指定 `ChildId` 所对应的子进程规格说明
  - `check_childspecs([ChildSpec])`  检查一个子进程规格说明是否有效。在调用 `start_child/2` 函数前，可以用这个函数去测试一下规格说明的有效性
  - `count_children(SupervisorNameOrPid)`  分类列举出该监督者下的所有子进程，包括活动进程个数、子进程规格说明个数、监督者类型的个数和工作者类型的个数
  - `which_children(SupervisorNameorPid)`  返回一个指定监督者下所有子进程信息的列表

- 当子进程不多时，这些函数很适用于各种动态性要求（启动、终止等）。不过，由于内部使用的是列表，因此当需要快速访问大量子进程时，并不是很适用。此时所需要的是 `simple_one_for_one`

- 使用 `simple_one_for_one` 策略的监督者把所有子进程信息存放在一个字典中，这样可以快速查找，并且对监督者的所有子进程来说，只有一份子进程规格说明

- 编写 `simple_one_for_one` 策略监督者的方式基本上和其他策略的监督者类似，只有一点不同： `{M, F, A}` 元组中的参数列表 `A` 并不是全部参数，完整的参数是把 `supervisor:start_child(Sup, Args)` 调用中的 `Args` 追加到 `A` 之后的新列表

- 这里的 `supervisor:start_child/2` 的含义改变了。与原来的 `supervisor:start_child(Sup, Spec)` 调用 `erlang:apply(M, F, A)` 不同，现在的 `supervisor:start_child(Sup, Args)` 调用的是 `erlang:apply(M, F, A++Args)`

  ```erlang
  init(jamband) ->
      {ok, {{simple_one_for_one, 3, 60},
            [{jam_musician,
             {musicians, start_link, []},
             temporary, 1000, worker, [musicians]}
            ]}}
  ```

- 仅当明确知道要监督的子进程数量不多并且（或者）不需要频繁地操控子进程，或者对性能要求不高的情况下，可以动态地使用标准监督者。对于其他需要动态监督的情况，尽可能使用 `simple_one_for_one`
