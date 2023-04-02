---
title: 用Rebar3构建Erlang应用
author: shixiongfei
date: 2019-03-24 11:10:00
categories: 编程
tags: [编程, Erlang]
---

## 安装rebar3

MacOS可以简单的用homebrew安装，其他平台可以看[官方文档](http://www.rebar3.org)

```shell
$ brew install rebar3
```

## 创建Erlang应用

```shell
$ rebar3 new app myapp
```

生成的目录结构

```shell
$ cd myapp
$ tree
.
├── LICENSE
├── README.md
├── rebar.config
└── src
    ├── myapp.app.src
    ├── myapp_app.erl
    └── myapp_sup.erl
```

因为Rebar3只处理[OTP结构的项目](http://www.erlang.org/doc/design_principles/applications.html)，所以约定成俗用一个 .app.src 文件定义你的应用是一个OTP应用。

```shell
$ cat src/myapp.app.src
{application, myapp,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {myapp_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []}
 ]}.
```

src/myapp_app.erl是一个非常简单的代码框架。它只是让你可以启动和停止你的Erlang应用。

```shell
$ cat src/myapp_app.erl
-module(myapp_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    myapp_sup:start_link().
stop(_State) ->
    ok.
```

Rebar3用rebar.config来指定一些额外的元数据。

```shell
$ cat rebar.config
{erl_opts, [debug_info]}.
{deps, []}.
```

现在我们用Rebar3启动一个Erlang shell，它包含了你的应用及依赖的路径。

```shell
===> Verifying dependencies...
===> Compiling myapp
Erlang/OTP 21 [erts-10.3.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Eshell V10.3.1  (abort with ^G)
1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
===> Booted myapp
```

PS:如果直接粗暴的 `CTRL + C` 退出shell的话，可能会让控制台乱掉。正确的做法是 `CTRL + G` , `q` 。如果想了解更多关于rebar3 shell可以阅读这篇[文档](http://ferd.ca/rebar3-shell.html)

## Erlang包管理工具的准备工作

使用[rebar3_hex](https://github.com/tsloughter/rebar3_hex)的插件来从[Hex.pm](https://hex.pm)（Erlang和Elixir的包管理者）获取和安装Erlang包。简单地把如下一行加入到rebar.config文件中即可。

```text
{plugins, [rebar3_hex]}.
```

然后运行命令： `rebar3 update` 来启用这个插件。

```shell
$ rebar3 update
===> Fetching rebar3_hex ({pkg,<<"rebar3_hex">>,<<"6.5.0">>})
===> Fetching hex_core ({pkg,<<"hex_core">>,<<"0.5.0">>})
===> Fetching verl ({pkg,<<"verl">>,<<"1.0.1">>})
===> Compiling verl
===> Compiling hex_core
===> Compiling rebar3_hex
```

## 查找Erlang包

搜索命令让你远程查找在Hex.pm上发布的Erlang包。你可以在查询语句里用正则表达式字符。

```shell
$ rebar3 hex search cowboy
```

## 安装Erlang包

Rebar3能下载和安装Erlang包以及任何必要的依赖。在你的rebar.config文件里增加应用名字到deps配置项，然后运行命令：rebar3 compile。下面的例子，我们尝试使用两个Erlang包，cowboy和meck。

```text
{deps, [cowboy, meck]}.
```

```shell
$ rebar3 compile
===> Verifying dependencies...
===> Fetching cowboy ({pkg,<<"cowboy">>,<<"2.6.1">>})
===> Fetching cowlib ({pkg,<<"cowlib">>,<<"2.7.0">>})
===> Fetching ranch ({pkg,<<"ranch">>,<<"1.7.1">>})
===> Fetching meck ({pkg,<<"meck">>,<<"0.8.13">>})
===> Compiling cowlib
===> Compiling ranch
===> Compiling cowboy
===> Compiling meck
===> Compiling myapp
```

想要安装一个Erlang包的指定版本？在配置中把应用名字和版本写在一个元组中。你可以在[Hex主页](https://hex.pm/)上查询Erlang包的可用版本。

```text
{deps, [{cowboy, “1.0.2”}, {meck, "0.8.3"}]}.
```

## 列出已安装的Erlang包

rebar3 deps 命令列出你本地已经安装的包。

```shell
$ rebar3 deps
cowboy* (package)
```

## 卸载Erlang包

要卸载一个包，你首先要在rebar.config文件中将它删除，然后使用命令：rebar unlock。现在我们卸载meck包。

```shell
$ rebar3 unlock
$ rebar3 deps
cowboy* (package)
```

## 进一步阅读

<https://www.rebar3.org>
