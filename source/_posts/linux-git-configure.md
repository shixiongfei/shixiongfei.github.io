---
title: Linux Git配置
author: shixiongfei
date: 2019-03-25 21:14:00
categories: 技术
tags: [技术, Linux, Git]
---

安装Git

```shell
$ aptitude install git
```

配置Git帐号邮箱

```shell
$ git config --global user.name YourUserName
$ git config --global user.email YourEmail@example.com
```

查看Git帐号邮箱

```shell
$ git config user.name
$ git config user.email
```

查看Git版本状态

```shell
$ git status
```

Git提交数据

```shell
$ git commit -m "comment"
$ git push
```

**PS: Windows和MacOS上可以使用SourceTree作为Git客户端，非常好用。**
