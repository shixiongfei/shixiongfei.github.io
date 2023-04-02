---
title: Linux后台运行程序
author: shixiongfei
date: 2019-02-02 22:46:00
categories: 技术
tags: [技术, Linux]
---

在命令行的最后多加一个&即可把程序放到后台执行。例如:

```shell
$ your_command &
```

将程序的输出重定向到一个文件中，重定向的方法是在命令行最后加上 > filename。例如:

```shell
$ your_command > output.log &
```

借助nohup命令来帮我们实现后台驻守。例如:

```shell
$ nohup your_command &
```

使用nohup命令后，输出默认被重定向到nohup.out文件中。用下面这个命令来禁止生成nohup.out文件

```shell
$ nohup your_command > /dev/null 2>&1 &
```

另外说下程序前后台切换的方法吧：

- 把前台程序切换到后台的方法是先按CTRL + Z，这个时候程序会暂停执行，使用jobs命令查看任务序号n，然后输入bg n来让程序在后台继续执行。
- 把后台程序切换到前台的方法是使用jobs命令查看任务序号n，然后输入fg n来让程序到前台执行。
