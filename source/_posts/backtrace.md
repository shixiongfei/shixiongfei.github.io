---
title: 在Linux下追踪C程序堆栈
author: shixiongfei
date: 2018-11-29 00:12:00
categories: 编程
tags: [编程, C, Linux]
---

在Linux上使用BackTrace记录宕机时的堆栈，但记录的只有函数地址，没有堆栈名称。

可以使用下面的命令把对象反汇编出来，导成文本方便搜索。虽然不是完美方案，但也算可以定位问题。

```shell
$ objdump -j .text -Sl your_program --prefix-addresses > obj.txt
```
