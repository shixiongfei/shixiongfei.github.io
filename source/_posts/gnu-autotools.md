---
title: GNU AutoTools构建项目工程
author: shixiongfei
date: 2018-11-30 21:05:00
categories: 编程
tags: [编程, C, AutoTools]
---

测试项目的目录结构如下

```text
helloworld/
        └── src/
             ├── hello.c
             ├── hello.h
             └── main.c
```

测试代码如下

```c
// ----- hello.h -----
#ifndef _HELLO_H_
#define _HELLO_H_

void sayHello(char *name);

#endif
```

```c
// ----- hello.c -----
#include "hello.h"
#include <stdio.h>

void sayHello(char *name) {
    printf("Hello, %s!\n", name);
}
```

```c
// cat main.c hello.h hello.c
// ----- main.c -----
#include "hello.h"

int main(void) {
    sayHello("Make");
    return 1;
}
```

使用autotools构建项目

```shell
$ cd helloworld
$ autoscan      # 生成configure.scan
$ mv configure.scan configure.ac
$ vi configure.ac
    # -*- Autoconf -*-
    # Process this file with autoconf to produce a configure script.
    AC_INIT([helloworld], [1.0], [email@domain.com])
    AM_INIT_AUTOMAKE([-Wall -Werror foreign])
    AC_CONFIG_HEADERS([config.h])

    # Checks for programs.
    AC_PROG_CC

    # Checks for libraries.

    # Checks for header files.

    # Checks for typedefs, structures, and compiler characteristics.

    # Checks for library functions.
    AC_CONFIG_FILES([
    Makefile
    src/Makefile
    ])
    AC_OUTPUT
$ vi src/Makefile.am
    bin_PROGRAMS = helloworld
    helloworld_SOURCES = main.c hello.c
$ vi Makefile.am
    SUBDIRS = src
    dist_doc_DATA = README.md
$ autoreconf --install
$ ./configure
$ make          # 可以通过make dist编译发布版本
$ ./src/helloworld
```
