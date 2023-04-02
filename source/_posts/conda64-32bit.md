---
title: Conda 64bit安装32bit Python环境
date: 2018-12-10 23:57:00
author: shixiongfei
categories: 技术
tags: [技术, Python, Anaconda]
---

## 创建一个32bit的Python2.7环境

```shell
$ set CONDA_FORCE_32BIT=1
$ conda create -n py27_32 python=2.7
```

## 激活环境

```shell
$ set CONDA_FORCE_32BIT=1
$ activate py27_32
```

## 注销环境

```shell
$ deactivate py27_32
```

## 创建一个32bit的Python3.5环境

```shell
$ set CONDA_FORCE_32BIT=
$ conda create -n py35_64 python=3.5
```

## 激活环境

```shell
$ set CONDA_FORCE_32BIT=
$ activate py35_64
```

最好的方法是在批处理文件中写入激活命令，这样只需要键入一个命令，并且不会忘记设置正确的32/64位标志。
