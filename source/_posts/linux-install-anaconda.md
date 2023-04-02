---
title: Linux上安装Anaconda
date: 2018-12-05 15:16:00
author: shixiongfei
categories: 技术
tags: [技术, Linux, Python, Anaconda]
---

下载安装脚本

```shell
$ wget https://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh
```

运行安装脚本，安装目录为/usr/local/miniconda

```shell
$ sh Miniconda-latest-Linux-x86_64.sh -b -p /usr/local/miniconda
```

添加环境变量

```shell
$ vi ~/.bash_profile
$ export PATH=/usr/local/miniconda/bin:$PATH
$ source ~/.bash_profile
```
