---
title: 导入/生成SSH Key
author: shixiongfei
date: 2018-11-22 14:40:00
categories: 技术
tags: [技术, Linux]
---

生成新密钥

```shell
$ cd ~/.ssh
$ ssh-keygen -t rsa -C "yourname@domain.com"
```

Linux与MacOSX设置权限

```shell
$ chmod 0600 id_rsa*
```

Windows使用Putty生成id_rsa.ppk
