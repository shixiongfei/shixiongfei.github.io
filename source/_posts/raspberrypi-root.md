---
title: 树莓派启用root账户
author: shixiongfei
date: 2018-12-02 21:17:00
categories: 技术
tags: [技术, Linux, RaspberryPi]
---

树莓派（Raspberry-Pi）操作系统Raspbian的默认用户是pi，密码为raspberry。

可以直接连接网线使用ssh登录，下面将启用root账户。

- 首先设置root账户密码

```shell
$ sudo passwd root
```

- 解锁root账户

```shell
$ sudo passwd --unlock root
```

如果出现错误：

- passwd: password expiry information changed.

解决办法：

```shell
$ sudo vi /etc/ssh/sshd_config
 # PermitRootLogin without-password
 PermitRootLogin yes
$ reboot
```
