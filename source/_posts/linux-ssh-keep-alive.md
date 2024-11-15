---
title: 防止SSH自动断线
author: shixiongfei
date: 2019-03-07 17:15:00
categories: 技术
tags: [技术, Linux]
---

在连接远程SSH服务的时候，防止SSH自动断线或者无响应（无法再键盘输入）。

```shell
$ sudo vim /etc/ssh/sshd_config
    ClientAliveInterval 30
    ClientAliveCountMax 6
```

- ClientAliveInterval: 表示每隔多少秒，服务器端向客户端发送心跳。
- ClientAliveCountMax: 表示上述多少次心跳无响应之后，会认为Client已经断开。

这里配置总共允许无响应的时间是 30 * 6 = 180秒。

```shell
$ sudo systemctl restart sshd
```

最后重启一下 sshd 服务让配置生效。
