---
title: 使用SSH通道作为SOCKS代理服务器
author: shixiongfei
date: 2019-03-05 13:28:00
categories: 技术
tags: [技术, Linux]
---

服务器设置

- 首先确保服务器的SSH服务开启了TCP转发功能

```shell
$ vi /etc/ssh/sshd_config
    AllowTcpForwarding yes
$ reboot
```

客户端设置

- MacOS & Linux
  - `-D 12345`是告诉SSH在12345端口上运行SOCKS服务

```shell
$ ssh -D 12345 user@host.domain
```

- Windows
  - Windows上要借助Putty这个工具
    - 打开Putty，在Session标签中输入ssh账号信息
    - 进入Connection-SSH-Tunnels标签，在Source port中填入转发端口，并点击Add按钮
    - Destination选择Dynamic

PS: 在使用代理期间，登录的SSH窗口不能关闭。
