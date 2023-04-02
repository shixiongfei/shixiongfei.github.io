---
title: 树莓派自动启动SOCKS代理
author: shixiongfei
date: 2019-01-20 17:12:00
categories: 技术
tags: [技术, Linux, RaspberryPi]
---

使用树莓派做自动翻墙服务器，树莓派的命令和传统Debian略有不同，这里特别记录一下。

同样先开启SSH转发功能

```shell
$ vi /etc/ssh/sshd_config
    AllowTcpForwarding yes
$ reboot
```

编写一个启动脚本

```shell
$ echo PATH=$PATH
$ vi /root/socks.sh
    #!/bin/bash

    PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
    export PATH

    ssh -N -f -D0.0.0.0:12345 user@host.domain

$ chmod 777 /root/socks.sh
```

添加到自动启动

```shell
$ vi /etc/rc.local
    bash /root/socks.sh
```

最后写一个自动检测脚本，每5分钟检测一次，如果断线的话则自动重连

```shell
$ vi check_socks.sh
    #!/bin/bash

    PID=`ps -ef |grep "ssh " |grep -v grep |grep -v "$0" | awk '{print $2}'`
    if [ "$PID" == "" ]; then
      bash /root/socks.sh
    fi

$ crontab -e
    */5 * * * * bash /root/check_socks.sh
$ crontab -l
```

然后和正常使用SOCKS代理一样，连接12345端口进行连接。
