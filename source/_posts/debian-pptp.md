---
title: Debian设置PPTP
author: shixiongfei
date: 2019-03-21 23:37:00
categories: 技术
tags: [技术, Linux, Debian, PPTP]
---

## 设置PPTP

```shell
$ aptitude install pptpd
$ vi /etc/pptpd.conf
    localip 10.1.0.1
    remoteip 10.1.0.10-210

$ vi /etc/ppp/pptpd-options
    ms-dns 8.8.8.8
    ms-dns 8.8.4.4
    ms-dns 208.67.222.222
    ms-dns 208.67.220.220

$ vi /etc/sysctl.conf
    net.ipv4.ip_forward = 1

$ sysctl -p
$ vi /etc/rc.local
    iptables -t nat -A POSTROUTING -s 10.0.0.0/8 -o eth0 -j MASQUERADE

$ mknod /dev/ppp c 108 0
$ vi /etc/ppp/chap-secrets
    test pptpd 123456 *

$ reboot
```

- DNS 这里提供了 2 组，第一组是 Google DNS，第二组是 OpenDNS，任选一组就行了。

## 用简单脚本监测PPTPD服务

```shell
$ vi /root/check_pptpd.sh
    #!/bin/bash
    PID=`ps -ef |grep "pptpd" |grep -v grep |grep -v "$0" | awk '{print $2}'`
    if [ "$PID" == "" ]; then
      /etc/init.d/pptpd start
    fi
$ chmod 777 /root/check_pptpd.sh
$ crontab -e
    * */1 * * * bash /root/check_pptpd.sh
$ crontab -l
```

- 略微修改一下，同样适用于其他进程监测。
