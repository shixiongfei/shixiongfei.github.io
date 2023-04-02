---
title: 使用ntpd服务自动同步时间
author: shixiongfei
date: 2019-03-28 23:37:00
categories: 技术
tags: [技术, Linux, CentOS]
---

```shell
$ yum install ntp
$ systemctl enable ntpd.service
$ systemctl start ntpd.service
$ service ntpd status
```
