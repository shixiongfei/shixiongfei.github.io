---
title: Debian设置文件共享Samba
author: shixiongfei
date: 2019-03-18 21:47:00
categories: 技术
tags: [技术, Linux, Debian, Samba]
---

```shell
$ aptitude install samba
$ vi /etc/samba/smb.conf
    注释掉所有带pass字样的配置项
    security = share

    [fileserver]
    comment = Jenson File Server
    browseable = yes
    path = /var/ftp
    public = yes
    writable = no
    read only = yes
$ /etc/init.d/samba restart
```
