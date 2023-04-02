---
title: CentOS设置静态IP
author: shixiongfei
date: 2019-03-26 23:10:00
categories: 技术
tags: [技术, Linux, CentOS]
---

## 查看机器网卡配置文件列表

- 这里假设设置机器的网卡配置是enp0s0

```shell
$ ls /etc/sysconfig/network-scripts/ifcfg-*
```

## 命令行中设置静态IP

```shell
$ vi /etc/sysconfig/network-scripts/ifcfg-enp0s0
    BOOTPROTO="static"
    IPADDR="1.1.1.2"
    NETMASK="255.255.255.0"
    GATEWAY="1.1.1.1"
$ systemctl restart network.service
```

## 使用网络管理器

```shell
$ yum install NetworkManager-tui
$ nmtui edit enp0s0
$ systemctl restart network.service
```

## 修改DNS设置

```shell
$ vi /etc/resolv.conf
    nameserver 8.8.8.8
    nameserver 8.8.4.4
```

- 或者

```shell
$ vi /etc/sysconfig/network-scripts/ifcfg-enp0s0
    DNS1="8.8.8.8"
    DNS2="8.8.4.4"
```
