---
title: Debian下蓝牙使用
author: shixiongfei
date: 2019-04-13 23:12:00
categories: 技术
tags: [技术, Linux, Debian, 蓝牙]
---

## 安装必要的蓝牙软件包

```shell
$ aptitude install bluetooth bluez-utils bluez-compat
```

## 安装蓝牙文件传输服务

```shell
$ aptitude install obex-data-server obexpushd
```

## 启动蓝牙服务

```shell
$ /etc/init.d/bluetooth status
```

## 注册蓝牙文件传输服务

```shell
$ sdptool add --channel=4 OPUSH
```

## 查看蓝牙设备信息

```shell
$ hcitool dev
```

## 查看蓝牙配置信息

```shell
$ hciconfig -a
```

## 搜索蓝牙设备

```shell
$ hcitool scan
```

## 查看本地蓝牙服务(要查看远程的话，将local替换为远程蓝牙地址)

```shell
$ sdptool browse local
```
