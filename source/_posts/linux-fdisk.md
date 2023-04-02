---
title: Linux磁盘分区格式化
author: shixiongfei
date: 2019-03-16 23:44:00
categories: 技术
tags: [技术, Linux]
---

## 查看磁盘分区状态

```shell
$ fdisk -l
```

- Linux下可以使用“fdisk -l”命令查看数据盘相关信息。使用“df -h”命令，无法看到未分区和格式化的数据盘。

## 数据盘分区

```shell
$ fdisk /dev/vdb
```

- 按照界面的提示，依次输入“n”(新建分区)、“p”(新建扩展分区)、“1”(使用第1个主分区)，两次回车(使用默认配置)，输入“w”(保存分区表)，开始分区。

## 格式化新分区

```shell
$ mkfs.ext4 /dev/vdb1
```

## 挂载新分区

```shell
$ mkdir /mydata
$ mount /dev/vdb1 /mydata
$ df -h
```

## 添加分区信息

```shell
$ vi /etc/fstab
    /dev/vdb1 /mydata ext4 defaults 0 0
```
