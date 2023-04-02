---
title: Linux创建swap分区
author: shixiongfei
date: 2018-12-09 23:54:00
categories: 技术
tags: [技术, Linux]
---

创建分区并启用分区(这里创建 256M 交换分区)

```shell
$ cd /var/
$ dd if=/dev/zero of=swapfile bs=1024 count=262144
$ /sbin/mkswap swapfile
$ /sbin/swapon swapfile
```

然后加到fstab，开机自启动

```shell
$ vi /etc/fstab
    /var/swapfile swap swap defaults 0 0
```

最后可以使用 free -m 查看 swap 分区状态
