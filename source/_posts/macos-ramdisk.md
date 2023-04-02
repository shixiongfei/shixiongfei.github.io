---
title: MacOS创建内存盘
author: shixiongfei
date: 2019-01-07 11:55:00
categories: 技术
tags: [技术, MacOS]
---

使用命令行创建内存盘

```shell
$ diskutil erasevolume HFS+ 'RamDisk' `hdiutil attach -nomount ram://XXXXX`
```

- XXXXX = Size * 2048
  - 例如：RamDisk 4GB = 4096 \* 1024 \* 2 = 8388608

卸载内存盘

```shell
$ diskutil eject 'RamDisk'
```

隐藏内存盘

```shell
$ chflags hidden /Volumes/RamDisk
```

取消隐藏内存盘

```shell
$ chflags nohidden /Volumes/RamDisk
```

保存内存盘数据到磁盘

```shell
$ tar --exclude '.Trashes' -czf ~/Downloads/backup.tar.gz .
```

还原磁盘数据到内存盘

```shell
$ tar -zxf ~/Downloads/backup.tar.gz -C /Volumes/RamDisk
```
