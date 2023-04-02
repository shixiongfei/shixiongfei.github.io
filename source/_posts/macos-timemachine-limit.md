---
title: MacOS上限制Time Machine备份大小
author: shixiongfei
date: 2019-01-26 17:56:00
categories: 技术
tags: [技术, MacOS]
---

创建一个限制大小的稀疏磁盘映像，文件名使用主机名和以太网MAC地址。

例如：在桌面创建机器名为MyHost，以太网地址00:0C:29:01:98:27，300G备份大小的磁盘映像

```shell
$ hdiutil create -size 300g -type SPARSEBUNDLE -nospotlight -volname "TimeMachine" -fs HFS+J -verbose ~/Desktop/MyHost_000c29019827.sparsebundle
```

这个映像文件是自动增长，刚创建完时大小差不多是500M。然后把映像文件复制到移动硬盘或者支持AFP的NAS设备上，并双击挂载磁盘。

最后打开Time Machine选择备份磁盘后就可以开始备份了。

如果使用Windows机器想要以Samba共享方式备份的话，使用下面这个命令可以使Time Machine强制挂载。

```shell
$ sudo tmutil setdestination -p "/Volumes/TimeMachine"
```

PS：使用Samba方式备份可能会有不稳定或者文件写坏的情况，所以还是建议使用AFP的共享方式来进行备份。
