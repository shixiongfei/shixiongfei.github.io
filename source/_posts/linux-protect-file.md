---
title: Linux保护文件不被删除
author: shixiongfei
date: 2019-04-15 23:37:00
categories: 技术
tags: [技术, Linux]
---

保护文件夹内文件不被删除(root也无法删除)

```shell
$ chattr +a folder/
```

- 删除文件会报Operation not permitted错误。

查看文件夹属性

```shelll
$ lsattr
  -----a---------- ./folder
```

- 有a属性表示文件夹被特殊保护了，即便root权限也无法删除里面的内容。

恢复文件夹并删除内容

```shell
$ chattr -a folder/
```
