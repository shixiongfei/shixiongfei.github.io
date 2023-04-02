---
title: 干净的删除Git Commit记录
author: shixiongfei
date: 2021-12-16 23:12:00
categories: 技术
tags: [技术, Git]
---

执行如下的命令

```shell
git rebase -i commit号
```

将要删除的 commit 号的前缀 `pick` 改为 `drop`

查看记录是否已经删除

```shell
git log
```

最后通过如下命令将现在的状态推送到远程仓库即可

```shell
git push origin HEAD --force
```
