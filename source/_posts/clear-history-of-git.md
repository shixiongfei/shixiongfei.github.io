---
title: 清空Git仓库全部历史记录
date: 2020-10-29 23:04:00
author: shixiongfei
categories: 技术
tags: [技术, Git]
---

## 切换到 master 分支

```shell
git checkout master
```

## 创建一个干净的分支

```shell
git checkout --orphan new_master
```

## 提交全部文件

```shell
git add -A
git commit -m "msg"
```

## 删除 master 分支

```shell
git branch -D master
```

## 将新分支重命名为 master

```shell
git branch -m master
```

## 强制推动到远程仓库

```shell
git push -f origin master
```
