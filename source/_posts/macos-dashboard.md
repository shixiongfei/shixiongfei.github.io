---
title: MacOS禁用/启用Dashboard
author: shixiongfei
date: 2019-02-02 21:31:00
categories: 技术
tags: [技术, MacOS]
---

禁用Dashboard

```shell
$ defaults write com.apple.dashboard mcx-disabled -boolean YES
$ killall Dock
```

启用Dashboard

```shell
$ defaults write com.apple.dashboard mcx-disabled -boolean NO
$ killall Dock
```
