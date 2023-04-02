---
title: pip更新所有包
date: 2019-01-14 12:03:00
author: shixiongfei
categories: 编程
tags: [编程, Python]
---

```shell
$ pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U
```
