---
title: 生成CSR
author: shixiongfei
date: 2018-11-22 14:46:00
categories: 技术
tags: [技术, OpenSSL, CSR]
---

生成私钥

```shell
$ openssl genrsa -out your.domian.key 2048
```

生成CSR

```shell
$ openssl req -new -sha256 -key your.domian.key -out your.domian.csr
```
