---
title: kcptun配置文件
author: shixiongfei
date: 2019-03-09 17:29:00
categories: 技术
tags: [技术, KCP, KCPTun]
---

server-configure.json

```text
{
  "listen": ":12345",
  "target": "sss-address:54321",
  "key": "kcptun-password",
  "crypt": "salsa20",
  "mode": "fast2",
  "mtu": 1350,
  "sndwnd": 1024,
  "rcvwnd": 1024,
  "datashard": 5,
  "parityshard": 5,
  "dscp": 46,
  "nocomp": true,
  "acknodelay": false,
  "nodelay": 0,
  "interval": 40,
  "resend": 0,
  "nc": 0,
  "sockbuf": 16777217,
  "keepalive": 10
}
```

- 启动服务端 `server_xxx_yyy -c server-configure.json`

client-configure.json

```text
{
  "localaddr": ":54321",
  "remoteaddr": "kcptun-address:12345",
  "key": "kcptun-password",
  "crypt": "salsa20",
  "mode": "fast2",
  "conn": 1,
  "autoexpire": 60,
  "mtu": 1350,
  "sndwnd": 128,
  "rcvwnd": 1024,
  "datashard": 5,
  "parityshard": 5,
  "dscp": 46,
  "nocomp": true,
  "acknodelay": false,
  "nodelay": 0,
  "interval": 40,
  "resend": 0,
  "nc": 0,
  "sockbuf": 16777217,
  "keepalive": 10
}
```

- 启动客户端 `client_xxx_yyy -c client-configure.json`
