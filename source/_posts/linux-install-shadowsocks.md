---
title: 配置shadowsocks多用户服务
date: 2019-03-09 17:21:00
author: shixiongfei
categories: 技术
tags: [技术, Linux, CentOS, Shadowsocks]
---

安装服务

```shell
$ yum install m2crypto libsodium supervisor
$ pip install shadowsocks
```

配置服务

```shell
$ vi /etc/shadowsocks.json
    {
        "server":"0.0.0.0",
        "port_password":{
            "1234":"passwd1",
            "2345":"passwd2"
        },
        "timeout":600,
        "method":"aes-256-cfb"
    }
$ vi /etc/supervisord.d/shadowsocks.ini
    [program:shadowsocks]
    command=ssserver -c /etc/shadowsocks.json
    autostart=true
    autorestart=true
    user=root
    log_stderr=true
    logfile=/var/log/shadowsocks.log
```

配置supervisor自动启动服务

```shell
$ chkconfig supervisord on
$ systemctl enable supervisord.service
$ service supervisord start
```
