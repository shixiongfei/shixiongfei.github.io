---
title: 使用acme.sh自动签发https证书
author: shixiongfei
date: 2023-04-03 21:48:03
categories: 技术
tags: [技术, Linux, CentOS, AlmaLinux]
---

前置工作先安装`socat`

```shell
$ dnf install socat
```

安装`acme.sh`

```shell
$ curl https://get.acme.sh | sh -s email=my@example.com
```

`acme.sh`会自动安装到系统环境变量中，顺便会增加一个定时任务用于https证书的自动更新

```shell
$ crontab -l
0 0 * * * "/home/user/.acme.sh"/acme.sh --cron --home "/home/user/.acme.sh" > /dev/null
```

签发证书之前要对域名进行验证，我选择的是通过DNSAPI的方式验证。

登陆[阿里云后台](https://ram.console.aliyun.com/users)，创建一个用户并勾选OpenAPI

记得添加`AliyunDNSFullAccess / 管理云解析(DNS)`的权限权限

将`Key`和`Secret`添加到`.bashrc`中

```shell
$ vi .bashrc
export Ali_Key="sdfsdfsdfljlbjkljlkjsdfoiwje"
export Ali_Secret="jlsdflanljkljlfdsaklkjflsa"
```

签发证书，以我的短域名`shixf.com`为例

```shell
$ acme.sh --issue --dns dns_ali -d shixf.com -d www.shixf.com
```

等待差不多1分钟左右，证书就签发完毕了

最后安装证书到Nginx并重新加载配置

```shell
$ acme.sh --install-cert -d shixf.com \
  --key-file       /usr/local/certs/shixf-com/secret.key  \
  --fullchain-file /usr/local/certs/shixf-com/secret.pem \
  --reloadcmd      "service nginx force-reload"
```
