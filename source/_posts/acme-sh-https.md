---
title: 使用acme.sh自动签发https证书
author: shixiongfei
date: 2023-04-03 21:48:03
categories: 技术
tags: [技术, Linux, CentOS, AlmaLinux]
---

## 前置工作

先安装`socat`

```shell
$ dnf install socat
```

## 安装`acme.sh`

```shell
$ curl https://get.acme.sh | sh -s email=my@example.com
```

`acme.sh`会自动安装到系统环境变量中，顺便会增加一个定时任务用于https证书的自动更新

```shell
$ crontab -l
0 0 * * * "/home/user/.acme.sh"/acme.sh --cron --home "/home/user/.acme.sh" > /dev/null
```

## 验证域名并签发证书

因为ZeroSSL的签发不太稳定，所以我默认使用Let's Encrypt

```shell
acme.sh --set-default-ca --server letsencrypt
```

签发证书之前要对域名的所有权进行验证，通过DNSAPI进行验证的话，先要生成API Token，然后export到环境变量中，以CloudFlare举例

环境变量里添加API Token等验证信息

```shell
export CF_Token="DNS Token"
export CF_Zone_ID="Zone ID"
export CF_Account_ID="Account ID"
```

进行域名验证

```shell
acme.sh --issue --dns dns_cf -d shixiongfei.com -d '*.shixiongfei.com'
```

也可以通过DNS手动解析的方式进行验证。

```shell
acme.sh --issue --dns -d shixiongfei.com -d '*.shixiongfei.com' \
 --yes-I-know-dns-manual-mode-enough-go-ahead-please
```

控制台上会显示需要解析的记录名 `Domain: _acme-challenge.xxxxxx` 和对应需要解析的txt值 `TXT value: 'xxxxxxx'`。

将上述txt解析添加到域名解析记录中，再重新生成证书并认证

```shell
acme.sh  --renew -d shixiongfei.com \
 --yes-I-know-dns-manual-mode-enough-go-ahead-please
```

等待差不多1分钟左右，证书就签发完毕了。（txt记录验证完之后可以删除）

**注意：** 通过手动验证方式的可能无法自动续订证书

## 安装证书

最后安装证书到Nginx并重新加载配置

```shell
$ acme.sh --install-cert -d shixiongfei.com \
--key-file       /usr/local/certs/shixiongfei-com/secret.key  \
--fullchain-file /usr/local/certs/shixiongfei-com/secret.pem \
--reloadcmd      "service nginx force-reload"
```
