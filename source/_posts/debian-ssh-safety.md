---
title: Debian提高SSH安全
author: shixiongfei
date: 2019-04-17 00:12:00
categories: 技术
tags: [技术, Linux, Debian, SSH]
---

查看SSH登录日志。

```shell
$ cat /var/log/auth.log|awk '/Failed/{print $(NF-3)}'|sort|uniq -c|awk '{print $2"="$1;}'
```

安装DenyHosts，DenyHosts可以在一个IP连接登录失败一定次数后自动Ban这个IP，并在一段时间后再自动Unban。

```shell
$ aptitude install denyhosts
$ vi /etc/denyhosts.conf
$ /etc/init.d/denyhosts restart
```

其实默认配置已经足够安全了。最后修改SSH登录端口和更复杂的登录密码。

简要说明主要参数如下：

- PURGE_DENY：当一个IP被阻止以后，过多长时间被自动解禁。可选如3m（三分钟）、5h（5小时）、2d（两天）、8w（8周）、1y（一年）；
- PURGE_THRESHOLD：定义了某一IP最多被解封多少次。即某一IP由于暴力破解SSH密码被阻止/解封达到了PURGE_THRESHOLD次，则会被永久禁止；
- BLOCK_SERVICE：需要阻止的服务名；
- DENY_THRESHOLD_INVALID：某一无效用户名（不存在的用户）尝试多少次登录后被阻止；
- DENY_THRESHOLD_VALID：某一有效用户名尝试多少次登陆后被阻止（比如账号正确但密码错误），root除外；
- DENY_THRESHOLD_ROOT：root用户尝试登录多少次后被阻止；
- HOSTNAME_LOOKUP：是否尝试解析源IP的域名；

另外，当denyhosts误挡了正常的IP，要解除封锁特定IP时，需先停用denthosts;

再至/var/lib/denyhosts目录下，将hosts、hosts-restricted、hosts-root、hosts-valid及/etc/hosts.deny文件内，将包含误挡IP的那一整行都删除。

最后重启denyhosts服务!
