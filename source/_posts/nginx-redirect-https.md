---
title: Nginx服务器http重定向到https的正确写法
author: shixiongfei
date: 2018-12-17 00:24:00
categories: 技术
tags: [技术, Nginx]
---

http重定向到https使用了nginx的重定向命令。那么应该如何写重定向？之前老版本的nginx可能使用了以下两种类似的格式。

```text
rewrite ^/(.*)$ http://domain.com/$1 permanent;
```

```text
rewrite ^ http://domain.com$request_uri? permanent;
```

**现在nginx新版本已经换了种写法，上面这些已经不再推荐。**

下面是nginx http页面重定向到https页面最新支持的写法：

```text
server {
    listen      80;
    server_name    my.domain.com;
    return      301 https://$server_name$request_uri;
}

server {
    listen      443 ssl;
    server_name    my.domain.com;

    [....]
}
```
