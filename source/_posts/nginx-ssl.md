---
title: Nginx配置SSL
author: shixiongfei
date: 2018-12-18 23:39:15
categories: 技术
tags: [技术, Nginx]
---

将 80 端口重定向到 https

```text
server {
    listen 80;
    listen [::]:80;

    server_name *.domain.com;

    return 301 https://domain.com$request_uri;
}

server {
    listen 80;
    listen [::]:80;

    server_name domain.com;

    return 301 https://domain.com$request_uri;
}
```

假定我们只希望通过主域来访问网站，则可以将`https://*.domain.com`也都重定向到`https://domain.com`

```text
server {
    listen 443 ssl http2;
    listen [::]:443 ssl http2;

    server_name *.domain.com;

    # SSL
    ssl_certificate /usr/local/certs/domain-com/secret.pem;
    ssl_certificate_key /usr/local/certs/domain-com/secret.key;

    return 301 https://domain.com$request_uri;
}
```

这里我们假设将`/api/getip`反向代理到`4100`服务端口上，并且同时将所有访问`/post/xxxx`或者`/post/xxxx/`地址的操作都重定向为`xxxx.html`

```text
server {
    listen 443 ssl http2;
    listen [::]:443 ssl http2;

    server_name domain.com;
    root /usr/local/www/public;

    # SSL
    ssl_certificate /usr/local/certs/domain-com/secret.pem;
    ssl_certificate_key /usr/local/certs/domain-com/secret.key;

    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA:ECDHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA256:DHE-RSA-AES256-SHA:ECDHE-ECDSA-DES-CBC3-SHA:ECDHE-RSA-DES-CBC3-SHA:EDH-RSA-DES-CBC3-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:DES-CBC3-SHA:!DSS;
    ssl_prefer_server_ciphers on;

    location / {
        index index.html;
        rewrite ^/post/(.*)/$ /$1.html permanent;
        rewrite ^/post/(.*)$ /$1.html permanent;
    }

    location /api/getip {
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Host $http_host;
        proxy_set_header X-NginX-Proxy true;
        proxy_pass http://127.0.0.1:4100/;
        proxy_redirect default;
        proxy_connect_timeout 60s;
        proxy_read_timeout 120s;
        proxy_send_timeout 120s;
        client_max_body_size 50m;
        client_body_buffer_size 256k;
        proxy_buffer_size 256k;
        proxy_buffers 4 256k;
        proxy_busy_buffers_size 256k;
        proxy_temp_file_write_size 256k;
        proxy_max_temp_file_size 128m;
    }
}
```
