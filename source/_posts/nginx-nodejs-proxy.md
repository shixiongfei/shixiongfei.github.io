---
title: Nginx反向代理NodeJS
author: shixiongfei
date: 2018-12-15 00:06:00
categories: 技术
tags: [技术, Nginx, NodeJS]
---

## 安装Nginx

```shell
yum install nginx
```

## 配置反向代理

- 假定NodeJS程序运行在 <http://127.0.0.1:8080> 上，需要配置反向代理到80端口上

```shell
$ vi /etc/nginx/nginx.conf
    ...
    http {
        ...

        upstream myCluster {
            server 127.0.0.1:8080;
        }

        server {
            server_name www.example.com;
            location / {
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
                proxy_set_header Host $http_host;
                proxy_set_header X-NginX-Proxy true;
                proxy_pass http://myCluster;
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
        ...
        }
    ...
    }
```

## NodeJS获取客户真实IP

```javascript
function getClientIp (req) {
    return req.headers['x-forwarded-for'] || req.connection.remoteAddress;
}
```

## 目录代理

- web1作为前端端服务器，访问地址是 <http://192.168.1.1> ，要将 <http://192.168.1.1/bbs> 的请求交给web2。在web1的网站根目录下并没有bbs目录
- web1配置location 方式一

    ```text
    location /bbs/ {
        proxy_pass http://192.168.1.2/;     #有“/”
    }
    ```

  - 效果：通过 <http://192.168.1.1/bbs> 可以访问到web2网站根目录下的内容

- web2作为后端web服务器，访问地址是 <http://192.168.1.2>
- web1配置location 方式二 (location中不加 “/”)

    ```text
    location /bbs/ {
        proxy_pass http://192.168.1.2;      #无“/”
    }
    ```

  - 效果：要通过web1访问web2网站根目录的内容则需要输入 <http://192.168.1.1/bbs/bbs>
