---
title: Gollum安装(Git+Markdown+Wiki)
author: shixiongfei
date: 2019-01-29 21:55:00
categories: 技术
tags: [技术, Linux, Debian, Apache, Gollum, Git, Markdown, Wiki]
---

## 安装Gollum

- 安装Ruby

```shell
$ aptitude install ruby ruby-dev rubygems
```

- 安装相关依赖

```shell
$ aptitude install libicu-dev
```

- 安装Gollum

```shell
$ gem install gollum
```

- 安装Markdown支持

```shell
$ gem install redcarpet
$ gem install github-markdown
```

- 安装代码高亮

```shell
$ pip install Pygments
```

- 创建Git仓库或者Clone一个仓库

```shell
$ mkdir wiki
$ cd wiki
$ git init
$ gollum
# 访问http://localhost:4567
```

## 配置Apache2

- 启用Apache2的Proxy功能

```shell
$ a2enmod proxy
$ a2enmod proxy_http
```

- 配置apache

```shell
$ vi /etc/apache2/mods-available/proxy.conf
    ProxyRequests On
    <Proxy *>
        AddDefaultCharset off
        Order deny,allow
        #Deny from all
        Allow from .shixf.com
    </Proxy>

$ vi /etc/apache2/sites-available/default
    ProxyRequests Off
    ProxyPreserveHost On

    <Proxy *>
        Order deny,allow
        Allow from all
    </Proxy>

    ProxyPass /wiki http://localhost:4567/wiki
    ProxyPassReverse /wiki http://localhost:4567/wiki

    <Location /wiki>
        Order allow,deny
        Allow from all
        ProxyPass http://localhost:4567/wiki
        ProxyPassReverse /wiki
    </Location>
```

- 启动Gollum(在Git库中提交custom.css与custom.js)

```shell
$ gollum --host localhost --no-edit --js --css --mathjax --gollum-path /var/www/knowledge/ --base-path wiki
```

- 浏览器访问<http://localhost/wiki>

## 设置Gollum自动化

- 开机自动启动

```shell
$ echo PATH=$PATH
$ vi /var/www/wiki/.gollum.sh
    #!/bin/bash

    PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
    export PATH

    DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    gollum --host localhost --no-edit --js --css --mathjax --gollum-path $DIR --base-path wiki
$ vi /etc/rc.local
    bash /var/www/wiki/.gollum.sh &
```

- 自动更新Git(每小时更新一次)

```shell
$ echo PATH=$PATH
$ vi /var/www/wiki/.sync.sh
    #!/bin/bash

    PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
    export PATH

    DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    (cd $DIR && git pull && git push)
$ crontab -e
    0 */1 * * * bash /var/www/wiki/.sync.sh
```
