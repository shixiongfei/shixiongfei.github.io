---
title: Debian下Apache安装使用mod_wsgi
author: shixiongfei
date: 2019-04-29 23:48:00
categories: 技术
tags: [技术, Linux, Debian, Apache]
---

mod_wsgi 的速度比 mod_python 更快，也更容易使用，而且 mod_python 也停止维护了。把服务器上的 python 脚本改用 mod_wsgi 运行。

```shell
$ aptitude install libapache2-mod-wsgi
$ mkdir /var/www/py
$ chmod 777 /var/www/py
$ vi /etc/apache2/sites-available/default
    WSGIScriptAlias /hello /var/www/py/hello.wsgi

    <Directory "/var/www/py">
        Order allow,deny
        Allow from all
    </Directory>
$ /etc/init.d/apache2 restart
```

```shell
$ cd /var/www/py
$ vi hello.py
    def application(environ, start_response):
        start_response("200 OK",[('content-type',"text/html")])
        return ['<html><body>HelloWorld!</body></html>']
```

最后使用 <http://localhost/hello> 访问，看到 HelloWorld! 说明配置OK。
