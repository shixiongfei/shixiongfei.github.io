---
title: Debian下Apache不修改配置文件支持多站点
author: shixiongfei
date: 2019-04-30 23:19:00
categories: 技术
tags: [技术, Linux, Debian, Apache]
---

假定现在有 2 个域名，分别是 xxx.com 与 yyy.com，对应本地目录是 /var/www/xxx/ 与 /var/www/yyy/。

接下来我们创建 2 个有效站点的配置文件。

```shell
$ vi /etc/apache2/sites-available/xxx.com
    <VirtualHost *:80>
        ServerAdmin webmaster@localhost
        ServerName xxx.com
        ServerAlias www.xxx.com
        CustomLog /var/log/apache2/xxx.com-access.log combined
        DocumentRoot /var/www/xxx/

        <Directory /var/www/xxx/>
            Options Indexes FollowSymLinks MultiViews
            AllowOverride all
            Order allow,deny
            allow from all
        </Directory>
    </VirtualHost>

$ vi /etc/apache2/sites-available/yyy.com
    <VirtualHost *:80>
        ServerAdmin webmaster@localhost
        ServerName yyy.com
        ServerAlias www.yyy.com
        CustomLog /var/log/apache2/yyy.com-access.log combined
        DocumentRoot /var/www/yyy/

        <Directory /var/www/yyy/>
            Options Indexes FollowSymLinks MultiViews
            AllowOverride all
            Order allow,deny
            allow from all
        </Directory>
    </VirtualHost>
```

最后创建 2 个快捷方式并重启 apache。

```shell
$ ln -s /etc/apache2/sites-available/xxx.com /etc/apache2/sites-enabled/xxx.com
$ ln -s /etc/apache2/sites-available/yyy.com /etc/apache2/sites-enabled/yyy.com
$ /etc/init.d/apache2 force-reload
```
