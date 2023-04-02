---
title: Debian安装Apache2+PHP5+MySQL
author: shixiongfei
date: 2019-04-28 23:07:00
categories: 技术
tags: [技术, Linux, Debian, Apache, PHP, MySQL]
---

## 安装 apache2 mysql php5

```shell
$ aptitude install apache2
$ aptitude install mysql-server
$ aptitude install php5 libapache2-mod-php5
$ aptitude install php5-gd php5-cgi php5-curl php5-dev php5-idn
$ aptitude install php-pear php5-imagick php5-imap php5-mcrypt
$ aptitude install php5-memcache php5-ps php5-pspell php5-recode
$ aptitude install php5-snmp php5-sqlite php5-tidy php5-xmlrpc
$ aptitude install php5-xsl php5-common php5-mysql php5-json
```

## 启用apache2 rewrite功能

```shell
$ a2enmod rewrite
$ vi /etc/apache2/sites-available/default
    # 把所有 AllowOverride None 改为 AllowOverride all
$ /etc/init.d/apache2 restart
```

## 启用apache2 expires功能

```shell
$ a2enmod expires
$ vi /var/www/.htaccess
    <IfModule mod_expires.c>
        ExpiresActive On
        ExpiresByType image/jpg "access plus 1 year"
        ExpiresByType image/jpeg "access plus 1 year"
        ExpiresByType image/gif "access plus 1 year"
        ExpiresByType image/png "access plus 1 year"
        ExpiresByType text/css "access plus 1 month"
        ExpiresByType application/pdf "access plus 1 month"
        ExpiresByType text/x-javascript "access plus 1 month"
        ExpiresByType application/x-shockwave-flash "access plus 1 month"
        ExpiresByType image/x-icon "access plus 1 year"
        ExpiresDefault "access plus 2 days"
    </IfModule>
$ /etc/init.d/apache2 restart
```
