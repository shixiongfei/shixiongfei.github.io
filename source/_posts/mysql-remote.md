---
title: MySQL开启远程登录
author: shixiongfei
date: 2019-02-08 13:49:00
categories: 数据库
tags: [数据库, MySQL, MariaDB]
---

```shell
$ mysql -uroot -p123456
    > use mysql;
    > GRANT ALL PRIVILEGES ON *.* TO "root"@"%" IDENTIFIED BY "123456" WITH GRANT OPTION;
    > FLUSH PRIVILEGES;
    > select host,password,user from user;
    > quit
$ /etc/init.d/mysql restart
```
