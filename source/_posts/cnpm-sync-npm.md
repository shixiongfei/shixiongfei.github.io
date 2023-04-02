---
title: 使用cnpm搭建npm镜像服务器
date: 2018-12-03 15:03:00
author: shixiongfei
categories: 技术
tags: [技术, cnpm, npm, NodeJS]
---

**cnpm需要至少0.11.12版本，如果系统中的版本大于这个版本，可以跳过第一步。CentOS7软件仓库里的NodeJS版本有点旧，这里升级一下node与npm。**

下载地址：<https://nodejs.org/download/>

```shell
$ wget https://nodejs.org/dist/v0.12.7/node-v0.12.7.tar.gz
$ tar xvf node-v0.12.7.tar.gz
$ cd node-v0.12.7
$ ./configure
$ make
$ make install
$ ln -s /usr/local/bin/node /usr/bin/node
$ node -v

$ wget https://npmjs.org/install.sh
$ chmod +x install.sh
$ ./install.sh
```

## 下载cnpm源码

```shell
$ git clone https://github.com/cnpm/cnpmjs.org.git
```

## 配置MariaDB

```shell
$ mysql -uroot -p123456 -e 'CREATE DATABASE cnpmjs_test;'
$ mysql -uroot -p123456 'cnpmjs_test' < docs/db.sql
$ vi config/index.js
    database: {
        db: 'cnpmjs_test',
        username: 'root',
        password: '123456',
        dialect: 'mariadb',
        host: '127.0.0.1',
        port: 3306,
    },

    syncModel: 'all',
```

## 安装/升级依赖

```shell
$ make install
$ make autod
$ vi config/index.js
    registryPort: 7001,
    webPort: 7002,
    bindingHost: '0.0.0.0',
```

## 启动cnpm服务

```shell
$ npm run start
```

## 使用镜像服务器

```shell
$ npm config set registry http://127.0.0.1:7001/
```

## 使用cnpm与同步包

```shell
$ npm install -g cnpm
$ cnpm sync connect
```
