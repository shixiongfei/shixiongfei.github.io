---
title: CentOS7安装MariaDB10与XtraBackup
author: shixiongfei
date: 2019-01-16 14:27:00
categories: 数据库
tags: [数据库, MySQL, MariaDB, PerconaDB, Linux, CentOS]
---

## 添加MariaDB软件仓库

```shell
$ vi /etc/yum.repos.d/MariaDB.repo
    [mariadb]
    name=MariaDB
    baseurl=http://yum.mariadb.org/10.0/centos7-amd64/
    gpgkey=https://yum.mariadb.org/RPM-GPG-KEY-MariaDB
    gpgcheck=1
    priority=1
$ yum check-update
```

## 添加XtraBackup软件仓库

```shell
$ vi /etc/yum.repos.d/MariaDB.repo
    [percona]
    name=CentOS $releasever - Percona
    baseurl=http://repo.percona.com/centos/$releasever/os/$basearch/
    gpgkey=https://www.percona.com/downloads/RPM-GPG-KEY-percona
    gpgcheck=1
    priority=2
$ yum check-update
```

## 导入MariaDB证书

```shell
$ rpm --import https://yum.mariadb.org/RPM-GPG-KEY-MariaDB
```

## 导入Percona证书

```shell
$ rpm --import https://www.percona.com/downloads/RPM-GPG-KEY-percona
```

## 安装MariaDB

```shell
$ yum install MariaDB-server MariaDB-client
```

## 安装Percona Xtrabackup

```shell
$ yum install percona-xtrabackup
```

## 启动MariaDB

```shell
$ /etc/init.d/mysql start
```

## 开机自动启动MariaDB

```shell
$ chkconfig mysql on
```

- 或者

```shell
$ systemctl enable mariadb.service
```

## 初始化安全设置

```shell
$ /usr/bin/mysql_secure_installation
```

## 允许root远程登录

```shell
$ mysql -uroot -p123456
  MariaDB[] > GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' IDENTIFIED BY '123456' WITH GRANT OPTION;
  MariaDB[] > FLUSH PRIVILEGES;
```
