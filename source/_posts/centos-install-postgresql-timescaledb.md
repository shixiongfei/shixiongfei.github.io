---
title: CentOS7安装PostgreSQL和TimeScaleDB
date: 2019-09-07 20:58:00
author: shixiongfei
categories: 数据库
tags: [数据库, PostgreSQL, TimeScaleDB, Linux, CentOS]
---

## 安装EPEL仓库(安装数据库有些包依赖EPEL仓库)

- <https://fedoraproject.org/wiki/EPEL>

``` shell
$ yum install epel-release
```

## 安装PostgreSQL仓库

- <https://yum.postgresql.org>

```shell
$ yum install https://download.postgresql.org/pub/repos/yum/reporpms/EL-7-x86_64/pgdg-redhat-repo-latest.noarch.rpm
```

## 安装PostgreSQL 11

```shell
$ yum install postgresql11-server
```

## 初始化数据库

```shell
$ /usr/pgsql-11/bin/postgresql-11-setup initdb
```

## 修改配置文件允许远程连接

```shell
$ vi /var/lib/pgsql/11/data/postgresql.conf
  listen_addresses = '*'

$ vi /var/lib/pgsql/11/data/pg_hba.conf
  host    all             all             0.0.0.0/0               trust
```

## 启动PostgreSQL服务

```shell
$ systemctl enable postgresql-11
$ systemctl start postgresql-11
$ systemctl status postgresql-11
```

## 修改数据库账户postgres默认密码

```shell
$ su - postgres
$ psql
$ alter user postgres password '123456'
```

## 安装TimeScaleDB插件

- <https://docs.timescale.com/latest/getting-started/installation/rhel-centos/installation-yum>

## 安装TimeScaleDB

```shell
$ tee /etc/yum.repos.d/timescale_timescaledb.repo <<EOL
[timescale_timescaledb]
name=timescale_timescaledb
baseurl=https://packagecloud.io/timescale/timescaledb/el/7/\$basearch
repo_gpgcheck=1
gpgcheck=0
enabled=1
gpgkey=https://packagecloud.io/timescale/timescaledb/gpgkey
sslverify=1
sslcacert=/etc/pki/tls/certs/ca-bundle.crt
metadata_expire=300
EOL

$ yum install timescaledb-postgresql-11
$ timescaledb-tune --pg-config=/usr/pgsql-11/bin/pg_config
$ systemctl restart postgresql-11
```
