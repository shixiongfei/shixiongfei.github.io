---
title: CentOS使用rsync搭建镜像服务器
author: shixiongfei
date: 2019-05-08 22:37:00
categories: 技术
tags: [技术, Linux, CentOS]
---

## 安装配置Web服务

```shell
$ yum install httpd
$ mkdir /home/_mirrors
$ chmod -R 755 /home/_mirrors
$ ln -s /home/_mirrors /var/www/mirrors
$ vi /etc/httpd/conf/httpd.conf
    Alias /mirrors "/var/www/mirrors"
    <Directory "/var/www/mirrors">
        Options Indexes MultiViews FollowSymLinks
        AllowOverride None
        Order deny,allow
        Allow from all
    </Directory>
$ systemctl restart httpd.service
```

## 安装rsync

```shell
$ yum install rsync
```

常用yum源收集

- CentOS
  - <http://www.centos.org/download/mirrors/>
  - rsync://mirrors.yun-idc.com/centos/

- EPEL
  - <https://admin.fedoraproject.org/mirrormanager/mirrors>
  - rsync://dl.fedoraproject.org/fedora-epel

- RepoForge
  - <http://repoforge.org/faq/>
  - <http://mirror-status.repoforge.org>
  - rsync://ftp.riken.jp/repoforge/

- MariaDB
  - <https://mariadb.com/kb/en/mariadb/mirroring-mariadb/>
  - rsync.osuosl.org::mariadb

- Percona
  - <https://www.percona.com/blog/2014/02/14/using-percona-rsync-repositories-set-local-centos-mirror/>
  - rsync://rsync.percona.com/rsync/

## 自动同步脚本

```shell
$ mkdir /home/_mirrors/centos
$ mkdir /home/_mirrors/epel
$ mkdir /home/_mirrors/repoforge
$ mkdir /home/_mirrors/mariadbs
$ mkdir /home/_mirrors/percona

$ vi /home/_mirrors/yum_rsync.sh
    #!/bin/bash

    rsync -vai4CH --safe-links --delay-updates --delete rsync://mirrors.yun-idc.com/centos/ --exclude-from=/home/_mirrors/centos_exclude.list /home/_mirrors/centos/
    rsync -vai4CH --safe-links --delay-updates --delete rsync://dl.fedoraproject.org/fedora-epel --exclude-from=/home/_mirrors/epel_exclude.list /home/_mirrors/epel/
    rsync -vai4CH --safe-links --delay-updates --delete rsync://ftp.riken.jp/repoforge/ --exclude-from=/home/_mirrors/repoforge_exclude.list /home/_mirrors/repoforge/
    rsync -vai4CH --safe-links --delay-updates --delete rsync.osuosl.org::mariadb --exclude-from=/home/_mirrors/mariadb_exclude.list /home/_mirrors/mariadb/
    rsync -vai4CH --safe-links --delay-updates --delete rsync://rsync.percona.com/rsync/ --exclude-from=/home/_mirrors/percona_exclude.list /home/_mirrors/percona/
```

## 剔除不需要同步的内容

```shell
$ vi /home/_mirrors/centos_exclude.list
    2/
    2.1/
    3/
    3.1/
    3.3/
    3.4/
    3.5/
    3.6/
    3.7/
    3.8/
    3.9/
    4/
    4.0/
    4.1/
    4.2/
    4.3/
    4.4/
    4.5/
    4.6/
    4.7/
    4.8/
    4.9/
$ vi /home/_mirrors/epel_exclude.list
    4/
    4AS/
    4ES/
    4WS/
$ vi /home/_mirrors/repoforge_exclude.list
$ vi /home/_mirrors/mariadb_exclude.list
$ vi /home/_mirrors/percona_exclude.list
```

## 配置自动执行任务

- 每8小时同步一次镜像，使用flock确保任务不会被重复执行

```shell
$ echo PATH=$PATH
$ vi /home/_mirrors/auto_sync.sh
    #!/bin/bash

    PATH=/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/home/lishu/.local/bin:/home/lishu/bin
    export PATH

    flock -xn /var/lock/auto-yum-rsync.lock /home/_mirrors/yum_rsync.sh >> /var/log/yum_rsync.log

$ service crond status
$ crontab -e
    * */8 * * * /home/_mirrors/auto_sync.sh
$ crontab -l
```

## 配置YUM源

```shell
$ vi /etc/yum.repos.d/*.repo

# CentOS源
# mirrorlist
baseurl=http://127.0.0.1/centos/$releasever/os/$basearch/

# EPEL源
# mirrorlist
baseurl=http://127.0.0.1/epel/7/$basearch

# RepoForge源
# mirrorlist
baseurl=http://127.0.0.1/repoforge/redhat/el7/en/$basearch/rpmforge

# MariaDB源
# mirrorlist
baseurl=http://127.0.0.1/mariadb/10.0/centos7-amd64/

# Percona源
# mirrorlist
baseurl=http://127.0.0.1/percona/centos/$releasever/os/$basearch/
```
