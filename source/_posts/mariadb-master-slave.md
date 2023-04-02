---
title: MariaDB配置主从
author: shixiongfei
date: 2019-04-23 22:55:00
categories: 数据库
tags: [数据库, MySQL, MariaDB, Linux]
---

MariaDB配置主库

```shell
$ vi /etc/my.cnf.d/server.cnf
    [myslqld]
    log-bin=/var/log/mysql/master-bin
    server-id=1
    #注意：日志目录需要存在

$ service mysql start

$ mysql -uroot -p123456
    > CREATE USER 'backup'@'%' IDENTIFIED BY '123456';
    > GRANT REPLICATION SLAVE,RELOAD,SUPER ON *.* TO 'backup'@'%' IDENTIFIED BY '123456';
    > FLUSH PRIVILEGES;
    > SHOW MASTER STATUS;
```

MariaDB配置从库

```shell
$ vi /etc/my.cnf.d/server.cnf
    [myslqld]
    server-id=2
    datadir=/var/lib/mysql
    log-bin=/var/log/mysql/salve-bin
    relay-log=/var/log/mysql/relay-bin
    log-slave-updates=1
    binlog_format=mixed
    read-only=1
    max_connections=1000
    expire_logs_days=7
    log_bin_trust_function_creators=1
    slave-skip-errors=1062,1690     # 忽略1062和1690错误
    #注意：日志目录需要存在

$ service mysql restart

$ mysql -uroot -p123456
    > CHANGE MASTER TO master_host='192.168.1.10',master_port=3306,master_user='backup',master_password='123456',master_log_file='master-bin.000001',master_log_pos=0;
    > START SLAVE;
    > SHOW SLAVE STATUS;
```

忽略错误后，继续同步

- 该方法适用于主从库数据相差不大，或者要求数据可以不完全统一的情况，数据要求不严格的情况

```sql
> STOP SLAVE;
> SET GLOBAL SQL_SLAVE_SKIP_COUNTER=1;  #表示跳过一步错误，后面的数字可变
> START SLAVE;
```

**注：** 如果第一次同步出现1146数据表不存在错误的话，可以手工创建相应的数据库和数据表，然后STOP SLAVE; START SLAVE;
