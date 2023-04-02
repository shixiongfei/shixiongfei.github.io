---
title: MySQL InnoDB备份方案
author: shixiongfei
date: 2019-05-13 22:40:00
categories: 数据库
tags: [数据库, MySQL, MariaDB]
---

## XtraBackup介绍

Percona XtraBackup是开源免费的MySQL数据库热备份软件，它能对InnoDB和XtraDB存储引擎的数据库非阻塞地备份（对于MyISAM的备份同样需要加表锁）。XtraBackup支持所有的Percona Server、MySQL、MariaDB和Drizzle。

XtraBackup优势：

1. 无需停止数据库进行InnoDB热备
2. 增量备份MySQL
3. 流压缩传输到其它服务器
4. 能比较容易地创建主从同步
5. 备份MySQL时不会增大服务器负载

## 创建mysql备份用户

```shell
$ mysql -uroot -p123456 -e "CREATE USER 'backup'@'%' IDENTIFIED BY '123456'"
$ mysql -uroot -p123456 -e "GRANT RELOAD, LOCK TABLES, REPLICATION CLIENT, CREATE TABLESPACE, SUPER ON *.## TO 'backup'@'%'"
```

## mysql完整热备份与恢复

完整备份

```shell
$ innobackupex --user=backup --password=123456 /data/vbackup/hotbackup/full --no-timestamp --parallel=2
```

备份恢复准备

```shell
$ innobackupex --apply-log --use-memory=10G /data/vbackup/hotbackup/full
```

恢复备份

```shell
$ service mysql stop    # 停止mysql,清空MySQL的datadir
$ mv /usr/local/mysql/data /usr/local/mysql/data.old
$ mkdir -p /usr/local/mysql/data
$ innobackupex --copy-back --rsync /data/vbackup/hotbackup/full    # 恢复备份数据
$ chown -R mysql:mysql /usr/local/mysql/data    # 修改datadir的权限
```

## mysql增量热备份

创建基础备份(全备份)

```shell
$ innobackupex --user=backup --password=123456  /data/vbackup/hotbackup/base --no-timestamp --parallel=2
```

基于基础备份,创建增量备份inc_one

```shell
$ innobackupex --user=backup --password=123456 --incremental /data/vbackup/hotbackup/inc_one --incremental-basedir=/data/vbackup/hotbackup/base --no-timestamp --parallel=2
```

基于增量备份inc_one，创建增量备份inc_two

```shell
$ innobackupex --user=backup --password=123456 --incremental /data/vbackup/hotbackup/inc_two --incremental-basedir=/data/vbackup/hotbackup/inc_one --no-timestamp --parallel=2
```

## mysql增量热备份恢复

准备基础备份(应用基础备份xtrabackup日志中的提交的事务，但不回滚未提交的事务)

```shell
$ innobackupex --apply-log --redo-only --use-memory=10G /data/vbackup/hotbackup/base
```

将增量备份inc_one应用到基础备份

```shell
$ innobackupex --apply-log --redo-only --use-memory=10G /data/vbackup/hotbackup/base/ --incremental-dir=/data/vbackup/hotbackup/inc_one/
```

将增量备份inc_two应用到基础备份

```shell
$ innobackupex --apply-log --use-memory=10G /data/vbackup/hotbackup/base/ --incremental-dir=/data/vbackup/hotbackup/inc_two
```

- 注：当准备基础备份和合并增量备份(最后一个增量备份除外)的时候，需要使用 `--redo-only` 参数。

当所有的增量备份合并完成之后，再一次准备全备份,回滚未提交的事务

```shell
$ innobackupex --apply-log --use-memory=10G /data/vbackup/hotbackup/base/
```

恢复全备份并启动mysql

```shell
$ service mysql stop
$ rm -rf /usr/local/mysql/data.old && mv /usr/local/mysql/data /usr/local/mysql/data.old
$ mkdir -p /usr/local/mysql/data
$ innobackupex --copy-back --rsync /data/vbackup/hotbackup/base/
$ chown -R mysql:mysql /usr/local/mysql/data
$ service mysql start
```

## innobackupex部分备份

innobackupex部分备份，有以下三个参数可选择

1. `--databases="database1[.table1] ..."`
   - 比如: `--databases="employees sales.orders"`
2. `--tables-file=tables.txt`
   - tables.txt文件每一行有一个database.table
3. `--include=regexp,`
   - 比如: `--include='^database(1|2)\.reports'`

在准备“部分备份”和拷贝恢复“部分备份”到一个空的datadir之后，请确保datadir中有”mysql”数据库，否则mysql无法启动。

如果没有，则需要先创建系统的数据库再启动:

```shell
$ mysql_install_db --user=mysql
```

## innobackupex从库热备份

在主从复制环境中的”从库”做热备份的时候,可以使用 `--slave-info` 和 `--safe-slave-backup` 参数。

在备份的时候使用 `--slave-info` 会将Master的binary log的文件名和偏移位置记录打印出来，同时也保存到xtrabackup_slave_info文件中。
`--safe-slave-backup` 则会暂停Slave的SQL线程，等待到没有打开的临时表的时候开始备份。备份结束后SQL线程会自动启动，这样就可以确保一致性的复制状态。

## XtraBackup流备份与压缩备份

### tar流备份及压缩

将完全备份直接存储到tar压缩包,并将它进行gzip压缩

```shell
$ innobackupex --user=backup --password=123456 --stream=tar ./ | gzip - > backup.tar.gz
```

完全备份并gzip压缩发送到远程机器

```shell
$ innobackupex --user=backup --password=123456 --stream=tar ./ | gzip | ssh root@192.168.1.192 "cat - > /data/tarbackup/backup.tar.gz"
```

解压缩(必须使用 -i 参数)

```shell
$ tar -izxvf backup.tar.gz -C ./data/
```

注：解压之后的数据仍然需要进行 apply-log 之后才可以用于恢复

### xbstream流备份及压缩

将备份压缩到名为”backup.xbstream”的压缩包

```shell
$ innobackupex --user=backup --password=123456 --stream=xbstream ./ > backup.xbstream
```

解压backup.xbstream到./backup/

```shell
$ xbstream -x < backup.xbstream -C ./backup/
```

多线程并行文件拷贝和qpress并行快速压缩备份,然后直接发送到远程机器并进行xbstream解压保存

```shell
$ innobackupex --user=backup --password=123456 --compress --compress-threads=8 --parallel=4 --stream=xbstream ./ | ssh root@192.168.1.192 "xbstream -x -C /data/mysql/backup"
```

远程机器上对xbstream解压后的备份文件中的qpress压缩文件进行解压

```shell
$ cd /data/mysql/backup
$ for f in `find ./ -iname "*\.qp"`; do qpress -dT2 $f  $(dirname $f) && rm -f $f; done
```

注：解压之后的数据仍然需要进行 apply-log 之后才可以用于恢复。

### xbstream流增量备份

创建基础全备份,发送到远程机器并解压

```shell
$ innobackupex --user=backup --password=123456 --parallel=4 --stream=xbstream ./ | ssh root@192.168.1.192 "xbstream -x -C /data/www/mysql/incremental/base"
```

查看基础备份的checkpoint信息

```shell
$ ssh root@192.168.1.192 -C "cat /data/www/mysql/incremental/base/xtrabackup_checkpoints"
    backup_type = full-backuped
    from_lsn = 0
    to_lsn = 1807243286
    last_lsn = 1807243286
```

基于基础备份,创建增量备份one

```shell
$ innobackupex --user=backup --password=123456 --parallel=4 --incremental --incremental-lsn=1807243286 --stream=xbstream ./ | ssh root@192.168.1.192 "xbstream -x -C /data/www/mysql/incremental/one"
```

注：关于增量备份的日志应用和备份恢复，请参考上文的"mysql增量热备份恢复"部分

### xtrabackup选项注释

```text
--print-defaults显示默认选项。Xtrabackup默认情况会去读my.cnf文件，读取顺序是/etc/my.cnf /etc/mysql/my.cnf /usr/local/etc/my.cnf ~/.my.cnf
--no-defaults 忽略任何my.cnf文件选项
--defaults-file=#读取指定的my.cnf文件
--defaults-extra-file=#再读取另外一个文件
--target-dir=name目的目录，默认目录在./xtrabackup_backupfiles/，相对于datadir目录
--backup 备份
--stats 计算datadir目录统计信息
--prepare 从backup恢复
--export 在恢复时，创建文件导入到另一个数据库
--apply-log-only在恢复时，停止恢复进程不进行LSN只使用log
--print-param 打印出参数
--use-memory=#buffer_pool_size值
--suspend-at-end在备份时，创建xtrabackup_suspended文件，直到备份完成后删掉
--throttle=# 限制IO总数
--log-stream 记录标准输出信息xtrabackup_logfile
--extra-lsndir=name仅适用于backup，保存另一份xtrabackup_checkpoints文件
--incremental-lsn=name仅适用于backup，增量备份
--incremental-basedir=name仅适用于backup，增量备份目录
--incremental-dir=name仅适用于prepare，恢复指定目录下的.delta文件和日志文件
--tables=name 过滤某些表
--tables_file=name过滤database.table列表文件
--create-ib-logfile
-h,--datadir=name datadir目录
-t,--tmpdir=name tmpdir目录
--parallel=# 默认为1.传输数据文件的并行线程数。没有任何流模式的影响
--innodb_* 有关innodb参数
```

### innobackupex选项注释

```text
--defaults-file=[MY.CNF]该选项传递给xtrabackup子进程，从指定文件读取缺省选项
--apply-log 从备份恢复。
--redo-only 该选项强制跳过rollback阶段，只进行redo。这是有必要使用的，如果备份后，要使用增量改变的。
--copy-back 从备份目录拷贝数据和索引文件到datadir目录
--remote-host=HOSTNAME备份到远程主机上，使用ssh
--stream=[tar|cpio(notimplemented)] 指定备份标准输出格式
--tmpdir=DIRECTORY默认与tmpdir相同。使用—remote-host或—stream参数后，传输日志文件将存放在临时目录下
--use-memory=MB选项传递给xtrabackup子进程。恢复使用内存大小
--parallel=NUMBER-OF-THREADS选项传递给xtrabackup子进程，指定数据传输线程总数。默认为1
--throttle=IOS选项传递给xtrabackup子进程，限制IO线程数量
--sleep=MS 选项传递给xtrabackup子进程。每拷贝1MB数据暂停多少MS时间
--compress[=LEVEL]选项传递给xtrabackup子进程。压缩级别在0-9.1快速压缩，9最佳压缩，0不压缩。默认为1.
--include=REGEXP选项传递给xtrabackup子进程。使用正则进行匹配
--databases=LIST指定备份数据库
--tables-file=FILE
--uncompress选项传递给xtrabackup子进程。对压缩过的InnoDB数据文件不进行压缩
--export 仅使用于prepare选项。选项传递给xtrabackup子进程。
--user=NAME
--password=WORD
--host=HOST
--port=PORT
--slave-info 备份复制从服务端，主从信息记录在ibbackup_slave_info文件中
--socket=SOCKET
--no-timestamp不在备份根目录下创建以当前时间戳为名称的新的备份目录
--ibbackup=IBBACKUP-BINARYibbackup二进制路径
--no-lock 禁止表级锁。全部是InnoDB引擎表和不关系二进制日志位置下使用
--scpopt=SCP-OPTIONS指定scp参数
```
