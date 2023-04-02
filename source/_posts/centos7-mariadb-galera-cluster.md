---
title: CentOS7安装MariaDB-Galera-Cluster
author: shixiongfei
date: 2019-01-17 10:40:00
categories: 数据库
tags: [数据库, MySQL, MariaDB, Linux, CentOS]
---

**假设3台数据库服务器（db1为主节点，db2,db3为从节点）**

- Cluster node 1 has hostname db1 and IP address 1.1.1.1
- Cluster node 2 has hostname db2 and IP address 1.1.1.2
- Cluster node 3 has hostname db3 and IP address 1.1.1.3

## 安装MariaDB Galera Cluster

```shell
$ yum install MariaDB-Galera-server galera socat
```

## 初始化数据库(只需要主节点执行)

```shell
$ /etc/init.d/mysql start
$ /usr/bin/mysql_secure_installation
$ /etc/init.d/mysql stop
```

## 如果安装了MariaDB-server就先删除

```shell
$ yum remove MariaDB-server
```

## 登陆数据库，设置用于集群同步的用户和密码(只需要主节点执行)

```shell
$ mysql -uroot -p123456
  MariaDB[] > GRANT USAGE ON *.* to sst@'%' IDENTIFIED BY '123456';
  MariaDB[] > GRANT ALL PRIVILEGES on *.* to sst@'%';
  MariaDB[] > FLUSH PRIVILEGES;
  MariaDB[] > quit
```

## iptables开放系统端口3306,4444和4567

```shell
$ iptables -I INPUT -p tcp --dport 3306 -j ACCEPT
$ iptables -I INPUT -p tcp --dport 4444 -j ACCEPT
$ iptables -I INPUT -p tcp --dport 4567 -j ACCEPT
```

- 或者

```shell
$ iptables -F
```

## 关闭SELinux

```shell
$ vi /etc/selinux/config
    SELINUX=disabled
```

- 或者

```shell
$ setenforce 0
```

## 添加sst同步账号(只需要主节点执行)

```sql
GRANT ALL PRIVILEGES ON *.* TO 'sst'@'1.1.1.1' IDENTIFIED BY '123456';
GRANT ALL PRIVILEGES ON *.* TO 'sst'@'1.1.1.2' IDENTIFIED BY '123456';
GRANT ALL PRIVILEGES ON *.* TO 'sst'@'1.1.1.3' IDENTIFIED BY '123456';
FLUSH PRIVILEGES;
```

## 配置MariaDB Galera(三台机器同样配置)

```shell
$ cp /usr/share/mysql/wsrep.cnf /etc/my.cnf.d/
$ vi /etc/my.cnf.d/wsrep.cnf
    datadir=/var/lib/mysql
    wsrep_provider=/usr/lib64/galera/libgalera_smm.so
    wsrep_cluster_name="my_wsrep_cluster"
    wsrep_cluster_address="gcomm://1.1.1.1,1.1.1.2,1.1.1.3"
    wsrep_node_name=db1         # 在db2和db3上不要忘记修改名字
    wsrep_node_address=1.1.1.1  # 在db2和db3上不要忘记修改地址
    wsrep_sst_method=rsync      # 建议选择xtrabackup或xtrabackup-v2同步方式,xtrabackup方式需要安装percona-xtrabackup
    wsrep_sst_auth=sst:123456
```

## 主节点启动

```shell
$ /etc/init.d/mysql start --wsrep-new-cluster
```

## 从节点启动

```shell
$ /etc/init.d/mysql start
```

## 查看Galera状态

```shell
$ mysql -uroot -p123456 -e"show status like 'wsrep%';"
  wsrep_local_state_comment | Synced <-- cluster is synced
  wsrep_incoming_addresses  | 1.1.1.1:3306 <-- node db1 is a provider
  wsrep_cluster_size        | 1 <-- cluster consists of 1 node
  wsrep_ready               | ON <-- good :)
```

## InnoDB 相关参数

- 为了降低冲突，下列两项需要设置

```shell
$ vi /etc/my.cnf.d/wsrep.cnf
    innodb_autoinc_lock_mode=2
```

- 选配：（可以提高性能，galera保证不丢数据）

```shell
$ vi /etc/my.cnf.d/wsrep.cnf
    innodb_flush_log_at_trx_commit=2
```

- 如果innodb_flush_log_at_trx_commit设置为0，log buffer将每秒一次地写入log file中，并且log file的flush(刷到磁盘)操作同时进行.该模式下，在事务提交的时候，不会主动触发写入磁盘的操作。

- 如果innodb_flush_log_at_trx_commit设置为1，每次事务提交时MySQL都会把log buffer的数据写入log file，并且flush(刷到磁盘)中去.

- 如果innodb_flush_log_at_trx_commit设置为2，每次事务提交时MySQL都会把log buffer的数据写入log file. 但是flush(刷到磁盘)操作并不会同时进行。该模式下,MySQL会每秒执行一次 flush(刷到磁盘)操作。

- 设置binlog刷新

```shell
$ vi /etc/my.cnf.d/wsrep.cnf
    sync_binlog=100
```

- sync_binlog的默认值是0，像操作系统刷其他文件的机制一样，MySQL不会同步到磁盘中去而是依赖操作系统来刷新binary log。

- 当sync_binlog=N (N>0) ，MySQL在每写N次二进制日志binary log时，会使用fdatasync()函数将它的写二进制日志binary log同步到磁盘中去。

## 判断复制过程是否出现问题

```shell
$ mysql -uroot -p123456 -e"show status like 'wsrep%';"
  wsrep_flow_control_paused
```

- 正常情况下，其取值应该接近于0.0，大于0.0意味着有‘慢节点’影响了集群的速度

- 可以尝试通过增大wsrep_slave_threads来解决

## 找出慢节点

- 找出下面两个值最大的节点

```shell
$ mysql -uroot -p123456 -e"show status like 'wsrep%';"
  wsrep_flow_control_sent
  wsrep_local_recv_queue_avg
```

最后，任意数据库节点宕机（不论主从节点）都以从节点方式重启即可。
