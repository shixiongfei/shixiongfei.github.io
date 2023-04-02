---
title: MySQL InnoDB优化
author: shixiongfei
date: 2019-05-14 23:20:00
categories: 数据库
tags: [数据库, MySQL, MariaDB]
---

## 介绍

InnoDB给MySQL提供了具有提交，回滚和崩溃恢复能力的事务安全（ACID兼容）存储引擎。InnoDB锁定在行级并且也在SELECT语句提供一个Oracle风格一致的非锁定读。这些特色增加了多用户部署和性能。没有在InnoDB中扩大锁定的需要，因为在InnoDB中行级锁定适合非常小的空间。InnoDB也支持FOREIGN KEY强制。在SQL查询中，你可以自由地将InnoDB类型的表与其它MySQL的表的类型混合起来，甚至在同一个查询中也可以混合。

目前来说：InnoDB是为Mysql处理巨大数据量时的最大性能设计。它的CPU效率可能是任何其它基于磁盘的关系数据库引擎所不能匹敌的。在数据量大的网站或是应用中InnoDB是倍受青睐的。

另一方面，在数据库的复制操作中InnoDB也是能保证master和slave数据一致有一定的作用。

InnoDB的创始人：Heikki Tuuri

## 参数调优内容

### 内存利用方面

innodb_buffer_pool_size

- 这个参数和MyISAM的key_buffer_size有相似之处，但也是有差别的。这个参数主要缓存InnoDB表的索引，数据，插入数据时的缓冲。为Innodb加速优化首要参数。
- 该参数分配内存的原则：这个参数默认分配只有8M，可以说是非常小的一个值。如果是一个专用DB服务器，那么他可以占到内存的70%-80%。这个参数不能动态更改，所以分配需多考虑。分配过大，会使Swap占用过多，致使Mysql的查询特慢。如果你的数据比较小，那么可分配是你的数据大小＋10%左右做为这个参数的值。
  - 例如：数据大小为50M, 那么给这个值分配innodb_buffer_pool_size=64M
  - 这个参数分配值的使用情况可以根据 show innodb status\G; 中的

    ```text
    ----------------------
    BUFFER POOL AND MEMORY
    ----------------------

    Total memory allocated 4668764894;
    ```

  - 去确认使用情况，设置方法：

    ```text
    innodb_buffer_pool_size=4G
    ```

### 日志控制方面

innodb_log_file_size

- 作用：指定日志的大小
- 分配原则：几个日志成员大小加起来差不多和你的innodb_buffer_pool_size相等。上限为每个日志上限大小为4G。一般控制在几个LOG文件相加大小在2G以内为佳。具体情况还需要看你的事务大小，数据大小为依据。
  - 说明：这个值分配的大小和数据库的写入速度，事务大小，异常重启后的恢复有很大的关系。
  - 设置方法：

    ```text
    innodb_log_file_size=256M
    ```

innodb_log_files_in_group

- 作用：指定你有几个日志组。
- 分配原则：一般我们可以用 2 - 3 个日志组。默认为两个。
- 设置方法：

  ```text
  innodb_log_files_in_group=3
  ```

innodb_log_buffer_size

- 作用：事务在内存中的缓冲。
- 分配原则：控制在 2 - 8M，这个值不用太多的。他里面的内存一般一秒钟写到磁盘一次。具体写入方式和你的事务提交方式有关。在Oracle等数据库了解这个，一般最大指定为3M比较合适。
- 参考：Innodb_os_log_written(show global status 可以拿到)
- 如果这个值增长过快，可以适当的增加innodb_log_buffer_size
- 另外如果你需要处理大量的TEXT，或是BLOB字段，可以考虑增加这个参数的值。
- 设置方法：

  ```text
  innodb_log_buffer_size=3M
  ```

innodb_flush_logs_at_trx_commit

- 作用：控制事务的提交方式
- 分配原则：这个参数只有3个值，0, 1, 2 请确认一下自已能接受的级别。默认为1。
- 性能更高的可以设置为0或是2，但会丢失一秒钟的事务。
- 说明：
  - 这个参数的设置对InnoDB的性能有很大的影响，所以在这里给多说明一下。
  - 当这个值为1时：InnoDB的事务LOG在每次提交后写入日志文件，并对日志做刷新到磁盘。这个可以做到不丢任何一个事务。
  - 当这个值为2时：在每个提交，日志缓冲被写到文件，但不对日志文件做到磁盘操作的刷新,在对日志文件的刷新在值为2的情况也每秒发生一次。但需要注意的是，由于进程调用方面的问题，并不能保证每秒100%的发生。从而在性能上是最快的。但操作系统崩溃或掉电才会删除最后一秒的事务。
  - 当这个值为0时：日志缓冲每秒一次地被写到日志文件，并且对日志文件做到磁盘操作的刷新，但是在一个事务提交不做任何操作。mysqld进程的崩溃会删除崩溃前最后一秒的事务。
  - 从以上分析，当这个值不为１时，可以取得较好的性能，但遇到异常会有损失，所以需要根据自已的情况去衡量。
- 设置方法：

  ```text
  innodb_flush_logs_at_trx_commit=2
  ```

sync_binlog

- 作用：控制binlog刷到磁盘的时机
- sync_binlog的默认值是0，像操作系统刷其他文件的机制一样，MySQL不会同步到磁盘中去而是依赖操作系统来刷新binary log。
- 当sync_binlog=N (N>0) ，MySQL在每写N次二进制日志binary log时，会使用fdatasync()函数将它的写二进制日志binary log同步到磁盘中去。
- 设置方法：

  ```text
  sync_binlog=100
  ```

### 文件IO分配，空间占用方面

innodb_file_per_table

- 作用：使每个Innodb的表，有自已独立的表空间。如删除文件后可以回收那部分空间。
- 分配原则：只有使用不使用。但DB还需要有一个公共的表空间。
- 设置方法：

  ```text
  innodb_file_per_table=1
  ```

innodb_open_files

- 作用：限制Innodb能打开的表的数据。
- 分配原则：如果库里的表特别多的情况，请增加这个。这个值默认是300。
- 设置方法：

  ```text
  innodb_open_files=800
  ```

- 请适当的增加table_cache

### 其它相关参数

innodb_flush_method

- 作用：InnoDB和系统打交道的一个IO模型
- 分配原则：Windows不用设置。Unix可以设置：fsync() or O_SYNC/O_DSYNC
- 如果系统可以禁止系统的Cache那就把他禁了。
- Linux可以选择：O_DIRECT
- 直接写入磁盘，禁止系统Cache了
- 设置方法：

  ```text
  innodb_flush_method=O_DIRECT
  ```

innodb_max_dirty_pages_pct

- 作用：控制InnoDB的脏页在缓冲中在哪个百分比之下，值在范围1-100,默认为90.
- 这个参数的另一个用处：当InnoDB的内存分配过大，致使Swap占用严重时，可以适当的减小调整这个值，使达到Swap空间释放出来。建义：这个值最大在90%，最小在15%。太大，缓存中每次更新需要致换数据页太多，太小，放的数据页太小，更新操作太慢。
- 设置方法：

  ```text
  innodb_max_dirty_pages_pct=90
  ```

- 动态更改需要有Super权限：

  ```text
  set global innodb_max_dirty_pages_pct=50;
  ```

## 关于Schema设计规范及SQL使用建议

1. 所有的InnoDB表都设计一个无业务用途的自增列做主键，对于绝大多数场景都是如此，真正纯只读用InnoDB表的并不多，真如此的话还不如用TokuDB来得划算；
2. 字段长度满足需求前提下，尽可能选择长度小的。此外，字段属性尽量都加上NOT NULL约束，可一定程度提高性能；
3. 尽可能不使用TEXT/BLOB类型，确实需要的话，建议拆分到子表中，不要和主表放在一起，避免SELECT * 的时候读性能太差。
4. 读取数据时，只选取所需要的列，不要每次都SELECT *，避免产生严重的随机读问题，尤其是读到一些TEXT/BLOB列；
5. 对一个VARCHAR(N)列创建索引时，通常取其50%（甚至更小）左右长度创建前缀索引就足以满足80%以上的查询需求了，没必要创建整列的全长度索引；
6. 通常情况下，子查询的性能比较差，建议改造成JOIN写法；
7. 多表联接查询时，关联字段类型尽量一致，并且都要有索引；
8. 多表连接查询时，把结果集小的表（注意，这里是指过滤后的结果集，不一定是全表数据量小的）作为驱动表；
9. 多表联接并且有排序时，排序字段必须是驱动表里的，否则排序列无法用到索引；
10. 多用复合索引，少用多个独立索引，尤其是一些基数（Cardinality）太小（比如说，该列的唯一值总数少于255）的列就不要创建独立索引了；
11. 类似分页功能的SQL，建议先用主键关联，然后返回结果集，效率会高很多；

## 其他建议

1. 通常地，单表物理大小不超过10GB，单表行数不超过1亿条，行平均长度不超过8KB，如果机器性能足够，这些数据量MySQL是完全能处理的过来的，不用担心性能问题，这么建议主要是考虑ONLINE DDL的代价较高；
2. 不用太担心mysqld进程占用太多内存，只要不发生OOM kill和用到大量的SWAP都还好；
3. 在以往，单机上跑多实例的目的是能最大化利用计算资源，如果单实例已经能耗尽大部分计算资源的话，就没必要再跑多实例了；
4. 定期使用pt-duplicate-key-checker检查并删除重复的索引。定期使用pt-index-usage工具检查并删除使用频率很低的索引；
5. 定期采集slow query log，用pt-query-digest工具进行分析，可结合Anemometer系统进行slow query管理以便分析slow query并进行后续优化工作；
6. 可使用pt-kill杀掉超长时间的SQL请求，Percona版本中有个选项 innodb_kill_idle_transaction 也可实现该功能；
7. 使用pt-online-schema-change来完成大表的ONLINE DDL需求；
8. 定期使用pt-table-checksum、pt-table-sync来检查并修复mysql主从复制的数据差异；
