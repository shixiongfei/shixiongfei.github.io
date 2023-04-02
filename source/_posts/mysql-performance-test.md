---
title: MySQL性能测试
author: shixiongfei
date: 2019-04-24 00:11:00
categories: 数据库
tags: [数据库, MySQL, MariaDB]
---

mysqlslap测试

```shell
$ mysqlslap [options]
    --auto-generate-sql, -a
        自动生成测试表和数据，表示用mysqlslap工具自己生成的SQL脚本来测试并发压力。
    --auto-generate-sql-load-type=type
        测试语句的类型。代表要测试的环境是读操作还是写操作还是两者混合的。取值包括：read，key，write，update和mixed(默认)。
    --auto-generate-sql-add-auto-increment
        代表对生成的表自动添加auto_increment列，从5.1.18版本开始支持。
    --number-char-cols=N, -x N
        自动生成的测试表中包含多少个字符类型的列，默认1
    --number-int-cols=N, -y N
        自动生成的测试表中包含多少个数字类型的列，默认1
    --number-of-queries=N
        总的测试查询次数(并发客户数×每客户查询次数)
    --query=name, -q
        使用自定义脚本执行测试，例如可以调用自定义的一个存储过程或者sql语句来执行测试。
    --create-schema
        代表自定义的测试库名称，测试的schema，MySQL中schema也就是database。
    --commint=N
        多少条DML后提交一次。
    --compress, -C
        如果服务器和客户端支持都压缩，则压缩信息传递。
    --concurrency=N, -c N
        表示并发量，也就是模拟多少个客户端同时执行select。可指定多个值，以逗号或者--delimiter参数指定的值做为分隔符。例如：--concurrency=100,200,500。
    --engine=engine_name, -e engine_name
        代表要测试的引擎，可以有多个，用分隔符隔开。例如：--engines=myisam,innodb。
    --iterations=N, -i N
        测试执行的迭代次数，代表要在不同并发环境下，各自运行测试多少次。
    --only-print
        只打印测试语句而不实际执行。
    --detach=N
        执行N条语句后断开重连。
    --debug-info, -T
        打印内存和CPU的相关信息。
```

单线程测试

```shell
$ mysqlslap -a -uroot -p123456
```

多线程测试，模拟100并发

```shell
$ mysqlslap -a -c 100 -uroot -p123456
```

迭代测试，迭代10次

```shell
$ mysqlslap -a -i 10 -uroot -p123456
```

测试同时不同的存储引擎的性能进行对比

```shell
$ mysqlslap -a --concurrency=50,100 --number-of-queries 1000 --iterations=5 --engine=myisam,innodb --debug-info -uroot -p123456
```

执行一次测试，分别50和100个并发，执行1000次总查询

```shell
$ mysqlslap -a --concurrency=50,100 --number-of-queries 1000 --debug-info -uroot -p123456
```

50和100个并发分别得到一次测试结果(Benchmark)，并发数越多，执行完所有查询的时间越长。为了准确起见，可以多迭代测试几次

```shell
$ mysqlslap -a --concurrency=50,100 --number-of-queries 1000 --iterations=5 --debug-info -uroot -p123456
```

sysbench测试

```shell
$ sysbench [general-options] ... --test=<test-name> [test-options] ... command
    General options:
        --num-threads=N               创建测试线程的数目。默认为1.
        --max-requests=N              请求的最大数目。默认为10000，0代表不限制。
        --max-time=N                  最大执行时间，单位是s。默认是0,不限制。
        --forced-shutdown=STRING      超过max-time强制中断。默认是off。
        --thread-stack-size=SIZE      每个线程的堆栈大小。默认是32K。
        --init-rng=[on|off]           在测试开始时是否初始化随机数发生器。默认是off。
        --test=STRING                 指定测试项目名称。
        --debug=[on|off]              是否显示更多的调试信息。默认是off。
        --validate=[on|off]           在可能情况下执行验证检查。默认是off。
        --help=[on|off]               帮助信息。
        --version=[on|off]            版本信息。

    Compiled-in tests:
        fileio                        File I/O test
        cpu                           CPU performance test
        memory                        Memory functions speed test
        threads                       Threads subsystem performance test
        mutex                         Mutex performance test

    oltp options:
        --oltp-test-mode=STRING       执行模式{simple,complex(advanced transactional),nontrx(non-transactional),sp}。默认是complex
        --oltp-reconnect-mode=STRING  重新连接模式{session(不使用重新连接。每个线程断开只在测试结束),transaction(在每次事务结束后重新连接),query(在每个SQL语句执行完重新连接),random(对于每个事务随机选择以上重新连接模式)}。默认是session
        --oltp-sp-name=STRING         存储过程的名称。默认为空
        --oltp-read-only=[on|off]     只读模式。Update，delete，insert语句不可执行。默认是off
        --oltp-skip-trx=[on|off]      省略begin/commit语句。默认是off
        --oltp-range-size=N           查询范围。默认是100
        --oltp-point-selects=N        number of point selects [10]
        --oltp-simple-ranges=N        number of simple ranges [1]
        --oltp-sum-ranges=N           number of sum ranges [1]
        --oltp-order-ranges=N         number of ordered ranges [1]
        --oltp-distinct-ranges=N      number of distinct ranges [1]
        --oltp-index-updates=N        number of index update [1]
        --oltp-non-index-updates=N    number of non-index updates [1]
        --oltp-nontrx-mode=STRING     查询类型对于非事务执行模式{select, update_key, update_nokey, insert, delete} [select]
        --oltp-auto-inc=[on|off]      AUTO_INCREMENT是否开启。默认是on
        --oltp-connect-delay=N        在多少微秒后连接数据库。默认是10000
        --oltp-user-delay-min=N       每个请求最短等待时间。单位是ms。默认是0
        --oltp-user-delay-max=N       每个请求最长等待时间。单位是ms。默认是0
        --oltp-table-name=STRING      测试时使用到的表名。默认是sbtest
        --oltp-table-size=N           测试表的记录数。默认是10000
        --oltp-dist-type=STRING       分布的随机数{uniform(均匀分布),Gaussian(高斯分布),special(空间分布)}。默认是special
        --oltp-dist-iter=N            产生数的迭代次数。默认是12
        --oltp-dist-pct=N             值的百分比被视为'special' (for special distribution)。默认是1
        --oltp-dist-res=N             'special'的百分比值。默认是75

    General database options:
        --db-driver=STRING            指定数据库驱动程序('help' to get list of available drivers)
        --db-ps-mode=STRING           编制报表使用模式{auto, disable} [auto]

    Compiled-in database drivers:
        mysql - MySQL driver

    mysql options:
        --mysql-host=[LIST,...]       MySQL server host [localhost]
        --mysql-port=N                MySQL server port [3306]
        --mysql-socket=STRING         MySQL socket
        --mysql-user=STRING           MySQL user [sbtest]
        --mysql-password=STRING       MySQL password []
        --mysql-db=STRING             MySQL database name [sbtest]
        --mysql-table-engine=STRING   storage engine to use for the test table {myisam,innodb,bdb,heap,ndbcluster,federated} [innodb]
        --mysql-engine-trx=STRING     whether storage engine used is transactional or not {yes,no,auto} [auto]
        --mysql-ssl=[on|off]          use SSL connections, if available in the client library [off]
        --myisam-max-rows=N           max-rows parameter for MyISAM tables [1000000]
        --mysql-create-options=STRING additional options passed to CREATE TABLE []
```

安装sysbench

```shell
$ cd ~
$ yum install zlib-devel MariaDB-devel openssl-devel
$ git clone https://github.com/akopytov/sysbench.git
$ cd sysbench
$ ./autogen.sh
$ ./configure
$ make && make install
```

准备测试

```shell
$ cd ~/sysbench/sysbench/
$ sysbench --test=tests/db/oltp.lua --db-driver=mysql --num-threads=16 --mysql-table-engine=innodb --mysql-host=localhost --mysql-db=test_db --oltp-table-size=10000000 --oltp_tables_count=10 --mysql-user=root --mysql-password=123456 prepare
```

执行测试

```shell
$ cd ~/sysbench/sysbench/
$ sysbench --test=tests/db/oltp.lua --db-driver=mysql --num-threads=16 --mysql-table-engine=innodb --mysql-host=localhost --mysql-db=test_db --oltp-table-size=10000000 --oltp_tables_count=10 --oltp-read-only=off --mysql-user=root --mysql-password=123456 --report-interval=10 --max-time=3600 run
```

清理测试

```shell
$ cd ~/sysbench/sysbench/
$ sysbench --test=tests/db/oltp.lua --db-driver=mysql --num-threads=16 --mysql-table-engine=innodb --mysql-host=localhost --mysql-db=test_db --oltp-table-size=10000000 --oltp_tables_count=10 --mysql-user=root --mysql-password=123456 cleanup
```
