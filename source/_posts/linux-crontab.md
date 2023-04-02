---
title: Linux下Crontab定时任务
author: shixiongfei
date: 2019-02-12 16:06:00
categories: 技术
tags: [技术, Linux, Debian]
---

Debian上crontab的默认编辑器是nano，用下面这个命令配置成vim

```shell
$ update-alternatives --config editor
```

接下去看下常用命令

```shell
$ crontab -e # 执行文字编辑器来设定时程表
$ crontab -r # 删除目前的时程表
$ crontab -l # 列出目前的时程表
```

看下格式和样例

```shell
crontab task 格式
 * * * * * task
分 时 日 月 周 task
3 * * * * echo "每小时的03分钟执行"
0 6 * * * echo "每天早上06点钟执行"
0 */2 * * * echo "每两个小时执行"
0 23-7/2,8 * * * echo "晚上11点到早上8点之间每两个小时和早上八点执行"
0 11 4 * 1-3 echo "每个月的4号和每个礼拜的礼拜一到礼拜三的早上11点执行"
0 4 1 1 * echo "1月1日早上4点执行"
*/5 * * * * echo "每5分钟执行"
```

**注意事项**

- crontab执行时不会读取用户的环境变量，所以有时在crontab中添加了脚本任务后经常会发现执行失败，比如说执行ifconfig找不到这个命令之类的。

- 大多数情况下是可能由于环境变量设置问题。因此在编写脚本时要声明环境变量，防止发生此类错误。

- 使用这个命令查看当前PATH环境变量

```shell
$ echo PATH=$PATH
```

- 编写shell脚本
  - 复制环境变量，在脚本开头第一行#!/bin/bash的后面添加两行内容即可：

```bash
#!/bin/bash
PATH=OOXXOOXX  # 换成你复制的环境变量
export PATH

# script do somthing ...
```

DD-WRT设置

- 但是我在DD-WRT的路由上跑Python脚本，死活都无法正常工作。最终发现，在DD-WRT上所有运行的cron命令前都要加root。那接下去的工作就简单多了，指定为root并执行root目录中.profile环境变量。
- 其实就是把cron的写法小改一下，改成这种方式(估计openwrt下也会类似)

```text
 * * * * * root . $HOME/.profile;__your_command_here__
```

最后附上我路由中的设置命令

```shell
*/5 * * * * root . $HOME/.profile;/opt/usr/bin/python /mnt/sda_part1/var/dnspod_inter_ddns.py > /mnt/sda_part1/var/log/ddns.log
```
