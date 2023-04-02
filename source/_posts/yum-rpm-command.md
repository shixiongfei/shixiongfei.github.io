---
title: YUM/RPM常用命令
author: shixiongfei
date: 2018-12-12 00:03:00
categories: 技术
tags: [技术, Linux, CentOS, YUM, RPM]
---

## YUM常用命令

```text
yum check-update 检查可更新的所有软件包
yum update 下载更新系统已安装的所有软件包
yum upgrade 大规模的版本升级,与yum update不同的是,连旧的淘汰的包也升级
yum install <packages> 安装新软件包
yum update <packages> 更新指定的软件包
yum remove <packages> 卸载指定的软件包
yum autoremove <packages> 卸载指定的软件包及其依赖
yum groupinstall <groupnames> 安装指定软件组中的软件包
yum groupupdate <groupnames> 更新指定软件组中的软件包
yum groupremove <groupnames> 卸载指定软件组中的软件包
yum grouplist 查看系统中已经安装的和可用的软件组
yum list 列出资源库中所有可以安装或更新以及已经安装的rpm包
yum list <regex> 列出资源库中与正则表达式匹配的可以安装或更新以及已经安装的rpm包
yum list available 列出资源库中所有可以安装的rpm包
yum list available <regex> 列出资源库中与正则表达式匹配的所有可以安装的rpm包
yum list updates 列出资源库中所有可以更新的rpm包
yum list updates <regex> 列出资源库中与正则表达式匹配的所有可以更新的rpm包
yum list installed 列出资源库中所有已经安装的rpm包
yum list installed <regex> 列出资源库中与正则表达式匹配的所有已经安装的rpm包
yum list extras 列出已经安装的但是不包含在资源库中的rpm包
yum list extras <regex> 列出与正则表达式匹配的已经安装的但是不包含在资源库中的rpm包
yum list recent 列出最近被添加到资源库中的软件包
yum search <regex> 检测所有可用的软件的名称、描述、概述和已列出的维护者，查找与正则表达式匹配的值
yum provides <regex> 检测软件包中包含的文件以及软件提供的功能，查找与正则表达式匹配的值
yum clean headers 清除缓存中的rpm头文件
yum clean packages 清除缓存中rpm包文件
yum clean all 清除缓存中的rpm头文件和包文件
yum deplist <packages> 显示软件包的依赖信息
yum info 列出包信息
```

## RPM常用命令组合

```text
rpm -ivh: 安装显示安装进度--install --verbose --hash
rpm -Uvh: 升级软件包--Update
rpm -qpl: 列出RPM软件包内的文件信息[Query Package list]
rpm -qpi: 列出RPM软件包的描述信息[Query Package install package(s)]
rpm -qf: 查找指定文件属于哪个RPM软件包[Query File]
rpm -Va: 校验所有的RPM软件包，查找丢失的文件[View Lost]
rpm -e: 删除包
```

## RPM常用参数

```text
rpm -i, --install                  install package(s)
rpm -v, --verbose                  provide more detailed output
rpm -h, --hash                     print hash marks as package installs (good with -v)
rpm -e, --erase                    erase (uninstall) package
rpm -U, --upgrade=<packagefile>    upgrade package(s)
rpm －-replacepkge                 无论软件包是否已被安装，都强行安装软件包
rpm --test                         安装测试，并不实际安装
rpm --nodeps                       忽略软件包的依赖关系强行安装
rpm --force                        忽略软件包及文件的冲突

Query options (with -q or --query):
rpm -a, --all                      query/verify all packages
rpm -p, --package                  query/verify a package file
rpm -l, --list                     list files in package
rpm -d, --docfiles                 list all documentation files
rpm -f, --file                     query/verify package(s) owning file
```

## RPM源码包编译安装

- `.src.rpm`结尾的文件，这些文件是由软件的源代码包装而成的，用户要安装这类RPM软件包，必须使用命令：

```text
rpm --recompile xxx.src.rpm   这个命令会把源代码解包并编译、安装它，如果用户使用命令
rpm --rebuild xxx.src.rpm     在安装完成后，还会把编译生成的可执行文件重新包装成i386.rpm的RPM软件包
```
