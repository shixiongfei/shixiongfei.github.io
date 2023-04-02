---
title: YUM添加第三方软件仓库
author: shixiongfei
date: 2019-04-02 22:49:00
categories: 技术
tags: [技术, Linux, CentOS, YUM, RPM]
---

安装包管理优先级插件

```shell
$ yum install yum-priorities
$ vi /etc/yum.repos.d/CentOS-Base.repo
    [base] [updates] [extras]
        priority=5
    [centosplus]
        priority=6
```

EPEL软件仓库

```shell
$ yum install epel-release
$ vi /etc/yum.repos.d/epel.repo
    [epel]
        priority=11
```

RPMForge软件仓库

- RPMForge使用说明: <http://repoforge.org/use/>

```shell
$ yum install wget
$ wget http://pkgs.repoforge.org/rpmforge-release/rpmforge-release-0.5.3-1.el7.rf.x86_64.rpm
$ rpm -ivh rpmforge-release-0.5.3-1.el7.rf.x86_64.rpm
$ vi /etc/yum.repos.d/rpmforge.repo
    [rpmforge]
        priority=12
```

如果想使用第三方稳定版本仓库替换系统仓库，只需要将第三方仓库的`priority < 5`即可。
