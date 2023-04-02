---
title: Debian安装vsftpd
author: shixiongfei
date: 2019-05-09 23:39:00
categories: 技术
tags: [技术, Linux, Debian, FTP]
---

## 安装vsftpd服务器

```shell
$ aptitude install vsftpd
$ /etc/init.d/vsftpd start|stop|restart
```

vsftpd的配置文件是/etc/vsftpd.conf，在/etc/ftpusers记录着不允许访问FTP服务器的用户名单

## 设置匿名ftp上传/下载

```shell
$ cd /home
$ mkdir ftphome
$ chmod 777 ftphome
$ vi /etc/vsftpd.conf
    #匿名帐户
    #经测试如果目录权限为777可能会登录失败
    #只要把目录权限改为755即可。
    anonymous_enable=YES
    anon_root=/home/ftphome
    no_anon_password=YES
    #开启本地帐户
    local_enable=YES
    write_enable=YES
    local_umask=022
    local_root=/home/ftphome
    chroot_local_user=YES
    ftpd_banner=Welcome to ftp server
```

如果简单配置本地账号登录的话

```shell
$ vi /etc/vsftpd.conf
    anonymous_enable=NO
    local_enable=YES
    #local_root=/var/www 尽量使用用户主目录
    write_enable=YES
    chroot_local_user=YES
```

关于vsftpd配置参数可以参考man vsftpd.conf。

注意在添加ftp本地帐户的时候，不是所有帐户都需要登录shell的。可以用下面的命令来创建帐户并禁止登录shell

```shell
$ adduser ftpuser --home /home/ftpuser --ingroup ftpgroup --shell /bin/false
$ passwd ftpuser
$ chown -R ftpuser /home/ftpuser
$ chmod a-w /home/ftpuser
```

重启vsftpd

## vsftpd常见错误处理:

500 OOPS: vsftpd: refusing to run with writable root inside chroot ()

```shell
$ chmod a-w /home/ftpuser
```

530 Login incorrect.

```shell
$ vi /etc/shells
    /sbin/nologin
    /bin/false
```

553 Could not create file

- 这个和前面的500号错误有一点冲突，因为目录不可写，所以创建不了文件，但是如果设为可写，又不能登录。这个问题，在vsftpd 2.x版本中无法解决，但是在vsftpd 3.x中，可以增加一个配置项解决

  ```shell
  $ vi /etc/vsftpd.conf
      allow_writeable_chroot=YES
  $ chmod 777 /home/ftpuser
  ```

Debian7中默认安装的vsftpd的版本是2.x，通过下面的方法升级为3.x

```shell
$ vi /etc/apt/sources.list
    deb http://ftp.us.debian.org/debian sid main
$ aptitude update
$ aptitude upgrade vsftpd
```

重启vsftpd
