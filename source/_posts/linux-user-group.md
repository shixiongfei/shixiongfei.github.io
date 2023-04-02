---
title: Linux用户与组管理
author: shixiongfei
date: 2019-02-10 16:00:00
categories: 技术
tags: [技术, Linux]
---

查看所有用户

```shell
$ cat /etc/passwd
```

添加用户

```shell
$ adduser <new username>
    -s <shell type>
    -d <home directory>
    -g <group name>
```

创建一个禁止登录shell的用户:

```shell
$ adduser -s /sbin/nologin test_user
```

- 注：Shell 认证在 /etc/shells 文件中

删除用户

```shell
$ userdel <new username>
    -r To remove the user's home directory
```

修改用户

```shell
$ usermod <username>
```

修改密码

```shell
$ passwd <new username>
```

添加用户组

```shell
$ addgroup <group name>
```

删除用户组

```shell
$ groupdel <group name>
```

修改用户组名

```shell
$ groupmod <old name> <new name>
```

显示组内用户列表

```shell
$ groups
```

显示用户所在组

```shell
$ groups <username>
```
