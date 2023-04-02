---
title: Windows安装WSL+OCaml开发环境
author: shixiongfei
date: 2020-08-03 16:49:00
categories: 编程
tags: [编程, OCaml, WSL, Windows]
---

在 Windows 上尝试多种 OCaml 开发环境方案后，最后还是选择WSL方案。

这里做个简单记录，以备日后查用。

## 启用 WSL

进入“控制面板” - “程序和功能” - “启用或关闭Windows功能” - “适用于 Linux 的 Windows 子系统” - “确定”

## 下载 Fedora 系统镜像

其实最初我打算安装的是 CentOS 系统，但是装完后发现系统有不少问题。整个系统极容易卡死失去响应，基本属于完全无法正常使用的状况。

然后抱着死马当活马医的心态，尝试了下 Fedora，倒是出乎意料的顺利，有点意外。

[下载 Fedora 镜像](https://github.com/fedora-cloud/docker-brew-fedora/raw/33/x86_64/fedora-33-x86_64-20200709.tar.xz)

## 安装 Choco

根据[官网提示](https://chocolatey.org/install)，在 PowerShell (管理员权限运行)中运行如下命令：

```powershell
$ Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
```

## 安装 LxRunOffline

继续在 PowerShell 中执行下列命令

```powershell
$ choco install lxrunoffline
```

## 使用 LxRunOffline 安装 Fedora

刚装完 LxRunOffline 可能需要重启下 PowerShell，然后继续在 PowerShell 中执行下列命令：

```powershell
$ LxRunOffline install -n fedora -d 目标路径 -f fedora-33-x86_64-20200709.tar.xz
```

- -n 是安装的系统名称，可自定义；
- -d 是安装系统的目录；
- -f 是之前下载的镜像路径；

## 启动 Fedora

可以用以下方式启动：

1. `LxRunOffline run -n fedora`
2. `wsl -d fedora`

## 创建用户并修改默认登陆用户

进入 WSL Fedora 系统进行修改密码及添加用户操作

```shell
$ dnf install passwd
$ passwd        # 修改root密码
$ adduser shixf
$ passwd shixf  # 修改添加的用户密码
$ vi /etc/sudoers
  root ALL=(ALL) ALL
  shixf ALL=(ALL) ALL    # 添加这行新建的用户就拥有root权限了
$ cat /etc/passwd        # 看下新建的用户ID并记下来，后面会用到
```

在 PowerShell 中修改 WSL 默认用户

```powershell
$ LxRunOffline su -n fedora -v 1000
```

重启 WSL 就变成新用户了

## Fedora 安装 opam 依赖工具

```shell
$ dnf install diffutils
$ dnf install git
$ dnf install rsync
```

## 安装 opam 及初始化

```shell
$ dnf install opam
$ opam init --disable-sandboxing  # WSL下不disable sanboxing无法正常初始化opam
$ opam install merlin
$ opam install utop
$ opam install batteries
```

## 安装 Emacs

```shell
$ dnf install dbus-x11  # WSL会有dbus的问题，目前没有找到解决方案
$ dnf install emacs
```

## 安装 X Server

为了正常使用 Emacs，Windows 上需要安装一个 X Server，多次尝试之后，发现还是 Cygwin X 比较好。

到 [Cygwin 官网](https://cygwin.org/)下载[安装程序](https://cygwin.com/setup-x86_64.exe)，安装的时候需要勾选下列几个 [Cygwin/X](https://x.cygwin.com/docs/ug/setup.html) 相关的包：

- xorg-server (必选)
- xinit (必选)
- xorg-docs (可选)
- xlaunch (可选)

安装完之后可以通过下面这个命令来启动 X Server (为了方便可以建一个快捷方式)

```shell
$ C:\cygwin64\bin\XWin.exe :0 -multiwindow -listen tcp -noprimary
```

最后一步需要配置一下 WSL，告诉 WSL X Server已经在本地启动。配置方法只需将环境变量 `DISPLAY` 置为 `127.0.0.1:0` 即可。这里我将配置添加到 `./bashrc` 中，让 `bash` 每次自动设置环境变量。

```shell
$ echo "export DISPLAY='127.0.0.1:0'" >> ~/.bashrc
$ source ~/.bashrc
```

## 启动 Emacs

先启动 X Server，然后在 WSL 中输入

```shell
$ emacs
```

至此，如果一切配置正确的话，就可以看到独立窗口的 Emacs 进程。Emacs 的相关配置就请自行搞定了！

Good Luck!
