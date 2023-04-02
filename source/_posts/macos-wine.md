---
title: MacOS上通过Wine运行Windows程序
author: shixiongfei
date: 2019-02-22 13:20:00
categories: 技术
tags: [技术, MacOS]
---

安装wine和winetricks

```shell
$ brew install wine
$ brew install winetricks
```

初始化wine

```shell
$ winecfg
```

- 可能会需要下载一些系统环境。默认Windows版本是XP，选择Win10后确定

打开wine字体平滑(默认是关闭的)

```shell
$ wine regedit
  [HKEY_CURRENT_USER\Control Panel\Desktop]
  "FontSmoothingType"=dword:00000002
```

安装一些wine必要的运行库

```shell
$ winetricks gdiplus fontfix
```

安装中文字体支持(基本可以满足显示需求)

```shell
$ winetricks allfonts fakechinese
```
