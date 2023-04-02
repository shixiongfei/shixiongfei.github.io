---
title: 操作系统封装-定制自己的ISO
author: shixiongfei
date: 2019-02-26 10:45:00
categories: 技术
tags: [技术, Windows]
---

- 准备工作
  - VMWare(虚拟机，不建议VirtualBox)
  - Windows.iso(纯净系统母盘，微软官方下载)
  - Easy Sysprep(系统封装)
  - EasyU(优启通)
  - Dism++(系统优化)

- 安装基础系统
  - VMWare创建虚拟机
  - 安装基础系统，在快速设置的地方按Ctrl+Shift+F3进入审核模式
  - 对系统进行设置，在C盘建立一个sysprep的目录，把软件安装包都放进去

- 封装系统
  - 使用Easy Sysprep进行第一阶段封装
  - 为第二阶段做准备，将BIOS启动设置为PE的iso
  - 进入PE系统后，使用Easy Sysprep进行第二阶段封装。封装完之后不可再进系统了

- 制作安装镜像
  - 备份C盘到install.esd
  - 用UltraISO打开原厂ISO，替换install.esd
  - 最后另存出你专属的iso
