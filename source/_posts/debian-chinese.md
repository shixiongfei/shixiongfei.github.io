---
title: Debian安装中文环境
author: shixiongfei
date: 2019-03-02 01:08:00
categories: 技术
tags: [技术, Linux, Debian]
---

在locales列表中选择`zh_CN.*`，如果需要繁体就把`zh_HK.*`和`zh_TW.*`也选上。

(如果没有locales，就用aptitude install locales安装一下)

```shell
$ dpkg-reconfigure locales
```

可以使用locale -a查看当前系统已经安装的语言环境。

如果要使用自己的字体，把字体复制到/usr/local/share/fonts目录，然后运行sudo fc-cache

```shell
$ aptitude install ttf-dejavu ttf-wqy-zenhei xfonts-wqy ttf-wqy-microhei ttf-arphic-ukai ttf-arphic-uming xfonts-intl-chinese ttf-bitstream-vera
```

最后安装输入法

```shell
$ aptitude install scim scim-pinyin
```

安装完毕后重启系统。
