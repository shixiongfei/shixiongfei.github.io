---
title: MacOS安装Dnsmasq来加速DNS解析
author: shixiongfei
date: 2018-11-24 15:05:00
categories: 技术
tags: [技术, MacOS, DNS]
---

众所周知的原因，在兲朝没有干净的DNS解析，使用国内运营商的DNS解析服务会出现被劫持的情况，使用国外公共DNS解析服务会被GFW污染。所以在mbp上使用Dnsmasq搭建一个DNS解析服务，同时把Buffalo路由刷上dd-wrt系统，dd-wrt系统自带Dnsmasq服务，可以使家里所有电脑的解析都一致。因为我的mbp经常外带，所以要本地也建一个Dnsmasq。

Dnsmasq是一个简单的DNS缓存和DHCP服务程序，为什么使用它而不使用hosts？因为每台电脑上都维护一个hosts是个很痛苦的事情，而且Dnsmasq与hosts相比最大的好处是支持泛解析。只要路由使用Dnsmasq服务，其他电脑不用设置就都可以使用了。

虽然使用Dnsmasq不能解决DNS污染的问题，但是简化了hosts的维护，而且关键它支持DNS缓存，解析过的地址会优先使用缓存，不需要再解析了，所以很大程度上，可以加速网络访问。

下面是MacOS上安装Dnsmasq和相关配置的内容

```shell
$ brew install dnsmasq
$ cp /usr/local/opt/dnsmasq/dnsmasq.conf.example /usr/local/etc/dnsmasq.conf
$ vi /usr/local/etc/resolv.dnsmasq.conf
    # Google DNS
    nameserver 8.8.8.8
    nameserver 8.8.4.4
    # OpenDNS
    nameserver 208.67.222.222
    nameserver 208.67.220.220
$ vi /usr/local/etc/dnsmasq.conf
    resolv-file=/usr/local/etc/resolv.dnsmasq.conf
    strict-order
    listen-address=127.0.0.1 # 多个IP用逗号隔开
    no-hosts
    cache-size=32768

    address=/bitbucket.org/131.103.20.167
    address=/google.com/173.194.38.128
    address=/ggpht.com/173.194.38.128
    address=/googleapis.com/173.194.38.128
    address=/googleusercontent.com/173.194.38.128
    address=/gstatic.com/173.194.38.128
    address=/youtube.com/173.194.38.128
    address=/gmail.com/173.194.38.128
    address=/googlesource.com/173.194.38.128
    address=/googlelabs.com/173.194.38.128
    address=/googlecode.com/173.194.38.128
    address=/goo.gl/173.194.38.128
    address=/google.cn/203.208.46.210
    address=/google.com.hk/203.208.46.210
    address=/flickr.com/66.196.66.157
    address=/up.flickr.com/119.161.24.27
    address=/api.flickr.com/208.71.44.31
    address=/farm1.staticflickr.com/72.30.196.151
    address=/farm2.staticflickr.com/72.30.196.152
    address=/farm3.staticflickr.com/98.137.205.236
    address=/farm4.staticflickr.com/98.137.204.89
    address=/farm5.staticflickr.com/68.142.227.95
    address=/farm6.staticflickr.com/98.139.21.45
    address=/farm7.staticflickr.com/98.139.235.135
    address=/farm8.staticflickr.com/72.30.198.116
    address=/farm9.staticflickr.com/72.30.198.117
    address=/github.global.ssl.fastly.net/185.31.17.184
$ sudo cp -fv /usr/local/opt/dnsmasq/*.plist /Library/LaunchDaemons
$ sudo launchctl load /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
```

设置完上面，把系统DNS设置为127.0.0.1就行了，使用dig看看结果

```shell
$ dig google.com
```

如果修改了配置文件，可以通过下面的命令重启Dnsmasq

```shell
$ sudo launchctl stop homebrew.mxcl.dnsmasq
$ sudo launchctl start homebrew.mxcl.dnsmasq
```

MacOS清除系统DNS缓存命令

```shell
$ sudo killall -HUP mDNSResponder
```
