---
title: 使用Charles进行HTTPS抓包
date: 2019-05-02 23:43:00
author: shixiongfei
categories: 技术
tags: [技术, MacOS, iPhone, Charles, 抓包]
---

![](./images/charles-logo.png)

## iPhone抓包

- Mac必须与iPhone连接同一WiFi
- Proxy -> SSL Proxying Settings -> SSL Proxying -> Add
  - Host:为需要过滤的域名地址，*表示不过滤
  - Port:固定为443，*表示任意端口

  ![](./images/charles-step01.png)

- 查看Mac IP地址,iPhone添加代理

  ![](./images/charles-step02.png)
  ![](./images/charles-step03.png)

- Safari访问 <http://chls.pro/ssl>，安装描述文件

  ![](./images/charles-step04.png)

- 设置 -> 通用 -> 关于本机 -> 证书信任设置,开启完全信任

  ![](./images/charles-step05.png)

- 此时可以进行抓包了

  ![](./images/charles-step06.png)

## Mac端抓包

- 启动Charles客户端
- Proxy -> MacOS Proxy
- Proxy -> SSL Proxying Settings -> SSL Proxying -> Add
  - Host:为需要过滤的域名地址，*表示不过滤
  - Port:固定为443，*表示任意端口
- Help -> SSL Proxying -> Install Charles Root Certificate 此时会打开钥匙串访问安装Charles Proxy CA证书,双击证书,展开信任选项,选择始终信任,如果证书安装不了请搜索Charles Proxy CA,删除就已失效证书再进行安装操作.

  ![](./images/charles-step07.png)

- 此时Mac端可以进行抓包了

  ![](./images/charles-step08.png)
