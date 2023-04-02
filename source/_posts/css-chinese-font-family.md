---
title: CSS中Font-Family的中文字体
author: shixiongfei
date: 2019-01-02 23:53:00
categories: 编程
tags: [编程, CSS, 前端]
---

CSS中的font-family有中文字体的时候，通常有三种基本写法：

1. 直接中文
2. 英文形式
3. Unicode码

三种方法各有优缺点：

1. 中文形式的方便记忆，但在不支持中文的系统或者编码的页面则无法正常显示；
2. 英文形式的兼容了系统和编码的问题，但Firefox的某些版本和Opera不支持SimSun的写法；
3. Unicode码兼容行最好，但也存在记忆难的问题；

下面整理下常用的各种字体的不同显示形式，方便使用的时候查找：

- Windows

| 中文名      | 英文名             | Unicode                    |
|-------------|--------------------|----------------------------|
| *宋体       | SimSun             | \5B8B\4F53                 |
| *黑体       | SimHei             | \9ED1\4F53                 |
| *微软雅黑   | Microsoft YaHei    | \5FAE\8F6F\96C5\9ED1       |
| 微软正黑体  | Microsoft JhengHei | \5FAE\x8F6F\6B63\9ED1\4F53 |
| 新宋体      | NSimSun            | \65B0\5B8B\4F53            |
| 新细明体    | PMingLiU           | \65B0\7EC6\660E\4F53       |
| 细明体      | MingLiU            | \7EC6\660E\4F53            |
| 标楷体      | DFKai-SB           | \6807\6977\4F53            |
| 仿宋        | FangSong           | \4EFF\5B8B                 |
| 楷体        | KaiTi              | \6977\4F53                 |
| 仿宋_GB2312 | FangSong_GB2312    | \4EFF\5B8B_GB2312          |
| 楷体_GB2312 | KaiTi_GB2312       | \6977\4F53_GB2312          |

- Mac OS

| 中文名     | 英文名                  | Unicode                   |
|------------|-------------------------|---------------------------|
| *华文细黑  | STHeiti Light [STXihei] | \534E\6587\7EC6\9ED1      |
| *华文黑体  | STHeiti                 | \534E\6587\9ED1\4F53      |
| 华文楷体   | STKaiti                 | \534E\6587\6977\4F53      |
| 华文宋体   | STSong                  | \534E\6587\5B8B\4F53      |
| 华文仿宋   | STFangsong              | \534E\6587\4EFF\5B8B      |
| 丽黑       | Pro LiHei Pro Medium    | \4E3D\9ED1 Pro            |
| 丽宋       | Pro LiSong Pro Light    | \4E3D\5B8B Pro            |
| 标楷体     | BiauKai                 | \6807\6977\4F53           |
| 苹果丽中黑 | Apple LiGothic Medium   | \82F9\679C\4E3D\4E2D\9ED1 |
| 苹果丽细宋 | Apple LiSung Light      | \82F9\679C\4E3D\7EC6\5B8B |

上表中标*为常用字体
