---
title: ELO竞技分计算
author: shixiongfei
date: 2018-12-30 17:19:00
categories: 编程
tags: [编程, C]
---

## 基本公式

```calc
Rn = Ro + K * (G - Ge)
```

- Rn: 玩家比赛结束后的竞技分
- Ro: 玩家比赛前的竞技分
- K: 加成系数
- G: 胜负对局分
- Ge: 玩家预期得分

## K取值范围

K值是一个常量系数，按照国际象棋里的标准， K值对于大师选手为16，对于一般选手是32

例如实际游戏中可以这么运用：

- 初学者   < 1000.0   K = 32.0 + (胜/负 * 32.0)
- 有经验者 < 1500.0   K = 32.0 + (胜/负 * 16.0)
- 熟练者   < 2000.0   K = 32.0
- 专家     < 2200.0   K = 20.0
- 大师     < 2400.0   K = 15.0
- 宗师     > 2400.0   K = 10.0

胜负取值

- 胜 = 1
- 负 = 0

## G取值范围

- 胜利 G = 1.0
- 平局 G = 0.5
- 失败 G = 0.0

## Ge计算公式

```calc
D = Rb - Ra
Ge = 1 / (1 + pow(10, D / 400.0))
```

分母400是一个平衡的、万金油的值，让多数玩家积分保持标准正态分布的值

## 1 vs 1到N vs N扩展计算

从1 vs 1计算到N vs N计算，需要变化的是Ge值的计算。

```calc
D = (Ra / A队总分 * B队总分) - Ra
```

## ELO 1v1简单算法实现

```c
#ifndef _ELORATING_H_
#define _ELORATING_H_

#include <math.h>

static const unsigned int deno = 400;

static const double novice  = 1000.0;  //0-999 初学者
static const double someExp = 1500.0;  //1000-1499 有经验者
static const double skill   = 2000.0;  //1500-1999 熟练者
static const double expert  = 2200.0;  //2000-2199 专家
static const double master  = 2400.0;  //2200-2399 大师 >2400 宗师

/**
 * @brief         自适应k值
 *
 * @param score   积分
 * @param isWin   胜负因子
 *
 * @returns       k值结果
 */
static inline unsigned int adaptationK(double score, bool isWin) {
    return score < novice  ? (32.0 + (isWin ? 32.0 : 0.0)) :
           score < someExp ? (32.0 + (isWin ? 16.0 : 0.0)) :
           score < skill   ?  32.0 :
           score < expert  ?  20.0 :
           score < master  ?  15.0 : 10.0;
}

/**
 * @brief    计算a相对b的胜率
 *
 * @param a  a的积分
 * @param b  b的积分
 *
 * @returns  胜率
 */
static inline double getWinRate(double a, double b) {
    return 1.0 / (1.0 + pow(10, (b - a) / deno));
}

/**
 * @brief        计算积分变化
 *
 * @param a      a的积分
 * @param b      b的积分
 * @param isWin  a对b的胜负关系 true-a胜 false-a败
 * @param isDraw a对b是否平局   true-平（不查看isWin字段） false-有胜负（查看isWin字段）
 *
 * @returns      a的积分变化
 */
static inline double getScoreChg(double a, double b, bool isWin, bool isDraw) {
    double g       = isDraw ? 0.5 : (isWin ? 1.0 : 0.0);
    double winRate = getWinRate(a, b);
    return (g - winRate) * adaptationK(a, isWin);
}

/**
 * @brief         计算积分结果
 *
 * @param a       a的积分
 * @param b       b的积分
 * @param isWin   a对b的胜负关系 true-a胜 false-a败
 * @param isDraw  a对b是否平局   true-平（不查看isWin字段） false-有胜负（查看isWin字段）
 *
 * @returns       a的积分终值
 */
static inline double calcResult(double a, double b, bool isWin, bool isDraw) {
    return (a + getScoreChg(a, b, isWin, isDraw));
}

#endif
```
