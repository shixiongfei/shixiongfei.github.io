---
title: 均值回归系统
author: shixiongfei
date: 2021-04-24 21:07:38
categories: 投资理财
tags: [投资理财, 股票投资, 期货投资, 量化交易]
---

## 多头建仓

每天交易量 > 100000

昨日收盘价 < 布林带下轨

昨日收盘价 > 0.9 倍的 20SMA

每一个前三天比前一日收盘都是 %B < 0.25

今天收盘 > 布林带下轨

## 空头建仓

每天交易量 > 100000

昨日收盘价 < 布林带上轨

昨日收盘价 > 1.1 倍 20SMA

每一个前三天比前一日的收盘都是 %B > 0.75

今天收盘 < 布林带上轨

## 平仓

进场后 3 个交易日，使用“收盘市价”指令，出场每一个均值回归系统中的多空组合。当且只有当

- 不管是多头还是空头头寸到达或超过 20SMA(多头超过 20SMA，空头低于这个值)
- 进场后已经10个交易日

不管哪一个先到

## 计算公式

计算原理：取日K线，以N日均线做为均值回归的短期均衡价格水平(均值)，计算股价到均值的差值，求出差值的N日的平均标准差，从而判断差值的对于均值的偏离，当偏离超过2倍标准差时，我们就认为股价超涨或超跌，股价会遵循均值回归的理论，向均值不停地进行修复。

```calc
N日平均值     =  [T日股价 + (T-1)日股价 + ... + (T-(N-1))日股价]/N
差值          =  N日平均值 - N日股价
N日差值均值   =  [T日差值 + (T-1)日差值 + ... + (T-(N-1))日差值]/N
N日差值标准差 =  sqrt([(T日差值 - T日差值均值)^2 + ... + ((T-(N-1))日差值 - (T-(N-1))日差值均值)^2 ]/N)
```

如果N为20日，则

```calc
20日平均值    =  [T日股价 + (T-1)日股价 + ... + (T-19)日股价]/20
```

计算偏离点

```calc
T日差值 > T日差值标准差 * 2
```

我们以偏离点作为买入信号点，以均线和股价的下一个交点做为卖出信号点。这样我们就把均值回归的投资理论，变成了一个数学模型。