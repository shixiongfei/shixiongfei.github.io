---
title: BOLL指标通达信公式
author: shixiongfei
date: 2018-12-24 16:59:00
categories: 投资理财
tags: [投资理财, 技术指标, BOLL]
---

## 标准BOLL

- 参数：
  - N取值2~120，默认20
  - M取值1~100，默认2

```tdx
BOLL:MA(CLOSE,N);
UPPER:BOLL+M*STD(CLOSE,N);
LOWER:BOLL-M*STD(CLOSE,N);
```

## 多空布林线BBIBOLL

- 参数：
  - N取值2~100，默认11
  - M取值2~100，默认6

```tdx
CV:=CLOSE;
BBIBOLL:(MA(CV,3)+MA(CV,6)+MA(CV,12)+MA(CV,24))/4;
UPR:BBIBOLL+M*STD(BBIBOLL,N);
DWN:BBIBOLL-M*STD(BBIBOLL,N);
```

## 通达信BOLL-M

- 参数：
  - N取值范围2~120，默认20

```tdx
MID:=SMA(C,N,1);
VART1:=POW((C-MID),2);
VART2:=MA(VART1,N);
VART3:=SQRT(VART2);
BOLL:MID;
UB:REF(MID+2*VART3,1);
LB:REF(MID-2*VART3,1);
```

## BOLL指标与均线组合

- 参数：
  - M1取值范围2~250，默认5
  - M2取值范围2~250，默认60
  - P取值范围1~100，默认2
  - N取值范围2~250，默认26

```tdx
CV:=CLOSE;
买卖点:MA(CV,M1), COLORWHITE;
趋势:MA(CV,M2), COLORGREEN, LINETHICK2;
BOLL:MA(CV,N), COLORMAGENTA;
上轨:BOLL+P*STD(CV,N), COLORYELLOW;
下轨:BOLL-P*STD(CV,N), COLORYELLOW;
DRAWTEXT(CROSS(买卖点,BOLL), 买卖点, '买'), COLORRED;
DRAWTEXT(CROSS(BOLL,买卖点), 买卖点, '卖'), COLORGREEN;
```
