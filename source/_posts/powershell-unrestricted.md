---
title: PowerShell在此系统中禁止执行脚本解决方法
author: shixiongfei
date: 2018-11-25 23:59:00
categories: 技术
tags: [技术, Windows, PowerShell]
---

在PowerShell中直接脚本时会出现：

- 无法加载文件 **profile.ps1，因为在此系统上禁止运行脚本。有关详细信息，请参阅<http://go.microsoft.com/fwlink/?LinkID=135170> 中的 about_Execution_Policies。

运行 get-help about_signing 提示了解执行策略输入

```powershell
get-executionpolicy
```

显示Restricted即不允许执行任何脚本。

通过命令

```powershell
get-help set-executionpolicy 可知有以下执行策略：<Unrestricted> | <RemoteSigned> | <AllSigned> | <Restricted> | <Default> | <Bypass> | <Undefined>
```

然后修改其策略：

```powershell
set-executionpolicy remotesigned
执行策略更改
执行策略可帮助你防止执行不信任的脚本。更改执行策略可能会产生安全风险，如
http://go.microsoft.com/fwlink/?LinkID=135170 中的 about_Execution_Policies 帮助主题所述。是否要更改执行策略?
[Y] 是(Y)  [A] 全是(A)  [N] 否(N)  [L] 全否(L)  [S] 暂停(S)  [?] 帮助 (默认值为“N”): y
```

即可执行脚本
