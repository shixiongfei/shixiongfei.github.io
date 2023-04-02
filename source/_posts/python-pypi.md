---
title: 将Python代码打包放到PyPI上
date: 2019-04-21 23:32:00
author: shixiongfei
categories: 编程
tags: [编程, Python]
---

注册账号 <https://pypi.python.org/>

目录结构

```text
package
    |
    +-- LICENSE
    |
    +-- README.txt
    |
    +-- setup.py
    |
    +-- package
    .       |
    .       +-- __init__.py
    .       |
    .       +-- pyscript_01.py
    .       |
    .       +-- pyscript_02.py
    .       |
    .       +-- pyscript_more.py
    .       |
    .
```

`setup.py`模板

```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-

try:
    from setuptools import setup, find_packages
except ImportError:
    import ez_setup
    ez_setup.use_setuptools()
    from setuptools import setup, find_packages

import os

from package import __version__

setup(
    name = "package name",
    version = __version__,
    url = "",
    packages = find_packages(),
    include_package_data = True,
    install_requires = [''],
    keywords = "",
    author = "",
    author_email = "",
    description = "",
    license = "MIT",
    classifiers = [
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
    ]
)
```

`__init__.py`模板

```python
from package import function_01, function_02, function_more

__version__ = '0.1'
__all__ = ['function_01', 'function_02', 'function_more']
```

注册并发布到PyPI上

```shell
$ python setup.py register sdist upload
    # 会提示输入账号密码之类的巴拉巴拉..
    running check
    We need to know who you are, so please choose either:
     1. use your existing login,
     2. register as a new user,
     3. have the server generate a new password for you (and email it to you), or
     4. quit
    Your selection [default 1]:
    1
    Username: ooxx
    Password:
    Registering pys-filelock to https://pypi.python.org/pypi
    Server response (200): OK

    # 可以保存账号信息，下次再提交可以不带register参数
    I can store your PyPI login so future submissions will be faster.
    (the login will be stored in C:\Users\ooxx\.pypirc)
    Save your login (y/N)?y
    running sdist
```

最后就可以使用easy_install或者pip安装包了
