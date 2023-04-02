---
title: manylinux
author: shixiongfei
date: 2019-05-10 22:45:00
categories: 技术
tags: [技术, Linux, Docker, Python]
---

安装docker镜像

```shell
$ docker pull quay.io/pypa/manylinux1_x86_64
$ docker pull quay.io/pypa/manylinux1_i686
```

运行docker

```shell
$ docker run -i -t -v `pwd`:/io quay.io/pypa/manylinux1_x86_64 /bin/bash
```

查看支持的python

```shell
$ ls /opt/python/
```

发布Python包目录

```shell
$ cd /io/codes/pypkg
$ /opt/python/cp36-cp36m/bin/python setup.py bdist_wheel
$ cd dist
$ auditwheel show pypkg-xxx.whl
$ auditwheel repair pypkg-xxx.whl
```

使用twine上传pypi

```shell
$ cd dist/wheelhouse
$ twine upload pypkg-xxx.whl
```
