---
title: Lisp包管理工具ASDF
author: shixiongfei
date: 2019-02-16 20:49:00
categories: 编程
tags: [编程, Lisp]
---

ASDF 全称是 Another System Definition Facility，是 Common Lisp 重要的包管理工具。

ASDF 可以构建、编译、加载 Lisp 程序，Quicklisp 也依赖 ASDF。但是关于 ASDF 的中文资料非常少，这里做一个简单记录。

## 配置工作目录

我的所有 Lisp 项目都放在 `~/Codes/lisp/` 下，所以我希望 ASDF 能够读到我工作目录下的所有项目。

这里需要增加一个配置文件 `~/.config/common-lisp/source-registry.conf.d/projects.conf` 内容如下

```lisp
(:tree (:home "Codes/lisp/"))
```

(Windows上配置文件路径为：`C:\Users\your-username\AppData\Local\config\common-lisp\source-registry.conf.d\`)

在工作目录下创建一个项目，并在 REPL 中执行 `(asdf:initialize-source-registry)` 之后，我们的项目就可以被 `asdf:load-system` 加载了

### 另外还有一个更简单的方法，如下所示：

```lisp
;; startup file like ~/.sbclrc
(pushnew "~/path-to-projects/root/" asdf:*central-registry* :test #'equal)
```

## asdf:load-system 加载包

在 SBCL 中， `require` 函数默认已经被 ASDF 的加载实现替换了，所以可以直接用：

```lisp
(require :package)
```

其他 CL 实现可能需要显示用 ASDF 的加载函数：

```lisp
(asdf:load-system :package)
```

## ASDF 结构

例如我们要构建一个项目叫 cl-hello，我们需要在项目中创建一个 .asd 文件。通常这个文件会和项目同名，所以这里是 cl-hello.asd。

一个 .asd 文件的模版如下：

```lisp
(asdf:defsystem #:cl-hello
  :name "cl-hello"
  :description "This is my lisp package."
  :version "0.0.1"
  :author "yourname"
  :license "your license"
  :serial t
  :depends-on (#:depmod-1
               #:depmod-2
                ...
               #:depmod-n)
  :components
  ((:module "3rdpackage")
   (:module "src"
    :depends-on ("3rdpackage")
    :components ((:file "package")
                 (:file "myfile-1")
                 (:file "myfile-2")
                    ...
                 (:file "myfile-n")))))
```

通常项目还会创建一个 package.lisp 文件，这个文件的主要用途是定义导入和导出。

```lisp
(defpackage #:cl-hello
  (:use #:mod-1
        #:mod-2
        ...
        #:mod-n)
  (:export #:myfun-1
           #:myfun-2
            ...
           #:myfun-n))
```

## QuickProject

我们其实可以用 QuickProject 这个库来自动创建项目，它会自动帮助我们创建 .asd 文件。

首先用 QuickLoad 加载 QuickProject，如果系统里没有安装 QuickProject 会自动下载并加载：

```lisp
(ql:quickload :quickproject)
```

然后新建一个 helloworld 项目：

```lisp
(quickproject:make-project #p"~/Codes/lisp/helloworld" :author "shixiongfei")
```

QuickProject 会自动在 ~/Codes/lisp 目录下创建一个叫 helloworld 的文件夹，之前我们手动定义的内容它全部帮我们完成了：

```text
  ├── helloworld.asd
  ├── helloworld.lisp
  ├── package.lisp
  └── README.txt
```

## 编译独立可执行文件

可以通过 ASDF 与 make 命令配合来创建出独立可执行程序(self-contained executables)

首先需要在 .asd 文件中添加以下配置：

```lisp
:build-operation "program-op"  ;; leave as is
:build-pathname "<my-executable-name>"
:entry-point "<my-package:my-start-function>"
```

然后调用 `asdf:make :my-package` 就可以编译出程序了

再编写一个 Makefile 通过 make 来执行编译工作

```makefile
LISP ?= sbcl

build:
	$(LISP) --load my-app.asd \
		--eval '(ql:quickload :my-app)' \
		--eval '(asdf:make :my-app)' \
		--eval '(quit)'
```

## 参考资料

[asdf手册](https://common-lisp.net/project/asdf/asdf.html)
