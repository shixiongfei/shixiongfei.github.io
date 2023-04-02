---
title: Gunicorn运行Flask程序
author: shixiongfei
date: 2019-04-26 00:19:00
categories: 技术
tags: [技术, Python, Flask, Gnuicorn]
---

安装Gunicorn

```shell
$ pip install gunicorn
```

编写测试程序

```shell
$ vi /var/www/example/hello.py
    import flask
    app = flask.Flask(__name__)

    @app.route("/")
    def hello():
        return "Hello World!"

    if __name__ == "__main__":
        app.run(port=8080, host='0.0.0.0')
```

测试运行Gunicorn服务

```shell
$ gunicorn -w 4 -b 127.0.0.1:8080 hello:app
```

可以使用nginx反向代理访问服务，通过supervisor作为进程管理后台启动。
