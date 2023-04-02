---
title: Docker学习笔记
author: shixiongfei
date: 2019-07-20 14:36:00
categories: 技术
tags: [技术, Docker]
---

## 获取镜像

```shell
$ docker pull [选项] [Docker Registry地址]<仓库名>:<标签>
```

- Docker Registry地址：地址的格式一般是 <域名/IP>[:端口号]。默认地址是 Docker Hub。
- 仓库名：两段式名称，既 <用户名>/<软件名>。对于 Docker Hub，如果不给出用户名，则默认为 library，也就是官方镜像。

比如：

```shell
$ docker pull ubuntu:14.04
  14.04: Pulling from library/ubuntu
  bf5d46315322: Pull complete
  9f13e0ac480c: Pull complete
  e8988b5b3097: Pull complete
  40af181810e7: Pull complete
  e6f7c7e5c03e: Pull complete
  Digest: sha256:147913621d9cdea08853f6ba9116c2e27a3ceffecf3b492983ae97c3d643fbbe
  Status: Downloaded newer image for ubuntu:14.04
```

上面的命令中没有给出 Docker Registry 地址，因此将会从 Docker Hub 获取镜像。而镜像名称是 ubuntu:14.04，因此将会获取官方镜像 library/ubuntu 仓库中标签为 14.04 的镜像。

## 列出镜像

```shell
$ docker images
  REPOSITORY           TAG                 IMAGE ID            CREATED             SIZE
  redis                latest              5f515359c7f8        5 days ago          183 MB
  nginx                latest              05a60462f8ba        5 days ago          181 MB
  mongo                3.2                 fe9198c04d62        5 days ago          342 MB
  <none>               <none>              00285df0df87        5 days ago          342 MB
  ubuntu               16.04               f753707788c5        4 weeks ago         127 MB
  ubuntu               latest              f753707788c5        4 weeks ago         127 MB
  ubuntu               14.04               1e0c3dd64ccd        4 weeks ago         188 MB
```

## 虚悬镜像

上面的镜像列表中，还可以看到一个特殊的镜像，这个镜像既没有仓库名，也没有标签，均为 <none>。

```text
<none>               <none>              00285df0df87        5 days ago          342 MB
```

这类无标签镜像也被称为 虚悬镜像(dangling image) ，可以用下面的命令专门显示这类镜像：

```shell
$ docker images -f dangling=true
  REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
  <none>              <none>              00285df0df87        5 days ago          342 MB
```

## 中间层镜像

为了加速镜像构建、重复利用资源，Docker 会利用 中间层镜像。所以在使用一段时间后，可能会看到一些依赖的中间层镜像。默认的 docker images 列表中只会显示顶层镜像，如果希望显示包括中间层镜像在内的所有镜像的话，需要加 -a 参数。

```shell
$ docker images -a
```

## 列出部分镜像

根据仓库名列出镜像

```shell
$ docker images ubuntu
  REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
  ubuntu              16.04               f753707788c5        4 weeks ago         127 MB
  ubuntu              latest              f753707788c5        4 weeks ago         127 MB
  ubuntu              14.04               1e0c3dd64ccd        4 weeks ago         188 MB
```

列出特定的某个镜像，也就是说指定仓库名和标签

```shell
$ docker images ubuntu:16.04
  REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
  ubuntu              16.04               f753707788c5        4 weeks ago         127 MB
```

过滤器参数 --filter，或者简写 -f

```shell
$ docker images -f since=mongo:3.2
  REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
  redis               latest              5f515359c7f8        5 days ago          183 MB
  nginx               latest              05a60462f8ba        5 days ago          181 MB
```

想查看某个位置之前的镜像也可以，只需要把 since 换成 before 即可

此外，如果镜像构建时，定义了 LABEL，还可以通过 LABEL 来过滤

```shell
$ docker images -f label=com.example.version=0.1
  ...
```

## 搜索镜像

```shell
$ docker search (image-name)
```

## 查看镜像历史

```shell
$ docker history (image-name)
```

## 运行镜像

```shell
$ docker run -it --rm ubuntu:14.04 bash
  root@e7009c6ce357:/# cat /etc/os-release
  NAME="Ubuntu"
  VERSION="14.04.5 LTS, Trusty Tahr"
  ID=ubuntu
  ID_LIKE=debian
  PRETTY_NAME="Ubuntu 14.04.5 LTS"
  VERSION_ID="14.04"
  HOME_URL="http://www.ubuntu.com/"
  SUPPORT_URL="http://help.ubuntu.com/"
  BUG_REPORT_URL="http://bugs.launchpad.net/ubuntu/"
  root@e7009c6ce357:/# exit
  exit
```

- `-it` ： 这是两个参数，一个是 `-i` ：交互式操作，一个是 `-t` 终端。我们这里打算进入 bash 执行一些命令并查看返回结果，因此我们需要交互式终端。
- `--rm` ：这个参数是说容器退出后随之将其删除。默认情况下，为了排障需求，退出的容器并不会立即删除，除非手动 `docker rm` 。我们这里只是随便执行个命令，看看结果，不需要排障和保留结果，因此使用 `--rm` 可以避免浪费空间。
- ubuntu:14.04：这是指用 ubuntu:14.04 镜像为基础来启动容器。
- bash：放在镜像名后的是命令，这里我们希望有个交互式 Shell，因此用的是 bash。

## 进入容器

```shell
$ docker exec -it webserver bash
  root@3729b97e8226:/# echo '<h1>Hello, Docker!</h1>' > /usr/share/nginx/html/index.html
  root@3729b97e8226:/# exit
  exit
```

## 查看容器差异

```shell
$ docker diff webserver
  C /run
  A /run/nginx.pid
  C /usr/share/nginx/html/index.html
  C /var/cache/nginx
  A /var/cache/nginx/client_temp
  A /var/cache/nginx/fastcgi_temp
  A /var/cache/nginx/proxy_temp
  A /var/cache/nginx/scgi_temp
  A /var/cache/nginx/uwsgi_temp
```

## 保存镜像的修改内容

```shell
$ docker commit [选项] <容器ID或容器名> [<仓库名>[:<标签>]]
```

例如：

```shell
$ docker commit \
    --author "Who <who@email.com>" \
    --message "ooxx-aabb" \
    webserver \
    nginx:v2
  sha256:07e33465974800ce65751acc279adc6ed2dc5ed4e0838f8b86f0c87aa1795214
```

慎用 docker commit！！！ 使用 docker commit 命令虽然可以比较直观的帮助理解镜像分层存储的概念，但是实际环境中并不会这样使用。

## 使用 Dockerfile 定制镜像

Dockerfile 是一个文本文件，其内包含了一条条的指令(Instruction)，每一条指令构建一层，因此每一条指令的内容，就是描述该层应当如何构建。

Union FS 是有最大层数限制的，比如 AUFS，曾经是最大不得超过 42 层，现在是不得超过 127 层。

## FROM 指定基础镜像

FROM 就是指定基础镜像，因此一个 Dockerfile 中 FROM 是必备的指令，并且必须是第一条指令。

```text
FROM nginx
...
```

Docker 还存在一个特殊的镜像，名为 scratch。这个镜像是虚拟的概念，并不实际存在，它表示一个空白的镜像。

```text
FROM scratch
...
```

## RUN 执行命令

RUN 指令是用来执行命令行命令的。由于命令行的强大能力，RUN 指令在定制镜像时是最常用的指令之一。其格式有两种：

- shell 格式：RUN <命令>，就像直接在命令行中输入的命令一样。刚才写的 Dockrfile 中的 RUN 指令就是这种格式。

  ```text
  RUN echo '<h1>Hello, Docker!</h1>' > /usr/share/nginx/html/index.html
  ```

- exec 格式：RUN ["可执行文件", "参数1", "参数2"]，这更像是函数调用中的格式。

  ```text
  FROM debian:jessie

  RUN buildDeps='gcc libc6-dev make' \
      && apt-get update \
      && apt-get install -y $buildDeps \
      && wget -O redis.tar.gz "http://download.redis.io/releases/redis-3.2.5.tar.gz" \
      && mkdir -p /usr/src/redis \
      && tar -xzf redis.tar.gz -C /usr/src/redis --strip-components=1 \
      && make -C /usr/src/redis \
      && make -C /usr/src/redis install \
      && rm -rf /var/lib/apt/lists/* \
      && rm redis.tar.gz \
      && rm -r /usr/src/redis \
      && apt-get purge -y --auto-remove $buildDeps
  ```

很多人初学 Docker 制作出了很臃肿的镜像的原因之一，就是忘记了每一层构建的最后一定要清理掉无关文件。

## 构建镜像

```shell
$ docker build [选项] <上下文路径/URL/->
```

在 Dockerfile 文件所在目录执行：

```shell
$ docker build -t nginx:v3 .
  Sending build context to Docker daemon 2.048 kB
  Step 1 : FROM nginx
    ---> e43d811ce2f4
  Step 2 : RUN echo '<h1>Hello, Docker!</h1>' > /usr/share/nginx/html/index.html
    ---> Running in 9cdc27646c7b
    ---> 44aa4490ce2c
  Removing intermediate container 9cdc27646c7b
  Successfully built 44aa4490ce2c
```

## 未完待续...
