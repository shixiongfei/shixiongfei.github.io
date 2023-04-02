---
title: TypeScript项目配置
author: shixiongfei
date: 2022-10-06 16:42:00
categories: 编程
tags: [编程, TypeScript, JavaScript, NodeJS]
---

## npm 初始化项目

```shell
npm init
```

## 安装 TypeScript

```shell
npm install typescript --save-dev
npx tsc --version
```

## 安装 Node.js 内置类型声明

```shell
npm install @types/node --save-dev
```

## 初始化 tsconfig 

(可以删除所有注释的内容，让 tsconfig 看起来干净些)

```shell
npx tsc --init --rootDir src --outDir build \
--esModuleInterop --resolveJsonModule --lib es6 \
--module commonjs --allowJs true --noImplicitAny true
```

## 创建 src/main.ts

```shell
mkdir src
echo 'console.log("Hello world!")' > src/main.ts
```

可以通过 `npx tsc` 来编译项目工程 (编译后在 build 目录下可以看到 js 文件)

```shell
npx tsc
node ./build/main.js
```

修改 package.json，来支持 `npm run build` 和 `npm start` 命令

```json
"scripts": {
  "build": "tsc",
  "start": "tsc && node ./build/main.js"
},
```

## 安装 ESLint

```shell
npm install eslint --save-dev
npm install @typescript-eslint/parser --save-dev
npm install @typescript-eslint/eslint-plugin --save-dev
```

创建 .eslintrc.json 配置，内容如下:

```json
{
  "root": true,
  "parser": "@typescript-eslint/parser",
  "plugins": ["@typescript-eslint"],
  "extends": ["eslint:recommended", "plugin:@typescript-eslint/recommended"]
}
```

## 安装 Prettier

```shell
npm install prettier --save-dev
```

创建 .prettierrc.json 配置 (使用默认配置即可)，文件内容如下

```json
{}
```
