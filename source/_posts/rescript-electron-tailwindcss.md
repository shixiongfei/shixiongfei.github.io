---
title: ReScript + Electron + Tailwind CSS 项目配置
author: shixiongfei
date: 2023-05-23 17:19:22
categories: 编程
tags: [编程, ReScript, JavaScript, NodeJS, Electron, TailwindCSS]
---

## npm 初始化项目

```shell
npm init
```

## 安装 ReScript

```shell
npm install rescript --save-dev
npx rescript -version
```

配置 `package.json`，来支持 `npm run build` 和 `npm run watch` 命令

```json
"scripts": {
  // ...
  "build": "rescript",
  "watch": "rescript build -w",
},
```

## 初始化 bsconfig

```shell
npx rescript init
```

因为目前 `Electron` 仅支持 `CommonJS` 模块，所以这里将 `bsconfig.json` 中的 `module` 由 `es6` 修改为 `commonjs`，并将 `in-source` 改为 `false` （编译后的文件在 `lib/js` 目录中）

## 安装 React

```shell
npm install react react-dom @rescript/react
```

将 `@rescript/react` 添加到 `bsconfig.json`

```json
"bs-dependencies": [
  // ...
  "@rescript/react"
],
```

将 `jsx` 支持添加到 `bsconfig.json`

```json
"jsx": { "version": 4, "mode": "automatic" },
```

## 安装 Node.js 内置类型声明

```shell
npm install rescript-nodejs
```

将 `rescript-nodejs` 添加到 `bsconfig.json`

```json
"bs-dependencies": [
  // ...
  "rescript-nodejs"
],
```

## 安装 Electron

```shell
npm install electron --save-dev
```

## 安装 Electron Forge

```shell
npm install --save-dev @electron-forge/cli @electron-forge/maker-zip
```

配置 `package.json`，来支持 `npm start`、`npm run package` 和 `npm run make` 命令

```json
"scripts": {
  // ...
  "start": "npm run build && electron-forge start --inspect-electron",
  "package": "electron-forge package",
  "make": "electron-forge make"
}
```

添加配置文件 `forge.config.js`，内容如下（有些配置内容超前了，随后会逐个配置）：

```javascript
module.exports = {
  packagerConfig: {
    name: "ElectronStartup",
    icon: "./icons/favicon",
    ignore: [
      "app",
      "lib",
      "src",
      "node_modules",
      "bsconfig.json",
      "forge.config.js",
      "postcss.config.js",
      "tailwind.config.js",
      "webpack.main.config.js",
      "webpack.preload.config.js",
      "webpack.renderer.config.js",
      "webpack.shared.plugins.js",
      "webpack.shared.rules.js",
      "README.md",
    ],
  },
  rebuildConfig: {},
  makers: [
    {
      name: "@electron-forge/maker-zip",
      platforms: ["darwin", "win32", "linux"],
    },
  ],
  plugins: [
    {
      name: "@electron-forge/plugin-webpack",
      config: {
        mainConfig: "./webpack.main.config.js",
        renderer: {
          config: "./webpack.renderer.config.js",
          entryPoints: [
            {
              name: "main_window",
              html: "./app/index.html",
              js: "./app/renderer.js",
              preload: {
                config: "./webpack.preload.config.js",
                js: "./app/preload.js",
              },
            },
          ],
        },
      },
    },
  ],
};
```

## 配置 Webpack 打包工具

```shell
npm install --save-dev @electron-forge/plugin-webpack
npm install --save-dev @vercel/webpack-asset-relocator-loader node-loader css-loader style-loader
```

添加 `webpack.shared.rules.js` 配置文件，内容如下：

```javascript
const rules = [
  {
    test: /native_modules[/\\].+\.node$/,
    use: "node-loader",
  },
  {
    test: /[/\\]node_modules[/\\].+\.(m?js|node)$/,
    parser: { amd: false },
    use: {
      loader: "@vercel/webpack-asset-relocator-loader",
      options: {
        outputAssetBase: "native_modules",
      },
    },
  },
];

module.exports = {
  rules,
};
```

添加 `webpack.shared.plugins.js` 配置文件，内容如下：

```javascript
const plugins = [];

module.exports = {
  plugins,
};
```

添加 `webpack.main.config.js` 配置文件，内容如下：

```javascript
const { rules } = require("./webpack.shared.rules");
const { plugins } = require("./webpack.shared.plugins");

module.exports = {
  entry: {
    index: "./app/main.js",
  },
  module: {
    rules,
  },
  output: {
    devtoolModuleFilenameTemplate: "[absolute-resource-path]",
    filename: "[name].js",
  },
  plugins,
  target: "electron-main",
};
```

添加 `webpack.preload.config.js` 配置文件，内容如下：

```javascript
const { rules } = require("./webpack.shared.rules");
const { plugins } = require("./webpack.shared.plugins");

module.exports = {
  entry: {
    preload: "./app/preload.js",
  },
  module: {
    rules,
  },
  output: {
    devtoolModuleFilenameTemplate: "[absolute-resource-path]",
    filename: "[name].js",
  },
  plugins,
  target: "electron-preload",
};
```

添加 `webpack.renderer.config.js` 配置文件，内容如下：

```javascript
const { rules } = require("./webpack.shared.rules");
const { plugins } = require("./webpack.shared.plugins");

module.exports = {
  module: {
    rules: [
      ...rules,
      {
        test: /\.css$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" },
          { loader: "postcss-loader" },
        ],
      },
    ],
  },
  output: {
    devtoolModuleFilenameTemplate: "[absolute-resource-path]",
  },
  plugins,
  target: "electron-renderer",
};
```

## 安装 Tailwind CSS

```shell
npm install --save-dev tailwindcss postcss-loader
npx tailwindcss init
```

配置 `tailwind.config.js`

```javascript
content: ["./app/**/*.{html,js}", "./lib/js/**/*.{html,js}"],
```

添加 `postcss.config.js` 配置来让 `Webpack` 打包支持 `Tailwind CSS`，内容如下：

```javascript
const tailwindcss = require("tailwindcss");

module.exports = {
  plugins: [tailwindcss("./tailwind.config.js")],
};
```

## 编写程序代码

1. 将 `src/Demo.res` 改名为 `src/Index.res`，文件内容如下：

```rescript
module App = {
  @react.component
  let make = () => {
    <div className="m-4"> {React.string("ReScript Electron App Starter")} </div>
  }
}

switch ReactDOM.querySelector("#root") {
| Some(container) => {
    let root = ReactDOM.Client.createRoot(container)
    ReactDOM.Client.Root.render(root, <App />)
  }
| None => ()
}
```

2. 创建 `app/index.html` 文件，内容如下：

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>Electron Startup</title>

    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta
      http-equiv="Content-Security-Policy"
      content="default-src 'self'; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline'"
    />
    <meta
      http-equiv="X-Content-Security-Policy"
      content="default-src 'self'; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline'"
    />
  </head>
  <body>
    <div id="root"></div>
  </body>
</html>
```

3. 创建 `app/styles.css` 文件，内容如下：

```css
@tailwind base;
@tailwind components;
@tailwind utilities;

:root {
  color-scheme: light dark;
}

@media (prefers-color-scheme: dark) {
  body {
    background: #333;
    color: white;
  }
}

@media (prefers-color-scheme: light) {
  body {
    background: #ddd;
    color: black;
  }
}
```

4. 创建 `app/preload.js` 文件，内容为空即可

5. 创建 `app/main.js` 文件，内容如下：

```javascript
const { app, BrowserWindow, screen } = require("electron");

const createWindow = () => {
  const { width, height } = screen.getPrimaryDisplay().workAreaSize;
  const window = new BrowserWindow({
    width,
    height,
    icon: "./icons/favicon.png",
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      preload: MAIN_WINDOW_PRELOAD_WEBPACK_ENTRY,
    },
  });

  // window.webContents.openDevTools();
  window.loadURL(MAIN_WINDOW_WEBPACK_ENTRY);
  window.maximize();
};

app.whenReady().then(() => {
  createWindow();

  app.on("activate", () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      createWindow();
    }
  });
});

app.on("window-all-closed", () => {
  app.quit();
});
```

6. 创建 `app/renderer.js` 文件，内容如下：

```javascript
require("./styles.css");
require("../lib/js/src/Index.bs");
```

7. 修改 `package.json` 中的 `main`

```json
"main": ".webpack/main",
```

最后通过 `npm start` 启动项目就可以看到窗口界面了

完整项目可以到 [GitHub Repo](https://github.com/shixiongfei/rescript-electron-tailwindcss-template) 查看

## 参考资料

* [ReScript](https://rescript-lang.org/docs/manual/latest/installation)
* [ReScript & React](https://rescript-lang.org/docs/react/latest/installation)
* [Electron](https://www.electronjs.org/zh/docs/latest/tutorial/installation)
* [Electron Forge](https://www.electronforge.io/import-existing-project)
* [Electron Forge(Webpack Plugin)](https://www.electronforge.io/config/plugins/webpack)
* [Tailwind CSS](https://tailwindcss.com/docs/installation)
