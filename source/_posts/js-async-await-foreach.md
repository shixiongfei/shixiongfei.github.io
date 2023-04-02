---
title: async/await与forEach
author: shixiongfei
date: 2019-05-06 21:33:00
categories: 编程
tags: [编程, JavsScript, NodeJS]
---

首先使用forEach在Javascript里遍历一个数组，先上代码

```javascript
const returnNum = x => {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      resolve(x);
    }, 500);
  });
};

const example = async () => {
  const nums = [1,2,3];
  nums.forEach(async num => {
   const result = await returnNum(num);
   console.log(result);
  });
  console.log('after forEach');
};

example().then(() =>{
  console.log('done');
});
```

运行结果是

```text
after forEach
done
1
2
3
```

但是我们期望的运行结果是

```text
1
2
3
after foreach
done
```

这里使用 `for...of` 来修复这个问题，代码如下：

```javascript
const returnNum = x => {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      resolve(x);
    }, 500);
  });
};

const example = async () => {
  const nums = [1,2,3];
  for (const num of nums) {
   const result = await returnNum(num);
   console.log(result);
  };
  console.log('after forEach');
};

example().then(() =>{
  console.log('done');
});
```

为什么使用 `for...of` 可以正常运行而 `forEach` 不能呢？这是因为 `forEach` 期望的是一个同步函数，它不会对返回值做任何事情，它仅仅是按顺序一个一个调用函数。 `for...of` 会对函数的返回值做实际的 `await` 等待。
