---
title: C语言变参的一个陷阱
date: 2018-11-30 00:17:00
author: shixiongfei
categories: 编程
tags: [编程, C]
---

在C语言编程中，经常使用变参处理一些字符串工作。先来看段代码。

```c
char *string_vformat(const char *format, va_list argv) {
    char *str = 0;
    size_t str_len = vsnprintf(0, 0, format, argv);

    str = (char *)malloc(str_len + 1);

    vsprintf(str, format, argv);
    str[str_len] = 0;

    return str;
}

char *string_format(const char *format, ...) {
    char *str = 0;

    va_list args;
    va_start(args, format);

    str = string_vformat(format, args);

    va_end(args);
    return str;
}
```

上面这段字符串格式化代码，在大多数情况下，都可以工作的很好。但是在一些Unix操作系统中，这段代码会崩溃，原因是va_list失效了。

通过Google找到一段话： **you can even process a va_list only once on some platforms.**

正确的办法是当需要多次使用va_list对象时，应该使用va_copy拷贝多个副本来使用。下面给出改进后的代码。

```c
char *string_vformat(const char *format, va_list argv) {
    char *str = 0;
    va_list args;
    size_t str_len;

    /* va_list only once on some platforms */
    va_copy(args, argv);
    str_len = vsnprintf(0, 0, format, args);

    str = (char *)malloc(str_len + 1);

    vsprintf(str, format, argv);
    str[str_len] = 0;

    return str;
}
```

虽然va_copy是C99的标准，但微软的VC并没有实现。最后给出一个跨平台方案吧。

```c
#ifndef va_copy
# ifdef __va_copy
#  define va_copy(dest, src) __va_copy(dest, src)
# else
#  define va_copy(dest, src) memcpy((&dest), (&src), sizeof(va_list))
# endif
#endif
```
