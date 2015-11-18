# 惰性求值

## 简介

惰性求值（Lazy evaluation）是在需要时才进行求值的计算方式。The lazy evaluation includes iteration in data structure naturally and represents infinity in a simple matter, which promote modularity of programs. 你可以从[《为什么函数式编程如此重要》](www.md.chalmers.se/~rjmh/Papers/whyfp.html)中知晓惰性计算可以带来哪些好处。

[Haskell](www.haskell.org/)语言以采用惰性求值而广为人熟知。Scheme也部分采用了惰性求值。

## 用于惰性求值的函数

下面这些用于处理惰性求值的函数是在R^5RS中定义的。Intermediate states are called promise in that the way of evaluation is defined and the evaluation is not done yet. The final values are calculated by applying force to the promises.