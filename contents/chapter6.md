# 局部变量

## 简介

在前面的章节中，我已经讲述了如何定义函数。在本节中，我讲介绍局部变量，这将会使定义函数变得更加容易。

## let表达式

使用`let`表达式可以定义局部变量。格式如下：

```scheme
(let binds body)
```

变量在`binds`定义的形式中被声明并初始化。`body`由任意多个S-表达式构成。`binds`的格式如下：

```
[binds] → ((p1 v1) (p2 v2) ...)
```

声明了变量`p1`、`p2`，并分别为它们赋初值`v1`、`v2`。变量的**作用域（Scope）**为`body`体，也就是说变量只在`body`中有效。

> 例1：声明局部变量`i`和`j`，将它们与`1`、`2`绑定，然后求二者的和。

```scheme
(let ((i 1) (j 2))
  (+ i j))
;Value: 3
```

`let`表达式可以嵌套使用。

> 例2：声明局部变量`i`和`j`，并将分别将它们与`1`和`i+2`绑定，然后求它们的乘积。

```scheme
(let ((i 1))
  (let ((j (+ i 2)))
    (* i j)))
;Value: 3
```

由于变量的作用域仅在`body`中，下列代码会产生错误，因为在变量`j`的作用域中没有变量`i`的定义。

```scheme
(let ((i 1) (j (+ i 2)))
  (* i j))
;Error
```

`let*`表达式可以用于引用定义在同一个绑定中的变量。实际上，`let*`只是嵌套的`let`表达式的语法糖而已。

```scheme
(let* ((i 1) (j (+ i 2)))
  (* i j))
;Value: 3
```

> 例3：函数`quadric-equation`用于计算二次方程。它需要三个代表系数的参数：`a`、`b`、`c` （`ax^2 + bx + c = 0`），返回一个存放答案的实数表。通过逐步地使用`let`表达式，可以避免不必要的计算。

```scheme
;;;The scopes of variables d,e, and f are the regions with the same background colors.

(define (quadric-equation a b c)
  (if (zero? a)      
      'error                                      ; 1
      (let ((d (- (* b b) (* 4 a c))))            ; 2
        (if (negative? d)
            '()                                      ; 3
            (let ((e (/ b a -2)))                    ; 4
              (if (zero? d)
              (list e)
              (let ((f (/ (sqrt d) a 2)))        ; 5
                (list (+ e f) (- e f)))))))))

(quadric-equation 3 5 2)  ; solution of 3x^2+5x+2=0
;Value 12: (-2/3 -1)
```

> 这个函数的行为如下：
> 
> 1. 如果二次项系数`a`为`0`，函数返回`'error`。
> 2. 如果`a ≠ 0`，则将变量`d`与判别式`(b^2 - 4ac)`的值绑定。
> 3. 如果`d`为负数，则返回`'()`。
> 4. 如果`d`不为负数，则将变量`e`与`-b/2a`绑定。
> 5. 如果`d`为`0`，则返回一个包含`e`的表。
> 6. 如果`d`是正数，则将变量`f`与`√(d/2a)`绑定，并返回由`(+ e f)`和`(- e f)`> 构成的表。

实际上，`let`表达式只是`lambda`表达式的一个语法糖：

```scheme
(let ((p1 v1) (p2 v2) ...) exp1 exp2 ...)
;⇒
((lambda (p1 p2 ...)
    exp1 exp2 ...) v1 v2)
```

这是因为`lambda`表达式用于定义函数，它为变量建立了一个作用域。

> 练习1
> 
> 编写一个解决第四章练习1的函数，该函数旨在通过一个初始速度`v`和与水平面所成夹角`a`来计算飞行距离。

## 总结

本节中，我介绍了`let`表达式，`let`表达式是`lambda`表达式的一个语法糖。变量的作用域通过使用`let`表达式或`lambda`表达式来确定。在Scheme中，这个有效域由源代码的编写决定，这叫做**词法闭包（lexical closure）**。

## 习题解答

### 答案1

```scheme
(define (throw v a)
  (let ((r (/ (* 4 a (atan 1.0)) 180)))
    (/ (* 2 v v (cos r) (sin r)) 9.8)))
```
