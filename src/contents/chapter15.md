# 定义语法

## 简介

本章中，我会讲解如何自定义语法。用户定义语法称作**宏（Macro）**。Lisp/Scheme中的宏比C语言中的宏更加强大。宏可以使你的程序优美而紧凑。

宏是代码的变换。代码在被求值或编译前进行变换，and the procedure continues as if the transformed codes are written from the beginning.

你可以在Scheme中通过用符合R5RS规范的`syntax-rules`轻易地定义简单宏，相比之下，在Common Lisp中自定义语法就复杂多了。使用`syntax-rules`可以直接定义宏而不用担心**变量的捕获（Variable Capture）**。On the other hand, defining complicated macros that cannot be defined using the syntax-rules is more difficult than that of the Common Lisp.

## 实例：简单宏

我将以一个简单的宏作为例子。

[代码片段 1] 一个将变量赋值为’()的宏

```scheme
(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))
```

`syntax-reuls`的第二个参数由是变换前表达式构成的表。`_`代表宏的名字。简言之，**代码片段1**表示表达式`(nil! x)`会变换为`(set! x '())`.

这类过程不能通过函数来实现，这是因为函数的闭包性质限制它不能影响外部变量。让我们来用函数实现**代码片段1**，并观察效果。

```scheme
(define (f-nil! x)
   (set! x '()))
(define a 1)
;Value: a

(f-nil! a)
;Value: 1

a
;Value: 1           ; the value of a dose not change

(nil! a)
;Value: 1

a
;Value: ()          ; a becomes '()
```
{:caption=>''}

我会演示另外一个例子。我们编写宏`when`，其语义为：当谓词求值为真时，求值相应语句。

```scheme
(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))
```
{:caption=>'代码片段2'}

**代码片段2**中的`...`代表了任意多个数的表达式（包括0个表达式）。**代码片段2**揭示了诸如表达式`(when pred b1 ...)`会变换为`(if pred (begin b1 ...))`。

由于这个宏是将表达式变换为`if`特殊形式，因此它不能使用函数来实现。下面的例子演示了如何使用`when`。

```scheme
(let ((i 0))
  (when (= i 0)
    (display "i == 0")
    (newline)))
i == 0
;Unspecified return value
```

我会演示两个实宏：`while`和`for`。只要谓词部分求值为真，`while`就会对语句体求值。而数字在指定的范围中，`for`就会对语句体求值。

```scheme
(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))


(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (1+ i)))))))
```
{:caption=>'代码片段3'}

下面演示了如何实用它们：

```scheme
define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))


(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (1+ i)))))))
```

> 练习1
>
> 编写一个宏，其语义为：当谓词求值为假时执行相应的表达式。（语义与`when`相对）

## syntax-rule的更多细节

### 多个定义模式

`syntax-rule`可以定义一系列模式。比如，一个让变量增加的宏，如果给定了变量名，那么宏`incf`使该变量增加1。可以通过编写下面**代码片段4**这样的模式转换来实现宏`incf`。

```scheme
(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x))
    ((_ x i) (begin (set! x (+ x i)) x))))
(let ((i 0) (j 0))
  (incf i)
  (incf j 3)
  (display (list 'i '= i))
  (newline)
  (display (list 'j '= j)))
(i = 1)
(j = 3)
;Unspecified return value
```
{:caption=>'代码片段4'}

> 练习2  
>
> 编写用于从变量中减去一个数的宏`decf`。如果减量省略了，则从变量中减1。
  
> 练习3
>
> 改进**代码片段3**中的宏`for`，使得它可以接受一个参数作为步长。如果省略了步长，则默认为1。

### 宏的递归定义

特殊形式`or`和`and`是通过像下面这样递归定义的宏：

```scheme
(define-syntax my-and
  (syntax-rules ()
    ((_) #t)
    ((_ e) e)
    ((_ e1 e2 ...)
     (if e1
	 (my-and e2 ...)
	 #f))))

(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 ...)
     (let ((t e1))
       (if t t (my-or e2 ...))))))
```
{:caption=>'代码片段5'}

可以使用递归定义来编写复杂的宏。

> 练习4  
>
> 请自己实现`let*`。

### 使用保留字

`syntax-rule`的第一个参数是保留字表。比如，`cond`的定义如**代码片段6**所示，这当中，`else`是保留字。

```scheme
(define-syntax my-cond
  (syntax-rules (else)
    ((_ (else e1 ...))
     (begin e1 ...))
    ((_ (e1 e2 ...))
     (when e1 e2 ...))
    ((_ (e1 e2 ...) c1 ...)
     (if e1 
	 (begin e2 ...)
	 (cond c1 ...)))))
```
{:caption=>'代码片段6'}

## 局部语法

在Scheme中，可以使用`let-syntax`和`letrec-syntax`来定义**局部语法（Local Syntax）**。这种形式的用法和`define-syntax`是相似的。

##  Implementation Depending Macro Definition

有些宏无法使用`syntax-rules`来定义。定义这些宏的实现方法已经在Scheme实现中准备好了。由于这种行为严重依赖于实现，因此你可以跳过此节。

In the case of the MIT-Scheme, The sc-macro-transformer is available for such purpose, which allows to write macros in a similar way to taht of the Common Lisp. See the Common Lisp HyperSpec. about what ` , ,@ are. See MIT-Scheme manual about sc-macro-transfomrer and make-syntactic-closure. [code 7] shows simple examples.
[code 7]

在MIT-Scheme中，`sc-macro-transformer`就可用于这种情况，它允许用户用与Common Lisp中相似的方式来编写宏。关于`,`、`,@`的介绍，请参见[The Common Lisp HyperSpec](www.lispworks.com/documentation/HyperSpec/Body/02_df.htm)。关于`sc-macro-transformer`和`make-syntactic-closuer`请参见MIT-Scheme手册。**代码片段7**演示了一个简单的例子。

```scheme
(define-syntax show-vars
  (sc-macro-transformer
    (lambda (exp env)
      (let ((vars (cdr exp)))
           `(begin
	          (display
	            (list
	              ,@(map (lambda (v)
		                    (let ((w (make-syntactic-closure env '() v)))
		                         `(list ',w ,w)))
		                  vars)))
	  (newline))))))

(define-syntax random-choice
  (sc-macro-transformer
   (lambda (exp env)
     (let ((i -1))
       `(case (random ,(length (cdr exp)))
	  ,@(map (lambda (x)
		   `((,(incf i)) ,(make-syntactic-closure env '() x)))
		 (cdr exp)))))))

(define-syntax aif
  (sc-macro-transformer
   (lambda (exp env)
     (let ((test (make-syntactic-closure env '(it) (second exp)))
	   (cthen (make-syntactic-closure env '(it) (third exp)))
	   (celse (if (pair? (cdddr exp))
		      (make-syntactic-closure env '(it) (fourth exp))
		      #f)))
       `(let ((it ,test))
	  (if it ,cthen ,celse))))))
```