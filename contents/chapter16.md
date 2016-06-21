# 继续

## 简介

本章介绍的是Scheme中特有的数据类型——**继续（Continuation）**。由于其他程序设计语言并没有这种数据类型，因此它难于理解。当下，你并不需要彻底理解清楚，只需要大致了解。

我会讲解广义的继续和简短地介绍**Continuation-Passing-Style(CPS)**，然后再讲解Scheme中的继续。我认为通过这种方式理解继续会比较容易。

## 广义继续

继续是在返回到**顶层（Top level）**之前所需要执行的计算。实际上，继续存在于计算的每时每刻。以`(* 3 (+ 1 2))`为例，在求值完`(+ 1 2)`后，应该计算`{ (* 3 []) }`乘以3。然而，大多数语言都不显式地这么做，程序员对此并不熟悉。

## Continuation-Passing-Style(CPS)

### 简单的CPS

CPS是一种编程风格，在这种风格中，把依赖于当前函数结果的后续函数作为参数传递给当前函数。[代码1]展示了以CPS编写的加法和乘法。在`k+`和`k*`中，`k`是后续函数。

[代码1]

```scheme
(define (return x)
  x)

(define (k+ a b k)
  (k (+ a b)))

(define (k* a b k)
  (k (* a b)))
```

[例1]演示了如何使用CPS计算`(* 3 (+ 1 2))`。

[例1]

```scheme
(k+ 1 2 (lambda (x) (k* x 3 return)))
```

Scheme的普通形式中，值在括号内被计算并向括号外传递。与此相反，CPS中，值向括号内传递。如[例1]中，`k+`把`(+ 1 2)`的值传递给`(lambda (x) (k* x 3 return))`，而`k*`把`(* (+ 1 2) 3)`的结果传给`return`。

### 以CPS编写递归函数

递归函数同样可以以CPS编写。[代码2]展示了计算阶乘的函数如何用普通方式编写（`fact`）和以CPS编写(`kfact`)。

[代码2]

```scheme
;;; normal factorial
(define (fact n)
  (if (= n 1) 
      1
      (* n (fact (- n 1)))))

;;; CPS factorial
(define (kfact n k)
  (if (= n 1) 
      (k 1)
      (kfact (- n 1) (lambda (x) (k (* n x))))))
```

[例2]将3与4的阶乘相加。

[例2]

```scheme
;;; normal
(+ 3 (fact 4))

;;; CPS
(kfact 4 (lambda (x) (k+ x 3 return)))
```

[代码3]演示了如何分别用普通方式和CPS编写计算表中元素之积的函数。在CPS函数中，后继函数存储在局部变量`break`中，因此当元素乘以0时，可以立即退出。

[代码3]

```scheme
;;; normal
(define (product ls)
  (let loop ((ls ls) (acc 1))
    (cond
     ((null? ls) acc)
     ((zero? (car ls)) 0)
     (else (loop (cdr ls) (* (car ls) acc))))))

;;; CPS
(define (kproduct ls k)
  (let ((break k))
    (let loop ((ls ls) (k k))
      (cond
       ((null? ls) (k 1))
       ((zero? (car ls)) (break 0))
       (else (loop (cdr ls) (lambda (x) (k (* (car ls) x)))))))))
```

[例3]将100与`'(2 4 7)`的积相加。

[例3]

```scheme
;;; normal
(+ 100 (product '(2 4 7)))

;;; CPS
(kproduct '(2 4 7) (lambda (x) (k+ x 100 return)))
```

尽管CPS在这样简单的情况中并不实用，但在一些像是自然语言解析和逻辑编程等复杂程序中非常有用，因为与通常的编程风格相比，CPS可以更灵活地改变后续过程。

**异常处理（Exception handling）**就是这种情况的简单例子。[代码4]演示了`kproduct`的错误处理版本，程序中当非数字值出现在输入表中，在其被打印时，计算就会终止。

```scheme
(define (non-number-value-error x)
  (display "Value error: ")
  (display  x)
  (display " is not number.")
  (newline)
  'error)

(define (kproduct ls k k-value-error)
  (let ((break k))
    (let loop ((ls ls) (k k))
      (cond
       ((null? ls) (k 1))
       ((not (number? (car ls))) (k-value-error (car ls)))
       ((zero? (car ls)) (break 0))
       (else (loop (cdr ls) (lambda (x) (k (* (car ls) x)))))))))
```

```scheme
;;; valid
(kproduct '(2 4 7) 
	  (lambda (x) (k+ x 100 return)) 
	  non-number-value-error)
;Value: 156

;;; invalid
(kproduct '(2 4 7 hoge) 
	  (lambda (x) (k+ x 100 return)) 
	  non-number-value-error)
Value error: hoge is not number.
;Value: error
```

## Scheme中的继续

通过上面的讲解，你应该掌握了继续（continuation）。继续有下面的性质：

1. 存在于整个计算过程中；
2. 函数式程序设计语言和CPS可以显式地处理它。

另外，上面例子展示的是**闭包链（Chain of closure）**。

然而，阅读和编写CPS程序是痛苦的，以常规方式来处理继续会更方便一点。

因此，Scheme中将继续实现为**一级对象（first class object）**（这意味这Scheme中的继续是个普通数据类型），任何时候都可以通过名为`call-with-current-continuation`来调用。由于继续是普通数据类型，你可以随心所欲地重用。考虑到`call-with-current-continuation`名字过长，通常使用其缩略名`call/cc`。

```scheme
(define call/cc call-with-current-continuation)
```

函数`call-with-current-continuation (call/cc)`接受一个参数。该参数是一个函数，函数的参数接收当前继续。

下面是例子：

```scheme
(* 3 (call/cc (lambda (k) (+ 1 2))))     ;⇒ 9      ; [1]
(* 3 (call/cc (lambda (k) (+ 1 (k 2))))) ;⇒ 6      ; [2]
```

在**情况[1]**中，继续并没有被调用，语句的行为与普通S-表达式相同。另一方面，在**情况[2]**中，继续以2作为参数被调用。在这种情况中，继续的参数跳过了`call/cc`的处理，并逃逸至`call/cc`的外部。这种情况中，`k`是一个一元函数，等价于`(lambda (x) (* 3 x))`。

大体来说，当前继续存储了从`call/cc`调用点到顶层的处理过程。当前继续可以像其它数据类型那样被存储起来，并随心所欲地重用。

```scheme
(define cc)
  (* 3 (call/cc (lambda (k)
                  (set! cc k)
                  (+ 1 2))))
```

由于当前继续是回到顶层的处理过程，它的返回会忽略周围的S-表达式。

```scheme
(+ 100 (cc 3))  ;⇒ 9 
(+ 100 (cc 10)) ;⇒ 30
```

### 使用`call/cc`抛出值

从一个计算过程中逃逸出来，是使用当前继续的最容易的方法。[代码5]演示了搜索树（嵌套表）的函数。如果函数在树中找到`obj`，那么它返回该对象，否则返回`#f`。一旦找到`obj`，函数直接将其抛出至最外部。

```scheme
(define (find-leaf obj tree)
  (call/cc
    (lambda (cc)
       (letrec ((iter
	               (lambda (tree)
		              (cond
		                ((null?  tree) #f)
		                ((pair? tree)
		                   (iter (car tree))
		                   (iter (cdr tree)))
		                (else
		                  (if (eqv? obj tree)
		                    (cc obj)))))))
         (iter tree)))))
```

```scheme
(find-leaf 7 '(1 (2 3) 4 (5 (6 7))))
;⇒ 7

(find-leaf 8 '(1 (2 3) 4 (5 (6 7))))
;⇒ ()
```

[例6]演示了一个支持抛出的语法`block`。

```scheme
(define-syntax block
  (syntax-rules ()
    ((_ tag e1 ...)
     (call-with-current-continuation
       (lambda (tag)
	      e1 ...)))))
```

[例7]演示了如何使用它。

```scheme
(block break
   (map (lambda (x)
           (if (positive? x)
	       (sqrt x)
	       (break x)))
	'(1 2 3)))
;⇒ (1 1.4142135623730951 1.7320508075688772)

(block break
   (map (lambda (x)
           (if (positive? x)
	       (sqrt x)
	       (break x)))
	'(1 -2 3)))
;⇒ -2
```

### 生成器

我会讲解如何用`call/cc`实现一个树匹配的生成器。生成器以一个树为参数返回一个函数，每次调用这个返回的函数时，它会返回后续的叶子。你可以在[Teach Yourself Scheme in Fixnum Days的第13.3节](http://ds26gte.github.io/tyscheme/index-Z-H-15.html#node_sec_13.3)中找到这个函数的原始版本。生成器的使用方法如下：

```scheme
(define tr '((1 2) (3 (4 5))))
(define p (leaf-generator tr))

(p) ;=> 1
(p) ;=> 2
(p) ;=> 3
(p) ;=> 4
(p) ;=> 5
(p) ;=> ()  ; finally it returns '()
```

[代码6]给出了生成器的定义。这个和原始版本基本上相同，但有略微的修改。

[代码6]

```scheme
(define (leaf-generator tree)
  (let ((return '()))                                               ; 1
    (letrec ((continue                                              ; 2
      (lambda ()
        (let rec ((tree tree))                                      ; 3
          (cond                                                     ; 4
           ((null? tree) 'skip)                                     ; 5
           ((pair? tree) (rec (car tree)) (rec (cdr tree)))         ; 6
           (else                                                    ; 7
            (call/cc (lambda (lap-to-go)                            ; 8
                   (set! continue (lambda () (lap-to-go 'restart))) ; 9
                   (return tree))))))                               ;10
        (return '()))))                                             ;11
    (lambda ()                                                  ;12
      (call/cc (lambda (where-to-go)                            ;13
                 (set! return where-to-go)                      ;14
                 (continue)))))))
```

(译者注：原文中05，08行中命名let中的`rec`被写为`loop`，结合上下文，改为`rec`)

注释解释

编号  解释

- 1.定义本地变量`return`。
- 2.使用`letrec`定义`continue`。`continue`将当前叶子返回到前面，将当前继续赋给`continue`，并停止。
- 3.用`rec`定义命名let。
- 4.使用`cond`实现分支
- 5.如果是空表，什么也不做
- 6.如果是序对，递归地将序对的car和cdr应用于rec。
- 7.如果是叶子，
- 8.调用`call/cc`以获取当前状态(lap-to-go)
- 9.接着将当前状态赋给`continue`。所以除了原有的`continue`，`lap-to-go`也包含了当前状态。简而言之，它可以被如下的S-表达式中的**[ ]**表示。

```Scheme
(lambda ()
   (let rec ((tree tree0))  
      (cond                  
        ((null? tree) '())     
        ((pair? tree) (rec (car tree)) (rec (cdr tree)))  
        (else                                            
           [ ]
    (return '()))))
```

调用`lap-to-go`意味着(car tree)是叶子，且过程结束了，(rec (cdr tree))在下一次函数调用时开始运行。如果过程在**[ ]**之后结束，继续的参数将不起作用。                           

- 10.接着函数将找到的叶子返回到函数的调用处。`(return tree)`应该在`call/cc`中以重启过程。
- 11.在搜索了全部叶子之后返回空表。
- 12.这是一个返回叶子生成器的生成器。
- 13.首次调用`call/cc`
- 14.将表示返回值的当前状态赋给`return`。
- 15.然后调用`continue`。

由`leaf-generator`生成的函数的行为可以通过函数（tree-traverse）的行为来估计。过程停止在轨迹的'*'的注释处，并使得过程存储在`continue`。

一个常规的遍历函数：

```scheme
(define tree-traverse
  (lambda (tree)
    (cond
     ((null? tree) '_)
     ((pair? tree) (tree-traverse (car tree)) (tree-traverse (cdr tree)))
     (else
      (write tree)))))
```

当树为`'((1 2) 3)`时，`tree-traverse`的轨迹。

```
> (tree-traverse '((1 2) 3))
|(tree-traverse ((1 2) 3))
| (tree-traverse (1 2))
| |(tree-traverse 1)           
1| |#< void>               ; *
| (tree-traverse (2))
| |(tree-traverse 2)           
2| |< void>                ; *
| (tree-traverse '())
| _
|(tree-traverse (3))
| (tree-traverse 3)            
3| #< void>                ; *
|(tree-traverse '())
|_
_
```

### 协程

因为继续记录了后续计算过程，因此，用于多任务同时执行的**协程（Coroutine）**可以使用继续来实现。

**代码片段7**展示了一段交替打印数字和字母的程序。5 - 22行是队列的实现。(enqueue! queue obj)将一个`obj`添加在队列的末尾。(dequeue! queue)返回队列第一个元素并将它删除。

26 - 38行是协程的实现。

**process-queue**

过程的队列。
	
**(coroutine thunk)**

在`process-queue`末尾添加`thunk`。

**(start)**

取得`process-queue`的第一个过程并执行它。

**(pause)**

将当前继续添加到`process-queue`的末尾并执行队列里的第一个过程。这个函数将控制权交给另外一个协程。
	
42 - 61行显示如何使用它。一个显示数字例程和一个显示字母例程相互调用对方，结果显示在**例7**

```scheme
01:     ;;; abbreviation
02:     (define call/cc call-with-current-continuation)
03:     
04:     ;;; queue
05:     (define (make-queue)
06:       (cons '() '()))
07:     
08:     (define (enqueue! queue obj)
09:       (let ((lobj (list obj)))
10:         (if (null? (car queue))
11:     	(begin
12:     	  (set-car! queue lobj)
13:     	  (set-cdr! queue lobj))
14:     	(begin
15:     	  (set-cdr! (cdr queue) lobj)
16:     	  (set-cdr! queue lobj)))
17:         (car queue)))
18:     
19:     (define (dequeue! queue)
20:       (let ((obj (car (car queue))))
21:         (set-car! queue (cdr (car queue)))
22:         obj))
23:     
24:     
25:     ;;; coroutine   
26:     (define process-queue (make-queue))
27:     
28:     (define (coroutine thunk)
29:       (enqueue! process-queue thunk))
30:     
31:     (define (start)
32:        ((dequeue! process-queue)))
33:        
34:     (define (pause)
35:       (call/cc
36:        (lambda (k)
37:          (coroutine (lambda () (k #f)))
38:          (start))))
39:     
40:     
41:     ;;; example
42:     (coroutine (lambda ()
43:     	     (let loop ((i 0)) 
44:     	       (if (< i 10)
45:     		   (begin
46:     		     (display (1+ i)) 
47:     		     (display " ") 
48:     		     (pause) 
49:     		     (loop (1+ i)))))))
50:     		   
51:     (coroutine (lambda ()
52:     	     (let loop ((i 0)) 
53:     	       (if (< i 10)
54:     		   (begin
55:     		     (display (integer->char (+ i 97)))
56:     		     (display " ")
57:     		     (pause) 
58:     		     (loop (1+ i)))))))
59:     
60:     (newline)
61:     (start)
```

```scheme
(load "cor2.scm")
;Loading "cor2.scm"
1 a 2 b 3 c 4 d 5 e 6 f 7 g 8 h 9 i 10 j  -- done
;Unspecified return value
```

## 小结

本章中，我讲解了继续。

理解这些概念可能比较困难。但不要担心，有朝一日你终会明白。

下一章中，我将介绍惰性求值。
