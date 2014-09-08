# 继续

## 简介

本章介绍的是Scheme中特有的数据类型——**继续（Continuation）**。由于其他程序设计语言并没有这种数据类型，因此它难于理解。当下，你并不需要彻底理解清楚，只需要大致了解。我会讲解广义的继续和简短地介绍**Continuation-Passing-Style(CPS)**，然后再讲解Scheme中的继续。我认为通过这种方式理解继续会比较容易。

## 广义继续

继续是在返回到**顶层（Toplevel）**之前所需要执行的计算。实际上，继续存在于计算的每时每刻。以`(* 3 (+ 1 2))`为例，在求值完`(+ 1 2)`后，应该计算`{ (* 3 []) }`乘以3。然而，大多数语言都不显式地这么做，程序员对此并不熟悉。

## Continuation-Passing-Style(CPS)

### 简单的CPS

CPS是一种编程风格，在这种风格中，把依赖于当前函数结果的后续函数作为参数传递给当前函数。**代码片段1**展示了以CPS编写的加法和乘法。在`k+`和`k*`中，`k`是后续函数。

```scheme
(define (return x)
  x)

(define (k+ a b k)
  (k (+ a b)))

(define (k* a b k)
  (k (* a b)))
```
{:caption=>'代码片段1'}

**例1**演示了如何使用CPS计算`(* 3 (+ 1 2))`。

```scheme
(k+ 1 2 (lambda (x) (k* x 3 return)))
```
{:caption=>'例1'}

Scheme的普通形式中，值在括号外被计算（#TBD In the ordinary form of Scheme, values that are calculated in parentheses go outside of them. ）与此相反，CPS中，值向其它括号内传递。在**例1**中，`k+`把`(+ 1 2)`的值传递给`(lambda (x) (k* x 3 return))`，而`k*`把`(* (+ 1 2) 3)`的结果传给`return`。

### 以CPS编写递归函数

递归函数同样可以以CPS编写。**代码片段2**展示了计算阶乘的函数如何用普通方式编写（`fact`）和以CPS编写(`kfact`)。

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

**例2**将3与4的阶乘相加。

```scheme
;;; normal
(+ 3 (fact 4))

;;; CPS
(kfact 4 (lambda (x) (k+ x 3 return)))
```
{:caption=>'例2'}

**代码片段3**演示了如何分别用普通方式和CPS编写用于计算表中元素的积的函数。在CPS函数中，后继函数存储再局部变量`break`中，因此当元素乘以0时，可以立即退出。

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
{:caption=>'代码片段3'}

**例3**将100与`'(2 4 7)`的积相加。

```scheme
;;; normal
(+ 100 (product '(2 4 7)))

;;; CPS
(kproduct '(2 4 7) (lambda (x) (k+ x 100 return)))
```

{:caption=>'例3'}

尽管CPS再这样简单的情况中并不使用，但再一些像是自然语言解析和逻辑编程等复杂程序中非常有用，因为与通常的编程风格相比，CPS可以更灵活地改变后续过程。

**异常处理（Exception handling）**就是这种情况的简单例子。**代码片段4**演示了`kproduct`的错误处理版本，in that a non-number value is shown and the calculation is terminated when it appears in the input list.

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
{:caption=>'代码片段4'}

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
{:caption=>'例4'}

## Scheme中的继续

通过上面的讲解，你应该掌握了继续。继续有下面的性质：

1. 存在于整个计算过程中；
2. 函数式程序设计语言和CPS可以显式地处理它；

另外，上面的例子展示的是**闭包链（Chain of closure）**。然而，阅读和编写CPS程序是痛苦的，以通常的方式来处理继续会更方便一点。因此，Scheme中将继续实现为**一级对象（first class object）**（这意味这Scheme中的继续是个普通数据类型），任何时候都可以通过`call-with-current-continuation`来调用。由于继续是普通数据类型，你可以随心所欲地重用。考虑到`call-with-current-continuation`名字过长，通常使用其缩略名`call/cc`。

```scheme
(define call/cc call-with-current-continuation)
```

函数`call-with-current-continuation (call/cc)`接受一个参数。该参数是一个函数，函数的参数接收当前的继续。

下面是例子：

```scheme
(* 3 (call/cc (lambda (k) (+ 1 2))))     ;⇒ 9      ; [1]
(* 3 (call/cc (lambda (k) (+ 1 (k 2))))) ;⇒ 6      ; [2]
```

在**情况[1]**中，继续并没有被调用，语句的行为与普通S-表达式相同。另一方面，在**情况[2]**中，继续以2作为参数被调用。在这种情况中，继续的参数跳过了`call/cc`的处理，并逃逸至`call/cc`的外部。这种情况中，`k`是一个一元函数，等价于`(lambda (x) (* 3 x))`。

大体来说，当前继续存储了从`call/cc`调用点到顶层的处理过程。当前继续可以像其它数据类型那样存储起来，并随心所欲地重用。

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

从一个计算过程中逃逸出来，是使用当前继续的最容易的方法。**代码片段5**演示了搜索树（嵌套表）的函数。如果函数在树中找到`obj`，那么它返回该对象，否则返回`#f`。一旦找到`obj`，函数直接将其抛出至最外部。

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
{:caption=>'代码片段5'}

```scheme
(find-leaf 7 '(1 (2 3) 4 (5 (6 7))))
;⇒ 7

(find-leaf 8 '(1 (2 3) 4 (5 (6 7))))
;⇒ ()
```
{:caption=>'例5'}

**例6**演示了一个支持抛出的语法`block`。

```scheme
(define-syntax block
  (syntax-rules ()
    ((_ tag e1 ...)
     (call-with-current-continuation
       (lambda (tag)
	      e1 ...)))))
```

**例7**演示了如何使用它。

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
{:caption=>'例7'}

### 生成器

我会讲解如何用`call/cc`实现一个树匹配的生成器。生成器以一个树为参数返回一个函数，每次调用这个返回的函数时，它会返回后序的叶子。你可以在[Teach Yourself Scheme in Fixnum Days的第13.3节](www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-15.html#node_sec_13.3)中找到这个函数的原始版本。生成器是像下面这样使用的：

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

**代码片段6**给出了生成器的定义。这个和原始版本基本上相同，但有略微的修改。

```scheme
01:     (define (leaf-generator tree)
02:       (let ((return '()))                                                        ; 1
03:         (letrec ((continue                                                      ; 2
04:     	      (lambda ()
05:     		(let loop ((tree tree))                                     ; 3
06:     		  (cond                                                     ; 4
07:     		   ((null? tree) 'skip)                                     ; 5
08:     		   ((pair? tree) (loop (car tree)) (loop (cdr tree)))       ; 6
09:     		   (else                                                    ; 7
10:     		    (call/cc (lambda (lap-to-go)                            ; 8
11:     			       (set! continue (lambda () (lap-to-go 'restart)))    ; 9
12:     			       (return tree))))))                      ;10
13:     		(return '()))))                                         ;11
14:             (lambda ()                                                     ;12
15:               (call/cc (lambda (where-to-go)                               ;13
16:                          (set! return where-to-go)                         ;14
17:                          (continue)))))))
```
{:caption=>'代码片段6'}

注释解释

编号  解释
1.	declaring a local variable return.
2.	defining continue using letrec. The continue returns current leaf in front, assigns the current continuation to the next continue, and halts.
3.	defining rec using named let.
4.	branching using cond
5.	if empty list, does nothing.
6.	if pair, applies the rec recursively to its car and cdr.
7.	if leaf,
8.	invokes call/cc to get the current state (lap-to-go)
9.	"and set it to the next continue. The lap-to-go includes the current state in addition to the original continue. In short, it can be expressed by [ ]in the following S-expression.
                (lambda ()
                  (let rec ((tree tree0))  
                    (cond                  
                     ((null? tree) '())     
                     ((pair? tree) (rec (car tree)) (rec (cdr tree)))  
                     (else                                             
                      [ ]                    
                  (return '()))))                                       
As invoking lap-to-go means that (car tree) is a leaf and the process is finished, (rec (cdr tree)) starts at the next function is called. As the process starts after finishing the part of [ ], The argument of the continuation does not matter."
10.	Then the function return the found leaf to where the function is called. (return tree) should be inside of call/cc to restart the process.
11.	Returning an empty list after searching all leaves
12.	It is a generator that leaf-generator returns.
13.	First invoking call/cc
14.	and assign the plase to return values to return.
15.	Then calls continue.

The behaviour of the function generated by the leaf-generator can be estimated by the behavior of a conventional traversing function (tree-traverse). The process halts at '*' of the trace and remained process is stored in the continue. A conventional traverse functionF


```scheme
(define tree-traverse
  (lambda (tree)
    (cond
     ((null? tree) '_)
     ((pair? tree) (tree-traverse (car tree)) (tree-traverse (cdr tree)))
     (else
      (write tree)))))
```

Trace of the tree-traverse when tree '((1 2) 3) is given.

```shell
> (tree-traverse '((1 2) 3))
  | (tree-traverse ((1 2) 3))
  | (tree-traverse (1 2))
  | | (tree-traverse 1)           
1 | |  #< void>               ; *
  | | (tree-traverse (2))
  | | (tree-traverse 2)           
2 | |< void>                ; *
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

**代码片段7**shows a program that print numbers and alphabetic characters alternately. Lines 5 — 22 are imprementation of queue. (enqueue! queue obj) adds obj at the end of queue. (dequeue! queue) returns the first item of the queue with removing it.
Lines 26 — 38 are imprementation of a coroutine.
process-queue
The queue of processes.
(coroutine thunk)
adding thunk at the end of the process-queue.
(start)
picking up the first process of the process-queue and executing it.
(pause)
adding the current continuation at the end of the process-queue and execute the first process of the queue. This function hand over the control to the other routine.
Lines 42 — 61 show how to use it. The routine showing numbers and that showing alphabetic characters call e


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
{:caption=>'代码片段7'}

```scheme
(load "cor2.scm")
;Loading "cor2.scm"
1 a 2 b 3 c 4 d 5 e 6 f 7 g 8 h 9 i 10 j  -- done
;Unspecified return value
```
{:caption=>'例7'}

## 总结

本章中，我讲解了继续。理解这些概念可能比较困难。但不要担心，