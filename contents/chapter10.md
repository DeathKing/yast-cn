# 赋值

## 简介

因为Scheme是函数式语言，通常来说，你可以编写不使用赋值的语句。然而，如果使用赋值的话，有些算法就可以轻易实现了。尤其是内部状态和**继续（continuations ）**需要赋值。

尽管赋值非常习见并且易于理解，但它有一些本质上的缺陷。参见《计算机程序的构造和解释》的第三章第一节“赋值和局部状态”以及《为什么函数式编程如此重要》

R5RS中规定的用于赋值的特殊形式是`set!`、`set-car!`、`set-cdr!`、`string-set!`、`vector-set!`等。除此之外，有些实现也依赖于赋值。由于赋值改变了参数的值，因此它具有**破坏性（destructive）**。Scheme中，具有破坏性的方法都以`!`结尾，以警示程序员。

## set!

`set!`可以为一个参数赋值。与Common Lisp不同，`set!`无法给一个S-表达式赋值。
赋值前参数应被定义。

```scheme
(define var 1)
(set! var (* var 10))
var ⇒ 10

(let ((i 1))
    (set! i (+ i 3))
    i)
⇒ 4
```

## 赋值和内部状态

### 静态作用域（词法闭包）

Scheme中变量的作用域被限定在了源码中定义其的那个括号里。作用域与源代码书写方式一致的作用域称为**“词法闭包（Lexical closure）”**或**“静态作用域（Static scope）”**。This way of scope eliminates bags, as you can grasp the scope of parameters quite easily — written on the source code.另一方面，还有一种被称为**“动态作用域（Dynamic scope）”**的作用域。这种作用域仅在程序运行时确定。由于会在调试时带来种种问题，这种作用域现在已经不再使用。

特殊形式`let`、`lambda`、`letrec`生成闭包。lambda表达式的参数仅在函数定义内部有效。`let`只是`lambda`的语法糖，因此二者无异。

### 使用赋值和词法闭包来实现内部状态

你可以使用词法闭包来实现带有内部状态的过程。例如，用于模拟银行账户的过程可以按如下的方式编写：初始资金是10美元。函数接收一个整形参数。正数表示存入，负数表示取出。为了简单起见，这里允许存款为负数。

```scheme
(define bank-account
  (let ((balance 10))
    (lambda (n)
      (set! balance (+ balance n))
      balance)))
```

该过程将存款赋值为`(+ balance n)`。下面是调用这个过程的结果。

The procedure assigns(+ balance n)to the balance. Following is the result of calling this function.

```scheme
(bank-account 20)     ; donating 20 dollars 
;Value: 30

(bank-account -25)     ; withdrawing 25 dollars
;Value: 5
```

因为在Scheme中，你可以编写返回过程的过程，因此你可以编写一个创建银行账户的函数。这个例子喻示着使用函数式程序设计语言可以很容易实现面向对象程序设计语言。实际上，只需要在这个基础上再加一点东西就可以实现一门面向对象程序设计语言了。

```scheme
(define (make-bank-account balance)
  (lambda (n)
    (set! balance (+ balance n))
    balance))
(define gates-bank-account (make-bank-account 10))   ; Gates makes a bank account by donating  10 dollars
;Value: gates-bank-account

(gates-bank-account 50)                              ; donating 50 dollars
;Value: 60

(gates-bank-account -55)                             ; withdrawing 55 dollars
;Value: 5


(define torvalds-bank-account (make-bank-account 100))  ; Torvalds makes a bank account by donating 100 dollars
;Value: torvalds-bank-account

(torvalds-bank-account -70)                             ; withdrawing 70 dollars
;Value: 30

(torvalds-bank-account 300)                             ; donating 300 dollars
;Value: 330
```

### 副作用

Scheme过程的主要目的是返回一个值，而另一个目的则称为**副作用（Side Effect）**。赋值和IO操作就是副作用。


> 练习 1
> 
> 修改make-bank-account函数 
> Modify make-bank-account so that withdrawing more than balance causes error. hint: Use begin to group more than one S-expressions.

## 表的破坏性操作（set-car!，set-cdr!）

函数set-car!和set-cdr!分别为一个cons单元的car部分和cdr部分赋新值。和set!不同，这两个操作可以为S-表达式赋值。

```scheme
(define tree '((1 2) (3 4 5) (6 7 8 9)))

(set-car! (car tree) 100)  ; changing 1 to 100 

tree
 ((100 2) (3 4 5) (6 7 8 9))

(set-cdr! (third tree) '(a b c)) ; changing  '(7 8 9) to '(a b c) 

tree
⇒ ((100 2) (3 4 5) (6 a b c))
```

### 队列

队列可以用set-car!和set-cdr!实现。队列是一种**先进先出(First in first out, FIFO)**的数据结构，表则是**先进后出(First in last out，FILO)**。图表1展示了队列的结构。`cons-cell-top`的car部分指向表（头），而（`cons-cell-top`的）cdr部分指向表末的cons单元（表尾）。

入队操作按如下步骤进行（见图表2）：
1. 将当前最末的cons单元（可以通过`cons-cell-top`取得）的cdr部分重定向到新的元素。
2. 将`cons-cell-top`的cdr部分重定向到新的元素

出队操作按如下步骤进行（见图表3）
1. 将队首元素存放在一个局部变量里。
2. 将`cons-cell-top`的car部分重定向到表的第二个元素

[代码片段1]展示了如何实现队列。函数enqueue!返回将元素obj添加进队列queue后的队列。函数dequeue!将队列的首元素移出队列并将该元素的值作为返回值。

```scheme
(define (make-queue)
  (cons '() '()))

(define (enqueue! queue obj)
  (let ((lobj (cons obj '())))
    (if (null? (car queue))
	(begin
	  (set-car! queue lobj)
	  (set-cdr! queue lobj))
	(begin
	  (set-cdr! (cdr queue) lobj)
	  (set-cdr! queue lobj)))
    (car queue)))

(define (dequeue! queue)
  (let ((obj (car (car queue))))
    (set-car! queue (cdr (car queue)))
    obj))
(define q (make-queue))
;Value: q

(enqueue! q 'a)
;Value 12: (a)

(enqueue! q 'b)
;Value 12: (a b)

(enqueue! q 'c)
;Value 12: (a b c)

(dequeue! q)
;Value: a

q
;Value 13: ((b c) c)
```

## 小结

这一章中，我讲解了赋值和变量的作用域。虽然在Scheme中，赋值并不常用，但它对于某些算法和数据结构来说是必不可少的。滥用赋值会让你的代码丑陋。当万不得已时才使用赋值！在后面的几章里，我会介绍Scheme中的数据结构。

## 习题解答

### 练习1

```scheme
(define (make-bank-account amount)
  (lambda (n)
    (let ((m (+ amount n)))
      (if (negative? m)
	  'error
	  (begin
	    (set! amount m)
	    amount)))))
```
