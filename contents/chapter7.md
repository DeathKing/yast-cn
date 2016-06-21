# 重复 

## 简介

本章中我会介绍重复。通过重复，你可以编写“通常的”程序。虽然也可以使用`do`表达式，但Scheme中通常通过递归实现重复。

## 递归

在自己的定义中调用自己的函数叫做**递归函数（Recursive Function）**。虽然这听起来很奇怪，但是循环的常见方法。如果你把函数类比为机器的话，递归似乎毫无道理。然而，正因为函数是过程，函数调用自己是有意义的。比如说，让我们来考察一下文献调研吧。你可能需要去阅读你正在阅读的文献所引用的文献（cited-1）。进一步，你可能还需要去阅读文件（cite-1）所引用的其它文献。这样，文献调研就是一个递归的过程，你也可以重复这个调研过程直到满足了特定条件（比如说，你累了）。这样，将程序设计语言中的函数类比为人类活动（比如文献调研）将有助于理解递归函数。

我们通常使用计算阶乘来解释递归。

```scheme
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))
```

`(fact 5)`的计算过程如下：

```
(fact 5)
⇒ 5 * (fact 4)
⇒ 5 * 4 * (fact 3)
⇒ 5 * 4 * 3 * (fact 2)
⇒ 5 * 4 * 3 * 2 * (fact 1)
⇒ 5 * 4 * 3 * 2 * 1
⇒ 5 * 4 * 3 * 2
⇒ 5 * 4 * 6
⇒ 5 * 24
⇒ 120
```

`(fact 5)`调用`(fact 4)`，`(fact 4)`调用`(fact 3)`，最后`(fact 1)`被调用。`(fact 5)`，`(fact 4)`……以及`(fact 1)`都被分配了不同的存储空间，直到`(fact (- i 1))`返回一个值之前，`(fact i)`都会保留在内存中，由于存在函数调用的开销，这通常会占用更多地内存空间和计算时间。

然而，递归函数可以以一种简单的方式表达重复。表是被递归定义的，进而表和递归函数可以很好地配合。例如，一个让表中所有元素翻倍的函数可以像下面这样写。如果参数是空表，那么函数应该停止计算并返回一个空表。

```scheme
(define (list*2 ls)
  (if (null? ls)
      '()
      (cons (* 2 (car ls))
	         (list*2 (cdr ls)))))
```

> 练习1
> 
> 用递归编写下面的函数。
> 
> 1. 用于统计表中元素个数的`my-length`函数。（`length`是一个预定义函数）。
> 2. 一个求和表中元素的函数。
> 3. 一个分别接受一个表`ls`和一个对象`x`的函数，该函数返回从`ls`中删除`x`后得到的表。
> 4. 一个分别接受一个表`ls`和一个对象`x`的函数，该函数返回`x`在`ls`中首次出现的位置。索引从`0`开始。如果`x`不在`ls`中，函数返回`#f`。

## 尾递归

普通的递归调用并不高效因为它既浪费存储空间又具有函数调用开销。与之相反，尾递归函数包含了计算结果，当计算结束时直接将其返回。特别地，由于Scheme规范要求尾递归调用转化为循环，因此尾递归调用就不存在函数调用开销。

[代码片段2]展示了[代码片段1]中函数`fact`的尾递归版本。

```scheme
(define (fact-tail n)
  (fact-rec n n))

(define (fact-rec n p)
  (if (= n 1)
      p
      (let ((m (- n 1)))
	(fact-rec m (* p m)))))
```

`fact-tail`计算阶乘的过程像这样：

```
(fact-tail 5)
⇒ (fact-rec 5 5)
⇒ (fact-rec 4 20)
⇒ (fact-rec 3 60)
⇒ (fact-rec 2 120)
⇒ (fact-rec 1 120)
⇒ 120
```

因为`fact-rec`并不等待其它函数的计算结果，因此当它计算结束时即从内存中释放。计算通过修改`fact-rec`的参数来演进，这基本上等同于循环。如上文所述，Scheme将尾递归转化为循环，Scheme就无需提供循环的语法来实现重复。

> 练习2
>
> 用尾递归编写下面的函数
>
> 1. 用于翻转表元素顺序的`my-reverse`函数。（`reverse`函数是预定义函数）
> 2. 求和由数构成的表。
> 3. 将一个代表正整数的字符串转化为对应整数。例如，"1232"会被转化为1232。不需要检查不合法的输入。提示，字符到整数的转化是通过将字符#\0……#\9的ASCII减去48，可以使用函数`char->integer`来获得字符的ASCII码。函数`string->list`可以将字符串转化为由字符构成的表。

## 命名let

命名`let`（**named let**）可以用来表达循环。[代码片段3]中的函数`fact-let`展示了如何使用命名`let`来计算阶乘。`fact-let`函数使用了一个**命名`let`表达式**`(loop)`，这与在[代码片段2]中展示的`fact-rec`函数是不同的。在被注释为`;1`的那行，代码将参数`n1`和`p`都初始化为`n`。再每次循环后，参数在被注释为`;2`的那行更新：将`n1`减1，而将`p`乘以`(n1 - 1)`。

在Scheme中，用命名`let`来表达循环是俗成的方法。

```scheme
(define (fact-let n)
  (let loop((n1 n) (p n))           ; 1
    (if (= n1 1)                    
	p
	(let ((m (- n1 1)))
	  (loop m (* p m))))))      ; 2
```

> 练习3
>
> 用命名`let`编写下面的函数。
>
> 1. 练习1的问题3和问题4；
> 2. 练习2中的函数；
> 3. `range`函数：返回一个从`0`到`n`的表（但不包含`n`）。

## letrec

`letrec`类似于`let`，但它允许一个名字递归地调用它自己。语法`letrec`通常用于定义复杂的递归函数。[代码片段4]展示了`fact`函数的`letrec`版本。

```scheme
(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))     ; *
    (iter n n)))
```

正如被注释为`;*`的那行代码所示，局部变量`iter`可以在它的定义里面引用它自己。语法`letrec`是定义局部变量的俗成方式。

> 练习4
>
> 用`letrec`重写练习2。

## do表达式

虽然并不常见，但语法`do`也可用于表达重复。它的格式如下：

```scheme
(do binds (predicate value)
    body)
```

变量在`binds`部分被绑定，而如果`predicate`被求值为真，则函数从循环中**逃逸（escape）**出来，并返回值`value`，否则循环继续进行。

`binds`部分的格式如下所示：

```
[binds] → ((p1 i1 u1) (p2 i2 u2) ... )
```

变量`p1`，`p2`，...被分别初始化为`i1`，`i2`，...并在循环后分别被更新为`u1`，`u2`，...。

[代码片段5]演示了`fact`的`do`表达式版本。

```scheme
(define (fact-do n)
  (do ((n1 n (- n1 1)) (p n (* p (- n1 1)))) ((= n1 1) p)))
```

变量`n1`和`p`分别被初始化为`n`和`n`，在每次循环后分别被减去1和乘以`(n1 - 1)`。当`n1`变为`1`时，函数返回`p`。

我认为`do`比命名`let`还要复杂一些。

> 练习5
>
> 用`do`表达式重写练习2。

## 小结

现在你可以用我讲解过的技巧来编写常见程序了。通常来说，命名`let`用于编写简单的循环，而`letrec`则是用来写复杂的局部递归函数。

下一章中我讲讲解高阶函数。高阶函数使得你的代码更加“Scheme风味”。

## 习题解答

### 练习1

```scheme
; 1
(define (my-length ls)
  (if (null? ls)
      0
      (+ 1 (my-length (cdr ls)))))

; 2
(define (my-sum ls)
  (if (null? ls)
      0
      (+ (car ls) (my-sum (cdr ls)))))

; 3
(define (remove x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)))
        ((if (eqv? x h)
            (lambda (y) y)
            (lambda (y) (cons h y)))
         (remove x (cdr ls))))))
        
; 4
(define (position x ls)
  (position-aux x ls 0))

(define (position-aux x ls i)
  (cond
   ((null? ls) #f)
   ((eqv? x (car ls)) i)
   (else (position-aux x (cdr ls) (1+ i)))))
```

### 练习2

```scheme
; 1
(define (my-reverse ls)
  (my-reverse-rec ls ()))

(define (my-reverse-rec ls0 ls1)
  (if (null? ls0)
      ls1
      (my-reverse-rec (cdr ls0) (cons (car ls0) ls1))))

;-------------------
; 2
(define (my-sum-tail ls)
  (my-sum-rec ls 0))

(define (my-sum-rec ls n)
  (if (null? ls)
      n
      (my-sum-rec (cdr ls) (+ n (car ls)))))

;--------------------
; 3
(define (my-string->integer str)
  (char2int (string->list str) 0))

(define (char2int ls n)
  (if (null? ls)
      n
      (char2int (cdr ls) 
		(+ (- (char->integer (car ls)) 48)
		   (* n 10))))
```

### 练习3

```scheme
; 1
(define (remove x ls)
  (let loop((ls0 ls) (ls1 ()))
    (if (null? ls0) 
	(reverse ls1)
	(loop
	 (cdr ls0)
          (if (eqv? x (car ls0))
              ls1
            (cons (car ls0) ls1))))))

; 2
(define (position x ls)
  (let loop((ls0 ls) (i 0))
    (cond
     ((null? ls0) #f)
     ((eqv? x (car ls0)) i)
     (else (loop (cdr ls0) (1+ i))))))

; 3
(define (my-reverse-let ls)
  (let loop((ls0 ls) (ls1 ()))
    (if (null? ls0)
	ls1
	(loop (cdr ls0) (cons (car ls0) ls1)))))

; 4
(define (my-sum-let ls)
  (let loop((ls0 ls) (n 0))
    (if (null? ls0)
	n
	(loop (cdr ls0) (+ (car ls0) n)))))

; 5
(define (my-string->integer-let str)
  (let loop((ls0 (string->list str)) (n 0))
    (if (null? ls0)
	n
	(loop (cdr ls0)
	      (+ (- (char->integer (car ls0)) 48)
		 (* n 10))))))

; 6
(define (range n)
  (let loop((i 0) (ls1 ()))
    (if (= i n)
        (reverse ls1)
      (loop (1+ i) (cons i ls1)))))
```

### 练习4

```scheme
; 1
(define (my-reverse-letrec ls)
  (letrec ((iter (lambda (ls0 ls1)
		   (if (null? ls0)
		       ls1
		       (iter (cdr ls0) (cons (car ls0) ls1))))))
    (iter ls ())))

; 2
(define (my-sum-letrec ls)
  (letrec ((iter (lambda (ls0 n)
		   (if (null? ls0)
		       n
		       (iter (cdr ls0) (+ (car ls0) n))))))
    (iter ls 0)))

; 3
(define (my-string->integer-letrec str)
  (letrec ((iter (lambda (ls0 n)
		   (if (null? ls0)
		       n
		       (iter (cdr ls0)
			     (+ (- (char->integer (car ls0)) 48)
				(* n 10)))))))
    (iter (string->list str) 0)))
```

### 练习5

```scheme
; 1
(define (my-reverse-do ls)
  (do ((ls0 ls (cdr ls0)) (ls1 () (cons (car ls0) ls1)))
      ((null? ls0) ls1)))

; 2
(define (my-sum-do ls)
  (do ((ls0 ls (cdr ls0)) (n 0 (+ n (car ls0))))
      ((null? ls0) n)))

; 3
(define (my-string->integer-do str)
  (do ((ls0 (string->list str) (cdr ls0))
       (n 0 (+ (- (char->integer (car ls0)) 48) 
	       (* n 10))))
      ((null? ls0) n)))
```
