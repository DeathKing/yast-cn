# 定义语法

## 简介

本章中，我会讲解如何自定义语法。用户定义语法称作**宏（Macro）**。Lisp/Scheme中的宏比C语言中的宏更加强大。宏可以使你的程序优美而紧凑。

**宏是代码的变换**。代码在被求值或编译前进行变换，程序会继续执行就像变换后的代码一开始就写好了一样。

你可以在Scheme中通过用符合R5RS规范的`syntax-rules`轻易地定义简单宏，相比之下，在Common Lisp中自定义语法就复杂多了。使用`syntax-rules`可以直接定义宏而不用担心**变量捕获（Variable Capture）**。另一方面，Scheme中定义那些无法用`syntax-rules`定义的复杂的宏就比Common Lisp要困难。

## 简单宏的实例

我将以一个简单的宏作为例子。

[代码1]一个将变量赋值为'()的宏。

[代码1]

```scheme
(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))
```

`syntax-reuls`的第二个参数是变换前和变化后的表达式的序对所构成的表。`_`代表宏的名字。简言之，[代码1]表示表达式`(nil! x)`会变换为`(set! x '())`.

这类程序不能通过函数来实现，这是因为由于闭包性，函数不能影响外部变量。让我们来用函数版本来实现[代码1]，并观察效果。

[代码'1]

```scheme
(define (f-nil! x)
   (set! x '()))
```

```
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

我会演示另外一个例子。我们编写宏`when`，其语义为：当谓词求值为真时，求值相应语句。

[代码2]

```scheme
(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))
```

[代码2]中的`...`代表了任意多个数的表达式（包括0个表达式）。[代码2]揭示了表达式`(when pred b1 ...)`变换为`(if pred (begin b1 ...))`。

由于这个宏是将表达式变换为`if`特殊形式，因此它不能使用函数来实现。下面的例子演示了如何使用`when`。

```scheme
(let ((i 0))
  (when (= i 0)
    (display "i == 0")
    (newline)))
i == 0
;Unspecified return value
```

我会演示两个实际的宏：`while`和`for`（已在Scheme中实现）。只要谓词部分求值为真，`while`就会对语句体求值。而数字在指定的范围中，`for`就会对语句体求值。

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

下面演示了如何使用它们：

```scheme
(let ((i 0))
  (while (< i 10)
    (display i)
    (display #\Space)
    (set! i (+ i 1))))
0 1 2 3 4 5 6 7 8 9 
;Unspecified return value

(for (i 0 10)
  (display i)
  (display #\Space))
0 1 2 3 4 5 6 7 8 9 
;Unspecified return value
```

> 练习1
>
> 编写一个宏，其语义为：当谓词求值为假时执行相应的表达式。（语义与`when`相反。）

## syntax-rule的更多细节

### 多个定义模式

`syntax-rule`可以定义一系列模式。比如，一个让变量增加的宏，如果给定了变量名，那么宏`incf`使该变量增加1。可以通过编写如[代码4]这样的模式转换来实现宏`incf`。

[代码4]

```scheme
(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x))
    ((_ x i) (begin (set! x (+ x i)) x))))
```

```Scheme 
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

> 练习2  
>
> 编写用于从变量中减去一个数的宏`decf`。如果减量省略了，则从变量中减1。
  
> 练习3
>
> 改进[代码3]中的宏`for`，使得它可以接受一个参数作为步长。如果省略了步长，则默认为1。

### 宏的递归定义

代码形式`or`和`and`是通过像下面这样递归定义的宏：

[代码5]

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

可以使用递归定义来编写复杂的宏。

> 练习4  
>
> 请自己实现`let*`。

### 使用保留字

`syntax-rule`的第一个参数是保留字的表。比如，`cond`的定义如[代码6]所示，其中，`else`是保留字。

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

## 局部语法

在Scheme中，可以使用`let-syntax`和`letrec-syntax`来定义**局部语法（Local Syntax）**。这种形式的用法和`define-syntax`是相似的。

## 取决于宏定义的实现

有些宏无法使用`syntax-rules`来定义。定义这些宏的实现方法已经在Scheme实现中准备好了。由于这种行为严重依赖于实现，因此你可以跳过此节。

在MIT-Scheme中，`sc-macro-transformer`就可用于这种情况，它允许用户用与Common Lisp中相似的方式来编写宏。关于`,`、`,@`的介绍，请参见[The Common Lisp HyperSpec](www.lispworks.com/documentation/HyperSpec/Body/02_df.htm)。关于`sc-macro-transformer`和`make-syntactic-closuer`请参见MIT-Scheme手册。[代码7]演示了一个简单的例子。

[代码 7]

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

第一个宏`show-vars`用于显示变量的值。

```scheme
(let ((i 1) (j 3) (k 7))
  (show-vars i j k))
((i 1) (j 3) (k 7))
;Unspecified return value

```

代码形式`(show-vars i j k)`被展开成下面这样。因为宏只能返回一个表达式，所以需要用`begin`返回表达式的集合。

```scheme
(begin
  (display
   (list
    (list 'i i) (list 'j j) (list 'k k)))
  (newline))
```

第二个宏`random-choice`被用于从参数中随机选择一个值或者过程。

```scheme
(define (turn-right) 'right)
(define (turn-left) 'left)
(define (go-ahead) 'straight)
(define (stop) 'stop)

(random-choice (turn-right) (turn-left) (go-ahead) (stop))
;Value: right
```

代码形式被展开如下：

```scheme
(case (random 4)
  ((0) (turn-right))
  ((1) (turn-left))
  ((2) (go-ahead))
  ((3) (stop)))
```

第三个宏`aif`是一个回指宏（ anaphoric macro）。谓词的结果可以被指为`it`。变量`it`被捕获，以使得第二个参数`make-syntactic-closure`变为`'(it)`。

```scheme
(let ((i 4))
  (aif (memv i '(2 4 6 8))
       (car it)))
;Value: 4
```

下面显示了扩展结果。

````scheme
(let ((it (memv i '(2 4 6 8))))
  (if it
      (car it)
      #f))
```

## 结构的原始实现

结构（structure）可以通过[代码8]中的简单宏实现。这里定义的结构的本质是一个向量（vector）和由宏自动创建的取值以及赋值函数。如果你喜欢的Scheme版本么有结构的实现，你可以自己实现它们。

[代码8]

```scheme
01:     ;;; simple structure definition
02:     
03:     ;;; lists of symbols -> string
04:     (define (append-symbol . ls)
05:       (let loop ((ls (cdr ls)) (str (symbol->string (car ls))))
06:         (if (null? ls)
07:     	str
08:     	(loop (cdr ls) (string-append str "-" (symbol->string (car ls)))))))
09:     
10:     ;;; obj -> ls -> integer
11:     ;;; returns position of obj in ls
12:     (define (position obj ls)
13:       (letrec ((iter (lambda (i ls)
14:     		   (cond
15:     		    ((null? ls) #f)
16:     		    ((eq? obj (car ls)) i)
17:     		    (else (iter (1+ i) (cdr ls)))))))
18:         (iter 0 ls)))
19:     		  	       
20:     
21:     ;;; list -> integer -> list
22:     ;;; enumerate list items
23:     (define (slot-enumerate ls i)
24:       (if (null? ls)
25:           '()
26:         (cons `((,(car ls)) ,i) (slot-enumerate (cdr ls) (1+ i)))))
27:     
28:     ;;; define simple structure 
29:     (define-syntax defstruct
30:       (sc-macro-transformer
31:        (lambda (exp env)
32:          (let ((struct (second exp))
33:                (slots  (map (lambda (x) (if (pair? x) (car x) x)) (cddr exp)))
34:     	   (veclen (- (length exp) 1)))
35:     	   
36:            `(begin   
37:     	  (define ,(string->symbol (append-symbol 'make struct))   ; making instance
38:     	    (lambda ls
39:                   (let ((vec (vector ',struct ,@(map (lambda (x) (if (pair? x) (second x) #f)) (cddr exp)))))
40:     		(let loop ((ls ls))
41:     		  (if (null? ls)
42:     		      vec
43:     		      (begin
44:                            (vector-set! vec (case (first ls) ,@(slot-enumerate slots 1)) (second ls))
45:     			(loop (cddr ls))))))))
46:     
47:     	  (define ,(string->symbol (string-append (symbol->string struct) "?"))  ; predicate
48:     	    (lambda (obj)
49:     	      (and
50:     	       (vector? obj)
51:     	       (eq? (vector-ref obj 0) ',struct))))
52:     
53:     	  ,@(map
54:     	     (lambda (slot)
55:     	       (let ((p (1+ (position slot slots))))
56:     		 `(begin
57:     		    (define ,(string->symbol (append-symbol struct slot))    ; accessors
58:     		      (lambda (vec)
59:     			(vector-ref vec ,p)))
60:     
61:     		    (define-syntax ,(string->symbol                           ; modifier
62:     				     (string-append
63:     				      (append-symbol 'set struct slot) "!"))
64:     		      (syntax-rules ()
65:     			((_ s v) (vector-set! s ,p v)))))))
66:     	     slots)
67:     
68:     	  (define ,(string->symbol (append-symbol 'copy struct))      ; copier
69:     	    (lambda (vec)
70:     	      (let ((vec1 (make-vector ,veclen)))
71:     		(let loop ((i 0))
72:     		  (if (= i ,veclen)
73:     		      vec1
74:     		      (begin
75:     			(vector-set! vec1 i (vector-ref vec i))
76:     			(loop (1+ i)))))))))))))
```

下面演示了如何使用：

你可以定义一个结构，要么只给出槽（slot）的名字，要么给出槽（slot）的名字和缺省值。

```scheme
;;; Defining a structure point having 3 slots whose defaults are 0.0.
(defstruct point (x 0.0) (y 0.0) (z 0.0))
;Unspecified return value

(define p1 (make-point 'x 10 'y 20 'z 30))
;Value: p1

(point? p1)
;Value: #t

(point-x p1)
;Value: 10

;;; Default values are used for unspecified values when an instance is made.
(define p2 (make-point 'z 20))
;Value: p2

(point-x p2)
;Value: 0.

(point-z p2)
;Value: 20

;;; Changing a slot value
(set-point-y! p2 12)
;Unspecified return value

;;; The reality of the structure definde by [code 8] is a vector
p2
;Value 14: #(point 0. 12 20)

;;; Defining a structure 'book' with no default values.
(defstruct book title authors publisher year isbn)
;Unspecified return value

(define mon-month 
  (make-book 'title  
	     "The Mythical Man-Month: Essays on Software Engineering"
	     'authors
	     "F.Brooks"
	     'publisher
	     "Addison-Wesley"
	     'year
	     1995
	     'isbn
	     0201835959))
;Value: mon-month

mon-month
;Value 15: #(book 
"The Mythical Man-Month: Essays on Software Engineering" 
"F.Brooks" 
"Addison-Wesley" 
1995 
201835959)

(book-title mon-month)
;Value 13: "The Mythical Man-Month: Essays on Software Engineering"
```

## 小结

我简要介绍了Scheme里的宏。宏可以使你的代码更优雅。

`syntax-rules`使得编写宏很容易。另一方面，编写Common Lisp的宏，则要求特点的技巧。

## 习题解答

### 答案1

```scheme
(define-syntax unless
  (syntax-rules ()
    ((_ pred b1 ...)
     (if (not pred)
	 (begin
	   b1 ...)))))
```

第二个

### 答案2

```scheme
(define-syntax decf
  (syntax-rules ()
    ((_ x) (begin (set! x (- x 1)) x))
    ((_ x i) (begin (set! x (- x i)) x))))
```

### 答案3

```scheme
(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (1+ i)))))
    ((_ (i from to step) b1 ...)
     (let loop ((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ i step)))))))
```

### 答案4

```scheme
(define-syntax my-let*
  (syntax-rules ()
    ((_ ((p v)) b ...)
     (let ((p v)) b ...))
    ((_ ((p1 v1) (p2 v2) ...) b ...)
     (let ((p1 v1))
       (my-let* ((p2 v2) ...)
		b ...)))))
```