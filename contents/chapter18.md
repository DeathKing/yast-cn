# 非确定性

## 介绍

非确定性是一种通过仅定义问题来解决问题的算法。非确定性程序自动选择符合条件的选项。这项技术很适合逻辑编程。

例如，以下代码返回一对数，其和是一个质数。其中一个数从`'(4 6 7)`选取，另一个从`'(5 8 11)`选取。

```scheme
(let ((i (amb 4 6 7))
      (j (amb 5 8 11)))
  (if (prime? (+ i j))
      (list i j)
      (amb)))
;Value 23: (6 5)
```

`(amb 4 6 7)` 从4，6和7中返回一个合适的数，`(amb 5 8 11)`从5，8和11中返回一个合适的数。如果没有选出合适的值，(amb)返回假。

实际上，amb做了深度优先搜索。`(amb c1 c2 c3 ...)`创建了搜索路径依次检查`c1`，`c2`，`c3`，...并回溯。因此，非确定性是一种帮程序隐藏搜索的抽象。一旦我们有了amb，我们可以很容易地编写程序而无需思考计算机做了什么。

## 非确定性的实现

使用在非确定性中的回溯被实现为连接到继续（continuation）的闭包链。这个链被一个全局参数`fail`表示，该参数是一个复写自己的函数。

### 函数实现

第一步，我使用函数（名为choose）实现非确定性，演示于[代码1]。我首先定义一个全局参数`fail`，它的初始值是一个将返回`no-choice`到顶层的函数（22-26行）。然后通过在函数choose中重新定义`fail`实现闭包链。回溯通过调用之前的`fail`实现。

函数choose有如下行为：

1. 如果没有选项，调用(fail)。
2. 如果有任何选项，
    1. 将fail储存为fail0，并调用当前继续（continuation）。
    2. 在继续（continuation）中重新定义fail。fail重新被赋值回存在fail0里的原值，并对余下的选项应用（apply）choose。
    3. 返回第一个选项到继续（continuation）外面。 

[代码1]

```scheme
;;; abbreviation for call-with-current-continuation
(define call/cc call-with-current-continuation)

;;; This function is re-assigned in `choose` and `fail` itself.
(define fail #f)

;;; function for nondeterminism
(define (choose . ls)
  (if (null? ls) 
    (fail)
    (let ((fail0 fail))
      (call/cc
        (lambda (cc)
          (set! fail (lambda ()
                       (set! fail fail0)
                       (cc (apply choose (cdr ls)))))
          (cc (car ls)))))))

;;; write following at the end of file
;;; initial value for fail
(call/cc 
  (lambda (cc)
    (set! fail (lambda ()
                 (cc 'no-choice)))))
```

让我们看看choose是否可以找到毕达哥拉斯三元组。函数pythag用于寻找三元组。如果找到了，它返回一个表。如果没有找到，调用无参数的choose，以回溯。

[例1]

```
(define (sq x)
  (* x x))
;Value: sq

;;; Pythagorean triples
(define (pythag a b c)
  (if (= (+ (sq a) (sq b)) (sq c))
      (list a b c)
      (choose)))
;Value: pythag

(pythag (choose 1 2 3) (choose 3 4 5) (choose  4 5 6))
;Value 16: (3 4 5)
```

### 宏实现

为了对S-表达式使用非确定性操作，必须把操作定义为宏。例如，[例2]中所示函数an-integer-starting-from应该返回一个大于或等于n的整数，但是如果choose被以函数形式定义，它将不能正常工作，因为参数会立即求值。

[例2]

```scheme
(define (an-integer-starting-from n)
  (choose n (an-integer-starting-from (1+ n))))
;Value: an-integer-starting-from

(an-integer-starting-from 1)
;Aborting!: maximum recursion depth exceeded
```

为了解决这一点，我们定义了一个和[代码1]中定义一致但使用非确定性宏`amb`实现的choose。这个宏amb有和choose一样的递归调用自己的结构。

[代码1]中的1-5行和20-26行在下面的代码中得以重用。

[代码2]使用MIT-Scheme编译时，编译器给出如下警告：

```
;Warning: Possible inapplicable operator ()
```

但是代码可以正常工作。这些代码在[Petite Chez Scheme](http://www.scheme.com/petitechezscheme.html)下也可以运行。即使我没有试过其他Scheme实现，我认为amb的定义可以工作，只要它们遵守R5RS。你可以在[这里](http://www.shido.info/lisp/scheme_amb.zip)下载一个为MIT-Scheme做的专门实现。MIT-Scheme编译器不会对这个专门实现提出警告。

[代码2]

```scheme
;;; nondeterminism macro operator
(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((fail0 fail))
       (call/cc
    (lambda (cc)
      (set! fail
        (lambda ()
          (set! fail fail0)
          (cc (amb b ...))))
      (cc a)))))))
```

宏定义，amb，在参数为S-表达式时也和其他值一样正常工作。

[例3]

```
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (1+ n))))
;Value: an-integer-starting-from

(an-integer-starting-from 1)
;Value: 1

(amb)
;Value: 2

(amb)
;Value: 3
```

在[Teach Yourself Scheme in Fixnum Days](http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html#node_sec_14.2) 和[Dave Hername Code](http://www.ccs.neu.edu/home/dherman/code/amb.ss)中的amb实现使用`',@(map ...)'`展开参数。即使它们是直截了当的定义，但由于使用了两次`call/cc`，它们某种程度上仍很复杂。[代码2]所示的递归定义更简单，即使展开的S-表达式会很复杂。

### 应用于逻辑编程，使程序更简洁

[代码3]演示了非确定性应用逻辑编程，使得程序更简洁

[代码3]

```scheme
01:     ;;; returning all possibilities
02:     (define-syntax set-of
03:       (syntax-rules () 
04:         ((_ s) 
05:           (let ((acc '())) 
06:             (amb (let ((v s)) 
07:                    (set! acc (cons v acc)) 
08:                    (fail)) 
09:                  (reverse! acc))))))
10:     
11:     ;;; if not pred backtrack
12:     (define (assert pred)
13:       (or pred (amb)))
14:     
15:     ;;; returns arbitrary number larger or equal to n
16:     (define (an-integer-starting-from n)
17:       (amb n (an-integer-starting-from (1+ n))))
18:     
19:     ;;; returns arbitrary number between a and b
20:     (define (number-between a b)
21:       (let loop ((i a))
22:         (if (> i b)
23:             (amb)
24:           (amb i (loop (1+ i))))))
```

**(set-of `s`)**

返回满足`s`的所有可能性。宏的行为如下：

1. （第5行）一个表（acc）被定义，它有所欲哦满足`s`的结果。
2. （第6行）`s`的结果被赋给`v`，并加入到`acc`。如果结果没有带上`v`而直接被加入（如 (set! acc (cons s acc))），则会因为`s`使用了继续（continuation）而只在acc中存储了最后一个值。`s`改了了fail的值。
3. （第7，8行）在这之后，调用fail回溯。因为使用了继续（continuation），函数fail行为就像在第6行被调用。
4. （第9行）当所有可能的选项被找到时，调用`(reverse! acc)`并返回所有的可能选项。

定义假设amb从最左边参数开始搜索。

**(assert `pred`)**

如果谓词为假,就回溯。

**(an-integer-starting-from `n`)**

非确定性地返回从`n`开始的整数。

**(number-between `a` `b`)**

非确定性地返回`a`和`b`之间的整数

[例4]演示了如何使用set-of。得到了所有小于20的质数。

[例4]

```scheme
(define (prime? n)
  (let ((m (sqrt n)))
    (let loop ((i 2))
      (or (< m i)
          (and (not (zero? (modulo n i)))
               (loop (+ i (if (= i 2) 1 2))))))))

(define (gen-prime n)
  (let ((i (number-between  2 n)))
    (assert (prime? i))
    i))

(set-of (gen-prime 20))
;Value 12: (2 3 5 7 11 13 17 19)
```

## 逻辑编程的例子

让我们来解决[SICP中的习题4.42](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.2)作为例子。问题如下：

五位女同学参加一场考试。她们的家长对考试结果过分关心。为此她们约定，在给家里写信谈到考试时，每个姑娘都要写一句真话和一句假话。下面是从她们的信中摘出的句子：

贝蒂：“凯迪考第二，我只考了第三。”
艾赛尔：“你们应该高兴地听到我考了第一，琼第二。”
琼：“我考第三，可怜的艾赛尔考得最差。”
凯蒂：“我第二，玛丽只考了第四。”
玛丽：“我是第四，贝蒂的成绩最高。”

这五位姑娘的实际排名是什么？

[代码4]给出了这个问题的解法。

[代码4]

```scheme
01:     (define (xor a b)
02:       (if a (not b) b))
03:     
04:     (define (all-different? . ls)
05:       (let loop ((obj (car ls)) (ls (cdr ls)))
06:         (or (null? ls)
07:             (and (not (memv obj ls))
08:                  (loop (car ls) (cdr ls))))))
09:     
10:     ;;; SICP Exercise 4.42
11:     (define (girls-exam)
12:       (let ((kitty (number-between 1 5))
13:             (betty (number-between 1 5)))
14:         (assert (xor (= kitty 2) (= betty 3)))
15:         (let ((mary (number-between 1 5)))
16:           (assert (xor (= kitty 2) (= mary 4)))
17:           (assert (xor (= mary 4) (= betty 1)))
18:           (let ((ethel (number-between 1 5))
19:                 (joan (number-between 1 5)))
20:             (assert (xor (= ethel 1) (= joan 2)))
21:             (assert (xor (= joan 3) (= ethel 5)))
22:             (assert (all-different? kitty betty ethel joan mary))
23:             (map list '(kitty betty ethel joan mary) (list kitty betty ethel joan mary))))))
24:     
25:     ;;; Bad answer for ex 4.42
26:     (define (girls-exam-x)
27:       (let ((kitty (number-between 1 5))
28:             (betty (number-between 1 5))
29:             (mary (number-between 1 5))
30:             (ethel (number-between 1 5))
31:             (joan (number-between 1 5)))
32:         (assert (xor (= kitty 2) (= betty 3)))
33:         (assert (xor (= kitty 2) (= mary 4)))
34:         (assert (xor (= mary 4) (= betty 1)))
35:         (assert (xor (= ethel 1) (= joan 2)))
36:         (assert (xor (= joan 3) (= ethel 5)))
37:         (assert (all-different? kitty betty ethel joan mary))
38:         (map list '(kitty betty ethel joan mary) (list kitty betty ethel joan mary))))
```

**(xor a b)**以下条件满足，返回#t:

- a是#t，b是#f，或者
- a是#f，b是#t。


**(all-different? , ls)**

当`ls`的所有元素都不相同时，返回#t。

**(girls-exam)**

是解决谜题的主要函数。它返回名字和排名的表。每次参数赋值后都调用了assert是为了有效地减少死分支的运行时间。`(girls-exam-x)`则是一个坏例子。它在为所有参数赋值之后调用assert。这种情况下，无谓地搜索了大量的死分支。[例5]显示(girl-exam-x)的运行时间是(girl-exam)的10倍。

[例5]

```
(define-syntax cpu-time/sec
  (syntax-rules ()
    ((_ s)
     (with-timings
     (lambda () s)
       (lambda (run-time gc-time real-time)
     (write (internal-time/ticks->seconds run-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds gc-time))
     (write-char #\space)
     (write (internal-time/ticks->seconds real-time))
     (newline))))))
;Value: cpu-time/sec

(cpu-time/sec (girls-exam))
.03 0. .03
;Value 14: ((kitty 1) (betty 3) (ethel 5) (joan 2) (mary 4))

(cpu-time/sec (girls-exam-x))
.341 .29 .631
;Value 15: ((kitty 1) (betty 3) (ethel 5) (joan 2) (mary 4))
```

## 小结

当你使用了非确定性和用于逻辑编程分析技术时，你就可以写出看起来具有先见之明的程序。注意如果搜索路径里有循环我们就不能使用本章的代码。关于这一点，查看[SICP 4.3](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3)以获取更多信息。

写这一章时，我参考了[Teach Yourself Scheme in Fixnum Days](http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html)。

你可以在[这儿](http://www.shido.info/lisp/scheme_amb.zip)下载本章代码。
