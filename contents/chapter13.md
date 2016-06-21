# 关联表和哈希表

## 简介

本章中，我会讲解用于表示数据关联的关联表和哈希表。关联的数据是由键和值组成的序对，值由键唯一确定的。表1显示了书和作者构成的配对。书籍可以确定作者，反之由作者确定书籍则不可，这是因为一个作者可能会写很多本书。表1中，由于P. Graham和L.Carroll分别写了两本书，因此他们的书无法被作者的名字唯一确定。

表1：作者和书

Author        | Book 
:------------ | :------------------------------------------------------ 
P. Graham     |  On Lisp
P. Graham     |	 ANSI Common Lisp
E. S. Raymond |	 The Cathedral and the Bazaar
K. Dybvig     | The Scheme Programming Language
F. P. Brooks, Jr.|	The Mythical Man-Month
L. Carroll   | Alice's Adventures in Wonderland
L. Carroll   | Through the Looking-Glass, and What Alice Found There  

R5RS定义了关联表，因此它在所有Scheme实现中都可用。但是使用关联表搜索速度较慢（O(n)的时间复杂度）。使用哈希表在速度方面更好一些（O(1)的时间复杂度），但是哈希表并未在R5RS中定义而是依赖于相关实现。MIT-Scheme实现了哈希表。如果你喜欢的Scheme实现没有哈希表，你可以自己实现一个（见 <http://www.math.grin.edu/~stone/events/scheme-workshop/hash-tables.html>）。

## 关联表

关联表是一个由序对组成的表，它是一个用于表达关联的基本数据类型。符号，字符，和数字常被作为键使用，因为它们可以使用诸如`eq?`或者`eqv?`的快速比较函数被比较。在作为键被使用前，字符串应该被转换为符号，从而获得更好的性能。

下面是一个关联表的例子。关联表应该要么由点序对要么由普通表组成。

```scheme
'((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8))
'((1 2 3) (4 5 6) (7 8 9))
```

函数`assq`，`assv`，和`assoc`从关联表中搜寻一个项。这些函数从开始一步步搜索关联表。如果它们找到序对的`car`等于给定的`key`，就返回该序对。如果找不到函数返回`#f`。这些函数分别使用`eq?`，`eqv?`，和`equal?`比较键，这意味着`assq`最快，`assoc`最慢。这表示作为键的话，字符串，向量和表应该转化为符号或者数字（如果可能的话）以提高性能。

一般来说，[哈希表](http://www.shido.info/lisp/scheme_ah_e.html#hash)在大量数据中搜索表现得更好一些。

下面展示在关联表中进行搜索的例子。

```scheme
(define wc '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8)))
⇒ wc

(assq 'hi wc)
⇒  (hi . 3)

(assq 'you wc)
⇒  (you . 8)

(assq 'i wc)
⇒  ()


(define n '((1 2 3) (4 5 6) (7 8 9)))
⇒  n

(assv 1 n)
⇒  (1 2 3)

(assv 8 n)
⇒  ()
```

## 哈希表

[哈希表](http://en.wikipedia.org/wiki/Hash-table)是一种数据类型，它使用哈希函数将键转化为整数，并将值存储在由该整数所指示的位置。当表足够稀疏时，搜索，插入，更新都能以O(1)完成。下面展示了MIT-Scheme里哈希表的一些基本函数。查询[MIT-Scheme Manul](http://www.swiss.ai.mit.edu/projects/scheme/documentation/scheme_12.html#SEC119)获取更详细的信息。

**(make-eq-hash-table size),**

**(make-eqv-hash-table size),**

**(make-equal-hash-table size),**

**(make-string-hash-table size)**

这些函数创建哈希表。这些函数分别使用`eq?`，`eqv?`，`equal?`，和`string=?`比较键的值。哈希表的初始大小（`size`）可以选择性指定（optional）。由于只比较键的地址，所以`eq-hash-table`是最快的。由于键是序列，所以`equal-hash-table`和`string-hash-table`比较慢。

**(hash-table/put! hash-table key datum)**

将`hash-table`中`key`对应的值设为`datum`。

**(hash-table/get hash-table key default)**

返回`hash-table`中的`key`对应的值。如果`key`不存在于`hash-table`中，返回`default`。

**(hash-table->alist hash-table)**

将`hash-table`转换为关联表。


## 生成密码

让我们写一个密码创建程序作为关联表和哈希表的例子。

从字典里得到的密码很容易被破解，但另一方面，完全随机的密码又很难记忆和输入。程序使用无规则的拼写创建10个密码。密码应该尽可能频繁更改，但是我懒于自己创建密码。使用这个程序，我可以简单地改变密码。

程序由两部分构成。一部分用于创建连续字符出现频率的数据（stat-spell.scm），另一个用于基于这个数据创建密码（make-pw.scm）。

### stat-spell.scm

这个程序可以阅读英语句子，数据存在哈希表里，并转换为关联表输出到一个文件（stat-spell.data）。[代码1]显示了源代码。

[代码1]

```scheme
01:     ;;; make an alist of probable spelling from a given English text
02:     
03:     (define (skip-char? c)
04:       (or (not (char-graphic? c)) (memv c '(#\: #\; #\' #\" #\`))))
05:     
06:     (define (ss-make-alist c alist)
07:       (let ((p (assv c alist)))
08:         (if p
09:             (begin
10:              (set-cdr! p (1+ (cdr p)))
11:              alist)
12:           (cons (cons c 1) alist))))
13:     
14:     (define (ss-make-dat filename)
15:       (let ((char-hash (make-eqv-hash-table)))
16:         (with-input-from-file filename
17:           (lambda ()
18:     	(let loop ((c #\Space))
19:     	  (let ((c1 (read-char)))
20:                      (if (not (eof-object? c1))
21:                          (if (skip-char? c1)
22:                              (loop c)
23:                              (let ((c1 (char-downcase c1)))
24:     			   (hash-table/put! char-hash c
25:     					    (ss-make-alist c1 (hash-table/get char-hash c '())))
26:     			   (loop c1))))))))
27:         (with-output-to-file "stat-spell.dat"
28:           (lambda ()
29:     	(display "(define *stat-spell* \'(")
30:     	(newline)
31:     	(let loop ((alst (sort (hash-table->alist char-hash) 
32:     			       (lambda (x y) (char33:     	  (if (pair? alst)
34:     	      (begin
35:     		(write (car alst))
36:     		(newline)
37:     		(loop (cdr alst)))))
38:             (display "))")
39:             (newline)))))
```

**(skip-char? c)**

如果`c`不是图像字符或者`c`是 #\:, #\;, #\', or #\"，就返回#t。读取文本时，这些字符会被跳过。

**(ss-make-alist c alist)**

有两个参数；字符的频率的关联表（`alist`）和字符（`c`）。如果`c`在`alist`中，在序对的cdr部分增加一。如果不在，返回 (cons (cons c 1) alist)。这个函数使用了set-cdr!。

**(ss-make-dat filename)**

从名为`filename`的文件中读取字符，并使用跟随字符的频率的关联表来关联这些读出的字符。结果以关联表形式存储在文件[stat-spell.dat](http://www.shido.info/lisp/stat-spell.dat)。在34和35行，它在哈希表中更新了频率的关联表。存储在stat-spell.dat的最终数据是一个关联表的关联表。例如：

(#\v (#\y . 1) (#\a . 3) (#\o . 7) (#\e . 51) (#\i . 15))

表示 #\y, #\a, #\o, #\e, 和 #\i 跟随 #\v 之后出现的次数分别是1, 3, 7, 51, 和15次。

### make-pw.scm

基于 stat-spell.dat 创建十个密码。过程如下：

1. 基于频率数据创建由9到13个随机字符组成字符串表。字符 #\Space 被添加在表结尾。
2. 添加一个00到99之间的随机数在随机选取的字符串表的结尾。
3. 随机地将 #\Space 转换为 #\-, #\_, #\/, #\Space, #\., 或者 #\,。
4. 随机地将30%的字母字符变为大写。

```scheme
01:     ;;; make password from the alist of probable spelling
02:     
03:     (load "stat-spell.dat") ; *stat-spell* (alist for following characters) is in.
04:     
05:     (define (alist->hash al mode)
06:       (let ((h (case mode
07:                  ((eq) (make-eq-hash-table))
08:                  ((eqv) (make-eqv-hash-table))
09:                  ((equal) (make-equal-hash-table))
10:                  ((string) (make-string-hash-table)))))
11:         (for-each (lambda (p)
12:                     (hash-table/put! h (car p) (cdr p)))
13:                   al)
14:         h))
15:     
16:     (define *stat-spell-hash* (alist->hash *stat-spell* 'eqv))
17:     
18:     (define (pw-random-select vec)
19:       (vector-ref vec (random (vector-length vec))))
20:     
21:     (define (random00)
22:       (let loop ((i 0) (acc '()))
23:         (if (= i 2)
24:             (list->string acc)
25:           (loop (1+ i) (cons (pw-random-select '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) acc)))))
26:     
27:     (define (occasional-upcase c)
28:       (if (< (random 10) 3)
29:           (char-upcase c)
30:         c))
31:     
32:     (define (pw-enhance ls)
33:       (list->string
34:        (map (lambda (c)
35:               (cond
36:                ((char=? c #\Space)
37:                 (pw-random-select  '#(#\- #\_ #\/  #\Space  #\. #\, #\@ #\? #\( #\))))
38:                ((char-alphabetic? c)
39:                 (occasional-upcase c))
40:                (else c)))
41:             (cdr (reverse! ls)))))
42:         
43:     
44:     (define (random-following alist)
45:       (let ((n (random (apply + (map cdr alist)))))
46:         (let loop ((j 0) (alist alist))
47:           (if (pair? alist)
48:     	  (let* ((pair (car alist))
49:     		 (k (+ j (cdr pair))))
50:     	    (if (> k n)
51:     		(car pair)
52:     		(loop k (cdr alist))))))))
53:     
54:     (define (make-pw h n)
55:       (let loop ((i 0) (c #\Space) (acc '()))
56:         (if (= i n)
57:             (string-append
58:              (pw-enhance (cons #\Space (cons c acc)))
59:              (random00))
60:           (loop (1+ i)
61:             (random-following (hash-table/get h c '((#\Space . 1))))
62:             (cons c acc)))))
63:         
64:     (define (pw-candidates)
65:       (let loop ((i 0))
66:         (if (< i 10)
67:             (begin
68:              (display i)
69:              (display ": ")
70:              (write (make-pw *stat-spell-hash* (+ 9 (random 4))))
71:              (newline)
72:              (loop (1+ i)))
73:           'done)))
```

## 小结

你可以在[这儿](http://www.shido.info/lisp/scheme_ah.zip)下载密码生成程序。

我将在下一章讲解向量。