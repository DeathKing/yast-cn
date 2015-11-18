# 输入输出

## 简介

通过前面章节的学习，你已经可以在Scheme的交互式前端中编写并执行程序了。在本章中，我讲介绍如何输入和输出。使用这个特性，你可以从文件中读取数据或向文件中写入数据。

## 从文件输入

### open-input-file，read-char和eof-object?

函数`(open-input-file filename)`可以用于打开一个文件。此函数返回一个用于输入的端口。函数`(read-char port)`用于从端口中读取一个字符。当读取到**文件结尾（EOF）**时，此函数返回`eof-object`，你可以使用`eof-object?`来检查。函数`(close-input-port port)`用于关闭输入端口。[代码片段1]展示了一个函数，该函数以字符串形式返回了文件内容。

```scheme
(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
	  (begin
	    (close-input-port p)
	    (list->string (reverse ls1)))
	  (loop (cons c ls1) (read-char p))))))
```

比如，在[范例1]中展示的结果就是将[代码片段1]应用于文件hello.txt。由于换行符是由`'\n'`表示的，这就很容易阅读。然而，像格式化输出[范例2]，我们也可使用`display`函数。

```
Hello world!
Scheme is an elegant programming language.
```

```scheme
(cd "C:\\doc")
(read-file "hello.txt")
;Value 14: "Hello world!\nScheme is an elegant programming language.\n"
```

```scheme
(display (read-file "hello.txt"))
Hello world!
Scheme is an elegant programming language.
;Unspecified return value
```

### 语法call-with-input-file和with-input-from-file

你通过使用语法`call-with-input-file`和`with-input-from-file`来打开文件以供读取输入。这些语法是非常方便的，因为它们要处理错误。

> `(call-with-input-file filename procedure)`
>
> 该函数将名为`filename`的文件打开以供读取输入。函数`procedure`接受一个输入端口作为参数。文件有可能再次使用，因此当`procedure`结束时文件不会自动关闭，文件应该显式地关闭。[代码片段1]可以按照[代码片段2]那样用`call-with-input-file`编写。

```scheme
(define (read-file file-name)
  (call-with-input-file file-name
    (lambda (p)
      (let loop((ls1 '()) (c (read-char p)))
	(if (eof-object? c)
	    (begin
	      (close-input-port p)
	      (list->string (reverse ls1)))
	    (loop (cons c ls1) (read-char p)))))))
```

> `(with-input-from-file filename procedure)`
> 该函数将名为`filename`的文件作为标准输入打开。函数`procedure`不接受任何参数。当`procedure`退出时，文件自动被关闭。[代码片段3]展示了如何用`with-input-from-file`来重写[代码片段1]。

```scheme
(define (read-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls1 '()) (c (read-char)))
	(if (eof-object? c)
	    (list->string (reverse ls1))
	    (loop (cons c ls1) (read-char)))))))
```

### read

函数`(read port)`从端口`port`中读入一个S-表达式。用它来读诸如"paren.txt"中带括号的内容就很方便。

```
'(Hello world!
Scheme is an elegant programming language.)

'(Lisp is a programming language ready to evolve.)
```

```scheme
(define (s-read file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls1 '()) (s (read)))
	(if (eof-object? s)
	    (reverse ls1)
	    (loop (cons s ls1) (read)))))))
```

下面展示了用`s-read`读取"paren.txt"的结果。

```
(s-read "paren.txt")
⇒ ((quote (hello world! scheme is an elegant programming language.))
(quote (lisp is a programming language ready to evolve.)))
```

> 练习1
> 
> 编写函数`(read-lines)`，该函数返回一个由字符串构成的表，分别代表每一行的内容。在Scheme中，换行符是由`#\Linefeed`表示。下面演示了将该函数用于"hello.txt"的结果。
>  
> `(read-lines "hello.txt") ⇒ ("Hello world!" "Scheme is an elegant programming language.")`

## 输出至文件

### 打开一个用于输出的port

输出有和输入类似的函数，比如：

<dl>
  <dt><code>(open-output-file filename)</code></dt>
  <dd>该函数打开一个文件用作输出，放回该输出端口。</dd>

  <dt><code>(close-output-port port)</code></dt>
  <dd>关闭用于输出的端口。</dd>

  <dt><code>(call-with-output-file filename procedure)</code></dt>
  <dd>打开文件<code>filename</code>用于输出，并调用过程<code>procedure</code>。该函数以输出端口为参数。</dd>

  <dt><code>(with-output-to-file filename procedure)</code></dt>
  <dd>打开文件<code>filename</code>作为标准输出，并调用过程<code>procedure</code>。该过程没有参数。当控制权从过程<code>procedure</code>中返回时，文件被关闭。</dd>
</dl>

### 用于输出的函数

下面的函数可用于输出。如果参数`port`被省略的话，则输出至标准输出。

<dl>
  <dt><code>(write obj port)</code></dt>
  <dd>该函数将<code>obj</code>输出至<code>port</code>。字符串被双引号括起而字符具有前缀<code>#\</code>。</dd>

  <dt><code>(display obj port)</code></dt>
  <dd>该函数将<code>obj</code>输出至<code>port</code>。字符串*不被*双引号括起而字符*不*具有前缀<code>#\</code>。</dd>

  <dt><code>(newline port)</code></dt>
  <dd>以新行起始。</dd>

  <dt><code>(write-char char port)</code></dt>
  <dd>该函数向<code>port</code>写入一个字符。</dd>
</dl>

> 练习2
> 
> 编写函数`(my-copy-file)`实现文件的拷贝。

> 练习3
>
> 编写函数`(print-line)`，该函数具有任意多的字符作为参数，并将它们输出至标准输出。输出的字符应该用新行分隔。

## 小结

因为Scheme的IO设施非常的小，所以本章也十分短。下一章中，我会讲解赋值。

## 习题解答

### 答案1

```scheme
(define (group-list ls sep)
  (letrec ((iter (lambda (ls0 ls1)
		   (cond
		    ((null? ls0) (list ls1))
		    ((eqv? (car ls0) sep) 
		     (cons ls1 (iter (cdr ls0) '())))
		    (else (iter (cdr ls0) (cons (car ls0) ls1)))))))
    (map reverse (iter ls '()))))


(define (read-lines file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls1 '()) (c (read-char)))
	(if (eof-object? c)
	    (map list->string (group-list (reverse ls1) #\Linefeed))  ; *
	    (loop (cons c ls1) (read-char)))))))
```

示例：

```scheme
(group-list '(1 4 0 3 7 2 0 9 5 0 0 1 2 3) 0)
;Value 13: ((1 4) (3 7 2) (9 5) () (1 2 3))

(read-lines "hello.txt")
;Value 14: ("Hello world!" "Scheme is an elegant programming language." "")
```

### 答案2

```scheme
(define (my-copy-file from to)
  (let ((pfr (open-input-file from))
	(pto (open-output-file to)))
    (let loop((c (read-char pfr)))
      (if (eof-object? c)
	  (begin
	    (close-input-port pfr)
	    (close-output-port pto))
	  (begin
	    (write-char c pto)
	    (loop (read-char pfr)))))))
```

### 答案3

```scheme
(define (print-lines . lines)
  (let loop((ls0 lines))
    (if (pair? ls0)
        (begin
         (display (car ls0))
         (newline)
         (loop (cdr ls0))))))
```
