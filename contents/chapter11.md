# 字符与字符串

## 简介

我只介绍了表和数，因为它们在Scheme中最为常用。然而，Scheme也有像**字符（Character）**、**字符串（String）**、**符号（Symbol）**、**向量（Vector）**等的其它数据类型，我将在11到14章节中介绍它们。

## 字符

在某个字符前添加`#\`来表明该物是一个字符。例如，`#\a`表示字符a。字符`#\Space`、`#\Tab`、`#\Linefeed`和`#\Return`分别代表空格（Space）、制表符（Tab），Linefeed和返回（Return）。R5RS中定义了下面的与字符相关的函数。

<dl>
  <dt><code>(char? obj)</code></dt>
  <dd>如果obj是一个字符则返回<code>#t</code>。</dd>

  <dt><code>(char=? c1 c3)</code></dt>
  <dd>如果c1和c2是同一个字符的话则返回<code>#t</code>。</dd>

  <dt><code>(char->integer c)</code></dt>
  <dd>将c转化为对应的整数（字符代码，character code）。示例：<code>(char->integer #\a) => 97</code></dd>

  <dt><code>(integer->char n)</code></dt>
  <dd>该函数将一个整数转化为对应的字符。</dd>

  <dt><code>(char<? c1 c2)</code></dt>
  <dt><code>(char<= c1 c2)</code></dt>
  <dt><code>(char> c1 c2)</code></dt>
  <dt><code>(char>= c1 c2)</code></dt>
  <dd>这些函数用于比较字符。实际上，这些函数比较的是字符代码的大小。例如，<code>(char<? c1 c2)</code>等同于<code>(< (char->integer c1) (char->integer c2))</code></dd>

  <dt><code>(char-ci=? c1 c2)</code></dt>
  <dt><code>(char-ci<? c1 c2)</code></dt>
  <dt><code>(char-ci<=? c1 c2)</code></dt> 
  <dt><code>(char-ci>? c1 c2)</code></dt>
  <dt><code>(char-ci>=? c1 c2)</code></dt>
  <dd>这些比较函数对大小写不敏感。</dd>

  <dt><code>(char-alphabetic? c)</code></dt>
  <dt><code>(char-numeric? c)</code></dt>
  <dt><code>(char-whitespace? c)</code></dt>
  <dt><code>(char-upper-case? c)</code></dt>
  <dt><code>(char-lower-case? c)</code></dt>
<dd>这些函数分别用于检测字符c是否为字母、数字、空白符、大写字母或小写字母。</dd>

  <dt><code>(char-upcase c)</code></dt>
  <dt><code>(char-downcase c)</code></dt>
    <dd>这些函数分别返回字符C对应的大写或小写。</dd>
</dl>

## 字符串

字符串通过两个闭合的双引号表示。例如，”abc”表示字符串abc。R5RS定义了下面的函数。

<dl>
  <dt><code>(string? s)</code></dt>
  <dd>如果<code>s</code>是一个字符则返回<code>#t</code>。</dd>

  <dt><code>(make-string n c)</code></dt>
  <dd>返回由<code>n</code>个字符<code>c</code>组成的字符串。参数<code>c</code>可选。</dd>
  
  <dt><code>(string-length s)</code></dt>
  <dd>返回字符串<code>s</code>的长度。</dd>
  
  <dt><code>(string=? s1 s2)</code></dt>
  <dd>如果字符串<code>s1</code>和<code>s2</code>相同的话则返回<code>#t</code>。</dd>

  <dt><code>(string-ref s idx)</code></dt>
  <dd>返回字符串<code>s</code>中索引为<code>idx</code>的字符（索引从0开始计数）。</dd>

  <dt><code>(string-set! s idx c)</code></dt>
  <dd>将字符串<code>s</code>中索引为<code>idx</code>的字符设置为<code>c</code>。</dd>

  <dt><code>(substring s start end)</code></dt>
  <dd>返回字符串<code>s</code>从<code>start</code>开始到<code>end-1</code>处的子串。例如<code>(substring "abcdefg" 1 4) => "b c d"</code></dd>

  <dt><code>(string-append s1 s2 ...)</code></dt>
  <dd>连接两个字符串<code>s1</code>和<code>s2</code></dd>

  <dt><code>(string->list s)</code></dt>
  <dd>将字符串<code>s</code>转换为由字符构成的表。</dd>

  <dt><code>(list->string ls)</code></dt>
  <dd>将一个由字符构成的表转换为字符串。</dd>

  <dt><code>(string-copy s)</code></dt>
  <dd>复制字符串<code>s</code>。</dd>
</dl>

> 练习1
> 
> 编写一个函数`title-style`，该函数用于将每个单词的首字母大写。
>
> ```scheme
> (title-style "the cathedral and the bazaar")
> ;⇒ "The Cathedral And The Bazaar"
>```

## 小结

本章讲解了字符和字符串。下章我将讲解符号。符号是Lisp/Scheme中的一种字符型数据类型。使用这种数据类型，可以对文本进行快速操作。

## 习题解答

### 练习1

先将字符串转化为表，将空格之后的字符大写，最后将表转换会字符串。【译注：原文似有误。】

```scheme
(define (identity x) x)

(define (title-style str)
  (let loop ((ls (string->list str))
         (w #t)
         (acc '()))
    (if (null? ls)
    (list->string (reverse acc))
    (let ((c (car ls)))
      (loop (cdr ls)
        (char-whitespace? c)
        (cons ((if w char-upcase identity) c) acc))))))

;;; Another answer, You can assign caps to the string.
(define (title-style str)
  (let ((n (string-length str)))
    (let loop ((w #t) (i 0))
      (if (= i n)
      str
      (let ((c (string-ref str i)))
        (if w (string-set! str i (char-upcase c)))
        (loop (char-whitespace? c) (1+ i)))))))

(title-style "the cathedral and the bazaar")
;⇒ "The Cathedral And The Bazaar"
```
