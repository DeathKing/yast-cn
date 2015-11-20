# 向量和结构体

## 简介

本章中，我将讲解向量和结构体。

向量是一组通过整数索引的数据。与C语言中的数组不同，一个向量可以储存不同类型的数据。与表相比，向量更加紧凑且存取时间更短。但从另外一方面来说，向量是通过副作用来操作的，这样会造成负担。

Scheme中的结构体与C语言中的结构体类似。但Scheme中的结构体比C语言中的更容易使用，因为Scheme为结构体自动创建了读取函数和写入函数，这受益于Lisp/Scheme中的宏。

## 向量

### 字面值

向量通过闭合的`#(`和`)`表示，例如`#(1 2 3)`。作为字面值（literals）时，它们应该被引用（be quoted），例如：

```scheme
'#(1 2 3)             ; 整数向量
'#(a 0 #\a)           ; 由符号、整数和字符构成的向量
```

### 向量函数

下面的函数都是R5RS规定的函数：

**(vector? obj)**

如果obj是一个向量则返回#t。
	
**(make-vector k)**

**(make-vector k fill)**
	
返回有`k`个元素的向量。如果指定了第二个参数(`fill`)，那么所有的元素都会被初始化为`fill`。

**(vector obj …)**

返回由参数列表构成的向量。

**(vector-length vector)**

返回向量`vector`的长度。

**(vector-ref vector k)**

返回向量`vector`的索引为`k`的元素。（译注：和C语言类似，向量从0开始索引。）

**(vector-set! vector k obj)**

将向量`vector`的索引为`k`的元素修改为`obj`。

**(vector->list vector)**

将`vector`转换为表。

**(list->vector list)**

将`list`转换为向量。

**(vector-fill! vector fill)**

将向量`vector`的所有元素设置为`fill`。

例：一个对向量中元素求和的函数。

```scheme
     (define (vector-add v1 v2)
       (let ((lenv1 (vector-length v1))
     	(lenv2 (vector-length v2)))
         (if (= lenv1 lenv2)
     	(let ((v (make-vector lenv1)))
     	  (let loop ((i 0))
     	    (if (= i lenv1)
     		v
     		(begin
     		  (vector-set! v i (+ (vector-ref v1 i) (vector-ref v2 i)))
     		  (loop (1+ i))))))
    	(error "different dimensions."))))
```

> 练习1
> 
> 编写一个用于计算两向量内积的函数。

## 结构体

### 大体功能

虽然R5RS中没有定义结构体，但是在很多Scheme实现中，都实现了类似于Common Lisp中的结构体。

这些结构体本质上来说都是向量。每一个槽（slot）都通过使用一个宏来命名，我将会在下一章（十五章）中讲解这个问题。结构体通过不同的属性清楚地表示数据。定义结构体的宏自动为结构体创建**取值器（accessor）**和**赋值器（setter）**。你可以通过“程序”来写程序，这被认为是Lisp/Scheme最好之处之一。通过这个功能，你可以很快写出漂亮的程序。

### MIT-Scheme中的结构体

在MIT-Scheme中，结构体通过函数`define-structure`来定义。为了使你更加容易理解，我会用一个实例来讲解。请考虑书籍。书籍都有下列属性：

+ 标题
+ 作者
+ 出版商
+ 出版年份
+ ISBN号

因此结构体book就可以像下面这样定义：

```scheme
(define-structure book title authors publisher year isbn)
```

下面演示了如何注册[“大教堂与市集（The Cathedral and Bazaar）”](http://www.oreilly.com/catalog/cathbazpaper/)。

```scheme
(define bazaar 
  (make-book 
   "The Cathedral and the Bazaar"
   "Eric S. Raymond"
   "O'Reilly"
   1999
   0596001088))
```

然而，这样做多少有点不便，因为属性与值的关联并不清楚。参量`keyword-constructor`可以用于解决这个问题。下面的代码就是使用这个参量的重写版，这个版本中，属性与值的关联就非常清楚了。此外，制定这个参量后，参数的顺序就不重要了。参量`copier`可用于为结构体创建一个拷贝（copier）函数。

```scheme
(define-structure (book keyword-constructor copier) 
  title authors publisher year isbn)

(define bazaar 
  (make-book 
   'title "The Cathedral and the Bazaar"
   'authors "Eric S. Raymond"
   'publisher "O'Reilly"
   'year 1999	
   'isbn 0596001088))
```

+ 一个名字形如`[the name of structure]?`的函数用于检查某对象是否为特定结构体。例如，可使用函数`book?`来检查`bazaar`是否为`book`结构体的一个实例。

```scheme
(book? bazaar)
;Value: #t
```

+ 一个名字形如`copy-[structure name]`的函数用于拷贝结构体。例如，下面的代码演示了将`bazaar`拷贝到`cathedral`。

```scheme
(define cathedral (copy-book bazaar))
```

+ 一个名字形如`[structure name]-[attribute name]`的函数用于读取结构体某属性的值。例如，下面的代码演示了如何读取`bazaar`的`title`属性。

```scheme
(book-title bazaar)
;Value 18: "The Cathedral and the Bazaar"
```

+ 一个名字形如`set-[结构体名称]-[属性名称]!`用于将某属性设定为特定值。下面的代码演示了如何将`bazaar`的`year`字段更新到2001（《大教堂与市集》2001年再版）。

```scheme
(set-book-year! bazaar 2001)
;Unspecified return value

(book-year bazaar)
;Value: 2001
```

请参阅[MIT/GNU Scheme Reference: 2.10 Structure Definitions](www.gnu.org/software/mit-scheme/documentation/scheme_3.html#SEC41)以获得关于结构体的跟多信息。


## The Mastermind — 一个简单的密码破解游戏

作为向量的示例，我会演示一个简单的密码破解游戏。这是一个猜对手密码的游戏。密码是由0到9中四个不同的数组成的四位数。对手要通过使用'bulls'和'cows'的数量告知猜谜者猜测的准确程度。

1. bull的数量（Nbull）是指值和位置都正确的数字的数量。
2. cow的数量（Ncow）是指值正确但位置错误的数字的数量。

例如，密码是5601，猜测是1685，那么bull和cow和数分别是1和2。

计算机和用户相互猜测对方的密码。更少尝试次数的选手为胜利者。如果用户和电脑在相同的尝试次数中破解了密码就是平局。

### 表示四个数字

四位数字可以通过向量和计算bull以及cow的数量高效地表示。这种表达方法需要构成密码的数字都不相同。

创建长度为10的向量，每个索引（`k`）的值被设为`k`在密码中的数位。四个数位从低到高被计为1，2，3和4。如果数字没有出现，索引的值为0。例如，5601和1685可以表示如下：

```
5601 → #(2 1 0 0 0 4 3 0 0 0)
1685 → #(0 4 0 0 0 1 3 0 2 0)
```

5601这个例子中，数字0，1，5，和6分别出现在第2，第1，第4和第3位，那么在这个密码的向量表达式里索引0，1，5，6的值分别2是2，1，4和3，其他索引位都是0。

这种表达可以快速比较两个数字。如果两个向量的相同索引位的值都是正数情况下，如果值相等，就计为bull，如果值不相等，就计为cow。5601和1685这个例子的情况下，索引位6的值都为3，索引位1和索引位5的值都是正数，bull和cow的值为1和2。

### 程序的设计

程序的设计如下：

1. 程序生成一个表，该表包含了所有不同四位数的向量表示。
2. 程序从表中随机选取一个数字。
3. 重洗步骤（1）产生的表。
4. 程序首次猜用户的密码，用户给出bull和cow的数量。然后用户猜程序的密码，程序给出Nnull和Ncow。
5. 重复步骤（3）直到电脑或者程序的bull数量变为4为止。如果在同一次双方的数量都变为4，就是平局。

### 源代码

[代码1]展示了源代码。代码很长但并不十分复杂。游戏由一个递归函数mastermind-rec执行。

[代码1]

```scheme
 01:     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 02:     ;;;
 03:     ;;; mastermind.scm
 04:     ;;; by T.Shido
 05:     ;;;
 06:     ;;; User and computer try to locate the four-digit integer set by the opponents each other.
 07:     ;;; One who locates the integer with fewer question is the winner.
 08:     ;;; The four-digit integer contains four of numerals 0--9, like 0123, 3749 etc.
 09:     ;;; The opponents should tell the guesser
 10:     ;;; (1) number of numerals that are shared by the guessed and set numbers
 11:     ;;; at wrong position (cows)
 12:     ;;; and (2) number of numerals at collect position (bulls).
 13:     ;;; 
 14:     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 15:     ;;;
 16:     ;;; The four-digit integers are represented by 10-cell vectors in the program
 17:     ;;; The value of n-th cell is the number of column that n appears in the integer.
 18:     ;;; in n is not appears the value is 0.
 19:     ;;; for example, 1234 is represented as #(0 4 3 2 1 0 0 0 0 0) and
 20:     ;;; 3916 as #(0 2 0 4 0 0 1 0 0 3).
 21:     ;;; With this inner representation, the score of the guess can be calculated faster.
 22:     ;;;
 23:     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 24:     
 25:     
 26:     ;;;
 27:     (define (1- x) (- x 1))
 28:     
 29:     ;;;
 30:     (define (char2int c)
 31:       (- (char->integer c) (char->integer #\0)))
 32:     
 33:     ;;; converting a list of 4 numbers to the vector notation
 34:     (define (ls2nvec ls)
 35:       (let ((vec (make-vector 10 0)))
 36:         (let loop ((i (length ls)) (ls ls))
 37:           (if (> i 0)
 38:     	  (begin
 39:                (vector-set! vec (car ls) i)
 40:                (loop (1- i) (cdr ls)))
 41:             vec))))
 42:     
 43:     ;;; converting the vector notation to string
 44:     (define (nvec2int vec)
 45:       (let loop ((i 0) (n 0))
 46:         (if (= i 10)
 47:             n
 48:     	(let ((j (vector-ref vec i)))
 49:     	  (loop (1+ i) (+ n (if (> j 0)
 50:                                     (* i (expt 10 (1- j)))
 51:                                   0)))))))
 52:     
 53:     ;;;
 54:     (define (int2str i)
 55:       (string-append
 56:        (if (< i 1000) "0" "")
 57:        (number->string i)))
 58:     
 59:     ;;; reading integer from stdin
 60:     (define (read-integer str)
 61:       (string->number (read-from-stdin str)))
 62:     
 63:     ;;;
 64:     (define (read-from-stdin str)
 65:       (display str)
 66:       (newline)
 67:       (read-line))
 68:     
 69:     ;;;
 70:     (define (write-to-stdout . ls)
 71:       (for-each (lambda (obj) (display obj)) ls)
 72:       (newline))
 73:     
 74:     ;;; convert numeral string to the vector representation.
 75:     (define (str2nvec str)
 76:       (let ((vec (make-vector 10 0)))
 77:         (let loop ((i (string-length str)) (ls (string->list str)))
 78:           (if (pair? ls)
 79:     	  (begin
 80:                (vector-set! vec (char2int (car ls)) i)
 81:                (loop (1- i) (cdr ls)))
 82:             vec))))
 83:     
 84:     ;;; calculating the score of guess
 85:     (define (scoring vec0 vec1)
 86:       (let ((n (vector-length vec0)))
 87:         (let loop ((i 0) (score 0))
 88:           (if (< i n)
 89:     	  (let ((d0 (vector-ref vec0 i))
 90:                    (d1 (vector-ref vec1 i)))
 91:                 (loop (1+ i)
 92:     		  (+ score (if (and (< 0 d0) (< 0 d1))
 93:                                    (if (= d0 d1) 5 1)
 94:                                    0))))
 95:             score))))
 96:     
 97:     ;;; show bulls and cows calculated from the score of user's guess
 98:     (define (show-user-score score)
 99:       (write-to-stdout "Number of bulls and cows in your guess:" )
100:       (write-to-stdout "bulls: " (quotient score 5))
101:       (write-to-stdout "cows: " (modulo score 5))
102:       (newline))
103:     
104:     ;;; calculating the score of computer's guess from bulls and cows
105:     (define (read-my-score gu0)
106:       (write-to-stdout "My guess is: " (int2str (nvec2int gu0)))
107:       (write-to-stdout "Give number of bulls and cows in my guess." )
108:       (let ((na5 (* 5 (read-integer "bulls: "))))
109:         (+ na5 (read-integer "cows: ")))) ; the score is calculated by (5 * bull + cow)
110:     
111:     ;;; reading the user guess
112:     (define (read-user-guess)
113:       (newline)
114:       (str2nvec (read-from-stdin "Give your guess.")))
115:     
116:     ;;; shuffling the list of four-digit numbers
117:     (define (shuffle-numbers ls0)
118:       (let ((vec (list->vector ls0)))
119:         (let loop ((n (vector-length vec)) (ls1 '()))
120:           (if (= n 0)
121:               ls1
122:     	  (let* ((r (random n))
123:     		 (v (vector-ref vec r)))
124:     	    (vector-set! vec r (vector-ref vec (1- n)))
125:     	    (loop (1- n) (cons v ls1)))))))
126:     
127:     ;;; making a list of four-digit numbers in which numeral 0--9 appear once
128:     (define (make-numbers)
129:       (let ((ls1 '()))
130:         (letrec ((rec (lambda (i num ls)
131:     		    (if (= i 4)
132:     			(set! ls1 (cons (ls2nvec ls) ls1))
133:     			(for-each 
134:     			 (lambda (n)
135:     			   (rec (1+ i) (delv n num) (cons n ls)))
136:     			 num)))))
137:           (rec 0 '(0 1 2 3 4 5 6 7 8 9) '()))
138:         ls1))
139:     
140:     ;;;
141:     (define (game-over sc0 sc1)
142:       (write-to-stdout
143:        (cond
144:         ((= sc0 sc1) "Draw")
145:         ((> sc0 sc1) "I won.")
146:         (else "You won.")))
147:       'game-over)
148:     
149:     (define (scoring-user-guess an0 gu1)
150:       (let ((sc1 (scoring an0 gu1)))
151:         (show-user-score sc1)
152:         sc1))
153:     
154:     ;;; Practical main function. tail recursive.
155:     (define (mastermind-rec an0 candidates)
156:       (if (null? candidates)
157:           (error "Error. You gave wrong score for my guess, probably.")
158:           (let ((gu0 (car candidates)))
159:     	(let ((sc1 (scoring-user-guess an0 (read-user-guess)))
160:     	      (sc0 (read-my-score gu0)))
161:     	  (if (or (= sc0 20) (= sc1 20))
162:     	      (game-over sc0 sc1)
163:     	      (mastermind-rec an0 
164:     			   (keep-matching-items 
165:     			    (cdr candidates)
166:     			    (lambda (x) (= (scoring gu0 x) sc0)))))))))
167:     
168:     ;;; The main function called from the top-level
169:     (define (mastermind)
170:       (let ((ls0 (make-numbers)))
171:         (mastermind-rec (list-ref ls0 (random (length ls0))) (shuffle-numbers ls0))))
```

行数   |函数                            |说明
:------|:--------------------------------|:-----------------
27   |`(1- x)`                         |`x`减一
30   |`(char2int c)`                   |将字符`c`(#\0 -- #\9)转换为整数（0 -- 9）。
34   |`(ls2nvec ls)`                   |将四个数字的表（`ls`）转换为向量表达式。`'(5 3 6 0)->#(1 0 0 3 0 4 2 0 0 0)`
44   |`(nvec2int vec)`                 |将向量表达式`vec`转换为普通整数。
54   |`(int2str i)`                    |将一个四位数`i`转换为字符串。如果`i`小于1000，'0'被置于高位。
64   |`(read-from-stdin str)`          |将`str`显示于标准输出，并返回用户从标准输入输入的字符串。
70   |`(write-to-stdout . ls)`         |将`ls`的每个元素都输出到标准输出，并在行尾插入行结束符。
75   |`(str2nvec str)`                 |将用户输入的表示四位数的字符串`str`转换为向量表达式。
86   |`(scoring vec0 vec1)`            |以（5*Nnull + Ncow）计算两个整数（向量表达式）`vec0`和`vec1`的相似程度。
98   |`(show-user-score score)`        |通过相似度`score`计算Nbull和Ncow，并将它们显示在标准输出。
105  |`(read-my-score gu0)`            |显示计算机的猜测（gu0），让用户输入Nnull和Ncow，返回相似度score。
112  |`(read-user-guess)`              |返回用户猜测的向量表达式。
116  |`(shuffle-numbers ls0)`          |随机排序`ls0`。由于有随机读取的需求，将`ls0`转换为向量，然后随机读取向量的元素，以创建一个重排过的表。
128  |`(make-numbers)`                 |返回由所有不同四位数构成的表。
141  |`(game-over sc0 sc1)`            |通过比较计算机的得分（sc0）和用户的得分(sc1)确定胜利者。
149  |`(scoring-user-guess an0 gu1)`   |计算计算机的密码（an0）和用户的猜测（gu1）的相似度，使用show-uuser-score输出Nbull和Ncow。
155  |`(mastermind-rec an0 candidates)`|实际的主程序，它有两个参数；计算机密码（an0）和 猜测的表（candidate）。它计算计算机的得分（sc0）和用户的得分（sc1），如果`sc0`或者`sc1`为20，调用 (game-over sc0 sc1)。如果没有值为20，它根据`sc0`过滤猜测的表（candidate），并继续游戏。
169  |`(mastermind)`					 |在控制台调用该函数以开始游戏。

### 如何玩

输入如下代码启动游戏。最好在玩之前编译（你需要编译一次）。即使程序很简单，也很难取胜。

```scheme
(compile-file "mastermind.scm")
(load "mastermind")
(mastermind)
```

## 小结

这一章，我通过玩`mastermind`游戏讲解了向量和结构体。附上[`mastermind`的源代码](http://www.shido.info/lisp/scheme_vec.zip)。

我将在下一章讲自定义语法。自定义语法是Lisp/Scheme的一个优点。

## 习题解答

### 答案1

```scheme
     (define (inner-product vec1 vec2)
       (let ((len1 (vector-length vec1))
     	(len2 (vector-length vec2)))
         (if (= len1 len2)
     	(let loop ((i 0) (pro 0))
     	  (if (= i len1)
     	      pro
     	      (loop (1+ i) (+ pro
     			      (* (vector-ref vec1 i)
     				 (vector-ref vec2 i))))))
     	(error "different dimensions."))))
```
