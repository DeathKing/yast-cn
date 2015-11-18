# 安装MIT-Scheme

## 为什么使用Scheme

使用Scheme，你可以：

+ 编写漂亮的程序。
+ 享受编程的乐趣。

这些就是为什么要学习Scheme的原因。在你用Scheme编写一些实用程序的时候会遇到一些困难。

然而，正因为这是一门值得学习的语言，所以许多卓越的黑客钟爱Scheme。事实上，计算机程序的构造和解释（Structure and Interpretation of Computer Programs，SICP）——最好的计算机科学教科书之一——使用Scheme来描述演示程序。GNU也使用Scheme（一种被称作guile的实现）作为其应用软件的通用脚本语言。guild相当于MS-Word或者Excel（原文是Excell，应该是作者的笔误，译者注）中的宏。它被用来通过简单的脚本来操作应用程序。  

尽管Common Lisp更加适合构建实用应用程序，但我依然推荐你首先学习Scheme，因为这门语言：  

+ 设计紧凑
+ 语法简单

业界大牛提出过“Scheme使你成为更棒的程序员”的看法。即是你很少在商业项目上使用Scheme，但学习Scheme获得的良好感觉将会指导你使用其它的编程语言。

网络上的Scheme教程（真是多如牛毛）总是或多或少的有些困难，因而不太适合初学者。这样来说的话，本教程是面向新手程序员的，他们只需要对编程有一点了解即可。

## 目标读者

本教程的目标读者是仅有一点编程经验的PC用户，例如：

+ 教授使用Scheme授课，无法跟上进度的学生。
+ 想要学习编程的人。

Scheme的语法相当地简单，并且可以通过一个简单的方式解释清楚。尽管如此，对初学者来说这种解释还是太困难了。在本教程中，我会循序渐进地讲解。

Scheme代码仅由单词，括号和空格组成，这些最初可能会使你感到烦扰。然而，如果你使用了一个合适的编辑器，它会为你展示配对的括号和自动缩进。因此，你不用担心括号的配对，并且你可以通过缩进来阅读代码。如果缩进看起来很奇怪，你可以用编辑器找出错误的配对。

## 安装MIT-Scheme

这节是面向Windows用户的教程。我没有使用Macintosh的经验，因此无法提供给你相关的帮助。如果你是Unix（或者Linux）用户，（如果你无法自己安装）请让管理员安装它。Scheme的使用并不依赖于操作系统。只有安装才会因不同的系统而不同。

Scheme程序设计语言中有一些规范，最新的规范在Revised5 Report on the Algorithmic Language Scheme (R5RS)。

大多数的实现都是（完全或者部分地）基于R5RS。如果你使用的是部分符合R5RS的实现，那么在使用时你就得当心一点。在Windows系统上有很多免费的Scheme实现，比如：ChezScheme, MzScheme, DrScheme, SCM。在本教程中，我使用MIT/GNU Scheme，因为它高效并且非常容易安装。MIT-Scheme的解释器十分快速，除此之外它还能够将你的程序编译为本地代码。MIT-Scheme的问题就是它并不完全符合R5RS规范。稍后我会详细说明这点。事实上，只有MIT-Scheme和DrScheme有安装包。有人推荐DrScheme，但是它太慢了。如果你有手动安装软件的能力，我推荐你安装Petite Chez Scheme。这是一个运行在命令提示符（DOS Windows）下的非常棒的解释器。

《Scheme实现》比较了几种Scheme实现。当你习惯Scheme后，去尝试几种不同的实现将会是很好的主意。或许你需要一台Linux机器，因为大多数Scheme实现都是只能运行在Unix和Linux上的。

### 如何在Windows上安装MIT-Scheme

MIT-Scheme可以简单地通过下载并执行安装包来进行安装。

1. 访问MIT/GNU Scheme的主页，下载适用于Windows的二进制包： mit-scheme-N.N.N-ix86-win32.exe。
2. 双击下载好的安装包。安装包会询问一些事项，按照默认的设置进行即可。
3. 安装完毕后，有4个快捷方式被创建出来，分别是：Scheme，Compiler，Edwin和Documentation。Scheme，Compiler和Edwin都是指向同一个程序的快捷方式，但它们调用程序的参数不同。使用Compiler，你可以把程序编译为本地代码，这样可使你的程序运行时间更短。但反过来说，Compiler会消耗更多的内存。Edwin是一个类Emacs的编辑器，用于编辑Scheme程序。你可以使用这个编辑器或者你最喜欢的编辑器。
4. 你可以通过编辑配置文件scheme.ini来自定义MIT-Scheme。
scheme.ini文件的创建路径是由环境变量`HOMEPATH`决定的。你可以通过在命令提示符（DOS Windows）中输入`>set HOMEPATH`来取得`HOMEPATH`的值。在WinXP中，`HOMEPATH`被预定义为：`\Document and Setting\username`  

下面的代码给出了一个scheme.ini文件的例子：

```scheme
(cd "C:\\doc\\scheme")
(define call/cc call-with-current-continuation)
```

第一行代码代表将工作目录切换到C:\doc\scheme。通过这条代码，MIT-Scheme移动工作路径切换到这个路径，你不需要再敲击程序的绝对路径来载入Scheme程序。第二行是定义`call-with-current-continuation`的缩略词。

## 小结

安装非常容易（除了编辑scheme.ini文件），你应该毫无问题。

下一章节是如何与MIT-Scheme前端会话。

