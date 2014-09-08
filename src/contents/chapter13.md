# 关联表和哈希表

## 简介

本章中，我会讲解用于表示数据关联的关联表和哈希表。关联的数据室由键和值组成的序对，值由键唯一定义。表1显示了书籍和作者构成的配对。书籍可以确定作者，反之由作者定义书籍则不可，这是因为一个作者可能会写很多本书。表1中，由于P. Graham和L.Carroll分贝写了两本书，因此他们的书无法被作者的名字唯一定义。

表1

Author	Book
P. Graham	On Lisp
P. Graham	ANSI Common Lisp
E. S. Raymond	The Cathedral and the Bazaar
K. Dybvig	The Scheme Programming Language
F. P. Brooks, Jr.	The Mythical Man-Month
L. Carroll	Alice's Adventures in Wonderland
L. Carroll	Through the Looking-Glass, and What Alice Found There

R^5RS定义了关联表，因此它再所有Scheme实现中都可用。

