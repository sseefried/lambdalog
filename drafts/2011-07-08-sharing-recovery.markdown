---
title: Sharing Recovery in deeply embedded DSLs
author: Sean Seefried
tags: domain specific languages, deep embedding, sharing
---

# Introduction

I could easily start this post by banging on about how great embedded domain specific languages are but that's already been done quite adequately in a [number]() [of]() [places](). There are a few options when it comes to implementing domain specific languages. They can be a

* [shallow embeddeding]() or [internal]() DSL. In this case the terms of the [target language](LINK ME) are terms in the [host language](). e.g. functions in the target language are functions in the host language. In this case one runs a program by compiling the program. The code generated is just the code that is normally generated for the host language since terms in the target language *are* terms in the host language.

* [deeply embedding]() or [external]() DSL. In this case the terms of the target language are instances of a data structure in the host language (usually an [abstract syntax tree]() (AST)). The terms are constructed using various helper functions and are interpreted or compiled. This differs from the shallow embedding case because one must write the interpreter/compiler and does not gain the benefit of reusing the host language's compiler.

A not immediately obvious problem that occurs when one implements a deeply embedded DSL is that one can easily lose the *term sharing* implicitly present in the target language. I'll demonstrate this with an example. Say our target language is the [simply typed lambda calculus]() with arithmetic. A program
in this language might look something like:

$(2+3) + (2+3)$

One might write this in Haskell (using some clever overloading of arithmetic operators) as:

~~~{.haskell}
prog :: Exp Int
prog = let v = 2 + 3 in v + v
~~~

I'll give you the definition of this language soon but for the moment just trust me when I say this Haskell program generates the following AST

~~~{.haskell}
Add (Add (Const 2) (Const 3)) (Add (Const 2) (Const 3))
~~~

Notice that the sharing implicit in the let-expression of Haskell has now been removed. What we would prefer it to generate is something more like:

~~~{.haskell}
LetE (Add (Const 2) (Const 3)) (Add (Lvar 0) (Lvar 0))
~~~

We're using [de Bruijn indices]() to refer to variables. Here we have introduced a *let-node* which binds let-variable $0$ to the expression $2 + 3$ in the expression ${0} + {0}$ (using the notation that ${x}$ refers to let-variable (<code>Lvar</code>)with de Bruijn index $x$).

## Memory usage is the real problem

You might be thinking, what's the big deal? Who cares if a term gets replicated a few times. Can't we always recover the sharing using *common sub-expression elimination* ([CSE]())? This is true (if inefficient) but consider the case where one wants to generate the (unrolled) code that finds the $n$th Fibonacci number.

~~~{.haskell}
fib :: Int -> HOAS.Exp Int
fib x = fib' x 1 1
  where
   fib' :: Int -> HOAS.Exp Int -> HOAS.Exp Int -> HOAS.Exp Int
   fib' x e1 e2
    | x == 0    = e1
    | otherwise = fib' (x - 1) e2 (e1 + e2)
~~~

<code>fib 4</code> produces:

~~~{.haskell}
Add (Add (Add (Const 1) (Const 1)) (Const 1)) (Add (Const 1) (Const 1))
~~~

There's something a little special about this function. Depending on the value of the integer passed to <code>fib</code> the programs generates an AST. We call this style of program a *generator*. Just from the type alone it's not always possible to tell whether a program is a generator or not. However, in general a function is a generator if it creates an AST based on the value of a host language term (i.e. <code>Int</code> in this case). We cannot actually write the fibonacci function in the simply type lambda calculus since it does not contain the notion of recursion.

In fact, function <code>fib</code>, will produce an AST of size proportional to the $n$th fibonacci number! As soon as the AST is traversed -- whether it's to interpret it, generate code for it or transform it to another intermediate form -- this will quickly consume all of the machine's available memory as $n$ increases.

We saw what <code>fib 4</code> produces without sharing recovery. What we really want it to produce is:

~~~{.haskell}
Let (Const 1) 
    (Let (Add (Lvar 0) (Lvar 0)) 
         (Add (Lvar 0) (Add (Lvar 1) (Lvar 0))))
~~~

or equivalently $let v0 = 1 in let v1 = v0 + v0 in v0 + (v1 + v0)$

## What it looks like in the heap