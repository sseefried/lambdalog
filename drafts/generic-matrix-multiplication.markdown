---
title: Generic matrix multiplication
category: haskell
tags: Haskell, type classes, dot product, matrix multiplication, program derivation
---

This post is a follow-up to my earlier post on [generic dot products](posts/2011-06-27-generic-dot-products.html). In it I introduced a `dot` function that
would calculate the dot product on data structures with their *shape* encoded in their types. 
Given an arbitrary shaped data structure all that is necessary is to define `Functor`, `Applicative` and `Foldable` instances and one gets the ability to take the dot product of two values of the same shape for free.

Today we'll build upon this work and define a function `mmult` that performs a generalised form of matrix multiplication.  But first we must define what we mean by a *generalised matrix*

# Generalised Matrices

For some the notion that a matrix could be anything other than a two-dimensional array of numbers might seem strange, but if we think a little more deeply about what it is we do when we multiply two matrices together we see that the idea is quite generalisable.

The standard matrix multiplication algorithm goes as follows. If we take a matrix, $A$ with $m$ rows and $n$ columns (an $m \cross n$ matrix) and multiply it by, $B$, an $n \cross p$ matrix we yield, $C$, an *m × p* matrix. To determine the value at position $(i,j)$ in $C$ we take the dot product of row *i* in $A$ and column *j* in $B$.

We can see from this that we are really treating a matrix as being a vector of vectors. You can think of each column of $A$ as really being a collection of vectors, which are the rows of $A$. That is, it is a vector of row vectors. (As for matrix $B$ we can think of that as being the *dual* structure; it is a vector of column vectors.)
