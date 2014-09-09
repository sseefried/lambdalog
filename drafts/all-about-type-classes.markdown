---
title: What I know about type classes and their instances
category: haskell
tags: Haskell, type classes
---


# Monad

## `Monad (->) r`

`join` on functions returns the diagonal in *Cantor's diagonalisation*. 
~~~
join :: r -> (r -> a) -> (r -> a)  
join mm = \r -> mm r r
~~~