---
title: Haskell GADTs in Scala
author: Sean Seefried
tags: Haskell, Scala, GADTs
hours: 
---

<!-- 
I've had some really cool ideas for this post. Wouldn't it be cool if all the code
I wrote in this module were actually real code? Wouldn't it be cool if I could insert
expressions that one might put into GHCi and have that execute and be inserted into the
post?

Anything to be compiled should be in the follow tags.

~~~{.haskell .code}
~~~

Parameters to the compiler should be in a HTML comment and look something like:

ghc-params: --make

-->

~~~{.haskell}
{-# LANGUAGE GADTs #-}
module Exp where

data Exp a where
  LitInt  :: Int                        -> Exp Int
  LitBool :: Bool                       -> Exp Bool
  Add     :: Exp Int -> Exp Int         -> Exp Int
  Mul     :: Exp Int -> Exp Int         -> Exp Int
  Cond    :: Exp Bool -> Exp a -> Exp a -> Exp a
  EqE     :: Eq a => Exp a -> Exp a     -> Exp Bool

eval :: Exp a -> a
eval e = case e of
  LitInt i       -> i
  LitBool b      -> b
  Add e e'       -> eval e + eval e'
  Mul e e'       -> eval e * eval e'
  Cond b thn els -> if eval b then eval thn else eval els
  EqE e e'       -> eval e == eval e'
~~~

~~~{.scala}
abstract class Exp[A] 

case class LitInt(i: Int)                                     extends Exp[Int]
case class LitBool(b: Boolean)                                extends Exp[Boolean]
case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A]
case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean]
~~~

So you might think this is the way to define `eval`. I mean, it looks like the Haskell
version right? 

~~~{.scala}
abstract class Exp[A] {
  def eval = this match {
    case LitInt(i)       => i
    case LitBool(b)      => b
    case Add(e1, e2)     => e1.eval + e2.eval
    case Mul(e1, e2)     => e1.eval * e2.eval
    case Cond(b,thn,els) => if ( b.eval ) { thn.eval } else { els.eval }
    case Eq(e1,e2)       => e1.eval == e2.eval
  }

}

case class LitInt(i: Int)                                     extends Exp[Int]
case class LitBool(b: Boolean)                                extends Exp[Boolean]
case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A]
case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean]
~~~

But actually you need to do this.

~~~{.scala}
abstract class Exp[A] {
  def eval: A
}

case class LitInt(i: Int)                                     extends Exp[Int] {
  def eval = i
}

case class LitBool(b: Boolean)                                extends Exp[Boolean] {
  def eval = b
}

case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int] {
  def eval = e1.eval + e2.eval
}
case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int] {
  def eval = e1.eval * e2.eval
}
case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A] {
  def eval = if ( b.eval ) { thn.eval } else { els.eval }
}
case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean] {
  def eval = e1.eval == e2.eval
}
~~~


<!--
~~~{.haskell}
~~~
-->

<!--
~~~{.scala}
Here's some scala code
~~~
-->