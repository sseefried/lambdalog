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

This is an updated version of an earlier post. Owing to a comment by Jed Wesley-Smith I restructured this post somewhat to introduce two techniques for programming with [GADTs](gadts) in [Scala](scala). Thanks also go to [Tony Morris](http://blog.tmorris.net/).

First we'll start with a fairly canonical example of why GADTs are useful in Haskell. 

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

Here we have defined a data structure that represents the abstract syntax tree (AST) of a very
simple arithmetic language. Notice that it ensures terms are well-typed. For instance something like the following just doesn't type check.

~~~{.haskell}
LitInt 1 `Add` LitBool True -- this expression does not type check
~~~

I have also provided a function `eval` that evaluates terms in this language.

In Scala it is quite possible to define data structures which have the same properties as a GADT declaration in Haskell. You can do this with *case classes* as follows.

~~~{.scala}
abstract class Exp[A] 

case class LitInt(i: Int)                                     extends Exp[Int]
case class LitBool(b: Boolean)                                extends Exp[Boolean]
case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A]
case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean]
~~~

But how do we implement `eval`. You might think that the following code would work. I mean, it looks like the Haskell version, right? 

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

Unfortunately for us, this doesn't work. The Scala compiler is unable to instantiate the type `Exp[A]` to more specific ones (such as `LitInt` which extends `Exp[Int]`)

~~~
3: constructor cannot be instantiated to expected type;
  found   : FailedExp.LitInt
  required: FailedExp.Exp[A]
    case LitInt(i)       => i
        ^
~~~

There are two solutions to this problem.

# Solution 1: The object-oriented way

You must write `eval` the object-oriented way. The definition of `eval` gets spread over
each of the sub-classes of `Exp[A]`.

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

# Solution 2: The functional Haskell-like way

Personally I don't like the OO style as much as the Haskell-like style. However, it turns out that you can program in that style by using a companion object.

~~~{.scala}
object Exp {
  def evalAny[A](e: Exp[A]): A = e match {
    case LitInt(i)         => i
    case LitBool(b)        => b
    case Add(e1, e2)       => e1.eval + e2.eval
    case Mul(e1, e2)       => e1.eval * e2.eval
    case Cond(b, thn, els) => if (b.eval) { thn.eval } else { els.eval }
    case Eq(e1, e2)        => e1.eval == e2.eval
  }
}

abstract class Exp[A] {
  def eval: A = Exp.evalAny(this)
}

case class LitInt(i: Int)                                     extends Exp[Int]
case class LitBool(b: Boolean)                                extends Exp[Boolean]
case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A]
case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean]
~~~

Ah, much better. But why does this work when the previous style doesn't? The problem is that the constructors are not polymorphic. In Haskell-speak the type is:

~~~{.haskell}
LitInt :: Int -> Exp Int
~~~

not 

~~~{.haskell}
LitInt :: Int -> Exp a
~~~

The second solution is subtly different. Method `evalAny` is polymorphic but its type is instantiated to that of the value of whatever it is called on. For instance `evalAny` when called on `LitInt(42)` equates type variable `A` with `Int`. It can then correctly deduce that it does indeed take a value of `Exp[Int]` and produce a value of `Int`. 


<!--
~~~{.haskell}
~~~
-->

<!--
~~~{.scala}
Here's some scala code
~~~
-->

[gadts]: http://www.haskell.org/haskellwiki/GADT
[scala]: http://www.scala-lang.org/