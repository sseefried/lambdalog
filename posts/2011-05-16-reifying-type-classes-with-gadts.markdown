---
title: Reifying Type Classes with GADTs
author: Sean Seefried
tags: GADTs, Haskell, type classes
---

This year I've been contributing to
[Accelerate](http://hackage.haskell.org/package/accelerate), an
embedded language for regular, multi-dimensional array computations
targetting high-performance back-ends such as NVIDIA GPUs. Even a
cursory glance of the code base shows that it uses a number of
advanced Haskell extensions including *Generalized Algebraic
Data Types* ([GADTs](http://www.haskell.org/haskellwiki/GADT)) and
[type families](http://www.haskell.org/haskellwiki/GHC/Indexed_types).

Earlier this year I learned a technique which seems to be part of the
Haskell folklore but not necessarily well known: reifying types
belonging to a type class into a GADT value. A typical use-case for
this technique is using a type class defined in a library. Either you
don't have access to the source code (or for reasons of modularity/API
stability you don't want to touch it) but, nevertheless, you want to
add a method to the type class. For argument's sake let's call this
type class <code>C</code>.

The standard solution would be to declare a new type class, let's call it <code>D</code>, with
a super-class constraint on class <code>C</code>. (e.g. <code>class C a => D a</code>). You would
then have to write one instance of class <code>D</code> for each existing instance of class
<code>C</code>. Not only could this be a lot of work, you could run into the same problem of
needing to extend class <code>D</code> further down the track (perhaps necessitating the
declaration of class <code>E</code>).

What if there were a way to allow class <code>C</code> to be arbitrarily extensible? This being
a blog post on the topic, it should come as no surprise that there is, with one caveat: the
class must be *closed*. That is, the instances that have been written are complete and will not
need to be changed in the future.

I'll demonstrate how to reify types into values of a GADT using an
example. Say we have a class, <code>Counter</code>, with a
single method <code>inc</code> to increment a value.

~~~{.haskell}
{-# LANGUAGE GADTs #-}

class Counter a where
  inc :: a -> a
~~~

Here are all the instances we will ever write for this class.

~~~{.haskell}
data CounterR a where
  CounterRint  :: CounterR Int
  CounterRchar :: CounterR Char
  CounterRlist :: CounterR a -> CounterR [a]
  CounterRpair :: CounterR a -> CounterR b -> CounterR (a,b)

class Counter a where
  inc :: a -> a

instance Counter Int where
  inc a = a + 1

instance Counter Char where
  inc a = chr (ord a + 1)

instance Counter a => Counter [a] where
  inc as = map inc as

instance (Counter a, Counter b) => Counter (a,b) where
  inc (a,b) = (inc a, inc b)
~~~

Although the instances are fixed, we wish to allow others to effectively add new methods to the
class in the future. We can do this by declaring a GADT that
[reifies](http://en.wikipedia.org/wiki/Reification_(computer_science)) the instance types. By
convention we name this data structure by appending the character <code>R</code> to the class name.

~~~{.haskell}
data CounterR a where
  CounterRint  :: CounterR Int
  CounterRchar :: CounterR Char
  CounterRlist :: CounterR a -> CounterR [a]
  CounterRpair :: CounterR a -> CounterR b -> CounterR (a,b)
~~~

Please note the following properties of this declaration:

* There is one constructor for each instance of class <code>Counter</code>.

* Every constraint on the <code>Counter</code> class in an instance declaration becomes a
  parameter of the corresponding GADT constructor. e.g. <code>instance (Counter a, Counter b)
  => Counter (a,b)</code> becomes <code>CounterRpair :: CounterR a -> CounterR b -> CounterR
  (a,b)</code>.

We now need to add a new method signature to class <code>Counter</code> and provide an
implementation for it in each of the instances. By convention this method is called
<code>counterR</code>; that is, the name of the class with the first letter lower-cased and
suffixed with character 'R'.

Our module so far looks like this:

~~~{.haskell}
{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances #-}
module Counter where

import Data.Char

data CounterR a where
  CounterRint  :: CounterR Int
  CounterRchar :: CounterR Char
  CounterRlist :: CounterR a -> CounterR [a]
  CounterRpair :: CounterR a -> CounterR b -> CounterR (a,b)

class Counter a where
  inc :: a -> a
  counterR :: CounterR a

instance Counter Int where
  inc a = a + 1
  counterR = CounterRint

instance Counter Char where
  inc a = chr (ord a + 1)
  counterR = CounterRchar

instance Counter a => Counter [a] where
  inc as = map inc as
  counterR = CounterRlist counterR

instance (Counter a, Counter b) => Counter (a,b) where
  inc (a,b) = (inc a, inc b)
  counterR = CounterRpair counterR counterR
~~~

Note that definition of <code>counterR</code> for each instance
follows a very simple pattern. It is simply the application of the
appropriate constructor to the appropriate number of calls to
</code>counterR</code>.

It's important to be clear that values of <code>CounterR a</code>
don't represent *values* of type <code>a</code>, they represent the
*type* <code>a</code> itself.

For instance a value of type

~~~{.haskell}
CounterR (([Int], Int), (Int, Char))
~~~

reifies to the value

~~~{.haskell}
CounterRpair
  (CounterRpair (CounterRlist CounterRint) CounterRint))
  (CounterRpair CounterRint CounterRchar)
~~~

We can now write new methods on any type that is a member of class <code>Counter</code>. I'll
show you with an example. Say we now want to write a function that decrements instead of
incrementing. We can write this as follows, but please note that the function takes the
representation of the type *and* the value as an argument.

~~~{.haskell}
dec :: CounterR a -> a -> a
dec CounterRint a                    = a - 1
dec CounterRchar a                   = chr (ord a - 1)
dec (CounterRlist incRa) as          = map (dec incRa) as
dec (CounterRpair incRa incRb) (a,b) = (dec incRa a, dec incRb b)
~~~

There are only a few things to note:

* If <code>dec</code> were a method of class <code>Counter</code> then
  you would need to define it in each instance. Here you provide each
  implementation as a case of the <code>dec</code> function.
* Since <code>dec</code> requires a value of type
  <code>CounterR</code> each recursive call to <code>dec</code>
  requires that you pass an appropriate value of type
  <code>CounterR</code>. Fortunately, the appropriate value is
  precisely one of those pattern matched on the left hand side.

As I noted at the beginning. This technique is not new, just
relatively unknown. A
[paper](http://lambda-the-ultimate.org/node/3837) by Oliveira and
Sulzmann suggests unifying the two concepts. Also, GADTs have been used
as an [implementation
technique](http://repetae.net/computer/jhc/jhc-reify-typeclass.html)
in [JHC](http://repetae.net/computer/jhc/) instead of the normal [dictionary
passing](http://homepages.inf.ed.ac.uk/wadler/papers/class/class.ps.gz)
implementation.
