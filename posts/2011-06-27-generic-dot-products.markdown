---
title: Generic dot products
category: haskell
tags: Haskell, type classes, dot product, matrix multiplication, program derivation
---

This is the first in a series of posts about program derivation. In particular I
am attempting to derive a matrix multiplication algorithm that runs
efficiently on parallel architectures such as GPUs.

As I mentioned in an earlier [post](/posts/2011-05-16-reifying-type-classes-with-gadts.html), I've
been contributing to the [Accelerate](http://hackage.haskell.org/package/accelerate) project. The
Accelerate EDSL defines various *parallel primitives* such as <code>map</code>, <code>fold</code>,
and <code>scan</code> (and many more).

The <code>scan</code> primitive (also known as *all-prefix-sums*) is quite famous because it is
useful in a wide range of parallel algorithms and, at first glance, one could be forgiven for
thinking it is not amenable to parallelisation. However, one does exist; a well-known [work
efficient](link me) algorithm for <code>scan</code> was popularised by Guy Blelloch which performs
$O(n)$ work.

The algorithm is undeniably clever. Looking at it, it is not at all
obvious how one might have gone about developing it oneself. A
[recent](http://conal.net/blog/posts/deriving-list-scans/)
[series](http://conal.net/blog/posts/deriving-parallel-tree-scans/)
[of](http://conal.net/blog/posts/composable-parallel-scanning/)
[posts](http://conal.net/blog/posts/parallel-tree-scanning-by-composition/)
by [Conal Elliott](http://conal.net) set out to redress this situation
by attempting to *derive* the algorithm from a high level
specification. His success has inspired me to follow a similar process
to derive a work efficient matrix multiplication algorithm.

The process I am following is roughly as follows:

* generalise the concept of matrix multiplication to data structures other than
  lists or arrays.

* develop a generic implementation that relies, as far as possible,
  on reusable algebraic machinery in type classes such as
  [Functor](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Functor),
  [Applicative](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html),
  [Foldable](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html)
  and
  [Traversable](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Traversable.html).

* use this generic implementation as a *specification* to derive an efficient
  algorithm. To call it a hunch that the underlying data structure is going to
  be tree-like is an understatement.

In this post I will present my results in developing an implementation for a
*generic dot product*. The result is surprisingly elegant.

# What is a dot product?

In mathematics the *dot product* is usually defined on vectors. Given two vectors
of length $n$, $(a_1, \dots, a_n)$ and $(b_1, \dots, b_n)$ the dot product is
defined as:

$a_1 b_1 + \dots + a_n b_n$

or without the use of the pernicious "$\dots$":

$\sum_{i=1}^{n}{a_i b_i}$

The implementation for lists is fairly straightforward.

~~~{.haskell}
dot :: Num a => [a] -> [a] -> a
dot xs ys = foldl (+) 0 (zipWith (*) xs ys)
~~~

This definition will work just fine on two lists of different length, owing to
the definition of <code>zipWith</code>.

~~~{.haskell}
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
~~~

This is fine for lists but will become problematic later when we look at other
data structures.

There is no reason that this definition shouldn't be extended to other data
structures such as $n$-dimensional arrays or even trees. Let's look at how we
might define dot products on trees.

# Dot product on <code>Tree</code>s

We define trees as follows (however it does not really matter whether only the
leaves contain numbers or whether branch nodes can too):

~~~{.haskell}
data Tree a = Leaf a | Branch (Tree a) (Tree a)
~~~

For the sake of succinctness, I will represent trees using nested pairs denoted with curly
braces. e.g. <code>Branch (Leaf 1) (Leaf 2)</code> becomes <code>{1,2}</code>, <code>Branch (Leaf 1)
(Branch (Leaf 2) (Leaf 3))</code> becomes <code>{1,{2,3}}</code>.

What should be the dot product of <code>{1,{2,3}}</code> and
<code>{4,{5,6}}</code>? A reasonable answer would be <code>1 * 4 + 2 * 5 + 3 * 6 ==
32</code>. For each leaf in the first tree find the corresponding leaf in the
second tree, multiply them together and then sum all the results together.

This definition relies on the two trees having the same shape. To see why let's
see if we can we define a function in the style of <code>zipWith</code> for
trees. Unfortunately, this is problematic.

~~~{.haskell}
zipWithT f (Leaf a) (Leaf b)           = Leaf (f a b)
zipWithT f (Branch s t) (Branch s' t') = Branch (zipWithT f s s')
                                                (zipWithT f t t')
zipWithT f (Leaf a) (Branch s' t')     = {- ? -} undefined
zipWithT f (Branch s t) (Leaf b)       = {- ? -} undefined
~~~

There's a problem with the last two cases.
While I won't go so far as to say that there is no definition we could provide,
it's clear that there are a number of choices that could be taken. In each case
one needs to take an arbitrary element from the <code>Branch</code> argument and apply
function <code>f</code> to it and the <code>Leaf</code> argument.

Even if there is a definition that makes reasonable sense can we say whether it's possible to
provide a <code>zipWith</code>-like definition for an arbitrary data structure?

An alternative is to modify our data structures to contain phantom types that
represent the *shape* of the data structure. We can then define <code>dot</code>
such that it must take two arguments of exactly the same shape.

# Data structures with shapes

I'll illustrate this approach with vectors first, before moving onto
trees. Vectors are just lists with their length encode into their type.

## Vectors

First, we add some essentials to the top of our module.

~~~{.haskell}
{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
~~~

Now we define two new data types, <code>Z</code> and <code>S</code>, representing [Peano
numbers](http://en.wikipedia.org/wiki/Peano_axioms). Both data types are empty since they will only
be used as phantom types.

~~~{.haskell}
data Z
data S n
~~~

Now vectors.

~~~{.haskell}
infixr 5 `Cons`
data Vec n a where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a
~~~

If you haven't seen these data types before it's worth noting that you can
define *total* (vs *partial*) versions of <code>head</code> and
<code>tail</code>. Trying to take the head of an empty vector simply doesn't
type check.

~~~{.haskell}
headVec :: Vec (S n) a -> a
headVec (Cons x _) = x

tailVec :: Vec (S n) a -> Vec n a
tailVec (Cons _ xs) = xs
~~~

With can now define <code>zipWithVec</code>.

~~~{.haskell}
zipWithV :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWithV f Nil Nil = Nil
zipWithV f (Cons x xs) (Cons y ys) =
  f x y `zipWithVec` f xs ys
~~~

Unfortunately, GHC's type checker does not detect that a case such as the
one below is impossible. (In fact, if your warnings are turned up high enough GHC
will warn that two patterns are missing in the definition above.)

~~~{.haskell}
-- Although this pattern match is impossible GHC's type checker
-- won't complain
zipWithVec f (Cons x xs) Nil = {- something -} undefined
~~~

## Trees

The *length* of a tree is not quite a meaningful enough representation. Instead
we represent its *shape* as a nested tuples of the unit (<code>()</code>) type.

~~~{.haskell}
data Tree sh a where
  Leaf   :: a -> Tree () a
  Branch :: Tree m a -> Tree n a -> Tree (m,n) a
~~~

For example:

~~~{.haskell}
{1,{2,3}} :: Tree ((),((),())) Integer
~~~

The new definition of <code>zipWithT</code> only differs in
its type.

~~~{.haskell}
zipWithT :: (a -> b -> c) -> Tree sh a -> Tree sh b -> Tree sh c
zipWithT f (Leaf a)     (Leaf b)       = Leaf (f a b)
zipWithT f (Branch s t) (Branch s' t') = Branch (zipWithT f s s')
                                                (zipWithT f t t')
~~~

Now finish off the definitions:

~~~{.haskell}
foldlT :: (a -> b -> a) -> a -> Tree sh b -> a
foldlT f z (Leaf a)     = f z a
foldlT f z (Branch s t) = foldlT f (foldlT f z s) t

dotT :: Num a => Tree sh a -> Tree sh a -> a
dotT t1 t2 = foldlT (+) 0 (zipWithT (*) t1 t2
~~~~

# Generalising to arbitrary data structures

Any seasoned Haskell veteran knows the utility of type classes such as
<code>Functor</code>, <code>Applicative</code>, and <code>Foldable</code>.  We
have now seen that a dot product is essentially a <code>zipWith</code> followed
by a <code>fold</code>. (It makes little difference whether its a left or right
fold).

Since <code>zipWith</code> is really just <code>liftA2</code> (found in module
<code>Control.Applicative</code>) this leads us to the following definition:

~~~{.haskell}
dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot x y = foldl (+) 0 (liftA2 (*) x y)
~~~

This function requires instances for <code>Functor</code>, <code>Foldable</code> and
<code>Applicative</code>.  Given that instances for the first two type classes are both easy to
write (and in some cases derivable using Haskell's <code>deriving</code> syntax), I will only
discuss <code>Applicative</code> instances in this post. (The instances for vectors and
shape-encoded trees are left as an exercise for the reader.)

One might reasonably wonder, must the two arguments to <code>dot</code> have the same shape as
before? It turns out that, yes, they do and for similar reasons. I'll demonstrate the point by
looking at how to define <code>Applicative</code> instances for lists, vectors and trees.

## Lists

The default <code>Applicative</code> instance for lists is unsuitable for a generic dot
product. However, the <code>Applicative</code> instance on its wrapper type
[<code>ZipList</code>](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v:ZipList)
is adequate but has an unsatisfying (to say the least) definition for <code>pure</code>.

~~~{.haskell}
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
~~~

Of course, this is necessary for lists since we can't guarantee that two lists of the same length
will be applied together.  How else would you define <code>pure</code> to make it work on an
arbitrary length lists <code>xs</code>?

~~~{.haskell}
(+) <$> (pure 1) <*> (ZipList xs)</code>
~~~

The definition of <code>pure</code> is much more satisfying for vectors.

## Vectors

Obviously we want a similar definition for <code>pure</code> as for lists
(<code>ZipList</code>). But we don't want to produce an infinite list, just one of the appropriate
length.

Definining the <code>Applicative</code> instance for vector leads us to an interesting observation
(which holds true in general). You need *one instance for each constructor of a data type that has
its shape encode in its type*. Also *the instance heads mirror the type of the construtors*.

~~~{.haskell}
instance Applicative (Vec Z) where
  pure _                            = Nil
  Nil <*> Nil                       = Nil

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a                            = a `Cons` pure a
  (fa `Cons` fas) <*> (a `Cons` as) = fa a `Cons` (fas <*> as)
~~~

That's it. Function <code>pure</code> will produce a vector of just the right length.


## Trees

Unlike the case for lists, it's hard to define an <code>Applicative</code> instance for
non-shape-encoded trees. Let's have a look.

~~~{.haskell}
instance Applicative Tree where
  pure a = Leaf a
  (Leaf fa) <*> (Leaf b) = Leaf (fa b)
  (Branch fa fb) <*> (Branch a b) = Branch (fa <*> a) (fb <*> b)
  (Leaf fa) <*> (Branch a b) = {- ? -} undefined
  (Branch fa fb) <*> (Leaf a) = {- ? -} undefined
~~~

This problem has been
[noticed](http://www.haskell.org/pipermail/beginners/2010-March/003856.html) before on the
<tt>Haskell-beginners</tt> mailing list. The
[response](http://www.haskell.org/pipermail/beginners/2010-March/003857.html) is interesting because
it mentions the "unpleasant property of returning infinite tree[s]"; the same problem we had with
lists!


With shape-encode trees this is not a problem. Function <code>pure</code>
produces a tree of the appropriate shape. Also, note how the head of the second instance mirrors the
definition of the <code>Branch</code> constructor (:: Tree m a -> Tree n a -> Tree (m,n) a)

~~~{.haskell}
instance Applicative (Tree ()) where
  pure a                          = Leaf a
  Leaf fa <*> Leaf a              = Leaf (fa a)

instance (Applicative (Tree m), Applicative (Tree n))
         => Applicative (Tree (m,n)) where
  pure a                          = Branch (pure a) (pure a)
  (Branch fs ft) <*> (Branch s t) = Branch (fs <*> s) (ft <*> t)
~~~

# Arbitrary binary associative operators.

Phew, that's it. We now have an implementation for <code>dot</code> that
will work on an arbitrary data structure as long as one can define
<code>Functor</code>, <code>Foldable</code> and
<code>Applicative</code> instances.  We have also learned that it is a
good idea to encode the data structures shape in its type so that
<code>Applicative</code> instances can be defined as total functions
in a pleasantly non-arbitrary way. (This will be important later on
when we want to take the transpose of generic matrices, but I'm
getting ahead of myself.)

But what if you want to use binary associative operators other than
addition and multiplcation for the dot product? This is easy using
Haskell's <code>Monoid</code> type class, and it plays nicely with the
<code>Foldable</code> type class. In fact, it allows us to omit any
mention of identity elements using the method <code>fold:: (Foldable
t, Monoid m) => t m -> m</code>. We define an even more generic dot
product as follows:

~~~{.haskell}
dotGen :: (Foldable f, Applicative f, Monoid p, Monoid s)
       => (a -> p, p -> a) -> (a -> s, s-> a) -> f a -> f a -> a
dotGen (pinject, pproject) (sinject, sproject) x y =
   sproject . fold . fmap (sinject . pproject) $ liftA2 mappend px py
  where
    px = fmap pinject x
    py = fmap pinject y
~~~

This function takes two pairs of functions for injecting into and
projecting from monoids. We can then define our original
<code>dot</code> function using the existing <code>Sum</code> and
<code>Product</code> wrapper types.

~~~{.haskell}
dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot = dotGen (Product, getProduct) (Sum, getSum)
~~~

# In the next episode...

In my next post we will consider *generic matrix multiplication*. This operation
is defined over arbitrary collections of collections of numbers and, naturally,
makes use of our generic dot product. Until then, adios.