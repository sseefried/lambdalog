---
title: Sharing Recovery in deeply embedded DSLs
author: Sean Seefried
tags: domain specific languages, deep embedding, sharing
---

# Introduction

I could easily start this post by banging on about how great embedded domain specific languages
are but that's already been done quite adequately in a [number]() [of]() [places](). There are
a few options when it comes to implementing domain specific languages. They can be a

* [shallow embeddeding]() or [internal]() DSL. In this case the terms of the [target
  language](LINK ME) are terms in the [host language](). e.g. functions in the target language
  are functions in the host language. In this case one runs a program by compiling the
  program. The code generated is just the code that is normally generated for the host language
  since terms in the target language *are* terms in the host language.

* [deeply embedding]() or [external]() DSL. In this case the terms of the target language are
  instances of a data structure in the host language (usually an [abstract syntax tree]()
  (AST)). The terms are constructed using various helper functions and are interpreted or
  compiled. This differs from the shallow embedding case because one must write the
  interpreter/compiler and does not gain the benefit of reusing the host language's compiler.

A not immediately obvious problem that occurs when one implements a deeply embedded DSL is that
one can easily lose the *term sharing* implicitly present in the target language. I'll
demonstrate this with an example. Say our target language is the [simply typed lambda
calculus]() with arithmetic. A program in this language might look something like:

$(2+3) + (2+3)$

One might write this in Haskell (using some clever overloading of arithmetic operators) as:

~~~{.haskell}
prog :: Exp Int
prog = let v = 2 + 3 in v + v
~~~

I'll give you the definition of this language soon but for the moment just trust me when I say
this Haskell program generates the following AST

~~~{.haskell}
Add (Add (Const 2) (Const 3)) (Add (Const 2) (Const 3))
~~~

Notice that the sharing implicit in the let-expression of Haskell has
now been removed. What we would prefer it to generate is something
more like:

~~~{.haskell}
LetE (Add (Const 2) (Const 3)) (Add (Lvar 0) (Lvar 0))
~~~

We're using [de Bruijn indices]() to refer to variables. Here we have introduced a *let-node*
which binds let-variable $0$ to the expression $2 + 3$ in the expression ${0} + {0}$ (using the
notation that ${x}$ refers to let-variable (`Lvar`)with de Bruijn index $x$).

## Memory usage is the real problem

You might be thinking, what's the big deal? Who cares if a term gets replicated a few
times. Can't we always recover the sharing using *common sub-expression elimination* ([CSE]())?
This is true (if inefficient) but consider the case where one wants to generate the (unrolled)
code that finds the $n$th Fibonacci number.

~~~{.haskell}
fib :: Int -> HOAS.Exp Integer
fib x = fib' x 1 1
  where
   fib' :: Int -> HOAS.Exp Integer -> HOAS.Exp Integer -> HOAS.Exp Integer
   fib' x e1 e2
    | x == 0    = e1
    | otherwise = fib' (x - 1) e2 (e1 + e2)
~~~

`fib 4` produces:

~~~{.haskell}
Add (Add (Add (Const 1) (Const 1)) (Const 1)) (Add (Const 1) (Const 1))
~~~

There's something a little special about this function. Depending on the value of the integer
passed to `fib` the programs generates an AST. We call this style of program a
*generator*. Just from the type alone it's not always possible to tell whether a program is a
generator or not. However, in general a function is a generator if it creates an AST based on
the value of a host language term (i.e. `Int` in this case). We cannot actually
write the fibonacci function in the simply type lambda calculus since it does not contain the
notion of recursion.

In fact, function `fib`, will produce an AST of size proportional to the $n$th
fibonacci number! As soon as the AST is traversed -- whether it's to interpret it, generate
code for it or transform it to another intermediate form -- this will quickly consume all of
the machine's available memory as $n$ increases.

We saw what `fib 4` produces without sharing recovery. What we really want it to
produce is:

~~~{.haskell}
Let (Const 1)
    (Let (Add (Lvar 0) (Lvar 0))
         (Add (Lvar 0) (Add (Lvar 1) (Lvar 0))))
~~~

or equivalently `let v0 = 1 in let v1 = v0 + v0 in v0 + (v1 + v0)`

## What makes our sharing recovery different?

* Shallow embedding of type system in AST nodes.
* Using a Higher Order Abstract Syntax representation. How do you recover sharing "under the lambdas"?

## What it looks like in the heap

I'm going to show you what the ast for `fib 4` looks like in the heap. But before I can do that adequately it's time to introduce the AST for our language.

# The AST

## Elements

We have *primitive types* in our language -- integers and floating point reals.

~~~{.haskell}
class (Eq a, Show a, Typeable a) => Elt a

instance Elt Int
instance Elt Float
instance Elt Bool
~~~

We require that the elements are instances of the `Typeable` class because we use dynamic typing in our solution. In particular 
* when substituting in things from the environment during de Bruijn conversion
* during sharing recovery

We also have *number elements*.

~~~{.haskell}
class (Num a, Elt a) => NumElt a

instance NumElt Int
instance NumElt Float
~~~

## Lambda terms

### Values with function types vs. primitive types

Although functions are first class citizens in the the simply type lambda calculus with arithmetic (**STLCWA???**), we distinguish them from values with primitive types using two different data types so that we can ensure that only functions can be applied to other values.

### Pre-recursive data types

**FIXME: Check that pre-recursive is a real term or define it yourself.**

During sharing recovery we need to annotate the original AST with new nodes. The standard technique to
do this is define a *pre-recursive type* where each recursive occurrence of a type is replaced with a 
*type parameter*.

~~~{.haskell}
data Fun t where
  Lam   :: (Elt a, Elt b) => (Exp a -> Exp b) -> Fun (a -> b)

data PreExp exp fun t where
  Tag   :: Elt a => Int -> PreExp exp fun a -- ^ tag for lambda bound variables.
                                            -- Only used during conversion and sharing recovery.
  App   :: (Elt a, Elt b) => fun (a -> b) -> exp a -> PreExp exp fun b
  Const :: Elt a => a -> PreExp exp fun a
  Add   :: NumElt a => exp a -> exp a -> PreExp exp fun a
  Cond  :: Elt a => exp Bool -> exp a -> exp a -> PreExp exp fun a
  Eq    :: Elt a => exp a -> exp a -> PreExp exp fun Bool

newtype Exp a = Exp (PreExp Exp Fun a)
~~~

We have used convention of prefixing the name of the type with `Pre` to highlight that it is pre-recursive. The newtype `Exp` "ties the knot" ensure that all sub-expressions and sub-functions are of type `Exp` and `Fun` respectively. (You can tell that this type ties the knot because `Exp` appears on the left and right hand side of the equals sign.)

## What `fib 4` looks like in the heap

**FIXME: Use dot plugin**

This is what it looks like in the heap.

![AST of `fib 4` in the heap](/static/images/fib_4_heap.jpg)

I've used some syntactic conventions here:

* Rectangular nodes are of type `Exp a`
* Elliptical nodes are of type `PreExp Exp Fun a`

This graph was automatically generated. How did we discover the sharing in the heap? It turns out that GHC has provided run-time support for this via a mechanism called *stable names*. Stable names are abstract names for an object in the heap that support fast, not quite exact, comparison ($O(1)$) and hashing. In other language one can use pointer or reference equality to quickly compare two objects, which allows fast hash table implementations. One cannot compare object addresses in Haskell since the garbage collector can move objects around.

Two stable names that are equal are guaranteed to refer to the same object. The converse is not true; if two stable names are not equal then the objects they name may still be equal. Thus, observing sharing in the heap is not perfect and consequently our sharing recovery implementation may not recover all sharing.  However, it works well in practice.

# Sharing recovery

To recover sharing we need to introduce two new node types *let-nodes* and *let-variable-nodes*. The basic idea of sharing recovery is to find nodes that are shared in the heap, bind them to *let-variables*, and replace the shared nodes with *let-variable-nodes*. 

Our earlier definition of the pre-recursive type `PreExp` allows us to do this.

~~~{.haskell}
data SharingExp a where
  VarSharing :: Elt a => StableName (Exp a)                           -> SharingExp a
  LetSharing ::          StableSharingExp -> SharingExp a             -> SharingExp a
  ExpSharing :: Elt a => StableName (Exp a) -> PreExp SharingExp SharingFun a -> SharingExp a

data SharingFun a where
  TaggedSharingExp :: Elt b => Int {-unique -} -> SharingExp b -> SharingFun (a -> b)
~~~

Don't worry too much about the definition `SharingFun` for now; it will come in useful when we try to recover sharing in HOAS terms (i.e. "under the lambdas").

## What are we aiming for? 

Once all the `LetSharing` and `VarSharing` nodes have been inserted we want the graph to look as follows.
The numbers in nodes have the following meaning. Let-nodes have *binders* and *bodies*. A binder is identified by a number. In a let-node, `LetSharing n`, one identifies the binder by that number. We call it *binder $n$*. A let-variable-node, `VarSharing n` refers to binder $n$.

![`fib 4` with sharing recovered](/static/images/fib_4_recovered.jpg)

This time around:

* Rectangular boxes are nodes of type `LetSharing a`.
* Ellipses are, as they did before,  `PreExp` nodes but this time of their full type is
  `PreExp SharingExp SharingFun a`.

## A two phase approach

Sharing recovery is done in two phases. As a very high level overview:

* Phase 1 counts, through a top-down traversal, how many times each node is shared in the AST.
* Phase 2 insert let-nodes through a bottom-traversal.

Of course it's a little more detailed than that. In phase one we simultaneously create an occurrence map while creating an annotated tree *without sharing recovered*.

An occurrence map is a mapping from stable names of `PreExp` nodes to the number of times that node is shared in the heap. We call the number of times a node has been shared in the heap its *occurrence count*. Identically we say that this is the occurrence count of the stable name.

### Phase 1
Phase one traverses the tree in a *depth first* manner; it is a top-down traversal.

* If we encounter an `Exp` node we have *not* seen before we wrap up the `PreExp` node contained 
  within it with a `ExpSharing` node along with the stable name of the original `Exp` node it was under.
* If we encounter an `Exp` node we have seen before we replace it with a `VarSharing` node and
  increment the count in the occurrence map.

Expression `fib 4` produces a tree like this:

**In the final version of this blog post try produce the graphs all at once so that the stable names are all the same**

![`fib 4` after phase 1](/static/images/fib_4_after_phase_1.jpg)

In this figure we show the stable name of the `Exp` node that each `ExpSharing` replaced. (This information is stored in each `ExpSharing` node; see the definition above. **LINK ME**.) The numbers after the `VarSharing` nodes refer to these stable names. The idea behind them is that they stand in place of the `ExpSharing` node containing the particular stable name of an `Exp` node.

### Phase 2

Phase 2 of the algorithm is a bottom-up traversal of the tree. The occurrence map from phase 1 is an input into phase 2. As we move up the tree we keep track of how many times we have seen the stable names stored in `ExpSharing` nodes. When we have seen it as many times as its count in the occurrence map we insert a `LetSharing` node whose body is a new `VarSharing` node and whose binder is the expression so far.

**THERE IS AN OPPORTUNITY HERE FOR SOME FAIRLY FORMAL DEFINITIONS**

For instance, in figure **LINK** the `ExpSharing` nodes with `Exp` stable names $26$ **FIXME** and $25$ **FIXME** will become the binders of let-nodes. Let's use the abbreviation that $ES_n$ (for some n) refers to `ExpSharing` node with `Exp` stable name $n$ and $VS_n$ for the `VarSharing` node standing in place of $ES_n$.  Now consider the left branch of the tree and travel up from the leaves, stopping at each `ExpSharing` or `VarSharing` node. At $ES_{26}$ and $VS_{26}$ we have seen stable name $26$ once. Continue travelling upwards to $ES_{25}$. Here we have seen stable name $26$ twice. But it's occurrence count in the entire AST is 3 so this is still not enough. Only when we reach $ES_{24}$ have we seen it 3 times. This is the point to insert the `LetSharing` node (i.e. the let-node) with binder equal to the subtree rooted at $ES_{26}$. Incidentally this is also the place to insert another `LetSharing` node whose binder is the subtree rooted at $ES_{25}$. Note that this binder depends on the binder of $LS_{25}$ so getting the order right is important.

In fact, the description given so far is not the full story. It's a little more complicated than that.
You cannot necessarily insert a let-node when you have seen a stable name as many times as its occurrence count. This is because the binder of that let-node may contain `VarSharing` nodes for which there are no let-nodes yet inserted. You actually need to maintain dependencies between binders and the let-nodes they depend on.

**YOU HAVE NOT YET MENTIONED THE REPLACEMENT OF EXPSHARING WITH VARSHARING NODES**

**TODO**

- discuss dependencies between binders
- introduce the NodeCounts data structure
- discuss topological sort to determine order of let sharing nodes.


## Looking under the lambdas

Our representation of the simply typed lambda calculus with arithmetic uses Higher Order Abstract Syntax. How then do we recover sharing when it, so to speak, "under a lambda"? But first, why is this even important?

**Put some explanation in why**

Here's a program with some sharing under a lambda:

~~~{.haskell}
powTwo :: Int -> HOAS.Exp Integer
powTwo n = HOAS.app (aux n) 1
  where
    aux :: Int -> HOAS.Fun (Integer -> Integer)
    aux n
      | n == 0 = HOAS.lam (\v -> v)
      | otherwise =  HOAS.lam $ \v -> let v' = HOAS.app (aux (n-1)) v in v' + v'
~~~

This, rather contrived, function finds a power of two by building up a function progressively.

$powTwo\ 0 \equiv (\lambda v.v)\ 1$

$powTwo\ 1 \equiv (\lambda a.let\ b = (\lambda c.c)\ b\ in\ b + b)\ 1$ 

The AST itself is not that big. Its size is proportional to $n$ due to the fact that is just building up a closure on the run-time heap. However, without sharing recovery it will run in exponential time. Also, if it is converted to another intermediate form, say one using de Bruijn indices, its size will explode as per usual. 

As a crude measure of this let's see what happens when we convert to de Bruijn form and then find the length of the `show`ed result.

~~~
> map (\n -> length (show (convert $ powTwo n))) [1..10]
[81,189,405,837,1701,3429,6885,13797,27621,55269]
~~~

It is clear that sharing recovery needs to be able to look under lambdas. The solution is not too difficult in principle but has some subtleties. The basic idea is to apply the Haskell function under the `Lam` node to a dummy tag, thus creating a value of `PreExp Exp Fun a`, during Phase 1.

Remember I said I'd explain what the `SharingFun` type was all about. This is where it comes into its own. We generated a unique integer $i$, apply the higher order function to `Tag i` thus yielding a value of type `PreExp Exp Fun a` (for some `a`). We then recursively traverse into that yielding a value of 
type `PreExp SharingExp SharingFun a` and finally wrapped that in the `TaggedSharingExp` constructor, yielding a value of type `SharingFun a`. 

The unique value, $i$, essentially acts as a variable name. We have converted from HOAS to an explicit, named, representation of lambda terms. Once sharing recovery has been done this can easily be converted back into a HOAS representation if desired. We personally are not interested in that. We are only interested in converting to de Bruijn notation, which is also straightforward. Here's how:

* Initialise a finite map from unique identifiers to de Bruijn indices. Call this the *tag map*
* Traverse the `SharingExp a` data structure.
* Keep track of how many lambdas one is under (i.e. how many `TaggedSharingExp`s one is under)
* When one pattern matches on a `TaggedSharingExp` add to the tag map a mapping from the
  unique identifier to the current lambda depth.
* When one pattern matches on `Tag j`, look up the mapping of $j$ in the finite map and replace it
  with the corresponding de Bruijn index.
  
# Other things to talk about

* The need for the nodes to be in weak head normal form (WHNF) before taking their stable names. 
  Use `seq`
* unsafePerformIO and why it's safe during sharing recovery.
* the loss of true referential transparency. The need to be careful not to do any evaluation between
  phase 1 and phase 2.
* An explanation of the graph of dependencies between binders and the topological sort. Do it
  diagrammatically. Choose good examples.
* Explain the node counts data structure.

# Proof of correctness (in Agda?)

