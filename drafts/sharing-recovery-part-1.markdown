---
title: Sharing Recovery in deeply embedded DSLs &mdash; Part 1
author: Sean Seefried
tags: domain specific languages, deep embedding, sharing
hours: 7.5
---

# Introduction

This post is about sharing recovery in deeply embedded domain specific languages (DSLs).
There's too much material for a single blog post, so I'm going to split it into two.
In the first post I'll provide some background, explain the problem, define what sharing recovery is, as well as providing some preliminary explanations and code snippets.

In the second post I'll dive into more depth. Since these are blog posts I'm not limited by
space so I'll aim for ease of reading instead of brevity or density. I apologise if I've gone
too far the other way and made your reading experience frustratingly slow paced.

# Background

I could easily begin by banging on about how great embedded domain specific languages are but
that's already been done quite adequately in a [number][hudak-paper] [of]() [places][my-paper]. Instead, I'll start
by distinguishing two ways one can implement domain specific languages. A DSL can be a:

* [shallow embeddeding][shallow-embedding] or [internal DSL][dsl-definitions]. In this case the
  terms of the [target language][target-language] are terms in the *host language*. e.g.
  functions in the target language are functions in the host language. In this case one runs a
  program by compiling the program. The code generated is just the code that is normally
  generated for the host language since terms in the target language *are* terms in the host
  language.

* [deep embedding][deep-embedding] or [external DSL][dsl-definitions]. In this case the terms of
  the target language are instances of a data structure in the host language (usually an
  [abstract syntax tree][ast] or AST). The terms are constructed using various helper functions
  and are interpreted or compiled. This differs from the shallow embedding case because one must
  write the interpreter/compiler and does not gain the benefit of reusing the host language's
  compiler.

A not immediately obvious problem that occurs when one implements a deeply embedded DSL is that
one can easily lose the *term sharing* implicitly present in the target language. I'll
demonstrate this with an example. Say our target language is the [simply typed lambda
[calculus][simply-typed-lambda-calculus] with arithmetic. A program in this language might look
something like:

$(2+3) + (2+3)$

One might write this in Haskell (using some clever overloading of arithmetic operators) as:

~~~{.haskell}
prog :: Exp Int
prog = let v = 2 + 3 in v + v
~~~

I'll provide the definition of this language later but, for the moment, just trust me when I say
this Haskell program generates the following AST:

~~~{.haskell}
Add (Add (Const 2) (Const 3)) (Add (Const 2) (Const 3))
~~~

Notice that the sharing implicit in the Haskell let-expression no longer exists. What we would
prefer it to generate is something more like:

~~~{.haskell}
LetE (Add (Const 2) (Const 3)) (Add (Lvar 0) (Lvar 0))
~~~

We're using [de Bruijn indices][de-bruijn-indices] to refer to variables. Here we have
introduced a *let-node* (denoted by the `LetE` constructor) which binds *let-variable* with de
Bruijn index $0$ (denoted by the `Lvar` constructor) to the expression $2 + 3$ in the expression
$\{0\} + \{0\}$ (using the notation that $\{x\}$ refers to let-variable with de Bruijn index
$x$).

## Space leaks are the real problem

You might be thinking, what's the big deal? Who cares if a term gets replicated a few times.
Can't we always recover the sharing using *common sub-expression elimination* ([CSE][cse])? This
is true, if not efficient, but consider the case where one wants to generate the
[unrolled][loop-unwinding] code that finds the $n$th Fibonacci number.

~~~{.haskell}
fib :: Int -> HOAS.Exp Integer
fib x = fib' x 1 1
  where
   fib' :: Int -> HOAS.Exp Integer -> HOAS.Exp Integer -> HOAS.Exp Integer
   fib' x e1 e2
    | x == 0    = e1
    | otherwise = fib' (x - 1) e2 (e1 + e2)
~~~

As an example, the expression `fib 4` evaluates to:

~~~{.haskell}
Add (Add (Add (Const 1) (Const 1)) (Const 1)) (Add (Const 1) (Const 1))
~~~

Function `fib` is perhaps a little more interesting than it first appears. Depending on the
value of the integer passed to `fib` the program generates an AST. What's interesting is that
this integer is a host language term and not a target language term. Consequently we call this
style of program a *generator*[^generator].

In fact, function `fib`, will produce an AST that is conceptually of a size proportional to the
$n$th fibonacci number! This sounds terrible but things aren't nearly so dire; even if one were
to fully evaluate the closure `fib 4` it would still not take up that much memory since there is
*implicit sharing* in the heap of the Haskell run-time.

Unfortunately, this implicit sharing can easily be lost. One obvious way, which happened to us
during the development of [Accelerate][accelerate], is to convert the AST to another data
structure. In our case, converting from a High Order Abstract Syntax ([HOAS][hoas])
representation to an explicit representation using de Bruijn indices led to the computer's
memory being completely consumed by multi-gigabyte ASTs.

The essence of *sharing recovery* is to take this implicit sharing in the heap and modify the
AST to represent it explicitly. We'll do this by looking at what `fib 4` looks like in the heap
when fully evaluated. But before I can do that adequately I'll need to introduce the data type
for the AST of our language. After that we'll be in much better shape to start talking about
sharing recovery.

# The AST

Our AST represents terms of the simply typed lambda calculus with arithmetic. We use a Higher Order Abstract Syntax ([HOAS][hoas]) representation for functions.

## Elements

We have *primitive types* in our language -- integers and floating point reals.

~~~{.haskell}
class (Eq a, Show a, Typeable a) => Elt a

instance Elt Int
instance Elt Float
instance Elt Bool
~~~

We require that the elements are instances of the `Typeable` class because we use dynamic
typing in our solution. In particular:

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

Although functions are first class citizens in the the simply typed lambda calculus with
arithmetic, we distinguish them from values with primitive types using two different data types. This ensures that functions are the only terms that can be applied to other values. In other words, it's a nice way of ensuring well-formed terms using Haskell's type system.

### Pre-recursive data types

During sharing recovery we need to annotate the original AST with new nodes. The standard
technique to do this is to define a *pre-recursive type* where each recursive occurrence of a
type is replaced with a *type parameter*.

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

We have used the convention of prefixing the name of the type with `Pre` to highlight that it is
pre-recursive. The newtype `Exp` "ties the knot"[^tying-the-knot] ensuring that all sub-expressions and sub-functions are of type `Exp` and `Fun` respectively.

## What `fib 4` looks like in the heap

This is what it looks like in the heap.

![AST of `fib 4` in the heap](/static/sharing-recovery/images/fib_4_heap.png)

I've used some syntactic conventions here:

* Rectangular nodes are of type `Exp a`
* Elliptical nodes are of type `PreExp Exp Fun a`

### Observing the heap via *stable names*

This graph was automatically generated. So, if this is the case how did we discover the sharing
in the heap? It turns out that GHC has provided run-time support for this via a mechanism
called *stable names*. Stable names are abstract names for an object in the heap that support
fast, not quite exact, comparison ($O(1)$) and hashing. In other language one can use pointer
or reference equality to quickly compare two objects, which allows fast hash table
implementations. One cannot compare object addresses in Haskell since the garbage collector can
move objects around.

Two stable names that are equal are guaranteed to refer to the same object. The converse is not
true; if two stable names are not equal then the objects they name may still be equal. Thus,
observing sharing in the heap is not perfect and consequently our sharing recovery
implementation may not recover all sharing.  However, it works well in practice.

# Sharing recovery

To recover sharing we need to introduce two new node types *let-nodes* and
*let-variable-nodes*. The basic idea of sharing recovery is to find nodes that are shared in
the heap, bind them to *let-variables*, and replace the shared nodes with *let-variable-nodes*.

Our earlier definition of the pre-recursive type `PreExp` allows us to do this.

<a name="sharing-exp-definition"></a>

~~~{.haskell}
data SharingExp a where
  VarSharing :: Elt a => StableName (Exp a)                           -> SharingExp a
  LetSharing ::          StableSharingExp -> SharingExp a             -> SharingExp a
  ExpSharing :: Elt a => StableName (Exp a) -> PreExp SharingExp SharingFun a -> SharingExp a

data SharingFun a where
  TaggedSharingExp :: Elt b => Int {-unique -} -> SharingExp b -> SharingFun (a -> b)

-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingExp where
  StableSharingExp :: Elt a => StableName (Exp a) -> SharingExp a -> StableSharingExp
~~~

Don't worry too much about the definition `SharingFun` for now; it will come in useful when we
try to recover sharing in HOAS terms (i.e. "under the lambdas").

## What are we aiming for?

Once all the `LetSharing` and `VarSharing` nodes have been inserted we want the graph to look
as follows.  The numbers in nodes have the following meaning. Let-nodes have *bound
expressions* and *bodies*. A bound expression is identified by a number called its
*binder*. (It is just equal to the stable name of the original `Exp` node.) The binder of
let-node, `LetSharing (StableSharingExp sn sharingExp) bound` is equal to `sn`. A
let-variable-node, `VarSharing sn` references bound expression `sharingExp`.

![`fib 4` with sharing recovered](/static/sharing-recovery/images/fib_4_recovered.png)

This time around, the notation is:

* Rectangular boxes are nodes of type `LetSharing a`.
* Ellipses, as they were before, are `PreExp` nodes. However, this time of their full type is
  `PreExp SharingExp SharingFun a`.

## Sharing recovery: a two phase approach

Sharing recovery is done in two phases. As a very high level overview:

* Phase 1 counts, through a top-down traversal, how many times each node is shared in the AST.
* Phase 2 insert let-nodes through a bottom-traversal.

Naturally, it's a little more detailed than that. In phase one we simultaneously create an
occurrence map while creating an annotated tree *without sharing recovered*. The annotated tree has type `SharingExp a`.

An *occurrence map* is a mapping from stable names of `PreExp` nodes to the number of times that
node is shared in the heap. We call the number of times a node has been shared in the heap its
*occurrence count*. Identically we say that this is the occurrence count of the stable name. More formally, for stable name $n$, we say $occ(n) = c$ if its occurrence count is $c$.

### Phase 1
Phase one traverses the tree in a *depth first* manner; it is a top-down traversal.

* If we encounter an `Exp` node we have *not* seen before we wrap up the `PreExp` node contained
  within it with an `ExpSharing` node along with the stable name of the original `Exp` node it
  was under.

* If we encounter an `Exp` node we have seen before we replace it with a `VarSharing` node and
  increment the count in the occurrence map.

Phase 1 transforms the AST of `fib 4` to tree:

<a name="fib-after-phase-1"></a>

![Figure `fib-after-phase-1`: `fib 4` after phase 1](/static/sharing-recovery/images/fib_4_after_phase_1.png)

In this figure we show the stable name of the `Exp` node that each `ExpSharing` replaced. (This
information is stored in each `ExpSharing` node; see the definition
[above](#sharing-exp-definition).) The numbers after the `VarSharing` nodes refer to these
stable names. The idea behind them is that they stand in place of the `ExpSharing` node
containing the particular stable name of an `Exp` node.

### Phase 2

Phase 2 of the algorithm is a bottom-up traversal of the tree and the occurrence map from phase 1 is an input into phase 2. As we move up the tree we keep track of how many times we have seen
the stable names stored in `ExpSharing` nodes. When we have seen it as many times as its count
in the occurrence map we insert a `LetSharing` node whose body is a new `VarSharing` node and
whose bound expression is the expression so far.

Here are some definitions to simplify the description:

* a *stable expression name* is the stable name from the original AST that node of type `Exp a` had. These stable names are referred to by nodes constructed with the `ExpSharing` and `VarSharing` constructors.
* we will use the short-hand $ES_{n}$ (for some $n$) to denote a node constructed with `ExpSharing` that references stable expression name $n$.
* we will use the short-hand $VS_{n}$ to denote a node constructed with `VarSharing`  that 
references stable expression name $n$.
* will will use the short-hand $LS_{n}$ to denote a node constructed with `LetSharing` that binds a bound expression referred to by $VS_{n}$.
* the *sharing count* is the number of times a particular stable name has been seen as we move progressively up the tree. Shorthand: $count(n) = c$ (for some stable name $n$ and sharing count $c$).

In figure [`fib-after-phase-1`](#fib-after-phase-1) the nodes $ES_{2}$ and
$ES_{4}$ will become the bound expressions of let-nodes. Now consider the left branch of the
tree and travel up from the leaves, stopping at each `ExpSharing` or `VarSharing` node. At
$ES_{4}$ and $VS_{4}$ and  $count(4) = 1$ at each node. Continue travelling upwards to $ES_{2}$. Here $count(4) = 2$ but $occ(4) = 3$ so this is still not enough. Only when we reach $ES_{1}$ do we have $occ(4) = 3$. This is the
point to insert the let-node (constructed with `LetSharing`) with bound expression equal to the
subtree rooted at $ES_{4}$. Incidentally this is also the place to insert another let-node 
node whose bound expression is the subtree rooted at $ES_{2}$. Note that this bound expression
depends on the bound expression of $LS_{4}$ so getting the order right is important.

In fact, the description given so far is not the full story. You cannot necessarily insert a
let-node when $count(n) = occ(n)$. This is because
the bound expression of that let-node may contain let-variable-nodes for which there are no
let-nodes yet inserted. You actually need to maintain dependencies between bound expressions and
the let-nodes they depend on.

# To be continued &hellip;

In the [part 2][part-2] we will look at the algorithm in more detail.

[part-2]: drafts/sharing-recovery-part-2.html
[hudak-paper]: http://portal.acm.org/citation.cfm?id=242477
[my-paper]: http://www.springerlink.com/content/5fcgra65yy0c9df5/
[shallow-embedding]: http://en.wiktionary.org/wiki/shallow_embedding
[dsl-definitions]: http://martinfowler.com/bliki/DomainSpecificLanguage.html
[target-language]: http://en.wikipedia.org/wiki/Target_language
[deep-embedding]: http://en.wiktionary.org/wiki/deep_embedding
[ast]: http://en.wikipedia.org/wiki/Abstract_syntax_tree
[simply-typed-lambda-calculus]: http://en.wikipedia.org/wiki/Abstract_syntax_tree
[adjacency-list]: http://en.wikipedia.org/wiki/Adjacency_list
[cse]: http://en.wikipedia.org/wiki/Common_subexpression_elimination
[loop-unwinding]: http://en.wikipedia.org/wiki/Loop_unwinding
[hashmap]: http://hackage.haskell.org/packages/archive/unordered-containers/0.1.4.0/doc/html/Data-HashMap-Lazy.html
[hashset]: http://hackage.haskell.org/packages/archive/unordered-containers/0.1.4.0/doc/html/Data-HashSet.html
[de-bruijn-indices]: http://en.wikipedia.org/wiki/De_Bruijn_indices
[hoas]: http://en.wikipedia.org/wiki/Higher_order_abstract_syntax


[^generator]: However, just from the type alone it's not always possible to tell whether a
 program is a generator or not. The general rule is that a function is a generator if it
 creates an AST based on the value of a host language term (i.e. `Int` in this case). In 
 fact, we cannot actually write the fibonacci function in the simply type lambda calculus 
 since it does not
 contain the notion of recursion.
[^tying-the-knot]: You can tell that this type ties the knot because `Exp` appears on the
  left and right hand side of the equals sign.