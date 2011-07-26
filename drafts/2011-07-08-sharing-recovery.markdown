---
title: Sharing Recovery in deeply embedded DSLs
author: Sean Seefried
tags: domain specific languages, deep embedding, sharing
hours: 7.5
---

# Introduction

I could easily start this post by banging on about how great embedded domain specific languages
are but that's already been done quite adequately in a [number]() [of]() [places](). There are
a two main options when it comes to implementing domain specific languages. They can be a:

* [shallow embeddeding][shallow-embedding] or [internal DSL][dsl-definitions]. In this case the
  terms of the [target language][target-language] are terms in the *host
  language*. e.g. functions in the target language are functions in the host language. In this
  case one runs a program by compiling the program. The code generated is just the code that is
  normally generated for the host language since terms in the target language *are* terms in
  the host language.

* [deep embedding] or [external DSL][dsl-definitions]. In this case the terms of the target
  language are instances of a data structure in the host language (usually an [abstract syntax
  tree][ast] or AST). The terms are constructed using various helper functions and are
  interpreted or compiled. This differs from the shallow embedding case because one must write
  the interpreter/compiler and does not gain the benefit of reusing the host language's
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

I'll give you the definition of this language soon but, for the moment, just trust me when I say
this Haskell program generates the following AST

~~~{.haskell}
Add (Add (Const 2) (Const 3)) (Add (Const 2) (Const 3))
~~~

Notice that the sharing implicit in the Haskell let-expression no longer exists. What we would
prefer it to generate is something more like:

~~~{.haskell}
LetE (Add (Const 2) (Const 3)) (Add (Lvar 0) (Lvar 0))
~~~

We're using [de Bruijn indices]() to refer to variables. Here we have introduced a *let-node*
which binds let-variable $0$ to the expression $2 + 3$ in the expression ${0} + {0}$ (using the
notation that ${x}$ refers to let-variable (`Lvar`)with de Bruijn index $x$).

## Memory usage is the real problem

You might be thinking, what's the big deal? Who cares if a term gets replicated a few
times. Can't we always recover the sharing using *common sub-expression elimination*
([CSE][cse])?  This is true (if inefficient) but consider the case where one wants to generate
the [unrolled][loop-unwinding] code that finds the $n$th Fibonacci number.

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
this integer is a host language term and not a target langauge term. Consequently we call this
style of program a *generator*. However, just from the type alone it's not always possible to
tell whether a program is a generator or not. The general rule is that a function is a
generator if it creates an AST based on the value of a host language term (i.e. `Int` in this
case). In fact, we cannot actually write the fibonacci function in the simply type lambda
calculus since it does not contain the notion of recursion.

In fact, function `fib`, will produce an AST of size proportional to the $n$th fibonacci
number! This sounds terrible but only because this is its *conceptual* size. In fact, things
aren't nearly so dire; even if one were to fully evaluate the closure `fib 4` it would still
not take up that much memory since there is *implicit sharing* in the heap of the Haskell
run-time.

Unfortunately, this implicit sharing can easily be lost. One obvious way, which happened to us
during the development of Accelerate, is to convert the AST to another data structure.  In our
case, converting from a HOAS representation to an explict representation using de Bruijn
indices led to the computer's memory being completely consumed by multi-gigabyte ASTs.

The essence of *sharing recovery* is to take this implicit sharing in the heap and modify the
AST to represent it explicitly. We'll do this by looking at what `fib 4` looks like in the heap
when fully evaluated. But before I can do that adequately I'll need to introduce the data type
for the AST of our language. After that we'll be in much better shape to start talking about
sharing recovery.

# The AST

Our AST represents terms of the simply typed lambda calculus with arithmetic.

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
arithmetic (**STLCWA???**), we distinguish them from values with primitive types using two
different data types so that we can ensure that only functions can be applied to other values.

### Pre-recursive data types

During sharing recovery we need to annotate the original AST with new nodes. The standard
technique to do this is define a *pre-recursive type* where each recursive occurrence of a type
is replaced with a *type parameter*.

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

We have used convention of prefixing the name of the type with `Pre` to highlight that it is
pre-recursive. The newtype `Exp` "ties the knot" ensure that all sub-expressions and
sub-functions are of type `Exp` and `Fun` respectively. (You can tell that this type ties the
knot because `Exp` appears on the left and right hand side of the equals sign.)

## What `fib 4` looks like in the heap

**FIXME: Use dot plugin**

This is what it looks like in the heap.

![AST of `fib 4` in the heap](/static/images/fib_4_heap.jpg)

I've used some syntactic conventions here:

* Rectangular nodes are of type `Exp a`
* Elliptical nodes are of type `PreExp Exp Fun a`

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

![`fib 4` with sharing recovered](/static/images/fib_4_recovered.jpg)

The notation this time around is:

* Rectangular boxes are nodes of type `LetSharing a`.
* Ellipses are, as they did before,  `PreExp` nodes but this time of their full type is
  `PreExp SharingExp SharingFun a`.

## A two phase approach

Sharing recovery is done in two phases. As a very high level overview:

* Phase 1 counts, through a top-down traversal, how many times each node is shared in the AST.
* Phase 2 insert let-nodes through a bottom-traversal.

Of course it's a little more detailed than that. In phase one we simultaneously create an
occurrence map while creating an annotated tree *without sharing recovered*.

An occurrence map is a mapping from stable names of `PreExp` nodes to the number of times that
node is shared in the heap. We call the number of times a node has been shared in the heap its
*occurrence count*. Identically we say that this is the occurrence count of the stable name.

### Phase 1
Phase one traverses the tree in a *depth first* manner; it is a top-down traversal.

* If we encounter an `Exp` node we have *not* seen before we wrap up the `PreExp` node
  contained within it with a `ExpSharing` node along with the stable name of the original `Exp`
  node it was under.  * If we encounter an `Exp` node we have seen before we replace it with a
  `VarSharing` node and increment the count in the occurrence map.

Expression `fib 4` produces a tree like this:

**In the final version of this blog post try produce the graphs all at once so that the stable
  names are all the same**

![`fib 4` after phase 1](/static/images/fib_4_after_phase_1.jpg)

In this figure we show the stable name of the `Exp` node that each `ExpSharing` replaced. (This
information is stored in each `ExpSharing` node; see the definition above. **LINK ME**.) The
numbers after the `VarSharing` nodes refer to these stable names. The idea behind them is that
they stand in place of the `ExpSharing` node containing the particular stable name of an `Exp`
node.

### Phase 2

Phase 2 of the algorithm is a bottom-up traversal of the tree. The occurrence map from phase 1
is an input into phase 2. As we move up the tree we keep track of how many times we have seen
the stable names stored in `ExpSharing` nodes. When we have seen it as many times as its count
in the occurrence map we insert a `LetSharing` node whose body is a new `VarSharing` node and
whose bound expression is the expression so far.

**THERE IS AN OPPORTUNITY HERE FOR SOME FAIRLY FORMAL DEFINITIONS**

For instance, in figure **LINK** the `ExpSharing` nodes with `Exp` stable names $26$ **FIXME**
and $25$ **FIXME** will become the bound expressions of let-nodes. Let's use the abbreviation
that $ES_n$ (for some n) refers to `ExpSharing` node with `Exp` stable name $n$ and $VS_n$ for
the `VarSharing` node standing in place of $ES_n$.  Now consider the left branch of the tree
and travel up from the leaves, stopping at each `ExpSharing` or `VarSharing` node. At $ES_{26}$
and $VS_{26}$ we have seen stable name $26$ once. Continue travelling upwards to
$ES_{25}$. Here we have seen stable name $26$ twice. But it's occurrence count in the entire
AST is 3 so this is still not enough. Only when we reach $ES_{24}$ have we seen it 3
times. This is the point to insert the `LetSharing` node (i.e. the let-node) with bound
expression equal to the subtree rooted at $ES_{26}$. Incidentally this is also the place to
insert another `LetSharing` node whose bound expression is the subtree rooted at
$ES_{25}$. Note that this bound expression depends on the bound expression of $LS_{25}$ so
getting the order right is important.

In fact, the description given so far is not the full story. It's a little more complicated
than that.  You cannot necessarily insert a let-node when you have seen a stable name as many
times as its occurrence count. This is because the bound expression of that let-node may
contain `VarSharing` nodes for which there are no let-nodes yet inserted. You actually need to
maintain dependencies between bound expressions and the let-nodes they depend on.

**YOU HAVE NOT YET MENTIONED THE REPLACEMENT OF EXPSHARING WITH VARSHARING NODES**

**TODO**

## The algorithm in more detail

The AST after Phase 1 of the algorithm is an intermediate, ill-formed state. The job of Phase 2
is to replace shared subtrees with let-variable-nodes and move them to become the bound
expressions of newly inserted let-nodes.  Subtrees are merely shuffled around; they are neither
replicated nor removed.  When we replace a shared subtree with a let-variable-node we need to
keep that subtree somewhere until it is ready to be inserted as the bound expression of a
let-node.  We introduce a new data structure, called `SharedNodes`, to achieve this aim.

The `SharedNodes` data structure is responsible for:

* storing shared subtrees that have been replaced with let-variable-nodes.
* maintaining a count of how many times a particular stable name has been seen. We call this the
  *sharing count* (to be distinguished from the *occurrence count*)
* maintaining dependencies between shared subtrees.

We say a let-variable-node *references* a shared subtree (that will eventually become the bound
expression of a let-node).

**FIXME: NEED TO PUT THIS DEFINITION SOMEWHERE EARLIER PERHAPS?**

The *stable expression name* is the stable name of the original `Exp` node that a `ExpSharing`
or `VarSharing` node replaced during Phase 1 of the sharing recovery algorithm. Throughout my
exposition `ExpSharing` and `VarSharing` will be identified by these stable expression
names. i.e. I will use phrases like "the stable expression name of node $x$". I will also speak
of stable expression names of trees (and subtrees). This is intended to mean the stable
expression name of the root node of the tree.

A dependency between two shared subtrees, $T_{1}$ and $T_{2}$, exists when $T_{1}$ contains a
let-variable-node that references $T_{2}$.

### Dependency groups and the `SharedNodes` data structure.

Dependencies between subtrees induce the notion of *dependency groups*. A dependency group is
represented as a graph; the nodes are the stable expression names of the trees and the edges
the dependencies between them.

We use an [adjacency list][adjacency-list] representation for the graph. This requires the use
of [`HashMap`][hashmap] and [`HashSet`][hashset] data structures from the
[`unordered-containers`](http://hackage.haskell.org/package/unordered-containers) package
because stable names can only be compared for equality (and hence have no `Ord` instance).

~~~{.haskell}
data DepGroup = DepGroup { sharedNodeMap :: HashMap StableExpName (StableSharingExp, Int)
                         , edges       :: HashMap StableExpName (HashSet StableExpName) }
~~~

The nodes of the graph are simply the keys of the `sharedNodeMap`: stable names. 
The values are pairs of the shared subtrees and currently observed count.

The `SharedNodes` data structure is simply a list of dependency groups.

~~~{.haskell}
newtype SharedNodes = SharedNodes [DepGroup] deriving Show
~~~

We maintain the invariant that all dependency groups within a shared nodes collection are
*independent*; they don't overlap. More formally, if $G_1$ and $G_2$ are dependency groups then
$nodes(G_1) \cap nodes(G_2) = \varnothing$.

### Joining *shared node collections*

Shared node collections are lists of dependency groups. Joining two shared node collections
requires that we join the two lists of dependency groups somehow. Each of the dependency groups
in each shared node collection is guaranteed to be independent, but naturally, this guarantee
doesn't hold between the two lists of dependency groups. Joining then, is simply the process of
finding which dependency groups overlap and *merging* them together.

~~~{.haskell}
(+++) :: SharedNodes -> SharedNodes -> SharedNodes
SharedNodes us +++ SharedNodes vs = SharedNodes $ merge us vs
  where
    merge :: [DepGroup] -> [DepGroup] -> [DepGroup]
    merge []                         ys                         = ys
    merge xs                         []                         = xs
    merge (xs:xss) yss                  = mergeInto xs (merge xss yss)

    mergeInto :: DepGroup -> [DepGroup] -> [DepGroup]
    mergeInto xs [] = [xs]
    mergeInto xs (ys:yss)
      | overlap xs ys = mergeInto (mergeDepGroup xs ys) yss
      | otherwise     = ys:mergeInto xs yss

    -- Note: This is quadratic in complexity. HashSet does not have an 'intersection' method
    -- since implementing it efficienctially would require an ordering on elements in the set.
    overlap :: DepGroup -> DepGroup -> Bool
    overlap dg1 dg2 = overlap' (HashMap.keys $ sharedNodeMap dg1)
                               (HashMap.keys $ sharedNodeMap dg2)
      where
        overlap' []  _     = False
        overlap' (x:xs) ys = x `elem` ys || overlap' xs ys
~~~

### Merging dependency groups

But what do we mean by merging a dependency group? Briefly, this means that we take the union
of the nodes and the edges, ensuring that the sharing counts of nodes in both are summed
together to get the new sharing count.

~~~{.haskell}
mergeDepGroup :: DepGroup -> DepGroup -> DepGroup
mergeDepGroup dg1 dg2 = DepGroup newSharedNodeMap newEdges
  where
    newSharedNodeMap = Map.foldlWithKey' (\m k v -> Map.insertWith updateCount k v m)
                                 (sharedNodeMap dg1) (sharedNodeMap dg2)
    updateCount (sa1, count1) (sa2, count2) = (sa1 `pickNoneVar` sa2, count1 + count2)
    newEdges = Map.foldlWithKey' (\m k v -> Map.insertWith Set.union k v m)
                                 (edges dg1) (edges dg2)

pickNoneVar :: StableSharingExp -> StableSharingExp -> StableSharingExp
(StableSharingExp _ (VarSharing _)) `pickNoneVar` sa2                                 = sa2
sa1                                 `pickNoneVar` _sa2                                = sa1
~~~

The new shared node map is generated by inserting all the values from the second map into the
first.  If two values have the same key (i.e. stable name) we then merge the values. This is
done via `updateCount` which sums their counts and picks the `StableSharingExp` containing a
non-`VarSharing` node (if any). A non-`VarSharing` node will be picked by the time a shared
node's count is equal to its occurrence count. (See proof of correctness later.)

The edges of the new dependency group are created by inserting all the adjacency lists from the
second dependency group into the first. If two values have the same key (i.e. stable name) then
the new adjacency list is taken to be union of the two existing adjacency lists for that key in
their respective dependency groups.

## What happens as we go up the tree?

If, and only if, the node we are currently examining (let's call it $N$):

* has an occurrence count greater than 1.
* is an `ExpSharing` node.

then

1. We replace the node with a `VarSharing` node.
2. We create a `SharedNodes` structure containing one big dependency group by: 
  * inserting into the `sharedNodeMap` the tree we just replaced and incrementing its 
    count. (A node with the same stable expression name might already be there.)  
  * joining the all the child `SharedNodes` structures into one. Call this the
    *children shared nodes collection* (CSNC) **FIMXE: CRAP DEFINITION** * merging all 
    the dependency groups in the CSNC into one big dependency group.  This reflects the 
    fact that the shared tree we have just replaced with a `VarSharing` node depends on all 
    the other shared trees that were replaced by `VarSharing` nodes within it.  
    **FIXME: AWKWARD BUT ON THE RIGHT TRACK** * inserting an edge from stable 
    expression name of $N$ to stable expression names of the root nodes of all the 
    child dependency groups.
    
The code to do this is here:

~~~{.haskell}
sharedNode :: StableSharingExp -> SharedNodes -> SharedNodes
sharedNode stableSharingExp (SharedNodes depGroups) =
  SharedNodes $ [depGroup]
  where
    mergedDepGroup :: DepGroup
    mergedDepGroup = foldl mergeDepGroup (emptyDepGroup stableSharingExp) depGroups
    depGroup :: DepGroup
    depGroup =  foldr (depGroupInsertEdge stableSharingExp)
                      (depGroupInsertBoundExp stableSharingExp mergedDepGroup)
                      (map (stableExpNameOf . depGroupRoot) depGroups)

depGroupInsertBoundExp :: StableSharingExp -> DepGroup -> DepGroup
depGroupInsertBoundExp sa dg = dg { sharedNodeMap = newSharedNodeMap }
  where
    san = stableExpNameOf sa
    newNode = case HashMap.lookup san (sharedNodeMap dg) of
                 Just (sa', count) -> (sa `pickNoneVar` sa', 1 + count)
                 Nothing            -> (sa, 1)
    newSharedNodeMap = HashMap.insert san newNode (sharedNodeMap dg)

-- Precondition: The node must already be a member
depGroupInsertEdge :: StableSharingExp -> StableExpName -> DepGroup -> DepGroup
depGroupInsertEdge src tgtSA dg = dg { edges = newEdges }
  where
    srcSA    = stableExpNameOf src
    newEdges = HashMap.insertWith HashSet.union srcSA (HashSet.singleton tgtSA) (edges dg)
~~~

## Let's see an example

~~~{.haskell}
manyAdds :: Exp Integer
manyAdds = let one = constant 1
                two = constant 2
                add1 = one + two
                add2 = one + add1
            in add1 + add2
~~~

In practice you will always get the same graph after phase 1 for this program, because the
algorithms descends depth first in a deterministic order. However, by just reversing the order
of the argument in `add1 + add2` one gets a different graph. One of these demonstrates very
nicely why we need to keep track of dependencies between `DepGroup`s.

**PUT IN PICTURE OF THIS GRAPH. SHOW THE OTHER TOO See many_adds_after_phase1.ps*.

We have labeled the nodes in this graph using letters of the alphabet to illustrate what
is going on. Occurrence map is as follows:

* 2756 -> 2
* 2755 -> 2
* all other stable names occur 1 time.

[

Quick word on notation:

An entry in the sharedNodeMap of form `sn -> (X, n)` means that stable expression name maps to
node `X` and it has a sharing count of `n`.

]

Traversing bottom up. The order in which we will encounter nodes is F, G, D, E, B, C, A.

### Tree F

We do nothing to **F** since 2733 has occurrence count 1. Shared nodes are empty

~~~{.haskell}
SharedNodes []
~~~

### Tree G

We do nothing to **G** since its a `VarSharing` node. Create a `SharedNodes` object as
follows. `depGroup` is set **G**. `sharedNodeMap` maps 2756 to **G** with count 1.

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot = G
                      , sharedNodeMap = [ 2756 -> (G, 1) ]
                      , edges = [] }]
~~~

[We'll often say Tree X has occurrence count $n$. By this we mean that "the tree rooted at node
X (of type `SharingExp a`) has a stable expression name $sn$, which maps to occurrence count $n$]

### Tree D

Tree D has occurrence count 2. We replace it with node `VarSharing 2755`. Call this **D'**.  
Joining shared nodes and merging dependency groups just gives us the same dependency group as
we got for node **G**. We then insert a new mapping `2755 -> (D,1)` and an edge `2755 -> 2756`.

~~~{.haskell}
SharedNodes [ DepGroup { depGroupRoot = D
                       , sharedNodeMap = [2755 -> (D,1), 2756 -> (G,1) ]
                       , edges = [ 2755 -> 2756 ] }]
~~~

### Tree E

Tree E has occurrence count 2. We replace it with `VarSharing 2756`. Call this **E'**

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot = E, sharedNodeMap = [ 2756 -> (E,1)] }]
~~~

### Tree B

Now we get to an interesting part. Tree B is not shared, but joining the shared nodes
collection of the children gives us: 

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot = D
                      , sharedNodeMap = [2755 -> (D,1), 2756 -> (G,2)]
                      , edges = [ 2755 -> 2756 ] }]
~~~

(Note: <code>G `pickNoneVar` E = E</code>) **[FIXME: Perhaps use LaTeX equiv?]**

A naive, and incorrect algorithm, would attempt to insert a let-node at this point
because the sharing count of 2756 at this point is equal to the occurrence count. The let-node
would have a bound expression of tree E. We simply can't do this because we have not yet
inserted a let-node whose bound expression will be tree D. It will be inserted "further up the
tree" so to speak, and it will depend on variable 2756 being in scope.

**[FIXME: Have I adequately discussed scope?]**.

Put another way, we would be constructing an expression like:

`let v0 = 2 + v1 in let v1 = 1 in (v0 + v1) + v1`

### Tree C

Similar to Tree G.


~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot = C
                      , sharedNodeMap = [ 2755 -> (C, 1) ]
                      , edges = [] }]
~~~

### Tree A

Joining the shared nodes collection of tree **B** and tree **C** we get (noting that <code>D
`pickNoneVar` C = D</code>):

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot = D
                      , sharedNodeMap = [2755 -> (D,2), 2756 -> (G,2)]
                      , edges = [ 2755 -> 2756 ] }]
~~~

We now have a dependency group where each node has a sharing count equal to its occurrence
count. This means that we are ready to insert let nodes. We perform a [topological sort]()
and discover that the order of the let-nodes should be $2756, 2755$.

**[Let's get consistent with our notation. Perhaps we should have something like $SN_2756$ for
  a stable expression name?
  - use different fonts in Haskell code for the trees. Requires modifications to Pandoc? Hope not.
]**

## Looking under the lambdas

Our representation of the simply typed lambda calculus with arithmetic uses Higher Order
Abstract Syntax. How then do we recover sharing when it is, so to speak, "under a lambda"? The
short answer is that we convert from HOAS to an explicit representation of lambda terms.

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

The AST itself is not that big. Its size is proportional to $n$ due to the fact that is just
building up a closure on the run-time heap. However, without sharing recovery it will run in
exponential time. Also, if it is converted to another intermediate form, say one using de
Bruijn indices, its size will explode as per usual.

As a crude measure of this let's see what happens when we convert to de Bruijn form and then
find the length of the `show`ed result.

~~~
> map (\n -> length (show (convert $ powTwo n))) [1..10]
[81,189,405,837,1701,3429,6885,13797,27621,55269]
~~~

It is clear that sharing recovery needs to be able to look under lambdas. The solution is not
too difficult in principle but has some subtleties. The basic idea is to apply the Haskell
function under the `Lam` node to a dummy tag, thus creating a value of `PreExp Exp Fun a`,
during Phase 1.

Remember, I said I'd explain what the `SharingFun` type was all about. This is where it comes
into its own. We generated a unique integer $i$, apply the higher order function to `Tag i`
thus yielding a value of type `PreExp Exp Fun a` (for some `a`). We then recursively traverse
into that yielding a value of type `PreExp SharingExp SharingFun a` and finally wrapped that in
the `TaggedSharingExp` constructor, yielding a value of type `SharingFun a`.

The unique value, $i$, essentially acts as a variable name. We have converted from HOAS to an
explicit, named, representation of lambda terms. Once sharing recovery has been done this can
easily be converted back into a HOAS representation if desired. 

**FIXME: Possibly cut**
-------------
We personally are not interested in that.
We are only interested in converting to de Bruijn notation, which is also
straightforward. Here's how:

* Initialise a finite map from unique identifiers to de Bruijn indices. Call this the *tag map*
* Traverse the `SharingExp a` data structure.
* Keep track of how many lambdas one is under (i.e. how many `TaggedSharingExp`s one is under)
* When one pattern matches on a `TaggedSharingExp` add to the tag map a mapping from the
  unique identifier to the current lambda depth.
* When one pattern matches on `Tag j`, look up the mapping of $j$ in the finite map and replace it
  with the corresponding de Bruijn index.
------------

# Referential transparency

Technically we lose referential transparency when we are able to observe the sharing implicit
in the heap.  Consider some expression $E$. The essence of observable sharing is that one can
get a reference to an expression and then determine whether two expressions are the same in the
heap or not. If we call this built-in operator `ref` then `ref $E$ == ref $E$` could evaluate
to either `True` or `False`, thus breaking referential transparency.

We use the solution of [Gill et all](FIXME) which uses stable names. These references can only
be obtained in the `IO` monad, when makes it clear that referential transparency may be
broken.

# Weak Head Normal Form

It's very important that we evaluate expressions to WHNF *before* obtaining their stable
names. Otherwise we may be obtaining the stable name of a closure that will only be evaluated
and garbage collected once we evaluate it later.

We limit the portion of the program that must evaluate inside the referentially opaque domain
of the `IO` monad. We traverse the AST, evaluating each node (of type `Exp a`) to WHNF before obtaining its
stable name. At precisely this moment we throw away the old node and replace it with one of
type `SharingExp a` which contains (within its constructor) the stable name of the node of type
`Exp a`.  In doing this we replace an AST whose structure in the heap we care about with an
annotated AST that contains a snapshot of this information, but is itself referentially
transparent.

Technically it is possible that the stable name of an object in the heap will change during the
traversal (due to garbage collection), but this happens rarely in practice.

# Why is `unsafePerformIO` safe?

The side effects of `makeOccurrenceMap`

# What makes our sharing recovery different?

**FIXME: Expand this section**

* mention related work in this area.

* Shallow embedding of type system in AST nodes.

* Using a Higher Order Abstract Syntax representation. How do you recover sharing "under the
  lambdas"?

# In the next episode

A proof of correctness.

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

