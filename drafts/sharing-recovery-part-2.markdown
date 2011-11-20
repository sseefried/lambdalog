---
title: Sharing Recovery in deeply embedded DSLs &mdash; Part 2
author: Sean Seefried
tags: domain specific languages, deep embedding, sharing
---

# Introduction

If haven't yet read [part 1](drafts/sharing-recovery-part-1.html) you should probably do that now.

In the last post we looked at what sharing recovery is and why it is required in deeply embedded DSLs. In this post
we describe the algorithm in more detail.

# The algorithm in more detail

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

## Dependency groups and the `SharedNodes` data structure.

Dependencies between subtrees induce the notion of *dependency groups*. A dependency group is
represented as a graph; the nodes are the stable expression names of the trees and the edges
the dependencies between them.

We use an [adjacency list][adjacency-list] representation for the graph. This requires the use
of [`HashMap`][hashmap] and [`HashSet`][hashset] data structures from the
[`unordered-containers`](http://hackage.haskell.org/package/unordered-containers) package
because stable names can only be compared for equality (and hence have no `Ord` instance).

~~~{.haskell}
data DepGroup = DepGroup { depGroupRoot  :: StableSharingExp
                         , sharedNodeMap :: HashMap StableExpName (StableSharingExp, Int)
                         , edges         :: HashMap StableExpName (HashSet StableExpName) }
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

## Joining *shared node collections*

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

## Merging dependency groups

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

# What happens as we go up the tree?

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

# Let's see an example

~~~{.haskell}
manyAdds :: Exp Integer
manyAdds = let one = constant 1
                two = constant 2
                add1 = one + two
                add2 = one + add1
            in add1 + add2
~~~

Before sharing recovery this AST looks like:

![AST of `manyAdds` before sharing recovery](/static/sharing-recovery/images/example_before.png)

In practice you will always get the same graph after phase 1 for this program, because the
algorithms descends depth first in a deterministic order. However, by just reversing the order
of the argument in `add1 + add2` one gets a different graph. One of these demonstrates very
nicely why we need to keep track of dependencies between `DepGroup`s. It looks like this:

![AST of `manyAdds` after phase 1 of sharing recovery](/static/sharing-recovery/images/example_1.png)



We have labeled the nodes in this graph using letters of the alphabet to illustrate what
is going on. Occurrence map is as follows:

* $occ(4) = 2$
* $occ(3) = 2$
* $occ(n) = 1$, for all other $n$.

[

Quick word on notation:

An entry in the sharedNodeMap of form `sn -> (X, n)` means that stable expression name maps to
node `X` and it has a sharing count of `n`.

]

Traversing bottom up. The order in which we will encounter nodes is F, G, D, E, B, C, A.

## Tree F

**[FIXME: use occ(SN_x) in most places]**

We leave tree $F$ as it is since $occ(F) = 1$. Shared nodes are empty

~~~{.haskell}
SharedNodes []
~~~

## Tree G

We leave $G$ as it is since it's a `VarSharing` node. The shared nodes collection maps
$4 \rightarrow (G, 1)$.

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot  = < G >
                      , sharedNodeMap = < [ 4 -> (G,1) ] >
                      , edges         = < [] > }]
~~~

**[FIXME: Put this text somewhere. We'll often say Tree X has occurrence count $n$. By this we mean that "the tree rooted at node
X (of type `SharingExp a`) has a stable expression name $sn$, which maps to occurrence count $n$. We might write this as $occ(X) = n$ ]**

## Tree D

Since $occ(D) = 2$, we replace it with node `VarSharing 3`. Call this tree $D'$.
Joining shared nodes and merging dependency groups just gives us the same dependency group as
we got for node $G$. We then insert a new mapping $3 \rightarrow (D, 1)$ and an edge $3 \rightarrow 4$.

~~~{.haskell}
SharedNodes [ DepGroup { depGroupRoot  = < D >
                       , sharedNodeMap = < [ 3 -> (D,1), 4 -> (G,1) ] >
                       , edges         = < [ 3 -> 4 ] > }] 
~~~

The AST now looks like: 

![AST of `manyAdds` after replacing $D$ with $D'$](/static/sharing-recovery/images/example_2.png)

## Tree E

Similar to tree $G$, $occ(E) = 2$ so we replace it with `VarSharing 4`. Call this tree $E'$. 

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot  = < E >
                      , sharedNodeMap = < [ 4 -> (E,1)] > 
                      , edges         = < [] > }]
~~~

![AST of `manyAdds` after replacing $E$ with $E'$](/static/sharing-recovery/images/example_3.png)

## Tree B

Now we get to an interesting part. Tree $B$ is not shared, but joining the shared nodes
collections of the children gives us: 

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot  = < D >
                      , sharedNodeMap = < [ 3 -> (D,1), 4 -> (G,2) ] >
                      , edges         = < [ 3 -> 4 ] > }]
~~~

(Note: <code>G `pickNoneVar` E == E</code>)

A naive, and incorrect algorithm, would attempt to insert a let-node at this point
because the sharing count of 4 at this point is equal to the occurrence count. The let-node
would have a bound expression of tree E. We simply can't do this because we have not yet
inserted a let-node whose bound expression will be tree D. It will be inserted "further up the
tree" so to speak, and it will depend on variable 4 being in scope.

**[FIXME: Have I adequately discussed scope?]**.

Put another way, we would be constructing an expression like:

`let v0 = 2 + v1 in let v1 = 1 in (v0 + v1) + v1`

## Tree C

Similar to Tree G.

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot  = < C >
                      , sharedNodeMap = < [ 3 -> (C, 1) ] >
                      , edges         = < [] > }]
~~~

## Tree A

Joining the shared nodes collection of tree **B** and tree **C** we get (noting that <code>D
`pickNoneVar` C = D</code>):

~~~{.haskell}
SharedNodes [DepGroup { depGroupRoot  = < D >
                      , sharedNodeMap = < [ 3 -> (D,2), 4 -> (G,2)] >
                      , edges         = < [ 3 -> 4 ] > }]
~~~

We now have a dependency group where each node has a sharing count equal to its occurrence
count. This means that we are ready to insert let nodes. We perform a [topological sort]()
and discover that the order of the let-nodes should be $[4, 3]$.

We insert the let nodes, in the appropriate order, finally getting.

![AST of `manyAdds` after sharing recovery](/static/sharing-recovery/images/example_after.png)

**[Let's get consistent with our notation. Perhaps we should have something like $SN_{4}$ for
  a stable expression name?
  - use different fonts in Haskell code for the trees. Requires modifications to Pandoc? Hope not.
]**

# Looking under the lambdas

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

The side effects of `makeOccurrenceMap` FIXME: Add explanation

# What makes our sharing recovery different?

**FIXME: Expand this section**

* mention related work in this area.

* Shallow embedding of type system in AST nodes.

* Using a Higher Order Abstract Syntax representation. How do you recover sharing "under the
  lambdas"?

# In the next episode

A proof of correctness.
