---
title: Definition follows meaning
category: haskell
tags: Haskell, type classes, type class morphisms
---

# Introduction

Recently I spent some time visiting Conal Elliott.  While I was there I asked him more
about his *type class morphism* principle. The concept always struck me as incredibly useful but I
found my understanding of it to be quite tenuous. I resolved to understand it by the time I left
his place. To help me he posed a real example that he had encountered in his work to me as a
puzzle. This blog post will explain the process I went through to derive a solution.

# Uniform pairs

A uniform pair is a pair which has the same type for each element of the pair.

~~~{.haskell}
data Pair a = Pair a a
~~~

The puzzle: given this definition what instances would you provide for `Functor`, `Applicative`,
`Monad`, etc? Once you had come up with a definition how would you know that they were the *right*
ones? They might well satisfy the laws for each type class but is this enough? We'll see that type
class morphisms provide an extra level of assurance that we have found suitable definitions.

# Meanings

The genesis of the type class morphism principle came up in Conal's work on functional reactive
programing. He wanted users of the library to think of the `Behaviour` type as being a function of
type `Time -> a`. Put another way, the "meaning" of `Behaviour` was functions. Conal became
concerned that the instances he'd defined on `Behaviour` might not match up with the instances of
the same type classes on functions, so he decided to check. To his delight they were the same. Thus
a general general principle was born: the type class instances for the meaning of a data type could
be used to inform the type classes instances one wrote for the data type itself. Put succinctly the
principle is: *The instance's meanings should follow meaning's instances*. The original post can be
found [here](http://lambdalog.seanseefried.com/drafts/definition-follows-meaning.html]).

## Uniform pairs are functions on `Bool`s

So, what is a suitable meaning for the uniform pair? Well, it turns out that there is an interesting
isomorphism between data structures and functions. The full details are beyond the scope of this
post. However, it's not too much of a stretch of the imagination to think of a uniform pair of
things as a function from booleans to things. i.e.

~~~{.haskell}
⟦⋅⟧ :: Pair a ⟶ (Bool ⟶ a)
⟦ Pair a b ⟧ ≣ λx → if x then a else b
~~~

## Denotational semantics

Here is a quick intro to what we're doing with denotational semantics.  We'll start with the
traditional formulation and I'll show you how we're using it in a slightly different way.

Denotational semantics is a way of precisely specifying the meanings of languages. This is done by
defining a mapping from the language's *syntax* to a mathematical model, its *meaning*.

For a syntactic category ℂ we specify:

* a mathematical model ⟦ℂ⟧ of meanings
* a semantic function that maps syntax to meanings. ⟦⋅⟧<sub>ℂ</sub> :: ℂ ⟶ ⟦ℂ⟧

We write ⟦⋅⟧ when the syntactic category is unambiguous and, for a given syntactic expression *e*,
we write ⟦*e*⟧ as shorthand for ⟦⋅⟧ *e*.

### Data types not syntax

Where we depart from the traditional formulation of denotational semantics is this. Instead of
mapping syntax to meanings we're mapping meanings to meanings. Specifically we're mapping from one
data type to another data type. The next question is, what language should we use to express our
meanings?

### Haskell as our model of meanings

We use Haskell as our model of meanings, both as the domain and range of the meaning function. This
can be a bit confusing, especially if one is familiar with the traditional formulation of
denotational semantics.

Let's say our meaning function has type T ⟶ Tʹ. We will write equations that define the meaning
function as follows:

~~~
⟦t⟧ ≣ tʹ
~~~

Obviously *t* and *tʹ* are syntactic. All meanings need to be represented syntactically. But please
don't treat them as syntax, but instead treat them as the mathematical entities. Another way to look
at it is this: there is another, implicit, meaning function from Haskell syntax to the mathematical
objects they denote. Every time we see a piece of Haskell syntax one can think of this meaning
function being applied behind the scenes. However, as long as this is understood we can safely elide
it from our presentation.

# What is a type class morphism?

A type class morphism expresses a relationship between:

a) the meaning of a type class method applied to values of the data type, and
b) the meaning's type class method applied to the meanings of the data type.

This relationship *preserves the structure* of the type class methods.

## Be careful about types

In the following definitions be aware that although occurrences of type class methods will appear on
the left and right hand side of the equations they are not the same; they are defined on different
types.

## The `Functor` morphism

The functor morphism is equivalent to a *natural transformation* in category theory.

~~~{.haskell}
⟦ fmap f m ⟧ ≣ fmap f ⟦ m ⟧
~~~

## The `Applicative` morphism

~~~{.haskell}
⟦ pure a ⟧    = pure a
⟦ mf ⊛ mx ⟧ = ⟦mf⟧ ⊛ ⟦mx⟧
~~~

## The `Monad` morphism

~~~{.haskell}
⟦ return a ⟧ ≣ return a
⟦ join mm  ⟧ ≣ (join ∘ fmap ⟦⋅⟧) ⟦mm⟧
~~~

or in terms of bind:

~~~{.haskell}
⟦ u >>= k⟧ ≣ ⟦u⟧ >>= ⟦⋅⟧ ∘ k
~~~

## Laws are preserved by morphisms

The wonderful thing about these morphisms is that the ensure that if the laws (e.g. Monad laws) hold
for the meaning of a data type then they will also hold for the data type itself. Conal puts this
nicely: the laws come if not "for free" then at least "already paid for".

I'll demonstrate preservation of laws for the Applicative type class and leave the others an
exercise.

### Proof that Applicative morphism preserves Applicative laws

The laws that we need to show are preserved are:

* **Identity**     -- *pure id ⊛ u = u*
* **Composition**  -- *pure (∘) ⊛ u ⊛ v ⊛ w = u ⊛ (v ⊛ w)*
* **Homomorphism** -- *pure f ⊛ pure x = pure (f x)*
* **Interchange**  -- *u ⊛ pure x = pure (λf → f x) ⊛ u*

Note, we are proving these laws on *any* meaning function *⟦⋅⟧ :: D* ⟶ *Dʹ*.

#### Identity law

Need to show that if *pure id ∘ u = u* then *⟦pure id ∘ u⟧ = ⟦u⟧*

~~~{.haskell}
   ⟦pure id ⊛ u⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   ⟦pure id⟧ ⊛ ⟦u⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   pure id ⊛ ⟦u⟧
≣ {- Identity on meaning -}
   ⟦u⟧
~~~

#### Homomorphism law

~~~{.haskell}
   ⟦pure f ⊛ pure x⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   ⟦pure f⟧ ⊛ ⟦pure x⟧
≣ {- ⟦⋅⟧ is an Applicative morphism (twice) -}
   pure f ⊛ pure x
≣ {- Homomorphism law on the meaning -}
   pure (f x)
  {- ⟦⋅⟧ is an Applicative morphism (backwards) -}
≣  ⟦pure (f x)⟧
~~~

#### Composition law

~~~{.haskell}
   ⟦pure (∘) ⊛ u ⊛ v ⊛ w⟧
≣ {- ⟦⋅⟧ is an Applicative morphism (applied thrice)-}
   ⟦pure (∘)⟧ ⊛ ⟦u⟧ ⊛ ⟦v⟧ ⊛ ⟦w⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   pure (∘) ⊛ ⟦u⟧ ⊛ ⟦v⟧ ⊛ ⟦w⟧
≣ {- Composition law on meaning -}
   ⟦u⟧ ⊛ (⟦v⟧ ⊛ ⟦w⟧)
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   ⟦u⟧ ⊛ ⟦(v ⊛ w)⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   ⟦u ⊛ (v ⊛ w)⟧
~~~

#### Interchange law

~~~{.haskell}
    ⟦u ⊛ pure x⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
    ⟦u⟧ ⊛ ⟦pure x⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
    ⟦u⟧ ⊛ pure x
≣ {- Interchange law -}
    pure (λf → f x) ⊛ ⟦u⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
    ⟦pure (λf → f x)⟧ ⊛ ⟦u⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
    ⟦pure (λf → f x) ⊛ u⟧
~~~

# Preliminaries

With that out of the way we are now almost ready to derive instance methods for uniform pairs. First
we define a function that will simplify some of the equational reasoning to come.

~~~{.haskell}
cond :: a ⟶ a ⟶ Bool ⟶ a
cond e t c = if c then t else e
~~~

Next let's define the meaning `Pair` in terms of `cond`.

~~~{.haskell}
⟦Pair a b⟧ ≣ cond a b
~~~

Here are some obvious properties of `cond`.

~~~{.haskell}
cond a b False ≣ a
cond a b True  ≣ b
~~~

Here are a couple of properties of `cond` that we will use later.

## Composition
~~~{.haskell}
    f ∘ cond e t
= {- Definition of cond -}
    f ∘ (\c → if c then t else e)
= {- Definition of (∘) -}
    \x → f ((\c → if c then t else e) c)
= {- Simplify -}
    \x → f (if x then t else e)
= {- Distribute f over if-expression-}
    \x → if x then f t else f e
= {- Definition of cond -}
    cond (f e) (f t)
~~~

## Constant
~~~{.haskell}
    cond a a
= {- Definition of cond -}
    \c → if c then a else a
= {- Property of if-expression -}
    \c → a
= {- Definition of const -}
    const a

# Derivation

And now we are finally ready to derive instance implementations. Each derivation follows the same
basic structure:

* the type class morphism is applied
* various equational reasoning techniques are used
* the meaning function is applied in reverse (i.e. RHS is replaced with LHS)

An important thing to remember is that the resulting implementations may not be
the only ones. They are merely *sufficient* not *necessary*.

## `Functor` instance

~~~{.haskell}
   ⟦ fmap f (Pair a b) ⟧
≣ {- functor morphism -}
   fmap f ⟦ Pair a b ⟧
≣ {- meaning of Pair -}
   fmap f (cond a b)
≣ {- fmap is composition for functions -}
   f ∘ cond a b
≣ {- "Composition" law of cond -}
   cond (f a) (f b)
≣ {- Meaning of Pair }
   ⟦ Pair (f a) (f b) ⟧
~~~

Thus as sufficient `Functor` instance is:

~~~{.haskell}
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair (f a) (f b)
~~~

## `Applicative` instance

### Definition of `pure`

~~~{.haskell}
   ⟦ pure a ⟧
≣ {- Applicative morphism -}
   pure a
≣ {- pure on (→) r -}
   const a
≣ {- "Constant" law of cond -}
   cond a a
≣ {- Meaning of Pair -}
   ⟦ Pair a a ⟧
~~~

### Definition of `⊛`

~~~{.haskell}
   ⟦ (Pair fa fb) ⊛ (Pair a b) ⟧
≣ {- Applicative morphism -}
   ⟦ Pair fa fb ⟧ ⊛ ⟦ Pair a b ⟧
≣ {- ⊛ on (⟶) r -}
   λx → ⟦ Pair fa fb ⟧ x (⟦ Pair a b ⟧ x)
≣ {- Meaning of Pair -}
   λx → cond fa fb x (cond a b x)
~~~

Now we perform a case analysis.

#### Case `x ≣ True`

~~~{.haskell}
   cond fa fb True (cond a b True)
≣ {- cond applied to True -}
   fb (cond a b True)
≣ {- cond applied to True -}
   fb b
~~~

#### Case `x ≣ False`

~~~{.haskell}
   cond fa fb False (cond a b False)
≣ {- cond applied to False -}
   fa (cond a b False)
≣ {- cond applied to False -}
   fa a
~~~

Thus, in the end we get:

~~~{.haskell}
   cond (fa a) (fb b)
≣ {- Meaning of Pair -}
  ⟦ Pair (fa a) (fb b) ⟧
~~~

Therefore a sufficient implementation is:

~~~{.haskell}
instance Applicative (Pair a) where
  pure a                  = Pair a a
  (Pair fa fb) ⊛ Pair a b = Pair (fa a) (fb b)
~~~

## `Monad` instance

~~~{.haskell}
  ⟦ join (Pair (Pair a aʹ) (Pair b bʹ)) ⟧
≣ {- Monad morphism -}
  join (fmap ⟦⋅⟧ ⟦ Pair (Pair a aʹ) (Pair b bʹ) ⟧)
≣ {- Meaning of Pair -}
  join (fmap ⟦⋅⟧ (cond (Pair a aʹ) (Pair b bʹ)))
≣ {- fmap on (⟶) r -}
  join (⟦⋅⟧ ∘ (cond (Pair a aʹ) (Pair b bʹ)))
≣ {- Property 1 of cond -}
  join (cond ⟦Pair a aʹ⟧ ⟦Pair b bʹ⟧
≣ {- Meaning of Pair -}
  join (cond (cond a aʹ) (cond b bʹ))
≣ {- join on (⟶) r -}
  λr → (cond (cond a aʹ) (cond b bʹ)) r r
~~~

Now for some case analysis on `r`.

#### Case `r ≣ True`

~~~{.haskell}
   cond (cond a aʹ) (cond b bʹ)) True True
≣ {- cond applied to True -}
   cond b bʹ True
≣ {- cond applied to True -}
   bʹ
~~~

#### Case `r ≣ False`

~~~{.haskell}
   cond (cond a aʹ) (cond b bʹ)) False False
≣ {- cond applied to False -}
   cond a aʹ False
≣ {- cond applied to False -}
   a
~~~

Thus we get

~~~{.haskell}
  cond a bʹ
≣ {- Meaning of Pair -}
  ⟦ Pair a bʹ⟧
~~~

Therefore a sufficient `Monad` instance is:

~~~{.haskell}
joinPair :: Pair (Pair a) ⟶ Pair a
joinPair (Pair (Pair a _) (Pair _ bʹ)) = Pair a bʹ

instance Monad (Pair a)  where
  return a = Pair a a
  m >>= k  = joinPair (fmap k m)
~~~

# Confidence building

You could be forgiven for thinking the definition of `join` is a little fishy. It's clearly throwing
away a lot of information: the second element of the first pair and the first element of the second.
However, if we look at the `join` instance for functions we find that it's equivalent to Cantor's
diagonalisation.

~~~{.haskell}
join :: (r ⟶ r ⟶ a) ⟶ (r ⟶ a)
join f ≣ λr → f r r
~~~

In general, this function throws away almost all the information of function `f :: r ⟶ r ⟶ a`.
Let's draw the values of `f` as a two dimensional array, where `r<n>` is the `n`th value of the type
`r` and `a<n><m>` is the value obtained by applying `f` to `r<n>` and `r<m>`.

~~~
f :: r ⟶ r ⟶ a

 f    r0  r1  r2 ⋯

r0   a00 a01 a02
r1   a10 a11 a12
r2   a20 a21 a22
  ⋮                ⋱
~~~

Function `join` will select those only on the diagonal throwing away all the others. The fact that
`join` for functions has this behaviour gives us confidence that the definition on uniform pairs is
correct.

# Conclusion

That's all folks. In this post we've seen how denotational semantics coupled with type class
morphisms can be used to derive type class instaince implementations which preserve the behaviour of
their meaning (i.e. denotations).

We introduced type class morphisms which preserve the structure of type class instance methods. We
also showed how these morphisms preserve the laws of the type classes they act on ensuring that if
the laws hold on the meaning's instances the laws will also hold on any derived implementation.

We finished with a sample derivation on uniform pairs.

To summarise:

* denotational semantics is a great framework in which to do program derivation because it allows
  you to use equational reasoning.
* morphisms preserve structure of instance methods
* laws are preserved by type class morphisms
* instances can be derived from the meaning's instances and morphisms