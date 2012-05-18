---
title: Definition follows meaning
category: haskell
tags: Haskell, type classes, type class morphisms
---

-----------------------------

Hi Conal,

I started writing a blog post about applying type class morphisms. The
beauty of trying to explain to others is, of course, that it forces
you to be more precise about your own thinking.

I wanted to run some things by you because I don't think
we're being as precise as we could be and perhaps this is some of the
confusion that people had with your paper.

1. If `⟦⋅⟧` is a function from one syntactic category to another then
the syntactic category of the domain is "Haskell expressions plus
uniform pairs" and the range is "Haskell expressions". Plus, if we
write `⟦Pair a b⟧ ≣ \x -> if x then a else b` this is obviously not
the full definition.

Let's call the syntactic category of "Haskell
expressions", C, and the syntactic category of "Haskell expressions
plus pairs", P.

Then `⟦⋅⟧` :: P -> C` and it contains many boring structurally
recursive cases in it like:

⟦ if *C* then *T* else *E* ⟧ ≣ if ⟦ *C* ⟧ then ⟦ *T* ⟧ else ⟦ *E* ⟧

Plus, we have to introduce the notion of variables. *C*, *T* and *E*
are all variables that stand for whole syntactic expressions in
syntactic categories P or C.

When we write `⟦Pair a b⟧ ≣ \x -> if x then a else b` we are really
adding just one extra definitional equation to all those other boring
structural recursive ones, right?

2. Now that I think about it even the equational reasoning part of the
derivations below requires some care in the way you think about it.

When I replace an expression such as `f (if x then a else b)` with
`if x then f a else f b`, clearly what I'm saying is the *meaning*
of the first expression is the same as the meaning of the second
expression. But this time I'm not talking about a syntactic
category. They both have the same meaning mathematically.

So, is it fair to say that when we are doing equational reasoning that
there is an implicit meaning function being applied to each Haskell
expression? Except this meaning function has as its range mathematical
functions. (An even more deep question would be "how do we know that
mathematical functions are well defined". I guess you have to stop
somewhere, right?)

I'm sure Scott and Strachey must have come across this confusion. When
I get back I'm going to have to go to the library to get Stoy's book
again because I'm sure something like this was mentioned in the first chapter.

Sean

----------------------------



# Introduction

Just recently I spent some time staying at Conal's house near San Andreas.
While I was there I asked him more about his *type class morphism* principle.
It's a concept that always strikes me as incredibly useful when I understand
it but that I always found to slip away from me because I have not used it.
I resolved to understand it by the time I left. The next day he
posed a real example that he had encountered in his work to me as a puzzle.
This blog post will explain the process I went through to derive the same
answer as him.

# Uniform pairs

A uniform pair is a pair which has the same type for each element of the pair.

~~~{.haskell}
data Pair a = Pair a a
~~~

The puzzle: given this definition what instances would you provide for
`Functor`, `Applicative`, `Monad`, etc? Once you had come up with a definition
how would you know that they were the *right* ones? They might well satisfy
the laws for each type class but is this enough? We'll see that type class
morphisms provide an extra level of assurance that we have found the right definitions.

# Meanings

The genesis of the idea. In Conal's work on functional reactive
programming he found himself telling users of the library, think of
these things as functions even though they're really not. This was the
"meaning" of the `Behaviour` data type. But when it came time to check
whether his type class instances on the `Behaviour` type were in
concordance with the ones on functions he found they were not. He
found he'd been lying to his users and sought a general principle to
ensure that the two types stayed in harmony.

TODO: Check it was `Behaviour`. Clean up paragraph.

## Uniform pairs are functions on `Bool`s

So, what is a suitable meaning for the uniform pair? Well, it turns
out that there is an interesting isomorphism between data structures
and functions. The full details are beyond the scope of this post
(FIXME: research and link to it).

You can think of a uniform pair of things as a function from booleans to things. i.e.

~~~{.haskell}
⟦⋅⟧ :: Pair a ⟶ (Bool ⟶ a)
⟦ Pair a b ⟧ ≣ λx → if x then a else b
~~~

## Meaning functions

Before continuing I want to make an important, but subtle, point about
the meaning function.  It is best to think of it purely as a
mathematical function. It maps values from one syntactic category to
another and nothing more. In particular

* we never define the meaning function in Haskell
* we write it as `⟦⋅⟧` if we want to use it as a prefix
  function. i.e. `⟦⋅⟧ x ≣ ⟦ x ⟧





# What is a type class morphism?

Be aware in the following definitions that although occurrences of
type class methods will appear on the left and right hand side of the
equations they are not the same; they're defined on different types.

## The `Functor` morphism

~~~{.haskell}
⟦ fmap f m ⟧ ≣ fmap f ⟦ m ⟧
~~~

## The `Applicative` morphism

~~~{.haskell}
⟦ pure a ⟧    = pure a
⟦ mf ⊛ mx ⟧ = ⟦mf⟧ ⊛ ⟦mx⟧
~~~

## The `Monad` morphism

The `return`, `join` and `fmap` are on different types depending on whether
they're inside the meaning brackets or not.

~~~{.haskell}
⟦ return a ⟧ ≣ return a
⟦ join mm  ⟧ ≣ (join ∘ fmap ⟦⋅⟧) ⟦mm⟧
~~~

or in terms of bind:

~~~{.haskell}
⟦ u >>= k⟧ ≣ ⟦u⟧ >>= ⟦⋅⟧ ∘ k
~~~

## The laws are satisfied by definition.


# Derivation

## `Functor` instance

~~~{.haskell}

   ⟦ fmap f (Pair a b) ⟧
≣ {- functor morphism -}
   fmap f ⟦ Pair a b ⟧
≣ {- meaning of Pair -}
   fmap f (λx → if x then a else b)
≣ {- Definition of fmap for (→) r -}
   f ∘ (λx → if x then a else b)
≣ {- distribute f over if expression -}
   λx → if x then f a else f b
≣ {- Meaning of Pair }
   ⟦ Pair (f a) (f b) ⟧
~~~

The `Functor` instance is thus:

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
≣ {- Definition of const -}
   λ_ → a
≣ {- Expand to if equivalent if-expression -}
   λx → if x then a else a
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
   λx → (λy → if y then fa else fb) x ((λy → if y then a else b) x)
≣ {- Beta reduction -}
   λx → (if x then fa else fb) (if x then a else b)
≣ {- TODO: Re-express. Law of if-expression -}
   λx → if x then (fa a) else (fb b)
≣ {- Meaning of Pair -}
   ⟦ Pair (fa a) (fb b) ⟧
~~~

The `Applicative` instance is thus:

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
  join (fmap ⟦⋅⟧ (λx → if x then (Pair a aʹ) else (Pair b bʹ)))
≣ {- fmap on (⟶) r -}
  join (⟦⋅⟧ ∘ (λx → if x then (Pair a aʹ) else (Pair b bʹ)))
≣ {- Definition of (∘) -}
  join (λx -> ⟦⋅⟧ ((λx → (if x then (Pair a aʹ) else (Pair b bʹ))) x))
≣ {- Simplify -}
  join (λx -> ⟦ if x then (Pair a aʹ) else (Pair b bʹ) ⟧)
≣ {- f (if x then a else b) ≣ if x then f a else f b -}
  join (λx -> if x then ⟦Pair a aʹ⟧ else ⟦Pair b bʹ⟧)
≣ {- Meaning of Pair -}
  join (λx → if x then (λy → if y then a else aʹ)
                  else (λy → if y then b else bʹ))
≣ {- join on (⟶) r -} TODO: Derive join implementation earlier.
  λr → (λx → if x then (λy → if y then a else aʹ)
                  else (λy → if y then b else bʹ)) r r
≣ {- Beta reduction -}
  λr → (if r then (λy → if y then a else aʹ)
             else (λy → if y then b else bʹ)) r
≣ {- Distribution of application over if-expression FIXME -}
  λr → if r then (if r then a else aʹ) else (if r then b else bʹ)
≣ {- Specialisation of r in "then" and "else" branches -}
  λr → if r then (if True then a else aʹ) else (if False then b else bʹ)
≣ {- Simplification -}
  λr → if r then a else bʹ
≣ {- Meaning of Pair -}
  ⟦ Pair a bʹ⟧
~~~

The `Monad` instance is thus:

~~~{.haskell}
joinPair :: Pair (Pair a) ⟶ Pair a
joinPair (Pair (Pair a _) (Pair _ bʹ)) = Pair a bʹ

instance Monad (Pair a)  where
  return a = Pair a a
  m >>= k  = joinPair (fmap k m)
~~~

TODO: Talk about Cantor's diagonalisation.

Seems fishy but it isn't! Yes, you are throwing away information but so
does the instance for functions.

# Unclear aspects

The type class morphisms. How do we know they're correct?

------------------------

Some unicode symbols you need:
Composition: ∘
Dot operator: ⋅
Brackets: ⟦⟧
Lambda: λ
Arrow: →
Prime: ʹ
Equiv: ≣
Long right arrow: ⟶
Meaning function: ⟦⋅⟧