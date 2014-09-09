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

-------------------------------------------------------------------------------------------------
Conal: I suspect there's some fuzziness hiding behind these last two sentences.
Enough for what? Suitable for what? And do we really want assurance? (I'd
rather have truth.)

I gave some motivation for semantic TCMs in the section called "Why care about type class
morphisms?" of my first TCM post.

Sean-14-Sep-2012: I think I will come back to this introduction once this round of feedback comes
back. There are things further on in the post that I still need to clear up first.
-------------------------------------------------------------------------------------------------


# Meanings

The genesis of the type class morphism principle came up in Conal's work on functional reactive
programing. He wanted users of the library to think of the `Behaviour` type as being a function of
type `Time -> a`. Put another way, the "meaning" of `Behaviour` was functions of time. Conal noticed
nice relationships between methods on `Behaviour`s and the same methods on the meanings of those
behaviours in the `Functor` class. Spurred on by this discovery he looked for similar
relationships for other type classes.
---------------------------------------------------------------------------------------------------
Conal[in response to earlier pargraph]: It really didn't go down that way. I was doing code/math
poetry on the denotational semantics, which I regularly do on my code. While playing with
variations, I noticed pretty relationships between methods on behaviors and the same methods on the
meanings. Once I noticed this pattern for one class (Functor), I looked for it in other class
instances and in other libraries.
---------------------------------------------------------------------------------------------------
Thus a general general principle was born: the type class instances for the meaning of a data type
could be used to inform the type classes instances one wrote for the data type itself. Succinctly,
the principle is: *The instance's meanings should follow meaning's instances*. The original post can
be found [here]().
---------------------------------------------------------------------------------------------------
Conal:Broken link. To which post did you want to link? The first was probably
http://conal.net/blog/posts /simplifying-semantics-with-type-class-morphisms . There are link-backs
from later posts. And my TCM paper has a lot more info and would be useful to readers.
----------------------------------------------------------------------------------------------------


## Uniform pairs denote functions on `Bool`s

So, what is a suitable meaning for the uniform pair? Well, it turns out that there is an interesting
isomorphism between data structures and functions. The full details are beyond the scope of this
post. However, it's not too much of a stretch of the imagination to think of a uniform pair of
things as a function from booleans to things. i.e.

~~~{.haskell}
⟦⋅⟧ :: Pair a ⟶ (Bool ⟶ a)
⟦ Pair a b ⟧ ≣ λx → if x then b else a
⟦ ⊥ ⟧        ≣ ⊥
~~~

---------------------------------------------------------------------------------------------------
Conal: You mention "isomorphism" but give only an embedding. I think embedding suffices for this
post, but isomorphism can help a lot as well, leading to mechanical ("aha"-free) instance
derivations.

Sean-05-Sep-2012:  I'm having trouble seeing how it's not an isomorphism.
For a given `Pair a b` we get a unique function in the range (of type `Bool ⟶ a`). That covers
going one direction of the isomorphism.
Now, for a given function of type `Bool ⟶ a` we can depict its results as a truth table, assuming
it is not ⊥. (If it is ⊥ it corresponds to the uniform pair ⊥.)

  F | T
 ---+----
  x | y

Doesn't this correspond to a unique uniform pair, namely `Pair x y`? How is this not an
isomorphism? In my original draft I had left out the `⟦ ⊥ ⟧ ≣ ⊥` case. Is that why it wasn't
an isomorphism? Is it more subtle than that? `Pair ⊥ ⊥` would correspond to
`λx → if x then ⊥ else ⊥` which I guess is just `const ⊥`. For functions of type `Bool ⟶ a` is
this the same as `⊥`? That is, does this mean `Pair ⊥ ⊥` is `⊥`.  Is this why it's not an
isomorphism?
---------------------------------------------------------------------------------------------------

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
---------------------------------------------------------------------------------------------------
Conal: Eep. Everything is a meaning. Everything is a data type. I wonder what you are trying to say
here.

Sean-05-Sep-2012: I appreciate that, from a certain perspective, everything is a meaning and
everything is a data type. I guess what I'm trying to get at here is that denotational
semantics is normally defined on pure syntax. e.g. for a simple arithmetic language

⟦1 + 2⟧ ≣ ⟦1⟧ + ⟦2⟧ ≣ 1 + 2 ≣ 3


The 1,2 and + inside and outside of the Oxford brackets are quite distinct though! I should
really use a different font. Further in this post, I mention another meaning function being "applied
behind the scenes". I really feel this is only happening on the 1,2, and + that are *not* in
Oxford brackets. The symbols within the brackets are to be regarded as "dumb syntax", while
the symbols outside the brackets should be seen as fully fledged mathematical objects.

I want to be able to make a fine distinction similar to the one I made in the paragraph above
between "dumb syntax" and mathematical objects. I want people to see the data types *inside*
the Oxford brackets, not as pure syntax, but as the mathematical objected denoted by the data type.
e.g. `Pair (1+2) (3+4)` and `Pair 3 7` are syntactically distinct, but semantically the same.

I'm up for any suggestions on how I can achieve this. Perhaps I can rework parts of what I wrote
in this comment to you to achieve this purpose?
---------------------------------------------------------------------------------------------------

### Haskell as our model of meanings

We use Haskell as our model of meanings, both as the domain and range of the meaning function.
---------------------------------------------------------------------------------------------------
Conal: I don't know what "use Haskell as our model of meanings" could mean, considering that Haskell
is a language.

Sean-14-Sep-2012: Fair point! But my desire to write this sentence is related to what I wrote
as my response to your previous comment. We need some syntactic representation for meanings,
and I'm using Haskell's syntax. How do I phrase this, and remain precise? I think an answer
to my response to your previous comment will also help me here.
---------------------------------------------------------------------------------------------------
This can be a bit confusing, especially if one is familiar with the traditional formulation of
denotational semantics.

Let's say our meaning function has type T ⟶ Tʹ. We will write equations that define the meaning
function as follows:

~~~
⟦t⟧ ≣ tʹ
~~~

Obviously *t* and *tʹ* are syntactic. All meanings need to be represented syntactically.
---------------------------------------------------------------------------------------------------
Conal: Huh??
Sean-14-Sep-2012: Ah, this isn't always true, is it? The user interfaces for Shady effects
*are* direct experiences. They don't denote anything. They *are* the thing. Okay, I concede this.
But here I need to use syntax. Human beings (except maybe Neo) can't see The Matrix. :-)

So how do I clarify all this? It's hard!
---------------------------------------------------------------------------------------------------
But please don't treat them as syntax, but instead treat them as the mathematical entities. There is
another, implicit, meaning function from Haskell syntax to the mathematical objects the syntax
denotes. Every time we see a piece of Haskell syntax one can think of this meaning function being
applied behind the scenes. However, as long as this is understood we can safely elide it from our
presentation.
---------------------------------------------------------------------------------------------------
Conal: Similarly, whenever we say "3 + 4 == 7" (not just in Haskell), there's a "meaning function
being applied behind the scenes" from terms to numbers.
Sean-14-Sep-2012: I agree.
---------------------------------------------------------------------------------------------------

# What is a type class morphism?

A type class morphism expresses a relationship between:

a) the meaning of a type class method applied to values of the data type, and
b) the meaning's type class method applied to the meanings of the data type.

This relationship *preserves the structure* of each method of a type class.

## Be careful about types

In the following definitions be aware that although occurrences of type class methods will appear on
the left and right hand side of the equations they are not the same; they are defined on different
types.

## The `Functor` morphism

The functor morphism is equivalent to a *natural transformation* in category theory.

~~~{.haskell}
⟦ fmap f m ⟧ ≣ fmap f ⟦ m ⟧
~~~

This is often written `⟦⋅⟧ ∘ fmap f ≣ fmap f ∘ ⟦⋅⟧` and drawn as a commuting diagram.

---------------------------------------------------------------------------------------------------
Conal: often written "⟦⋅⟧ ∘ fmap f ≣ fmap f ∘ ⟦⋅⟧" and often drawn as a commuting diagram.
Sean-14-Sep-2012: Added that.
---------------------------------------------------------------------------------------------------

## The `Applicative` morphism

~~~{.haskell}
⟦ pure a ⟧    = pure a
⟦ mf ⊛ mx ⟧ = ⟦mf⟧ ⊛ ⟦mx⟧
~~~

## The `Monad` morphism

~~~{.haskell}
⟦ return a ⟧ ≣ return a
⟦ join mm  ⟧ ≣ join (fmap ⟦⋅⟧ ⟦mm⟧)
~~~

---------------------------------------------------------------------------------------------------
Conal: I'd rewrite this join morphism consistently using composition or consistently not using it, i.e.,
either

    ⟦⋅⟧  ∘ join ≣ join ∘ fmap ⟦⋅⟧ ∘ ⟦⋅⟧

or

    ⟦ join mm  ⟧ ≣ join (fmap ⟦⋅⟧ ⟦mm⟧)

Sean-14-Sep-2012: I went for the latter
---------------------------------------------------------------------------------------------------


The monad morphism can also be defined in terms of *return* and *bind*, although not
nearly as elegantly. We favour the one defined in terms of *return* and *join*.


~~~{.haskell}
⟦ return a ⟧ ≣ return a
⟦ u >>= k  ⟧ ≣ ⟦u⟧ >>= ⟦⋅⟧ ∘ k
~~~

## Laws are preserved by morphisms

A wonderful thing about these morphisms is that they ensure that if the laws (e.g. Monad laws) hold
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

---------------------------------------------------------------------------------------------------
Conal: Hm. What are you saying here? What's a meaning function? Couldn't *every* function be said to
be a meaning function? There's a critical condition that these proofs depend on.

Sean-14-Sep-2012: I'm not sure what that is.
---------------------------------------------------------------------------------------------------


#### Identity law

Need to show that if *pure id ∘ u = u* then *⟦pure id ∘ u⟧ = ⟦u⟧*
---------------------------------------------------------------------------------------------------
Conal: Something is seriously askew here. This conditional property follows simply from the
function-ness of ⟦⋅⟧, doesn't it? Are you using the assumption that pure id ∘ u = u in your
proof below? What do you really want to prove here?

Sean-14-Sep-2012: Has this got something to do with equality being semantic (i.e if ⟦a⟧ ≣ ⟦b⟧ then
a ≣ b`)? If so, then yes, it follows from the function-ness.

I'm not using all of these properties in my proof below. Perhaps that makes much of this extraenous.
I just wanted to show my readers that if ⟦⋅⟧ is an Applicative morphism (and in this case ⟦⋅⟧ is from
an arbitrary domain to another, not from uniform pairs to boolean functions) then it preserves
the Applicative laws -- just as you do in your paper. It's just an aside.
---------------------------------------------------------------------------------------------------

~~~{.haskell}
   ⟦pure id ⊛ u⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
---------------------------------------------------------------------------------------------------
Conal: How do you know?
Sean-14-Sep-2012: I'm assuming it is an Applicative morphism, and showing that the Applicative
laws are preserved.
---------------------------------------------------------------------------------------------------
   ⟦pure id⟧ ⊛ ⟦u⟧
≣ {- ⟦⋅⟧ is an Applicative morphism -}
   pure id ⊛ ⟦u⟧
≣ {- Identity on meaning -}a
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

With the proof that Applicative morphisms preserve Applicative laws out of the way we are now almost
ready to derive instance methods for uniform pairs. First we define a function that will simplify
some of the equational reasoning to come.

~~~{.haskell}
cond :: a ⟶ a ⟶ Bool ⟶ a
cond e t c = if c then t else e
~~~

Next let's define the meaning `Pair` in terms of `cond`.

~~~{.haskell}
⟦Pair a b⟧ ≣ cond a b
~~~

Here are some properties of `cond` which will we use to simplify the presentation.
They can be easily verified by the reader.
---------------------------------------------------------------------------------------------------
Conal: Are they obvious? If so, is the obviousness property useful? Are the cond properties useful?

Sean-14-Sep-2012: I removed the word obvious and added some explanation on the "Cases" properties.
---------------------------------------------------------------------------------------------------

## Cases

~~~{.haskell}
cond a b False ≣ a
cond a b True  ≣ b
~~~

The "cases" property provides a nice proof technique. For an expression `cond e1 e2 b` where `e1`
and `e2` are arbitrary expressions, if we find that `cond e1 e2 False` simplifies to `e1'` and
`cond e1 e2 True` simplifies to `e2'` then we can say:

~~~{.haskell}
cond e1 e2 ≣ cond e1' e2'
~~~

## Composition

~~~{.haskell}
    f ∘ cond e t
= {- Definition of cond -}
    f ∘ (λc → if c then t else e)
= {- Definition of (∘) -}
    λx → f ((λc → if c then t else e) c)
= {- Simplify -}
    λx → f (if x then t else e)
= {- Distribute f over if-expression-}
    λx → if x then f t else f e
= {- Definition of cond -}
    cond (f e) (f t)
~~~

## Constant

~~~{.haskell}
    cond a a
= {- Definition of cond -}
    λc → if c then a else a
= {- Property of if-expression -}
    λc → a
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

---------------------------------------------------------------------------------------------------
Conal: Really? What do you mean by "implementation" here? You could throw in variations like 'id'
applications, but you'd still get the same result Pair. Do the SECs inevitably lead to these
instances? In other words, is it the case that for every Functor instance of Pair a b such that ⟦⋅⟧
is a Functor morphism, is it the case that fmap f (Pair a b) ≣ Pair (f a) (f b)?

Sean-14-Sep-2012: What is an SEC? Also, I'm having trouble working out what you mean by
"throwing in variations like 'id'". It can't be `⟦⋅⟧ ≣ id`
since that wouldn't have the right type. I can't see how `fmap` could just be `id` either.
I'm confused.
---------------------------------------------------------------------------------------------------

## `Functor` instance

~~~{.haskell}
   ⟦ fmap f (Pair a b) ⟧
≣ {- functor morphism -}

---------------------------------------------------------------------------------------------------
Conal: What, what? How do you know that ⟦⋅⟧ is a Functor morphism? You don't even know what fmap is
yet.

Sean-14-Sep-2012: This is where I really start to get confused. I realise that in your TCM paper
you started off showing that various things were type class morphisms. e.g. you already had your
definition for `fmap` and would then show that  ⟦⋅⟧ was a Functor morphism by showing
`⟦ fmap f m ⟧ ≣ fmap f ⟦ m ⟧`. But that's not what I'm trying to do. I'm *assuming* that
⟦⋅⟧ is a Functor morphism and trying to find a definition for `fmap`. Isn't this what you start
doing in the latter part of your TCM paper?

Thus, I believe I can answer "yes" to your question above ("is it the case that for every Functor
instance of Pair a b such that ⟦⋅⟧ is a Functor morphism, is it the case that fmap f (Pair a b) ≣
Pair (f a) (f b)").

If we take `⟦ Pair a b ⟧ ≣ cond a b` and *assume* ⟦⋅⟧ is a Functor morphism then I believe we can
say that `fmap f (Pair a b) ≣ Pair (f a) (f b)`. Isn't that what the equational reasoning shows us?
Am I missing out the case for bottom? If so, isn't that easy to show? i.e.

   ⟦ fmap f ⊥ ⟧
≣ {- functor morphism -}
   fmap f ⟦ ⊥ ⟧
≣ {- meaning of bottom -}
   fmap f ⊥
  {- evaluate -}
≣  ⊥
  {- meaning of bottom -}
   ⟦ ⊥ ⟧
---------------------------------------------------------------------------------------------------

   fmap f ⟦ Pair a b ⟧
≣ {- meaning of Pair -}
   fmap f (cond a b)
≣ {- fmap is composition for functions -}
   f ∘ cond a b
≣ {- "Composition" law of cond -}
   cond (f a) (f b)
≣ {- Meaning of Pair -}
   ⟦ Pair (f a) (f b) ⟧
~~~

Thus, a sufficient `Functor` instance is:

~~~{.haskell}
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair (f a) (f b)
~~~

---------------------------------------------------------------------------------------------------
Conal: Sufficient for what? What have you proved here? I think there are two goals: derivation and
proof. I.e., finding a fmap definition and then proving something about it. If you're careful,
most of your work goes into both goals.

Sean-14-Sep-2012: I think I'm thinking about this very differently to you. I'm not trying to prove
anything. I'm trying to find an implemenation which respects the fact that ⟦⋅⟧ has a particular
definitions *and* is a Functor morphism. I'm not trying to prove ⟦⋅⟧ is a Functor morphism, I'm
assuming it is one.

Let's say I'm completely confused. How would I do the derivation with out assuming that ⟦⋅⟧ is
Functor morphism? If that is possible then I would see why you need to prove that ⟦⋅⟧ is a Functor
morphism afterwards.
--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------
Conal: A subtle and important point. Your proof above only covers pairs of the form `Pair a b` and
yet I think you're claiming that it handles *all* pairs.

Sean-14-Sep-2012: `Pair a b` is a sufficiently general expression that it covers all values except
 `⊥`, right? (I'm assuming that either `a` or `b`, or both, could be `⊥`. I also realise that
  `Pair ⊥ ⊥` is *not* `⊥`.) So I'm just missing the derivation of `fmap f ⊥`, right?
---------------------------------------------------------------------------------------------------

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
   λx → (⟦ Pair fa fb ⟧ x) (⟦ Pair a b ⟧ x)
≣ {- Meaning of Pair -}
   λx → (cond fa fb x) (cond a b x)
~~~

Now we perform a case analysis.

#### Case `x ≣ True`

~~~{.haskell}
   (cond fa fb True) (cond a b True)
≣ {- cond applied to True -}
   fb (cond a b True)
≣ {- cond applied to True -}
   fb b
~~~

#### Case `x ≣ False`

~~~{.haskell}
   (cond fa fb False) (cond a b False)
≣ {- cond applied to False -}
   fa (cond a b False)
≣ {- cond applied to False -}
   fa a
~~~

Thus, in the end, we get:

~~~{.haskell}
   cond (fa a) (fb b)
≣ {- Meaning of Pair -}
  ⟦ Pair (fa a) (fb b) ⟧
~~~

Therefore a sufficient implementation is:

~~~{.haskell}
instance Applicative (Pair a) where
  pure a                  = Pair a a
  Pair fa fb ⊛ Pair a b = Pair (fa a) (fb b)
~~~

---------------------------------------------------------------------------------------------------
Conal: Hopefully you'll have made "sufficient" clear in the functor derivation.
---------------------------------------------------------------------------------------------------

## `Monad` instance

~~~{.haskell}
  ⟦ join (Pair (Pair a aʹ) (Pair b bʹ)) ⟧
≣ {- Monad morphism -}
  join (fmap ⟦⋅⟧ ⟦ Pair (Pair a aʹ) (Pair b bʹ) ⟧)
≣ {- Meaning of Pair -}
  join (fmap ⟦⋅⟧ (cond (Pair a aʹ) (Pair b bʹ)))
≣ {- fmap on (⟶) r -}
  join (⟦⋅⟧ ∘ (cond (Pair a aʹ) (Pair b bʹ)))
≣ {- Meaning of Pair -}
  join (cond ⟦Pair a aʹ⟧ ⟦Pair b bʹ⟧
≣ {- Meaning of Pair -}
  join (cond (cond a aʹ) (cond b bʹ))
≣ {- join on (⟶) r -}
  λr → (cond (cond a aʹ) (cond b bʹ)) r r
~~~

Now for a case analysis on `r`.

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

Thus, we get:

~~~{.haskell}
  cond a bʹ
≣ {- Meaning of Pair -}
  ⟦ Pair a bʹ⟧
~~~

Therefore a sufficient `Monad` instance is:

~~~{.haskell}
instance Monad (Pair a)  where
  return a                           = Pair a a
  join (Pair (Pair a _) (Pair _ bʹ)) = Pair a bʹ
~~~

Currently the Monad class doesn't have `join` as a method, which is a shame. Until it does,
we have to accomodate via a standard translation.

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

This definition of `join` throws away some information of function `f :: r ⟶ r ⟶ a`.
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

Function `join` will select those only on the diagonal throwing away all the others. In the  case
that `r` is an infinite domain this amounts to throwing away almost all the information of function
`f`.  The fact that `join` for functions has this behaviour gives us confidence that the definition
on uniform pairs is correct.

---------------------------------------------------------------------------------------------------
Conal: By "us", I assume you mean you & the reader(s). I suspect this join fact doesn't really give
most of them confidence. Moreover, confidence isn't really in itself a good thing, is it? And what
you mean by "is correct"? Maybe some more work clarifying the overall goal up front and reminding
your readers.

Sean-14-Sep-2012: I will need to do this, but I think I will have to wait until the next round of
feedback from you. There are still things I'm unclear about.
---------------------------------------------------------------------------------------------------

# Conclusion

That's all folks. In this post we've seen how denotational semantics coupled with type class
morphisms can be used to derive type class instaince implementations that preserve the behaviour of
their meaning (i.e. denotations).

We introduced type class morphisms which preserve the structure of type class instance methods. We
also showed how these morphisms preserve the laws of the type classes they act on, ensuring that if
the laws hold on the meaning's instances the laws will also hold on any derived implementation.

We finished with a sample derivation on uniform pairs.

To summarise:

* Denotational semantics is a great framework in which to do program derivation because it allows
  you to use equational reasoning.
* Morphisms preserve structure of instance methods
* Laws are preserved by type class morphisms
* Instances can be derived from the meaning's instances and morphisms

---------------------------------------------------------------------------------------------------
Conal: If you can clarify the motivations/benefits of semantic TCMs (i.e., semantic functions as
TCMs), then you could reinforce it here at the end.
---------------------------------------------------------------------------------------------------