## All you ever wanted to know about `clarsimp`

`clarsimp` interleaves `clarify` and `simp`. `clarify` applies all obvious reasoning steps without
splitting the goal into parts and without applying *unsafe* rules.

Modifiers to clarsimp:

* `simp`. Add lemmas to the collection of simplifications. Lemmas should be of form 
   `⟦ ?A1; ...; ?An ⟧ ⟹ ?e1 = ?e2`. `?A1` through `?An` must unify with terms in the assumptions.
   The order is important. `?e1` is replaced with `?e2` not vice versa. Also, terms are replaced in
   both the assumptions and the goal. There are options (e.g. `no_asm`) that change this behaviour.
* `dest`. Apply the lemmas as `drule`s. Takes rules of the form `?C ⟹ ?C'`. `?C` is matched with an
assumption and replaced with `?C'`. This only occurs in the assumptions, not the goal.
* `dest!`. Same as `dest` but marks the lemmas as being *safe*. If `dest` doesn't work try `dest!`

## When should I use `locate`, `thm`, `term`, `find_theorems`, etc?

## locate

* locate should subsume all the others
* `locate` finds types and definitions (for things with definitions)
* `locate` finds abbreviations, constants
* Can't be used on theorems

- you should use term to find the type of something

## Hints on searching for theorems

* Use module names in the "name". e.g. I was looking for a basic equality introduction rule. This is
  likely to be in one of the basic packages, such as HOL. Just do find_theorems name: HOL "_ = _"

## How to re-enable Unicode shortcuts

Got to menu `Tokens -> Enable Shortcuts`. Turn it off and then on again. 

## `find_theorem` has many options

If you use a predicate that takes arguments you must add underscores for each of those arguments e.g.

```
find_theorems "st_tcb_at' _ _ _⟶ tcb_at' _ _"
```

## find_theorem solves

Sees if there is a lemma that directly solves a goal.


## What if Isabelle gets confused?

Sometimes Isabelle in Emacs mode gets confused. The goals it is showing
just aren't in sync with where the blue highlight of the buffer is. What
do you do? You can tell this has happened when you replay some steps that used
to work and they give a different result.

Go to *isabelle* buffer and type 

oops;

to get out of proof mode

end;

to get out of theory mode



## Pain point: `find_theorems` requires exact match

Was trying to match `ct_in_state` for a theorem and it didn't work. But `ct_in_state'` did. Exact match required.


## Pain point: Loading Theorems is slow!

Solution: Checkpoints at various stages along the way, in dependency order.

## Pain point: The trace just really isn't that useful.


## The schematic variable

The schematic variable or unknown can be instantiated by any other term during a proof.

Schematic variables often have a relationship with bound variables.

* "All" elimination is a an unsafe step because it can be done too early.

## Unsafe vs. Safe rules

Here's how you can tell if a rule is safe or unsafe.

* An elimination rule is unsafe if the LHS does not contain all the schematic
variables on the RHS. e.g.  `notE:: ⟦¬ ?P; ?P⟧ ⟹ ?R`

* An introduction rule is unsafe if the the LHS does not contain all the schematic variables on the RHS.
  e.g. `disjI1: ?P ⟹ ?P ∨ ?Q` or `disjI2: ?Q ⟹ ?P ∨ ?Q`

### Safe Rules

allI, exE

### Unsafe rules

allE, exI


For natural deduction:

* Safe rules: `conjI`, `impI`, `notI`, `iffI`, `refl`, `ccontr`, `classical`, `conjE`, `disjE`
* Unsafe rules: `disjI1`, `disjI2`, `impE`, `iffD1`, `iffD2`, `notE`

The fact that `impE` is unsafe can often be problematic. I find it is useful to use `cast_tac` in these situations. Here is a situation where
`erule notE` doesn't work.

```
goal (1 subgoal):
1. ⟦ ¬ b ⟶ ¬ a ; a ⟧ ⟹ b

apply (erule impE)

goal (2 subgoals):
 1. a ⟹ ¬ b
 2. ⟦a; ¬ a⟧ ⟹ b
```

But, if we use `case_tac` we get:

```
goal (1 subgoal):
 1. ⟦¬ b ⟶ ¬ a; a⟧ ⟹ b

apply (case_tac b)

goal (2 subgoals):
 1. ⟦¬ b ⟶ ¬ a; a; b⟧ ⟹ b
 2. ⟦¬ b ⟶ ¬ a; a; ¬ b⟧ ⟹ b

apply (assumption)
apply (erule impE)

goal (2 subgoals):
 1. ⟦a; ¬ b⟧ ⟹ ¬ b
 2. ⟦a; ¬ b; ¬ a⟧ ⟹ b

apply (assumption)
apply (erule_tac P="a" in notE)

goal (1 subgoal):
 1. ⟦a; ¬ b⟧ ⟹ a

apply (assumption)
done
```


------------------------

obj_at'
-------

```
obj_at' ?P ?p ?s ≡
∃ko obj. ksPSpace ?s ?p = Some ko ∧ 
         is_aligned ?p (objBitsKO ko) ∧ fst (projectKO ko ?s) = {(obj, ?s)} ∧ 
         ?P obj ∧ ps_clear ?p (objBitsKO ko) ?s
```

Apart from some fairly boring well-formedness conditions this function just states that predicate ?P
holds on an object in memory.


valid_queues
------------
```
valid_queues ≡
  λs. ∀p. (∀t∈set (ksReadyQueues s p). obj_at' (inQ p and runnable' ∘ tcbState) t s)
  ∧ distinct (ksReadyQueues s p)
```

States that for all states, s, all priorities, p, and all threads, t, in ksReadyQueues s p, the
object t is in queue of priority p and runnable and also that every thread in ksReadyQueues sp is
distinct.

valid_queues'
-------------
```
valid_queues' ≡ λs. ∀p t. obj_at' (inQ p) t s ⟶ t ∈ set (ksReadyQueues s p)
```

runnable' (simplified definition)
---------
```
runnable' Running                     = True
runnable' Restart                     = True
runnable' _                           = False
```

activatable' (simplified definition)
------------------------------------
```
activatable' Running                     = True
activatable' Restart                     = True
activatable' IdleThreadState             = True
activatable' _                           = False
```

Some facts about ksReadyQueues
------------------------------

* For kernel state `ks`, The only way to change `ksReadyQueues ks` is with `setQueue` 
* The only function that removes something from the `ksReadyQueues ks` is `tchSchedDequeue` 
* The functions that add to `ksReadyQueues ks` are `tcbSchedEnqueue`, `tcbSchedAppend`

Proof of asyncIPCCancel_queues
------------------------------

Once we've unwrapped the definition of asyncIPCCancel we get:

~~~
⦃valid_queues⦄
    let isWaiting = async_endpoint_case False (λword1 word2. False) (λx. True)
    in do aep ← getAsyncEP ae;
          haskell_assert (isWaiting aep) [];
          queue' ← return (delete t $ aepQueue aep);
          aep' ← case queue' of [] ⇒ return async_endpoint.IdleAEP |
                      a # list     ⇒ return $ aepQueue_update (λ_. queue') aep;
          setAsyncEP ae aep';
          setThreadState Structures_H.thread_state.Inactive t
       od 
⦃λ_. valid_queues⦄
~~~

The endpoint queues shouldn't have anything to do with 'valid_queues'. The only thing we should have
to worry about is 'setThreadState'.

Useful theorems about valid_queues:

```
threadSet_valid_queues:
  ⦃valid_queues and (λs. ∀p. (∃tcb. inQ p tcb ∧ ¬ inQ p (?f tcb)) ⟶
                     obj_at' (λtcb. inQ p tcb ∧ ¬ inQ p (?f tcb)) ?t s
                     ⟶ ?t ∉ set (ksReadyQueues s p))⦄
  threadSet ?f ?t ⦃λrv. valid_queues⦄



sts_valid_queues: 
  ⦃λs. valid_queues s ∧ ((∃p. ?t ∈ set (ksReadyQueues s p)) ⟶ runnable' ?st)⦄
    setThreadState ?st ?t
  ⦃λrv. valid_queues⦄

set_aep_valid_queues: ⦃valid_queues⦄ setAsyncEP ?p ?aep ⦃λrv. valid_queues⦄

get_aep_inv': ⦃?P⦄ getAsyncEP ?aep ⦃λrv. ?P⦄
```

A whole bunch of useful hoare triple rules
------------------------------------------

```
hoare_seq_ext: ⟦⋀x. ⦃?B x⦄ ?g x ⦃?C⦄; ⦃?A⦄ ?f ⦃?B⦄⟧ ⟹ ⦃?A⦄ ?f >>= ?g ⦃?C⦄
hoare_drop_imps: 
  ⦃?P⦄ ?f ⦃?Q⦄ ⟹ ⦃?P⦄ ?f ⦃λr s. ?R r s ⟶ ?Q r s⦄
  ⦃?P⦄ ?f ⦃?Q⦄, - ⟹ ⦃?P⦄ ?f ⦃λr s. ?R r s ⟶ ?Q r s⦄, -
  ⦃?P⦄ ?f -, ⦃?Q⦄ ⟹ ⦃?P⦄ ?f -, ⦃λr s. ?R r s ⟶ ?Q r s⦄
```

hoare_strengthen_post: \<lbrakk>\<lbrace>?P\<rbrace> ?a \<lbrace>?Q\<rbrace>; \<And>r s. ?Q r s \<Longrightarrow> ?R r s\<rbrakk> \<Longrightarrow> \<lbrace>?P\<rbrace> ?a \<lbrace>?R\<rbrace>

pred_conj_def: ?P and ?Q ≡ λx. ?P x ∧ ?Q x


```
hoare_conj:               ⟦⦃?P⦄ ?f ⦃?Q⦄; ⦃?P'⦄ ?f ⦃?Q'⦄⟧ ⟹ ⦃?P and ?P'⦄ ?f ⦃?Q And ?Q'⦄
hoare_conjD1:              ⦃?P⦄ ?f ⦃λrv. ?Q rv and ?R rv⦄ ⟹ ⦃?P⦄ ?f ⦃?Q⦄
hoare_conjD2:              ⦃?P⦄ ?f ⦃λrv. ?Q rv and ?R rv⦄ ⟹ ⦃?P⦄ ?f ⦃?R⦄
hoare_conjI:                  ⟦⦃?P⦄ ?f ⦃?Q⦄; ⦃?P⦄ ?f ⦃?R⦄⟧
                          ⟹ ⦃?P⦄ ?f ⦃λr s. ?Q r s ∧ ?R r s⦄
hoare_elim_pred_conj:         ⦃?P⦄ ?f ⦃λr s. ?Q r s ∧ ?Q' r s⦄ 
                          ⟹ ⦃?P⦄ ?f ⦃λr. ?Q r and ?Q' r⦄
hoare_post_comb_imp_conj: ⟦⦃?P'⦄ ?f ⦃?Q⦄; ⦃?P⦄ ?f ⦃?Q'⦄; ⋀s. ?P s ⟹ ?P' s⟧ ⟹ 
                          ⦃?P⦄ ?f ⦃λrv s. ?Q rv s ∧ ?Q' rv s⦄
hoare_post_conj:          ⟦⦃?P⦄ ?a ⦃?Q⦄; ⦃?P⦄ ?a ⦃?R⦄⟧ ⟹ ⦃?P⦄ ?a ⦃?Q And ?R⦄
hoare_vcg_conj_lift:          ⟦⦃?P⦄ ?f ⦃?Q⦄; ⦃?P'⦄ ?f ⦃?Q'⦄⟧ 
                          ⟹ ⦃λs. ?P s ∧ ?P' s⦄ ?f ⦃λrv s. ?Q rv s ∧ ?Q' rv s⦄
hoare_post_imp:           ⟦⋀r s. ?Q r s ⟹ ?R r s; ⦃?P⦄ ?a ⦃?Q⦄⟧ ⟹ ⦃?P⦄ ?a ⦃?R⦄
```

switchToThread_def
------------------

```
> switchToThread :: PPtr TCB -> Kernel ()
> switchToThread thread = do
>         Arch.switchToThread thread
>         tcbSchedDequeue thread
>         setCurThread thread

> tcbSchedDequeue :: PPtr TCB -> Kernel ()
> tcbSchedDequeue thread = do
>     queued <- threadGet tcbQueued thread
>     when queued $ do
>         prio <- threadGet tcbPriority thread
>         queue <- getQueue prio
>         setQueue prio $ filter (/=thread) queue
>         threadSet (\t -> t { tcbQueued = False }) thread
```

```
tcbSchedDequeue_ksQ:
  ⦃λs. ?P (set (ksReadyQueues s ?p) - {?t}) ∧ 
   obj_at' (tcbQueued and op = ?p ∘ tcbPriority) ?t s⦄ 
      tcbSchedDequeue ?t 
  ⦃λ_ s. ?P (set (ksReadyQueues s ?p))⦄

Arch_switchToThread_invs: ⦃invs'⦄ ArchThreadDecls_H.switchToThread ?t ⦃λrv. invs'⦄
Arch_switchToThread_st_tcb': ⦃st_tcb_at' ?P ?t⦄
                                 ArchThreadDecls_H.switchToThread ?t 
                             ⦃λrv. st_tcb_at' ?P ?t⦄
Arch_switchToThread_tcb': ⦃tcb_at' ?t⦄
                              ArchThreadDecls_H.switchToThread ?t
                          ⦃λrv. tcb_at' ?t⦄
setCurThread_invs:
  ⦃invs' and st_tcb_at' activatable' ?t and obj_at' (λx. ¬ tcbQueued x) ?t⦄
      setCurThread ?t
  ⦃λrv. invs'⦄
```


When wp is playing up apply the wp_once methodology
---------------------------------------------------
- Apply wp_once until it no longer successed.
- Then apply wp_once with a particular rule. e.g. apply (wp_once

More on clarsimp
----------------

apply (clarsimp split: )

Emacs lore - customizing tokens
-------------------------------
Tokens -> Customize -> Shortcuts

You can use this to say add "\\" as a shortcut for "\<lambda>"



Usefulness of obj_at'_weakenE
------------------------------

FIXME: Provide an example.

obj_at'_weakenE: ⟦obj_at' ?P ?p ?s; ⋀k. ?P k ⟹ ?P' k⟧ ⟹ obj_at' ?P' ?p ?s


Difference between clarsimp and fastimp
---------------------------------------

- clarsimp goes as far as it can (it may do nothing)
- fastsimp tries harder and breaks if it can't discharge the goal.



Contrapositives
----------------
```
contrapos_nn: ⟦¬ ?Q; ?P ⟹ ?Q⟧ ⟹ ¬ ?P
contrapos_np: ⟦¬ ?Q; ¬ ?P ⟹ ?Q⟧ ⟹ ?P
contrapos_pn: ⟦?Q; ?P ⟹ ¬ ?Q⟧ ⟹ ¬ ?P
contrapos_pp: ⟦?Q; ¬ ?P ⟹ ¬ ?Q⟧ ⟹ ?P
rev_contrapos: ⟦?P ⟹ ?Q; ¬ ?Q⟧ ⟹ ¬ ?P

contrapos_imp: ?P ⟶ ?Q ⟹ ¬ ?Q ⟶ ¬ ?P

bool_contrapos: ⟦?P ?x; ¬ ?P False⟧ ⟹ ?P True
```

Schematic variables
-------------------

Have a look at this proof:
Explains a lot about the problem with getting rid of

```
lemma a: "tcb_at' t s ⟹ ∃p. obj_at' (λtcb. tcbPriority tcb = p) t s"
  apply (rule exI)
  apply (clarsimp simp: obj_at'_def)
```



Normal forms
----------------

* TODO: Look up HHNF
* TODO: Look up Wenzel's ISAR thesis. (2nd chapter)

There is a normal form where there are no nested meta-implications in the conclusion
but they are allowed in the premises.


erule(<n>)
----------

This means unify an additional <n> premises. This can be really useful when you know you're just
going to "rule assumption" something away in the next step.

Elimination rule format
----------------------

```
⟦ ?P ; ... ⟹ ?C ⟧ ⟹ ?C
```

* Fails if it can't unify `?P` and `?C`.
* Notice `?C` appears as conclusion and as conclusion of a nested meta-implication.

Quantification and unification
------------------------------

`?t = ?t` does not unifiy with `⋀x. x = ?y` since the `x` is quantified.
This is why the following proof (which is false and shouldn't be able to be proved)
doesn't go through.

```
lemma "∃y. ∀x. x = y"

  goal (1 subgoal):
   1. ∃y. ∀x. x = y

apply(rule exI)

  goal (1 subgoal):
   1. ∀x. x = ?y

apply (rule allI)

  goal (1 subgoal):
   1. ⋀x. x = ?y
```

Theorem `refl` just doesn't unify with this. Which is a good thing.

*Parameters* then *unknowns*
----------------------------

Create paramaters first, unknowns later.

TODO: Work out what this means. You've seen this before in a real proof. Tim mentioned that things must be done in the right order.
Find an example!


Forward proofs with `frule` and `drule`
---------------------------------------


Here's how `frule` works.

Rule:       `⟦ A_1; ...; A_m ⟧ ⟹ A`

Subgoal: `1. ⟦ B_1; ...; B_n ⟧ ⟹ C`

Substitution: σ(B_i) ≡ σ(A_1) (i.e. One of the subgoal premisies, `B_i`, unifies with `A_1`)

New subgoals. 

```
1.   σ(⟦ B_1; ...; B_n    ⟧ ⟹ A_2)
              ...
m-1. σ(⟦ B_1; ...; B_n    ⟧ ⟹ A_m)
m.   σ(⟦ B_1; ...; B_n; A ⟧ ⟹ C)
```

`drule` also deletes `B_i`


Now to see it in action.

```
lemma "A ∧ B ⟹ ¬ ¬ A"

    goal (1 subgoal)
     1. A ∧ B ⟹ ¬ ¬ A

thm conjunct1

    ?P ∧ ?Q ⟹ ?P

apply (drule conjuctI)

    goal (1 subgoal):
     1. A ⟹ ¬ ¬ A
```

These are called *forward proofs* because the conclusion is not changing. We are discharging assumptions.

More proof methods
------------------

TODO: Make a cheat sheet of all of these (including locate and find_theorems)

* `apply (intro <intro-rules>)`. Repeatedly applies intro rules
* `apply (elim <elim-rules)`. Repeatedly applies elim rules
* `apply clarify`. Applies all safe rules that do not split the goal.
* `apply safe`. Applies all safe rules.
* `apply blast`. Tableaux prover that works well on predicate logic.
* `apply fast`. Another automatic search technique

Single line comments
--------------------

This can be done with

```
-- "comment in quotes"
```

Hoare triples are backwards!
---------------------------

Whenever you're looking at a hoare triple make sure the next thing you focus on in the last statement in the program between
the pre and post conditions. They go backwards!

Limiting how many goals a proof method touches
----------------------------------------------

Outside the brackets put `[x]` where `x` is strictly positive integer. This is the number of goals that the proof method will limit itself to. e.g.
 
```
apply (wp)[2]
```

This means that `wp` will only attempt to discharge 2 of the subgoals before stopping.

Simulating `wp` at a fine-grained level
---------------------------------------

Use `wp_once`. It does one single step of whatever `wp` would do. Good to work out what `wp` is doing behind the scenes.

Another variation on `wp`: `wpc`
--------------------------------

Proof method `wpc` is the same as `wp` except that it will run a `case_tac` on any statements that appear in the subgoals which are of form `case <something> ...`

When to use `case_tac`
----------------------

One situation to use `case_tac` is when you see something like:

[| (P = a --> Q1 ) /\ (P = b --> Q2) /\ (P = c --> Q3) |]

How to do a `corres` proof
--------------------------

1. Get rid of all `corres` bits
2. Get rid of all hoare triples doing `wp` style proof.
3. Get rid of remaining implication style goals.

More on using "simp"
--------------------

There are a few variations on `simp`. The extra directives determine whether assumptions are used or whether they are simplified. i.e. whether their 

* `simp`               -- use and simplify assumptions
* `simp (no_asm)`      -- ignore assumptions
* `simp (no_asm_use)`  -- *simplify* but to not *use* assumptions
* `simp (no_asm_simp)` -- *use* but do not *simplify* assumptions

Isabelle Proof Cache
--------------------

You want to be using this. Proving some of the theories can take on the order of hours.

FIXME: How do you use it?

Lexicographically sorting terms in a sum, product, conjunction etc, using `simp`
--------------------------------------------------------------------------------

`apply (simp add: add_ac)`

(b + c) + a ⤳ a + (b + c)


ASCII shortcuts for unicode symbols
-----------------------------------

Tokens -> Customize -> Shortcuts

Speeding proofs up
------------------

Make sure you don't have Full Annotations on. Go to:

  Proof General: Quick Options -> Processing -> Full Annotations.

When this option is on you can hover over a line and have
an annotation of the current goals appear.

Rule `hoare_vcg_all_lift`
-----------------------

This rule is incredibly useful when you have a hoare-triple lemma that works on a predicate at an arbitrary "point" but you have a goal which is quantified over all points.

e.g.

I just had a situation where I had:

~~~
⋀state. ⟦state = Structures_H.thread_state.Restart; isCall⟧   ⟹ 
  ⦃?Q17 state⦄ replyFromKernel thread (0, reply) ⦃λrv s. ∀p. thread ∉ set (ksReadyQueues s p)⦄
~~~

And I had this lemma already proved. An arbitrary predicate `?P` on `ksReadyQueues s ?p`
(at an arbitrary priority `?p`) is preserved by function `replyFromKernel`.

~~~
rfk_ksQ: ⦃λs. ?P (ksReadyQueues s ?p)⦄ replyFromKernel ?t ?x1.0 ⦃λ_ s. ?P (ksReadyQueues s ?p)⦄
~~~

The problem is that the post-condition doesn't unify with the post-condition of our goal since it
is universally quantified. Fortunately, a `hoare_vcg_all_lift` allows us to prove a hoare triple
containing universally quantified predicates if we can prove a hoare triple containing those
same predicates applied at a specific "point".

~~~
hoare_vcg_all_lift: (⋀x. ⦃?P x⦄ ?f ⦃?Q x⦄) ⟹ ⦃λs. ∀x. ?P x s⦄ ?f ⦃λrv s. ∀x. ?Q x rv s⦄
~~~

Useful emacs shortcuts
----------------------

It's very useful to have emacs shortcuts so that you can turn "Trace Unification" and "Show types" on and off.

"Show types" is useful for working out whether you're looking at a specialised instance of a polymorphic function. This can
change what is true quite markedly. For instance, "setObject" when applied to the "cte" data clearly can't change a "tcb" data 
stucture. This is actually a theorem but it's not obvious what is going on unless you see the types.


Where to find your images
-------------------------

Images such as `BASE_REFINE`, `CBASE_REFINE`, etc can be found in

`$HOME/.isabelle/heaps-<mangled-path-to-isabelle>/<ml-compiler>`

e.g.

~~~
~/.isabelle/heaps-home-sseefried-l4-l4.verified-isabelle/polyml-5.3.0_x86-linux
~~~


Building CBASE_REFINE
---------------------

Interestingly it does not build on top of the BASE_REFINE. Instead it just builds on top of one 
ancestor. However, many of the theories it's built on such as BASE_REFINE as 

What is `wps`?
--------------

`wps` helps you avoid uses of `hoare_lift_Pf`. This little gem of a theorem says that

if you can show that for an arbitrary predicate that `λs. P (?f s)` is preserved and that for an arbitrary
`x` that `?P x` is preserved then `λs. ?P (?f s)` is preserved.


⟦⋀x. ⦃?P x⦄ ?m ⦃λ_. ?P x⦄; 
 ⋀P. ⦃λs. P (?f s)⦄ ?m ⦃λ_ s. P (?f s)⦄ ⟧ 
⟹ ⦃λs. ?P (?f s) s⦄ ?m ⦃λ_ s. ?P (?f s) s⦄

Breakdown of `invs'`
--------------------

~~~
invs'
  valid_state'
    
  cur_tcb'
~~~


~~~
thm invs'_def
  thm valid_state'_def
    thm valid_pspace'_def
      thm valid_objs'_def
      thm  pspace_aligned'_def
      thm pspace_distinct'_def
      thm no_0_obj'_def
      thm valid_mdb'_def
    thm valid_queues_def
    thm sym_refs_def
    thm if_live_then_nonz_cap'_def
    thm if_unsafe_then_cap'_def
    thm valid_idle'_def
    thm valid_global_refs'_def
    thm valid_arch_state'_def
    thm valid_irq_node'_def
    thm valid_irq_handlers'_def
    thm valid_irq_states'_def
    thm valid_machine_state'_def
    thm irqs_masked'_def
    thm valid_queues'_def
    thm ct_not_inQ_def
    thm valid_pde_mappings'_def
    thm pspace_domain_valid_def
  thm cur_tcb'_def
~~~


Source code abbreviations
-------------------------

CTE - Capability Table Entry.

MDB - Mapping database. An old name for the "capability derivation tree". Described as follows: The
mapping database consists of a tree structure for each physical page that can be mapped at user
level. It is used to keep track of all CTEs pointing to each kernel object, so capabilities can be
recursively revoked.


More on MDBs
------------

`MDBNode`s are stored in a doubly linked list but you can derive a mapping hierarchy. This is called
the *capability derivation tree*. The doubly linked list is equivalent to a prefix traversal of the
derivation tree.

The "Strict Read Only" option.
-----------------------------

Turning on this option can save you from stuffing up the state of Isabelle
when you are "undoing" changes.

Often I'll find that undoing stuff edits things in the processed region thus causing
Isabelle to attempt an undo. Unfortunately it sometimes doesn't do this correctly.

Proof General -> Quick Options -> Read Only -> Strict Read Only