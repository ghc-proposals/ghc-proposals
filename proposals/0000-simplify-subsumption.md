---
author: Simon Peyton Jones
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/287).

# Simplify subsumption

This proposal argues that GHC’s type system is a bit too liberal; in particular, the subsumption judgement is too liberal.  It adds a little bit of convenience for the programmer, but at quite a high cost in terms of changing semantics and type-system complexity.   So I propose that we should simplify the type system, by removing
* Covariance of function types
* Contravariance of function types
* Deep skolemisation
* Deep instantiation

All four features are implemented in GHC.

However, **all four change the semantics of Haskell*, by performing eta-expansion, changing bottom into non-bottom.  That’s a pretty serious change, which needs pretty serious motivation.  But actually all we get in exchange is a minor improvement in programming convenience.  So I argue that:

* **The benefit, in terms of programming convenience, is small**.  In particular, while using these features accepts some more programs, any such program is easily accepted without these features, after a small edit.

* **The cost is large**.  There is extra complexity in the compiler.  There is extra complexity in the formal description of the type system, and in any paper we write about it.  But, worst of all, the features change the semantics of the program.

* A recent new cost is that the [Quick Look Impredicativity proposal]( https://github.com/ghc-proposals/ghc-proposals/pull/274) gains extra power by not having contravariance.

Here is a thought experiment.  Suppose GHC lacked all four features, and someone proposed adding them.  That proposal would never leave the launchpad. A minor change in programming convenience, in exchange for changing language semantics?  No.  If that’s true, then the only issue would be back-compat issues: how many libraries would be affected, and how painful they would be to fix.  We need to get data on that.

## Motivation

The baseline for this proposal is [Practical Type Inference for Arbitrary Rank Types]( https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/), and specifically Section 4.6 which concerns the subsumption judgement.  That section discusses
* **Covariance** of the function arrow (Fig 7, rule FUN)
* **Contravariance** of the function arrow (Fig 7, rule FUN)
* **Deep skolemisation** (Fig 7, rule DEEP-SKOL)
* **Deep instantiation**.  Section (4.7.3) says that we don’t need deep instantiation, but GHC actually does that too, for reasons sketched below.

The proposal argues to remove all four.  The following subsections descuss each in turn.

### Deep skolemisation

Consider these types (paper, 4.6.1):
```
f :: ∀ab.a → b → b)
g :: (∀p.p → (∀q.q → q)) → Int
```
Is `(g f)` well typed?  Notice that `g` requires an argument with a forall to the right of an arrow. With deep skolemisation, the answer is “yes”.  But remember that GHC elaborates to System F.  Here is its elaboration of `(g f)`:
```
g (/\p. \(x:p). /\q.\(y:q). f @p @q x y)
```
The lambdas do the impedence matching to turn `f` into an argument of exactly the type that `g` expects.

BUT suppose `f` and `g` are defined like this:
```
f = bottom
g f = f `seq` 0
```
You would expect `g f` to diverge, since it seq’s on bottom.  But it won’t! it’ll return 0.  Yikes.

Without deep skolemisation, `(g f)` is rejected.  But the programmer can easily repair it by manual eta-expansion, to
```
g (\x y. f x y)
```
and now, of course, it is not surprising that the expression evaluates to 0.

### Deep instantiation

Suppose `f :: Int -> forall a. a -> a`.   Again, notice the forall to the right of the arrow.  Now consider this definition, which lacks a type signature:
```
g x = f
```
What type would you expect to infer for `g`?   The obvious answer (and the one we'd get without deep instantiation) is
```
g :: forall b. b -> Int -> forall a. a -> a
```
But GHC actually deeply instantiates `f` (for no very well-explained reason), so we get
```
g :: forall b a. b -> Int -> a -> a
```
with the buried foralls pulled to the top.  Perhaps that type is a tiny bit more explicable to the programmer.  But again, to produce that type, GHC must elaborate `g` to
```
g = /\ b a. \(x:b). \(i:Int). f i @a
```
GHC has eta-expaned `f`, which changes the semantics.  Yikes.

### Contravariance

Suppose you have
```
g :: ((forall a. a -> a) -> R) -> S
f :: (Int -> Int) -> R
```
Now, is `(g f)` well typed?   That depends on whether
```
(Int -> Int) -> R   <=    (forall a. a -> a) -> R
```
where `<=` is pronounced “is more polymorphic than”  see the paper sections 4.4. and 4.6.

Well, according to rule FUN of Figure 7, using contravariance of `(->)`, that is true if
```
forall a. a -> a    <=      Int -> Int
```
and that is certainly true.  But again, to witness that proof GHC needs to eta-expand during elaboration.  We get this elaboration of `(g f)`:
```
g (\(h : forall a. a->a).  f (h @Int))
```
Again we have changed the semantics.  Yikes.

Again, lacking covariance the program would be rejected, but is easily fixed by manual eta-expansion, thus `g (\h -> f h)`

### Covariance

Fig 7 in the paper also supports covariance of the function arrow, but exactly the same eta-expansion issues arise.

#


## Proposed Change Specification

There are no syntactic changes.

The changes to the type system is to simplify the subsumption judgement
by removing

* Covariance of function types
* Contravariance of function types
* Deep skolemisation
* Deep instantiation

This could be under control of a flag, but I propose that we
take a clear path to removing these features altogether, i.e.
ultimately, without a flag that enables them.

## Examples

See Motivation above.

## Effect and Interactions

* Everything (specification, implementation) becomes a bit simpler
* Quick Look Impredicativity gains more power

## Costs and Drawbacks

The main cost is that some existing programs will require some manual eta-expansion.

Jurriaan is gathering data on how many libraries are affected.

Ideally we'd like a deprecation saying "you are using this feature, and it's going to disappear".  But this would have falies positives: just because deep instantiation does eta-expansion does not imply that using only shallow instantaition would make the program un-typable.

## Alternatives

Status quo. But the the stauts quo is extremely unsatisfactory.


## Unresolved Questions

## Implementation Plan

Implemenation is relatively easy.  I can do it, or Richard, or Alejandro.
