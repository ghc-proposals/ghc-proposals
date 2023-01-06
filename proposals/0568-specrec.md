---
author: Andreas Klebinger
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/568).

# Transitive specialization support.

Implement a `{-# SPECREC foo #-}` pragma. Using such a pragma on a function will encourage GHC to specialize
all uses of `foo`, expose foos unfolding **and also do so for all use sites of `foo`** in a recursive fashion.

That is if we have

```haskell
{-# SPECREC foo #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String]
foo xs = elems 'c'
```

It will act as if we had written:

```haskell
{-# SPECREC foo #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

{-# SPECREC foo #-}
elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String]
foo xs = elems 'c'
```

And will cause `elems` and `elem` to be specialized to `(Foldable [], Eq Char)` and `(Eq Char)` respectively.

## Motivation

Currently it's all too easy for crucial specialization to fail because an intermediate function somewhere
did not get the proper INLINEABLE pragma. My hope would be that the `SPECREC` pragma would make it easier
to ensoure that a program is properly monomorphized. As it only requires the performance sensitive parts
to be annotated and all call sites up to main will be annotated automatically.

For an example one might look at the "Problem 2" section of [this blog post](https://www.tweag.io/blog/2022-12-22-making-ghc-faster-at-emitting-code/) from `TWEAG`.

There they describe the problem as:

> Sadly, the only way to force GHC to perform cross-module specialization is to use explicit SPECIALIZE pragmas on every definition you want to be specialized.
> This required adding dozens of pragmas of roughly the following form to various definitions throughout GHC: `{-# SPECIALIZE helloWorld :: HLine #-}`

Going on further:

> It’s worth reflecting a little on just how difficult this currently is to get right in Haskell. In other programming languages, programming against an interface and supplying multiple implementations is something that people do all the time without worrying nearly so much about the performance subtleties of doing so. In languages like C++ and Rust, specialization is guaranteed (since those languages never use dictionary passing), whereas in languages like Java, the JIT can dynamically detect monomorphic calls and specialize them on the fly. Both of these approaches have downsides, but they provide a great deal of utility, and they provide an important set of tools for writing efficient code without needing to break abstraction boundaries.

I believe this pragma would be one step in the right direction to lessen that pain.

## Proposed Change Specification

GHC should add a new `SPECREC` pragma which can be used on :

* Top level functions
* Class instance methods

The pragma will have the following effects:

* Any annotated binding will have it's unfolding exposed.
  No guarantee is given if in optimized or unoptimized form.
* Any annotated binding will be specialized if used at a monomorphic type.
* Any caller of such a binding will behave as if it was annotated with the pragma itself.

## Examples

A simple example might the `elem` case from the motivation section.

```haskell
{-# SPECREC foo #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String]
foo xs = elems 'c'
```

It will act as if we had written:

```haskell
{-# SPECREC foo #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

{-# SPECREC foo #-}
elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String]
foo xs = elems 'c'
```

And will cause `elems` and `elem` to be specialized to `(Foldable [], Eq Char)` and `(Eq Char)` respectively.

## Effect and Interactions

### Manual `inline` and `SPECIALIZE` pragmas.

Besides automatic specialisation this would also allow for `SPECREC` bindings to be manually
inlined using the magic inline function or explicitly specialised since the unfolding is
guaranteed to be available. The later can be useful in order to avoid repeated generation
of the same specialization at different use sites.

### `INLINE[ABLE]` pragmas

When a binding with an INLINEABLE or INLINE pragma get's marked as `SPECREC` the only difference
would be that GHC would expose the original rhs instead of the optimized one.

### Default methods

Instance methods can be annotated with SPECREC and instances implemented using the default
method will inherit the flag. For example if we have

```haskell
class Foldable t where
    -- ...
    foldMap :: Monoid m => (a -> m) -> t a -> m
    {-# SPECREC foldMap #-}
    foldMap f = foldr (mappend . f) mempty

instance Foldable [] where
    ...
    -- no explicit foldMap implementation
```

Then in any use of the `foldMap` method from the list instance GHC would behave as if we had put SPECREC
on the instance method.

Resulting in any callers of `foldMap @[]` to have their unfolding exposed, GHC specializing the foldMap call to the
Monoid instance used and any callers being marked `SPECREC` as well.

## Costs and Drawbacks

Given that SPECREC get's transitively put on every use site this could lead to a larger amount of
unfoldings being exposed resulting in increased compile times should this become common.

There is also a slight overhead from the pass needed to trasnfer the pragma from callees to callers.

## Alternatives

### Manual INLINE[ABLE] annotations

It's generally possible to achieve a similar effect by applying `INLINE[ABLE]` pragmas
to every function in the call graph. This is somewhat error prone and it can be hard to
verify that it works as expected.

### Using `-fexpose-all-unfoldings -fspecialise-aggressively` / Whole program compilation

This is another alternative. It generally comes with a very heavy compile time cost and
therefore is not as commonly used. It also requires users to explicitly enable it. While
SPECREC can be enabled by library authors and would take effect without users having to
take extra steps.

### Putting the annotations on Classes instead

It could be possible to put the flag on Classes instead of bindings. That is instead
of writing:

```haskell
{-# SPECREC foo #-}
elem :: Eq a => a -> [a] -> Bool
```

one would write

```haskell
{-# SPECREC Eq #-}
class Eq a where
  ...
```

causing every function with an Eq constraint to be marked. This seems equally usefull
but potentially harder to implement. However the pragma being required on the class
could cause difficulty when one doesn't define but merely uses the class.

Either way this would be compatible with a SPECREC pragma on functions so should not stand
in the way of this proposal.

## Unresolved Questions

### Allow the pragma on method declarations and classes?

Currently the proposal associated the pragma only with bindings.
However this could be expanded to allow the pragma on classes and
class method declarations.

Class example:

```haskell
{-# SPECREC Eq #-}
class Eq a where
  ...
```

Which would cause every function with an Eq constraint to be marked as SPECREC by the compiler.

Method declaration example:

```haskell
{-# SPECREC Eq #-}
class Foldable a where
  ...
  {-# SPECREC maximum #-}
  maximum :: forall a. Ord a => t a -> a
```

This would cause any caller of maximum to be marked as SPECREC independent of the actual instance used.

Both of these seem useful but i'm not too familiar with the relevant code to know how hard it would be to implement
these extensions.

### Exposing optimized vs unoptimized unfoldings

I stated that in absence of `INLINEABLE` pragmas `SPECREC` would expose optimized unfoldings. This is a design choice
that could be changed. In theory we could even have two versions: `{-# SPECREC #-}` and `{-# SPECREC INLINEABLE #-}`
similar to how SPECIALIZE works where GHC would do it's best to expose the original RHS in the later case.

### Name bikeshedding

The hardest question. Is `SPECREC` a reasonable name for such a pragma?

## Implementation Plan

I (Andreas Klebinger) implemented a prototype here: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9642 and would
be interested in implementing this.

## Endorsements


