---
author: Andreas Klebinger
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/568).

# Transitive specialization support.

Implement a `{-# SPECIALIZEABLE_REC foo #-}` pragma. Using such a pragma on a function will encourage GHC to specialize
all uses of `foo`, expose foos unfolding **and also do so for all use sites of `foo`** in a recursive fashion.

That is if we have

```haskell
{-# SPECIALIZEABLE_REC foo #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String] -> Bool
foo xs = elems 'c'
```

It will act as if we had written:

```haskell
{-# SPECIALIZEABLE_REC foo #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

{-# SPECIALIZEABLE_REC foo #-}
elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String] -> Bool
foo xs = elems 'c'
```

And will cause `elems` and `elem` to be specialized to `(Foldable [], Eq Char)` and `(Eq Char)` respectively.

## Motivation

Currently it's all too easy for crucial specialization to fail because an intermediate function somewhere
did not get the proper INLINEABLE pragma. My hope would be that the `SPECIALIZEABLE_REC` pragma would make it easier
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

GHC should add a two new pragmas: `SPECIALIZEABLE_REC` and `NOSPECIALIZEABLE_REC`.

Both pragmas can be used on :

* Top level functions
* Class instance methods

The `SPECIALIZEABLE_REC` pragma will have the following effects:

* Any annotated binding will have it's unfolding exposed.
  It will depend on any other pragmas on the function if optimized or unoptimized will be exposed
  with the default being optimized ones.
* Any annotated binding will be specialized if used at a monomorphic type.
* Any caller of such a binding will behave as if it was annotated with the pragma itself.

The `NOSPECIALIZEABLE_REC` pragma will prevent a caller from being affect by the `SPECIALIZEABLE_REC` flag
on any of the functions it calls.

## Examples

### `SPECIALIZEABLE_REC`
A simple example might the `elem` case from the motivation section.

```haskell
{-# SPECIALIZEABLE_REC elem #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String]
foo xs = elems 'c'
```

Then the pragma will be put on all callers as if we had written:

```haskell
{-# SPECIALIZEABLE_REC elem #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

{-# SPECIALIZEABLE_REC elems #-}
elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

{-# SPECIALIZEABLE_REC foo #-}
foo :: [String]
foo xs = elems 'c'
```

And will cause `elems` and `elem` to be specialized to `(Foldable [], Eq Char)` and `(Eq Char)` respectively.

Note that this is different from:

```haskell
{-# SPECIALISE elem :: Char -> String -> Bool #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

{-# SPECIALISE elem :: Char -> [String] -> Bool #-}
elems :: (Foldable t, Eq a) => a -> t [a] -> Bool
elems x = any (elem x)

foo :: [String]
foo xs = elems 'c'
```

The main difference being that with `SPECIALIZEABLE_REC`:

* We ensure the unfolding for `elem[s]` is available. And that it will be used to specialize these functions where possible.
  With `SPECIALISE` the unfolding of the original function might be stripped away and it makes it not any more likely to
  specialize at other call sites if they use a different `Eq` instance.
* We ensure any transitive callers of `elem[s]` also have their unfolding exposed and are specialized if possible.
  With `SPECIALISE` callers would not be any more likely to specialize.

### `NOSPECIALIZEABLE_REC`

```haskell
{-# SPECIALIZEABLE_REC elem #-}
elem :: Eq a => a -> [a] -> Bool
elem = ...

{-# NOSPECIALIZEABLE_REC any_elem #-}
any_elem :: (Foldable t, Eq a) => a -> t [a] -> Bool
any_elem x = any (elem x)
```

In this example `NOSPECIALIZEABLE_REC` prevents the pragma on `elem` from affecting `any_elem`.
This means any_elem is not any more or less likely to specialise than it would be if there where no pragmas involved at all.


## Effect and Interactions

### Manual `inline` and `SPECIALIZE` pragmas.

Besides automatic specialisation this would also allow for `SPECIALIZEABLE_REC` bindings to be manually
inlined using the magic inline function or explicitly specialised since the unfolding is
guaranteed to be available. The later can be useful in order to avoid repeated generation
of the same specialization at different use sites.

### `INLINE[ABLE]` pragmas

When a binding with an INLINEABLE or INLINE pragma get's marked as `SPECIALIZEABLE_REC` the only difference
would be that GHC would expose the original rhs instead of the optimized one.

Note that using INLINEABLE would also mean GHC will inline and specialize the unoptimised RHS of a bindings.
Which can cause difference in regards to what get's inlining, which rules fire, compile times and optimization
outcomes.

However it should ensure the same level of monomorphization as the use of `SPECIALIZEABLE_REC`.

### Default methods

Instance methods can be annotated with SPECIALIZEABLE_REC and instances implemented using the default
method will inherit the flag. For example if we have

```haskell
class Foldable t where
    -- ...
    foldMap :: Monoid m => (a -> m) -> t a -> m
    {-# SPECIALIZEABLE_REC foldMap #-}
    foldMap f = foldr (mappend . f) mempty

instance Foldable [] where
    ...
    -- no explicit foldMap implementation
```

Then in any use of the `foldMap` method from the list instance GHC would behave as if we had put SPECIALIZEABLE_REC
on the instance method.

Resulting in any callers of `foldMap @[]` to have their unfolding exposed, GHC specializing the foldMap call to the
Monoid instance used and any callers being marked `SPECIALIZEABLE_REC` as well.

## Costs and Drawbacks

Given that SPECIALIZEABLE_REC get's transitively put on every use site this could lead to a larger amount of
unfoldings being exposed resulting in increased compile times should this become common.

There is also a slight overhead from the pass needed to transfer the pragma from callees to callers.

## Alternatives

### Manual INLINE[ABLE] annotations

It's generally possible to achieve a similar effect by applying `INLINE[ABLE]` pragmas
to every function in the call graph. This is somewhat error prone and it can be hard to
verify that it works as expected.

### Using `-fexpose-all-unfoldings -fspecialise-aggressively` / Whole program compilation

This is another alternative. It generally comes with a very heavy compile time cost and
therefore is not as commonly used. It also requires users to explicitly enable it. While
SPECIALIZEABLE_REC can be enabled by library authors and would take effect without users having to
take extra steps.

### Putting the annotations on Classes instead

It could be possible to put the flag on Classes instead of bindings. That is instead
of writing:

```haskell
{-# SPECIALIZEABLE_REC foo #-}
elem :: Eq a => a -> [a] -> Bool
```

one would write

```haskell
{-# SPECIALIZEABLE_REC Eq #-}
class Eq a where
  ...
```

causing every function with an Eq constraint to be marked. This seems equally usefull
but potentially harder to implement. However the pragma being required on the class
could cause difficulty when one doesn't define but merely uses the class.

Either way this would be compatible with a SPECIALIZEABLE_REC pragma on functions so should not stand
in the way of this proposal.

## Unresolved Questions

### Allow the pragma on method declarations and classes?

Currently the proposal associated the pragma only with bindings.
However this could be expanded to allow the pragma on classes and
class method declarations.

Class example:

```haskell
{-# SPECIALIZEABLE_REC Eq #-}
class Eq a where
  ...
```

Which would cause every function with an Eq constraint to be marked as SPECIALIZEABLE_REC by the compiler.

Method declaration example:

```haskell
{-# SPECIALIZEABLE_REC Eq #-}
class Foldable a where
  ...
  {-# SPECIALIZEABLE_REC maximum #-}
  maximum :: forall a. Ord a => t a -> a
```

This would cause any caller of maximum to be marked as SPECIALIZEABLE_REC independent of the actual instance used.

Both of these seem useful but i'm not too familiar with the relevant code to know how hard it would be to implement
these extensions.

### Exposing optimized vs unoptimized unfoldings

I stated that in absence of `INLINEABLE` pragmas `SPECIALIZEABLE_REC` would expose optimized unfoldings. This is a design choice
that could be changed. In theory we could even have two versions: `{-# SPECIALIZEABLE_REC #-}` and `{-# SPECIALIZEABLE_REC INLINEABLE #-}`
similar to how SPECIALIZE works where GHC would do it's best to expose the original RHS in the later case.

### Name bikeshedding

The hardest question. Is `SPECIALIZEABLE_REC` a reasonable name for such a pragma?

## Implementation Plan

I (Andreas Klebinger) implemented a prototype here: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9642 and would
be interested in implementing this.

## Endorsements


