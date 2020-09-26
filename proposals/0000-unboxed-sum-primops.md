---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/367).

# Use unboxed sums for primops

Several primops are expressed rather awkwardly, because they naturally
produce sum types, and historically those have not been available to
primops. Now that we have unboxed sums, we can use those.

## Motivation

For motivation, I'll give a simple example:

```haskell
tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
```

It's not immediately clear from the type what the result is supposed to mean.
According to the documentation:

> If `MVar#` is empty, immediately return with integer 0 and value undefined.
> Otherwise, return with integer 1 and contents of `MVar#`, and set `MVar#` empty.

What is this *really* trying to express? We can see that in the
type of the user-facing function:

```haskell
tryTakeMVar :: MVar a -> IO (Maybe a)
```

I think we should bring that clarity to the primops.

## Proposed Change Specification

I propose to replace the primops that produce product-encoded sums
with ones that produce proper unboxed sums, and to add compatibility
wrappers in `GHC.Exts` with the old types.

The primop `addCFinalizerToWeak#` effectively takes an unboxed sum as
an argument. For simplicity and clarity, I propose to split it into
two functions instead.

### The full list

I give the proposed type signature of the new primop and its implementation
in terms of the old one. The definitions of the compatibility wrappers are
trivial.

```haskell
tryTakeMVarS# :: MVar# s a -> State# s -> (# State# s, (# (# #) | a #) #)
tryTakeMVarS# mv s = case tryTakeMVar# mv s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, a #) -> (# s', (# | a #) #)

tryReadMVarS# :: MVar# s a -> State# s -> (# State# s, (# (# #) | a #) #)
tryReadMVarS# mv s = case tryReadMVar# mv s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, a #) -> (# s', (# | a #) #)

addCFinalizerToWeak1# :: Addr# -> Addr# -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
addCFinalizerToWeak1# fptr ptr w
  = addCFinalizerToWeak# fptr ptr 0# nullAddr# w

addCFinalizerToWeak2# :: Addr# -> Addr# -> Addr# -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
addCFinalizerToWeak2# fptr eptr ptr w
  -- Note the reversal in argument order; the current primop passes the
  -- arguments *backwards*.
  = addCFinalizerToWeak# fptr ptr 1# eptr# w

deRefWeakS# :: Weak# a -> State# RealWorld -> (# State# RealWorld, (# (# #) | a #) #)
deRefWeakS# w s = case deRefWeak# w s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, a #) -> (# s', (# | a #) #)

finalizeWeakS# :: Weak# a -> State# RealWorld -> (# State# RealWorld, (# (# #) | State# RealWorld -> (# State# RealWorld, b #) #) #)
finalizeWeakS# w s = case finalizeWeak# w s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, f #) -> (# s', (# | f #) #)

getSparkS# :: State# s -> (# State# s, (# (# #) | a #) #)
getSparkS# s = case getSpark# s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, spark #) -> (# s', (# | spark #) #)
```

Additionally, the source code of `getApStackVal#` seems to suggest that it
naturally returns an unboxed sum. Unfortunately, it is not documented in
`GHC.Prim` and I have not been able to figure out what it does.

## Examples

The implementation of `tryTakeMVar` will be simplified from the current

```haskell
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
  case tryTakeMVar# m s of
    (# s', 0#, _ #) -> (# s', Nothing #)      -- MVar is empty
    (# s', _,  a #) -> (# s', Just a  #)      -- MVar is full
```

to the much more obvious

```haskell
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
  case tryTakeMVarS# m s of
    (# s', (# (# #) | #) #) -> (# s', Nothing #) -- MVar is empty
    (# s', (# | a #) #) -> (# s', Just a #)      -- MVar is full
```

## Effect and Interactions

Your proposed change addresses the issues raised in the
motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


## Costs and Drawbacks

The mechanism for generating primops will have to be updated to support
unboxed sums.

## Alternatives

We could write a version of `addCFinalizerToWeak#` that takes an unboxed sum.
This would match the current API a little more closely, but the ergonomics seem
fairly lousy compared to the splitting option:

```haskell
addCFinalizerToWeakS# :: Addr# -> (# Addr# | (# Addr#, Addr# #) #) -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
addCFinalizerToWeakS# fptr (# ptr | #) w
  = addCFinalizerToWeak# fptr ptr 0# nullAddr# w
addCFinalizerToWeakS# fptr (# | (# eptr, ptr #) #)
  = addCFinalizerToWeak# fptr ptr 1# eptr w
```

## Unresolved Questions

Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

## Endorsements

(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

