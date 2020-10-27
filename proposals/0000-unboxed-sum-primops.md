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
primops. Now that we have unboxed sums, we can use those. The underlying
primop implementations will remain pretty much the same, except that
they'll have to produce `1` where they now produce `0` and `2` where
they now produce `1`.

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
with ones that produce proper unboxed sums.

The primop `addCFinalizerToWeak#` effectively takes an unboxed sum as
an argument. For simplicity and clarity, I propose to split it into
two primops instead.

### The full list

I give the proposed type signature of the new primop and its implementation in
terms of the old one, where the old primop is renamed to add the `Old` suffix
where required.  The definitions of the compatibility wrappers are fairly
trivial.

```haskell
tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, (# (# #) | a #) #)
tryTakeMVar# mv s = case tryTakeMVarOld# mv s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, a #) -> (# s', (# | a #) #)

tryReadMVar# :: MVar# s a -> State# s -> (# State# s, (# (# #) | a #) #)
tryReadMVar# mv s = case tryReadMVarOld# mv s of
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

deRefWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, (# (# #) | a #) #)
deRefWeak# w s = case deRefWeakOld# w s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, a #) -> (# s', (# | a #) #)

finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, (# (# #) | State# RealWorld -> (# State# RealWorld, b #) #) #)
finalizeWeak# w s = case finalizeWeakOld# w s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, f #) -> (# s', (# | f #) #)

getSpark# :: State# s -> (# State# s, (# (# #) | a #) #)
getSpark# s = case getSparkOld# s of
  (# s', 0#, _ #) -> (# s', (# (# #) | #) #)
  (# s', _, spark #) -> (# s', (# | spark #) #)
```

Additionally, the source code of `getApStackVal#` seems to suggest that it
naturally returns an unboxed sum. Unfortunately, it is not documented in
`GHC.Prim` and I have not been able to figure out what it does.

### Migration plan

In version `v`, we will add a module `GHC.NewPrimops` with library implementations
of the future primops using the current ones. These primops will have the same
names as the current ones, but different types. We will deprecate the current
primops.

In version `v+1`, we will make the new primops actual primitives, exported from
`GHC.Prim`

In version `v+3`, we will deprecate the `GHC.NewPrimops` module.

In version `v+4`, we will remove the `GHC.NewPrimops` module.

Exceptions: `addCFinalizerToWeak1#` and `addCFinalizerToWeak2#` will be added as
primops in `GHC.Prim` in version `v`, and `addCFinalizerToWeak#` deprecated. In version
`v+1`, `addCFinalizerToWeak#` will be removed.

From a user's standpoint:

* In version `v`, users of the primops will respond to deprecation warnings by
  importing the new operations from `GHC.NewPrimops` and adjusting their code
  accordingly.

* In version `v+3`, users will again respond to deprecation warnings by
  changing their import declaration from `GHC.NewPrimops` to `GHC.Prim` with no
  further changes.

* Users of `addCFinalizerToWeak#` will respond to deprecation warnings in
  version `v` by changing their code to use `addCFinalizerToWeak1#` or
  `addCFinalizerToWeak2#`.

Why not deprecate `GHC.NewPrimops` immediately in version `v+1`? This would
force users to use unpleasant three-way-branching CPP to support multiple GHC
versions without warnings. With the proposed plan, exactly the same code will
work without warnings in versions `v`, `v+1`, and `v+2`. Users wishing to
support both GHC versions `v-1` and `v` will need only a two-way branch in
CPP.

## Examples

The implementation of `tryTakeMVar` will be simplified from the current

```haskell
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
  case tryTakeMVarOld# m s of
    (# s', 0#, _ #) -> (# s', Nothing #)      -- MVar is empty
    (# s', _,  a #) -> (# s', Just a  #)      -- MVar is full
```

to the much more obvious

```haskell
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
  case tryTakeMVar# m s of
    (# s', (# (# #) | #) #) -> (# s', Nothing #) -- MVar is empty
    (# s', (# | a #) #) -> (# s', Just a #)      -- MVar is full
```

`GHC.ForeignPtr` will require a few minor changes, splitting `insertCFinalizer`
into two functions and using those in `addForeignPtrFinalizer` and
`addPtrForeignPtrFinalizerEnv`.

## Effect and Interactions

Your proposed change addresses the issues raised in the
motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.

## Costs and Drawbacks

The mechanism for generating primops will have to be updated to support
unboxed sums. Any future changes to the representation of unboxed sums
will have to be propagated to the primop implementations. Fortunately,
not many will be affected.

## Alternatives

### Finalizer addition

We could write a version of `addCFinalizerToWeak#` that takes an unboxed sum.
This would match the current API a little more closely, but the ergonomics seem
fairly lousy compared to the splitting option:

```haskell
addCFinalizerToWeakS# :: (# (# Addr#, Addr# #) | (# Addr#, Addr#, Addr# #) #) -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
addCFinalizerToWeakS# (# (# fptr, ptr #) | #) w
  = addCFinalizerToWeak# fptr ptr 0# nullAddr# w
addCFinalizerToWeakS# (# | (# fptr, eptr, ptr #) #)
  = addCFinalizerToWeak# fptr ptr 1# eptr w
```

The best ergonomics and maximal flexibility would come from using something
like the idea in
[#361 (Variadic Array Creation Primops)](https://github.com/ghc-proposals/ghc-proposals/pull/361),
but it's hard to justify the effort to do so for an operation that cannot be
expected to be used very heavily.

### Migration timing

It would be possible to extend the migration path by waiting a version (or even
two) after adding the `GHC.NewPrimops` module before deprecating the old
primops. This would allow code to support both version `v-1` and version `v`
with neither CPP nor primops. Since relatively few people use these primops
directly, I'm not sure it's worth delaying the process for this.

## Unresolved Questions

What is `getApStackVal#`? Would it make sense to represent its result
using an unboxed sum? If so, what sort?

What about all the primops that use `Int#` to represent a Boolean result?
Should these all be changed, eventually, to return `(# (# #) | (# #) #)`?
Today, users can write nonsense like

```haskell
case x ==# y of
  3# -> True
  _ -> False
```

GHC won't tell the user that this is nonsense, and will actually generate
code that performs this bogus calculation.

If we eventually change those Boolean-result primops, we might want to change
the tag values from 1-indexed to 0-indexed to better match the current
conventions. Furthermore, it would probably be helpful in that case to offer a
sort of unboxed-sum version of `dataToTag#` to extract the `Int#` value if that
is specifically desired. Indeed, these changes might be useful regardless.

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

