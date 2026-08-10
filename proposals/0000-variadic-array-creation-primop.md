---
author: Vilem-Benjamin Liepelt (@buggymcbugfix)
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/361).

# Variadic Array Creation Primops

We propose two new primops `arrayOf#` and `smallArrayOf#` for allocating known length arrays. [Microbenchmarks](https://github.com/buggymcbugfix/not-not-a-blog/blob/master/blog/2020-08-17-array/post.md) show a ~2.3x improvement over statically unrolled writes and almost 5x improvement over dynamic array writes.

This proposal enables the following optimization: when the array elements are all literals or top-level symbols, we can allocate arrays statically. This effectively gives us array literals without adding any new syntax to the language.

The contents of this proposal was presented at a lightning talk at HIW @ ICFP 2020. [[slides](https://github.com/buggymcbugfix/not-not-a-blog/blob/master/talks/2020-08-28-hiw/soc.pdf)]

## Motivation

### Problem

Currently the creation of an array from some given values/bindings needs to be done in several steps. For example:

1. Create new array of desired size, initializing all slots to `undefined`.
2. Write all elements.
3. Freeze.

As a slight optimization we can initialize the array to the 0th element in step 1.

We are unnecessarily paying for two things here:

1. An unnecessary write of the whole array (step 1).
2. A GC write barrier because the array is initially mutable.

### Solution

It is not possible to return an array of uninitialized memory in the presence of GC. At any rate we pay for the write barrier. Ideally we'd want to atomically allocate an immutable array.

We could of course add a family of definitions

```hs
arrayOf0# :: Array# a
arrayOf1# :: a -> Array# a
arrayOf2# :: a -> a -> Array# a
arrayOf3# :: a -> a -> a -> Array# a
...
```

Evidently we would benefit from some generalization here, especially given that we may want to allocate arrays of thousands of elements! Really, what we'd want is a _variadic primop_.

There is a well-known trick for encoding variadic functions using type classes, alas we can't use such machinery in primop definitions. (To the best of my knowledge.)


## Proposed Change Specification

We propose here a novel way of achieving variadicity: we pass the parameters in an arbitrarily nested homogenous unboxed tuple.

```hs
let
  a1 :: Array# Int = arrayOf# (# 1, 2, 3, 4, 5, 6, 7 #) -- size 7
  a2 :: SmallArray# Bool = smallArrayOf# (# #)          -- size 0
in
  ...
```
During [unarise](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Stg/Unarise.hs), these will get rewritten as [arguments to the primops](https://gitlab.haskell.org/ghc/ghc/-/blob/c1e54439be3d38a1f972ac772cca7eec5e1519a9/compiler/GHC/StgToCmm/Prim.hs#L157). We are really quite fortunate; this all works quite neatly without much effort.

We can circumvent GHC's size-62-tuple restriction by nesting the unboxed tuples. Our WIP implementation shows that this is viable for thousands of elements.

As mentioned above, we can now statically allocate arrays when the arguments are known at compile time; this includes not only literals: a name/label is also a static argument.

##  Example

In `unordered-containers` we could change

```hs
pair :: a -> a -> Array a
pair x y = run $ do
    ary <- new 2 x
    write ary 1 y
    return ary
```

to

```hs
pair :: a -> a -> Array a
pair x y = Array (smallArrayOf# (# x, y #))
```

As another example, here is a self-contained Haskell source file where we allocate some arrays with the new primitive.

```hs
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (arrs) where

import GHC.Exts

data Array = Array !(Array# Bool)

arrs :: [Array]
arrs =
    [ Array (arrayOf# (# True #))                     -- size 1
    , Array (arrayOf# (# #))                          -- size 0
    , Array (arrayOf# (# False, True #))              -- size 2
    , Array (arrayOf# (# (# #), (# False, True #) #)) -- size 2*
    ]
    -- (*) structurally equal to previous due to unarise pass
```

### Lookup tables

Using the new primops and the static allocation optimization we can easily achieve static lookup tables, e.g. with a Typed Template Haskell combinator (which could be further generalized to `Foldable`):

```hs
smallArrayOf :: Lift a => [a] -> Code Q (SmallArray# a)
```

[[source here](https://github.com/buggymcbugfix/arrayOf-benchmark/blob/master/MkSmallArray.hs)]


## "Show me the typing rules alright!"

Unfortunately these primops are quite untyped:

```hs
arrayOf# :: forall r (a :: TYPE r) b. a -> Array# b
```
```hs
smallArrayOf# :: forall r (a :: TYPE r) b. a -> SmallArray# b
```

Stealing C variadic function syntax, we might want to imagine the type thus:

```hs
arrayOf# :: forall a. (# a, ... #) -> Array# a
```

We can express something like this with type classes but this proposal leaves it for safe wrappers to be provided via other libraries as it is currently not possible to define primops with class constraints or other fancy types.

## Effect and Interactions

- As mentioned in the introduction, we reduce array creation overhead by more than 2x, for the case of arrays with statically known length and by more than 5x vs dynamic allocation in a loop (although note that dynamic allocation is also more flexible since the length need not be known at compile time).
- Requires `-XUnboxedTuples` syntax.

## Costs and Drawbacks

- We introduce variadic primops as a new idiom, which depends on unboxed tuples and the unarise stg->stg pass.
- The types of the proposed primops are unsound (although there is ample precedent for this in GHC.Exts).
- More primops.

## Alternatives

- Fixed-length primops. Easy to make type safe, but at what size do we stop?
- Don't do this: it's "just" for perf (but, but, I want statically allocated arrays!!)

## Unresolved Questions

The static allocation optimization might silently not fire and thus unexpectedly allocate at runtime. We could add another set of primops `array#` and `smallArray#` that guarantee static allocation or else fail at compile time. This does mean, however, that code that compiles fine with `-O` may fail to compile with `-O0` (I think). This might be a price worth paying though for having something as close as possible to "array literals". Need to investigate this; perhaps we could make an extra effort to optimize the arguments to `arrayOf#`?

## Implementation Plan

@buggymcbugfix has implemented this for `SmallArray#` here: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3571.

### TODOs

- The static allocation optimization still needs SRT business sorted (if you are an RTS guru, please help!)
- Need to implement for `Array#` (pretty mechanical)
- Tests

## Endorsements

Done as part of a Haskell Summer of Code project with mentoring support from @AndreasPK, @andrewthad, @chessai.
