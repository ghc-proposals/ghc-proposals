---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/362).

# Incrementally cleared arrays

Add support for arrays of non-primitive values that can be initialized
incrementally, rather than all at once.

## Motivation

Lately, I've been playing around with trying to write data structures that
use arrays and have worst-case, rather than amortized, time bounds. For
structures full of primitive values like `Int` and `Char`, this is fairly
straightforward: it's possible to allocate a `MutableByteArray#` in constant time,
or close enough. But that changes altogether for non-primitive values.
To create a new `MutableArray#`, `MutableSmallArray#`, or `MutableArrayArray#`,
I must initialize the array elements to some value, which takes time linear in
the size of the array.

## Proposed Change Specification

I propose (nearly) the smallest change that will work for the particular
structures I'm currently experimenting with (see the Alternatives section for a
more flexible approach that would be considerably harder to implement). I'll
ignore the matter of `ArrayArray#` pending implementation of proposal #112; the
extension is straightforward.

### User-facing API

This API should probably appear in `primitive`, rather than `base`, but it should
suggest the general flavor.

```haskell
data IncrementallyClearedArray :: Type -> Type
data SmallIncrementallyClearedArray :: Type -> Type

newIncrementallyClearedArray :: Int -> ST s (IncrementallyClearedArray s)
newSmallIncrementallyClearedArray :: Int -> ST s (SmallIncrementallyClearedArray s)

clearIncrementallyClearedArray :: IncrementallyClearedArray s -> Int -> Int -> ST s ()
clearSmallIncrementallyClearedArray :: SmallIncrementallyClearedArray s -> Int -> Int -> ST s ()

completeIncrementallyClearedArray :: IncrementallyClearedArray s -> ST s (MutableArray s a)
completeSmallIncrementallyClearedArray :: SmallIncrementallyClearedArray s -> ST s (SmallMutableArray s a)

shrinkIncrementallyClearedArray :: IncrementallyClearedArray s -> Int -> ST s ()
shrinkSmallIncrementallyClearedArray :: SmallIncrementallyClearedArray s -> Int -> ST s ()

getSizeofSmallIncrementallyClearedArray :: SmallIncrementallyClearedArray s -> ST s Int
getSizeofSmallSmallIncrementallyClearedArray :: SmallSmallIncrementallyClearedArray s -> ST s Int
```

### Primitive API

```haskell
data IncrementallyClearedArray# :: Type -> TYPE 'UnliftedRep
data SmallIncrementallyClearedArray# :: Type -> TYPE 'UnliftedRep

newIncrementallyClearedArray# :: Int# -> State# s -> (# State# s, IncrementallyClearedArray# s #)
newSmallIncrementallyClearedArray# :: Int# -> State# s -> (# State# s, SmallIncrementallyClearedArray# s #)

clearIncrementallyClearedArray# :: IncrementallyClearedArray# s -> Int# -> Int# -> State# s -> State# s
clearSmallIncrementallyClearedArray# :: SmallIncrementallyClearedArray# s -> Int# -> Int# -> State# s -> State# s

completeIncrementallyClearedArray# :: IncrementallyClearedArray# s -> State# s -> (# State# s, MutableArray# s a #)
completeSmallIncrementallyClearedArray# :: SmallIncrementallyClearedArray# s -> State# s -> (# State# s, SmallMutableArray# s a #)

getSizeofSmallIncrementallyClearedArray# :: SmallIncrementallyClearedArray# s -> State# s -> (# State# s, Int# #)
getSizeofIncrementallyClearedArray# :: IncrementallyClearedArray# s -> State# s -> (# State# s, Int# #)

shrinkIncrementallyClearedArray# :: IncrementallyClearedArray# s -> Int# -> State# s -> State# s
shrinkSmallIncrementallyClearedArray# :: SmallIncrementallyClearedArray# s -> Int# -> State# s -> State# s
```

Except when necessary, I will describe only the `Array` version; `SmallArray` is
mostly identical.

An incremental array is created using `newIncrementallyClearedArray`. Slices of it
are cleared using `clearIncrementalArray`, which takes the first element to be
cleared and the number of elements to clear. Once all elements have been cleared,
it can be converted to a `MutableArray` using `completeIncrementalArray`.
`getSizeofIncrementallyClearedArray` and `shrinkIncrementallyClearedArray` work
just like the corresponding operations for `MutableByteArray`.

### Implementation

An `IncrementallyClearedArray#` and `IncrementallyClearedSmallArray#` are
actually `ByteArray#` in disguise. Creating one involves allocating a
`ByteArray#` large enough to accommodate both the array elements and (in the
case of `IncrementallyClearedArray#`) the card table. For
`IncrementallyClearedArray#`, we also initialize the card table to indicate
that it does *not* need to be scanned.

The clearing operations write "null
pointers" into the array slots. These are actually pointers to a statically
allocated object. Ideally, that object would be a thunk that throws an
exception. Since well-behaved exceptions don't really exist in `GHC.Prim`, we
should just make it a thunk that fails with an unchecked exception (like
`quotInt#` does when passed `0#`).

`completeIncrementallyClearedArray#` changes
the heap object type to indicate that it is now an `MutableArray#` and not a
`MutableByteArray#`. `getSizeofIncrementallyClearedArray#` and
`shrinkIncrementallyClearedArray#` work just like the corresponding operations
for byte arrays except that they make the necessary adjustments for the card
table in the `IncrementallyClearedArray#` case.

Why do we write the card table when we create the `IncrementallyClearedArray#`
and not when we complete it? This way, it's safe for multiple threads to
complete the same array; the second time the array is completed nothing changes.
If we wrote the card table on completion, then garbage collection would go
haywire if one thread wrote to the completed array and then another thread
cleared its card table.

## Examples

Consider a classic (ephemeral) array-doubling stack:

```haskell
data Stack s a = Stack
  { top :: !Int
  , size :: !Int
  , elements :: !(IORef (MutableArray s a))  -- length size
  }
```

When the stack fills up, a new array is allocated and the elements are
copied. This provides amortized constant time operations. To achieve
worst-case constant time operations, we need to perform both the
array allocation and array copying incrementally:

```haskell
data Stack s a = Stack
  { top :: !Int
  , size :: !Int
  , elements :: !(IORef (MutableArray s a)) -- length size
  , copying :: !(IORef (MutableArray s a)) -- length 2 * size
  , initializing :: !(IORef (IncrementallyClearedArray s)) -- length 4 * size
  }
```

If we perform enough initializations and copies per `push` operation,
then once the `elements` array is full, the `copying` array will
contain a full copy of it is an initial segment and the `initializing`
array will be ready to complete and install as the new `copying` array.

## Effect and Interactions

I do not foresee any significant interactions with existing features.

## Costs and Drawbacks

Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


## Alternatives

* A more flexible alternative would be to offer an array type that could be
  used *as an array* while it is in the process of being cleared. This would
  require that it keep track of which positions contain pointers that the
  garbage collector should follow and which do not. Unfortunately, this would
  require new heap object types, which are pretty expensive.

* In some contexts, it would be more convenient, and perhaps more efficient, to
  just use the `MutableArray#` or `SmallMutableArray#` types to represent
  arrays in the process of being cleared. In the `Stack` example above,
  this would allow for a single field representing an array that is first
  initialized and then copied. The downside is that it would be very easy
  to get mixed up and apply nonsensical operations to arrays in the wrong
  phase.

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

