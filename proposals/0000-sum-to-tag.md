---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/369).

# Add `sumToTag#` primop

Add a `sumToTag#` primop indicating which sum alternative is
represented by a particular unboxed sum value. For example,

```haskell
sumToTag# (# a | #) = 0#
sumToTag# (# | | b #) = 2#
```

## Motivation

It's sometimes useful to get the numerical value of an alternative.
For example, I might write

```haskell
newtype Trit# = Trit# (# (##) | (##) | (##) #)

tritToInt# :: Trit# -> Int#
tritToInt# (Trit# (# (##)|| #)) = 0#
tritToInt# (Trit# (# |(##)| #)) = 1#
tritToInt# (Trit# (# ||(##) #)) = 2#
```

Under the hood, unboxed sums are actually represented by `Int#` tags
alongside the "payload" values, often with some padding. For example,
`(# (##) | Bool #)` is represented as something like `(# Int#, Bool #)`.
The values look like this:

```haskell
(# (##) | #)   ~=   (# 1#, padding #)
(# | b #)      ~=   (# 2#, b #)
```

Note: the choice to make the first alternative `1#` rather than `0#`
is poorly motivated, and should probably be changed, but that's beyond
the strict scope of this proposal.

If we could only get our hands on the `Int#` tag, then we could calculate
`tritToInt#` very efficiently. The C-- code would look much like the
code that would be produced by

```haskell
tritToInt'# :: (# Int# #) -> Int#
tritToInt'# (# t #) = t - 1
```

Unfortunately, that's *not* the sort of code that `tritToInt#` will
actually produce. Rather, it will produce code like this:

```haskell
tritToInt''# :: (# Int# #) -> Int#
tritToInt''# (# 3# #) -> 2#
tritToInt''# (# 2# #) -> 1#
tritToInt''# _ -> 0#
```

This seems pretty silly, considering that the result of `tritToInt#`
is likely to be fed straight into some numerical computation. There
will be extra code and unnecessary conditional branches when all that's
needed is a simple decrement operation.

## Proposed Change Specification

Add a `sumToTag#` primop much like `dataToTag#`. Much like `dataToTag#` can be
compiled only when the *type constructor* of its argument is known and is an
algebraic datatype, `sumToTag#` can be compiled only when the *runtime
representation* of its argument is known and is `'SumRep xs` for some known
`xs`. `sumToTag#` will return the 0-based index of the sum alternative of its
argument.

## Examples

```haskell
sumToTag# ((# | 2# | #) :: (# (# Int, Char #) | Int# | Bool #)) = 1#

-- No need to pin down specific types
sumToTag# ((# | x | #) :: (# a | x | b :: TYPE 'UnliftedRep #)) = 1#
```

This will compile, but can never ever be called:

```haskell
foo :: forall (a :: TYPE ('SumRep '[])). a -> Int#
foo = sumToTag#
```

This will compile, but cannot be called with current primitives;
if that changes in the future, it will always return `0#`.

```haskell
bar :: forall (a :: TYPE ('SumRep '[ 'IntRep])). a -> Int#
bar = sumToTag#
```

## Effect and Interactions

It will be possible to extract tags efficiently. It would be
beneficial to implement a special rule for `case` of `sumToTag#`, in
case that should appear in the course of optimization.

```haskell
case sumToTag# a of
  0# -> e0
  1# -> e1
  _ -> e2

===>

case a of
  (# _|| #) -> e0
  (# |_| #) -> e1
  (# ||_ #) -> e2
```

One long-term option this primop gives us is to stop returning
`Int#` from primops whose values represent only two or three
specific values. For example, we would have the option of
writing

```haskell
(==#) :: Int# -> Int# -> (# (##) | (##) #)
```

which more faithfully represents what the return value can be,
while still being able to work with the `Int#` representation
directly using `sumToTag#`. I do *not* propose making such a
change at this time.

## Costs and Drawbacks

Implementing this primop should be extremely cheap. Implementing
the special `case` rule will probably not be terribly hard; its
structure should generally follow that of the rule for `case` of
`dataToTag#`.

## Alternatives

I do not know of any alternative approach.

## Unresolved Questions

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

