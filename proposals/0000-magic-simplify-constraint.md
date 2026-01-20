---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/479).

# Magic constraint simplification type family

When writing instances and sometimes other declarations in Template Haskell,
it can be quite difficult to produce the simplest constraint, even in rather
simple situations. I propose an entirely magical type family

```haskell
type GHC.Magic.SimplifyConstraint :: Constraint -> Constraint
```

to simplify constraints.

## Motivation

Suppose you want to derive `Eq` instances automatically using Template Haskell.
Each field of a datatype can lead to one or more constraints. For example, given

```haskell
data Foo f a b = Foo
  { this :: a
  , that :: f a
  , thother :: Maybe b
  , madre :: b
  , moof :: Int
  }
```

we would naively accumulate the following constraints:

this: `Eq a`

that: `Eq (f a)`

thother: `Eq (Maybe b)`

madre: `Eq b`

moof: `Eq Int`

These are rather redundant, as we only actually need to specify `Eq a`,
`Eq (f a)`, and `Eq b`.

## Proposed Change Specification

I propose adding a magical type family to `GHC.Magic`:

```haskell
type SimplifyConstraint :: Constraint -> Constraint
```

As the name suggests, this would simplify the constraint it's given using GHC's
usual constraint simplifier.

## Examples

Going back to the motivating example, we could splice in

```haskell
instance SimplifyConstraint (Eq a, Eq (f a), Eq (Maybe a), Eq b, Eq Int)
  => Eq (Foo f a b) where ...
```

GHC would simplify this to

```haskell
instance (Eq a, Eq (f a), Eq b)
  => Eq (Foo f a b) where ...
```

producing neither redundant/simplifiable constraint warnings nor unnecessary
arguments to the dictionary function.

As usual, GHC would be expected to use all available context to simplify. For
example, if (for whatever reason) I wrote

```haskell
f :: (Eq a, SimplifyConstraint (Eq (Maybe a))) => ...
```

then GHC would simplify that to

```haskell
f :: Eq a => ...
```

Extra parentheses could be used in the unusual case that an empty constraint
should be preserved:

```haskell
f' :: (Eq a, (SimplifyConstraint (Eq (Maybe a)))) => ...
```

would be simplified to

```haskell
f' :: (Eq a, ()) => ...
```

## Effect and Interactions

As discussed, it will become much easier to produce appropriate constraints
in Template Haskell splices. I don't foresee any obvious difficult interactions,
but I'm no constraint solver specialist.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


## Costs and Drawbacks

Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


## Alternatives

It would be possible to put the magic in Template Haskell rather than
`GHC.Magic`. Putting it in `GHC.Magic`, however, makes it available to
automatic code generation using other mechanisms.

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

(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

