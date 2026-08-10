---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/445).

# Non-overlappable instances

Sometimes it is useful to ensure that a particular class instance
is the absolute final word, and will never be overlapped. I propose
to add a `NON_OVERLAPPABLE` pragma to enforce this.


## Motivation

John Ericson [suggested](https://github.com/haskell/containers/issues/706)
a rather clever function for `Data.Map`, based on an idea by Dan Bornside.

```haskell
splitEither
  :: Map (Either b c) v
  -> (Map (Either b Void) v, Map (Either Void c) v)
splitEither = unsafeCoerce . spanAntitone isLeft
```

Unlike a more conventional approach to splitting such a map, this accomplishes
the split in `O(log n)` time, rather than `O(n)` time. It relies on the fact
that (ignoring certain "unsafe" functions we could easily move to an `Unsafe`
module), `spanAntitone isLeft` is guaranteed to produce one `Map` whose keys
are all `Left`s and another whose keys are all `Right`s. The `unsafeCoerce`,
therefore, won't actually coerce any keys from `b` or `c` to `Void`. Or at
least, that's what it thinks.

Unfortunately, that is *not* actually guaranteed. Some malicious (or ignorant)
person could render the arrangement thoroughly unsafe using overlapping or
incoherent instances. For example, they could write

```haskell
instance {-# OVERLAPPING #-} Ord (Either Int Char) where
  compare (Left a) (Left b) = compare a b
  compare (Right a) (Right b) = compare a b
  compare (Left _) (Right _) = GT -- Wait...
  compare (Right _) (Left _) = LT -- What?
```

Now, values really *will* be coerced to `Void`, and all sorts of demons will
fly out of noses.


## Proposed Change Specification

I propose a `NON_OVERLAPPABLE` pragma be available for instances, with the
same syntax as the `OVERLAPPABLE` pragma:

```haskell
instance {-# NON_OVERLAPPABLE #-} (Ord a, Ord b) => Ord (Either a b) where
  ...
```

It could optionally be combined with the `OVERLAPPING` or `INCOHERENT`
pragma in either order:

```haskell
instance {-# NON_OVERLAPPABLE #-} {-# OVERLAPPING #-} a ~ b => Foo Int a b
instance {-# NON_OVERLAPPABLE #-} {-# INCOHERENT #-} X a a
```

### Effects

For the sake of exposition, the text below describes all potential overlapping
instances as `OVERLAPPING` whether they are defined using the `OVERLAPPING`
pragma, the `OVERLAPS` pragma, or the deprecated `OverlappingInstances`
language feature.

A `NON_OVERLAPPABLE` pragma on an instance limits the effect of any
`{-# OVERLAPPING #-}` or `{-# INCOHERENT #-}` pragma that would lead to overlap
of the marked instance. Specifically, the user's guide description of when
an instance is *overlappable* is modified thus:

* An instance is *non-overlappable* if it has a `NON_OVERLAPPABLE` pragma.

* An instance is *overlappable* if it does not have a `NON_OVERLAPPABLE` pragma
  and one of the following hold: it has an `OVERLAPPABLE` or `OVERLAPS`
  pragma; or if the instance has no pragma and it appears in a module compiled
  with `OverlappingInstances`; or if the instance is incoherent.

The description on the process of searching for an instance of a target constraint
is modified thus:

* Find all instances `I` that *match* the target constraint; that is, the target
  constraint is a substitution instance of `I`. These instance declarations are the
  *candidates*.

* If more than one candidate is non-overlappable, the search fails.

* Eliminate any candidate `I X` for which there is another candidate `I Y` such that
  all of the following hold:

    * `I Y` is strictly more specific than `I X`. That is, `I Y` is a substitution
      instance of `I X` but not vice versa.

    * `I X` is not non-overlappable.

    * Either `I X` is overlappable, or `I Y` is overlapping. (This “either/or”
      design, rather than a “both/and” design, allow a client to deliberately
      override an instance from a library, without requiring a change to the
      library, as long as the library has not explicitly prohibited it.)

* If more than one non-incoherent candidate remains, the search fails.

* If a non-incoherent candidate and an incoherent, non-overlappable candidate remain,
  the search fails.

* If no candidates remain, the search fails.

* Choose a "prime candidate" from among the remaining candidates. If there is a
  non-incoherent candidate remaining, that one becomes the prime candidate.
  Otherwise, a randomly-chosen incoherent candidate becomes the prime candidate.

* Now find all instances, or in-scope given constraints, that *unify* with the
  target constraint, but do not *match* it. Such non-candidate instances might
  match when the target constraint is further instantiated. If any of them are
  non-overlappable, then the search fails. Otherwise, if the prime candidate
  is incoherent then the search succeeds, returning it. Otherwise, if all the
  unifying instances are incoherent top-level instances then the search succeeds,
  again returning the prime candidate. Otherwise, the search fails.

## Examples


## Effect and Interactions

The `Eq` and `Ord` instances for important types can now use `NON_OVERLAPPABLE`:

```haskell
instance {-# NON_OVERLAPPABLE #-} (Eq a, Eq b) => Eq (Either a b) where
instance {-# NON_OVERLAPPABLE #-} (Ord a, Ord b) => Ord (Either a b) where
```

Then no attempt to confuse people by overlapping them can succeed.

## Costs and Drawbacks

The instance resolution rules for `{-# INCOHERENT #-}` are already complex
and somewhat counterintuitive. They become a little more so.

## Alternatives

It is already possible to block undesirable overlap from the other direction—in
class declarations. It would be possible to write, for example,

```haskell
class Eq a where
  -- Not exported
  type NO_OVERLAP_Eq a
  type NO_OVERLAP_Eq a = ()
class Ord a where
  -- Not exported
  type NO_OVERLAP_Ord a
  type NO_OVERLAP_Ord a = ()
```

These otherwise-worthless associated types effectively ban *all* overlapping
instances for those classes. But that is quite a big hammer, and is likely
to break a lot of code.

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

