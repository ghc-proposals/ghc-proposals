---
author: Daniel Smith
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/510).

# Add GHC.Variants module to mirror GHC.Records

Recently the GHC.Records module was introduced for interacting with records without
the problem of field name conflicts.

Since Variants and Records are duals, it is natural to have a matching GHC.Variants
module for interacting with variants without the problem of constructor name conflicts.

[I will link the GHC.Records proposal here as I reference it frequently.](https://github.com/ghc-proposals/ghc-proposals/pull/6)


## Motivation

The motivations for GHC.Variants are largely the same as the motivations for GHC.Records.

Naming conflicts between constructors force the use of verbose and unappealing prefixes
or suffixes. This has the additional downside of making generically-derived instances
such as pretty printing, JSON serailization and DB serialization more verbose.

One common example of collisions is when constructors have the same name as another type they reference.
For example `data Animal = Dog Dog | Cat Cat | ...` or `data Region = City City | State State | ...`,
which will collide with the actual `Dog`/`City` constructors.

Another common example is enum types that are themselves describing the notion of a type.
For example a DB column flagging which child table points to it `data IdentityType = User | Group`,
which will collide with the actual `User`/`Group` constructors.

Not having access to constructors at the typeclass level also necessitates template haskell or
significant verbosity to define prisms or similar.

You can particularly see these prefixes get out of control when dealing with generated types, as
they generally err on the safe side and don't try to get too clever with shorter names that risk
collision, for example with [Stripe](https://hackage.haskell.org/package/stripeapi-2.0.1.0/docs/StripeAPI-Operations-PostPaymentIntentsIntentCancel.html#t:PostPaymentIntentsIntentCancelRequestBodyCancellationReason-39-).


## Proposed Change Specification

### GHC.Variants module

The first change is introducing a `GHC.Variants` module that is more or less the dual of
`GHC.Records`:

```haskell
module GHC.Variants where

type family ConstructorType {k} (x :: k) v

class HasConstructor {k} (x :: k) v where
    construct :: ConstructorType x v -> v

class HasConstructor x v => SetConstructor {k} (x :: k) v where
    setConstructor :: ConstructorType x v -> v -> v
    setConstructor = updateConstructor @x . const

    updateConstructor :: (ConstructorType x v -> ConstructorType x v) -> v -> v

    {-# MINIMAL updateConstructor #-}

class SetConstructor x v => MatchConstructor {k} (x :: k) v where
    match :: v -> Maybe (ConstructorType x v)
```

`MatchConstructor` is not technically a dual of anything in GHC.Records, due to its dual being
trivial. However prisms are still quite frequently used in practice, so it seems appropriate to
include it.

With that said `MatchConstructor` should explicitly not be thought of as a building block for
overloaded pattern matching, as it has the wrong type. In the same way that GHC.Records does not
currently have any building blocks for record construction (the dual of pattern matching a variant).

Credit to bss03 and affinehyperplane on /r/haskell for clarity on the dual aspects of matching.

Note that the design is based on [my prior proposal here](https://github.com/ghc-proposals/ghc-proposals/pull/286).
I was initially planning on waiting to see the outcome of that proposal before making this.
Whatever results from that proposal I will mirror here for consistency.

### Resolving instances

As with `GHC.Records` we now also need to add in some compiler magic for resolving instances of
these classes that have not been manually defined. The mechanism is equivalent, except it looks for
constructors being in scope, rather than fields.

This means it will have more or less the same edge cases and and interactions with other features
like ExistentialQuanitification, GADTs and DatatypeContexts as `GHC.Records`.

### IsLabel (->) instance

In addition to the above changes. I also propose adding an `IsLabel` instance for `->` so that we
get some nice syntax for overloaded variant construction right off the bat without needing brand
new syntax for it.

```
instance (HasConstructor x v, ConstructorType x v ~ a) => IsLabel x (a -> v) where
    fromLabel = construct @x
```

To allow for this instance to actually be used, we also need to allow labels to be uppercase.

This allows for code like `#Left 5 :: Either Int Bool` and `#Left 3 :: Either3 Int Bool Char`.

I know the `GHC.Records` proposal also included an `IsLabel (r -> a)` instance for reading from
fields, but given `RecordDotSyntax` has been accepted since that discussion, such an instance seems
unnecessary.


## Examples

```haskell
data MyType
    = NoFields
    | OneField Char
    | TwoFields Int Bool

noFields :: MyType
noFields = #NoFields ()

oneField :: MyType
oneField = #OneField 'c'

twoFields :: MyType
twoFields = #TwoFields (2, True)

justLetterC :: Maybe Char
justLetterC = match @"OneField" oneField

newOneField :: MyType
newOneField = setConstructor @"OneField" 'd' oneField

autoPrism :: forall {k} (x :: k) v. MatchConstructor x v => Prism' v (ConstructorType x v)
autoPrism = prism' (construct @x) (match @x)

oneFieldAgain :: MyType
oneFieldAgain = autoPrism @"OneField" # 'g'
```


## Effect and Interactions

To fully solve all the problems mentioned in an optimal way you will likely also need follow up
proposals (which I will likely make if this proposal is accepted) to deal with things like
optionally not generating constructors and overloaded total pattern matching.

However even with just the above there are a variety of benefits:

* Constructing sum types and interacting with them via prisms can be made more concise if they
  are defined in a module without any internal collisions, even if there are other constructors
  in other modules that do collide.

* Meta-interaction with sum types, for example generating prisms, can be done quite concisely
  without needing template haskell.

* You can pattern match a single constructor concisely without any preamble or an awkward two
  clause case statemnet with a catch-all.

With the above future proposals included you also get:

* You can freely define and use variant constructors and enums that collide with one another
  without collisions, and type inference will generally keep verbostiy to a minimum.

The main potential interaction with existing features is potential overlap with existing
`IsLabel (->)` instances, besides that the changes should be strictly additive and scoped.


## Costs and Drawbacks

As mentioned this prevents `#foo` from being used as a field selector, with `.foo` being the preferred
alternative.

Given that this is all based on GHC.Records, the costs and drawbacks should be fairly minimal, as
everything should more or less synchronize with the changes implemented there.


## Alternatives

There are a variety of alternative approaches to overloaded record fields compared to
the linked proposal, many of those can similarly be adapted to work for variant constructors.
However since the linked proposal has been accepted, I would argue that the most reasonable
approach is to mirror it.

One alternative decision that could have been made with this proposal is to try harder to preserve
currying when working with multiple argument constructors. However this requires significant
increased complexity, such as ConstructorType returning a list of types, and without clever encodings
you still end up with tuples when updating and matching. It also makes the duality less direct,
which could harm future language features taking advantage of this duality.

This proposal could also take on the task of deciding when to generate regular constructors,
however to keep the scope of the proposal and discussion more reasonable I have deferred that to a
later proposal. Unlike with record fields it may be desired to allow for specifying it on a
per-type or even per-constructor basis, for example `#`-prefixing the constructors.

Similarly this proposal could also take on the task of handling overloaded total pattern matching,
but again to keep scope down and also to keep parity with the linked proposal, this has been deferred.
The simplest way I can see to go about this is taking advantage of `GetField` to allow combining a
variant with a record of handlers (e.g. `Either a b -> { Left :: a -> c, Right :: b -> c } -> c`).
However once we have extensible records and variants, then another option is converting into an
extensible variant and destructing from there.


## Unresolved Questions

I am happy with the proposal as is, but I could see some of the alternatives leading to unresolved
questions if others disagree.


## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

