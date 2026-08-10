---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/446).

# Multiple standalone deriving clauses

When deriving multiple instances as part of a type declaration,
we can group multiple instances by deriving strategy:

```haskell
import Generic.Data (Generically (..))

newtype Foo a = Foo (Maybe a)
  deriving (Semigroup, Monoid) via Generically (Foo a)
  deriving newtype (Functor, Foldable)
  deriving stock (Generic, Traversable, Show)
```

I propose to allow the same for standalone deriving instances:

```haskell
import Generic.Data (Generically (..))

newtype Foo a = Foo (Maybe a)

deriving via Generically (Foo a)
  instance Semigroup (Foo a)
  instance Monoid (Foo a)
```

## Motivation

Being able to write a deriving strategy once and use it multiple times
can save a considerable amount of typing, especially when types get long.
For example, one might wish to do something like

```haskell
deriving via (Customizer ('FormatOptions ('Indentation 3) '["tomato", "potato"]) (Foo a))
  instance Parseable a => Parseable (Foo a)
  instance PrettyPrintable a => PrettyPrintable (Foo a)
```

Gathering multiple instances under one strategy also marks out, visually, that
a certain group of instances all behave as that strategy indicates.

Standalone derived instances, of course, are required whenever GHC needs help
determining the correct instance context, or whenever the instance head must
be restricted in some fashion. They are also required in many cases when using
Template Haskell. For example, if `deriveBlah ''x` produces a Template Haskell
splice deriving an instance of `Blah` for `x`, one might need to write

```haskell
data Potato = Russet | YukonGold
deriveBlah ''Potato

deriving via Blahishly Potato
  instance Diggable Potato
```

because an attached `deriving` clause would occur before the `Blah` derivation,
and therefore not see it.

Thus it would be best to allow standalone deriving and multiple derivations
per strategy to be combined.

## Proposed Change Specification

Change the syntax of standalone deriving so it takes a block of instances to
derive:

```haskell
deriving newtype
  { instance Show a => Show (Foo a)
  ; instance Read a => Read (Foo a) }
```

As usual, the layout rule can be used to drop braces and semicolons.

Such an instance block will mean *exactly* the same thing as the result of
syntactically splitting it up, copying the deriving strategy.

```haskell
deriving newtype instance Show a => Show (Foo a)
deriving newtype instance Read a => Read (Foo a)
```

Note that for `DerivingVia` this may mean that the target type will be
instantiated at different kinds for different instances in the block.

```haskell
newtype Target f (a :: k) = Target (f a)
data Roger (a :: k) = Roger
type C :: forall {k}. (k -> Type) -> Constraint
type D :: (Type -> Type) -> Constraint
type E :: (Bool -> Type) -> Constraint

deriving via Target Roger
  instance C Roger  -- via Target @k Roger
  instance D Roger  -- via Target @Type Roger
  instance E Roger  -- via Target @Bool Roger
```

## Effect and Interactions

The amount of boilerplate needed to use `DerivingVia` (especially) will be
reduced.

## Costs and Drawbacks

I don't foresee any major drawbacks, and the implementation cost seems
likely minimal.

## Alternatives

When a `DerivingVia` target is complicated, it is possible to write a
type synonym for it.

```haskell
type FooTarget a = Customizer ('FormatOptions ('Indentation 3) '["tomato", "potato"]) (Foo a)
deriving via FooTarget a
  instance Parseable a => Parseable (Foo a)
deriving via FooTarget a
  instance PrettyPrintable a => PrettyPrintable (Foo a)
```

Doing this introduces clutter in the namespace, and forces the user to remember
what arguments the type synonym takes in what order.

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

