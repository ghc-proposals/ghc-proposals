---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/373).

# Role annotations for type and data families

Type and data family applications give all their arguments nominal roles,
which prevents useful coercions. I propose to allow users to give
them role signatures to avoid this in some cases.

## Motivation

Type and data families can be used in the definitions of datatypes.
For example, we can write

```haskell
data Nat = Z | S Nat

type family Blob (height :: Nat) (a :: Type) :: Type where
  Blob 'Z a = a
  Blob ('S n) a = [Blob n a]

newtype T n a = T (Blob n a)
```

We would very much like to be able to write

```haskell
type role T nominal representational
```

Unfortunately, GHC pessimistically assumes that there might be an
instance of `Blob` forcing `a` to a `nominal` role. It therefore
infers

```haskell
type role T nominal nominal
```

In an application where we really need a representational role for `T`'s second
parameter, we're forced into disgusting code like this:

```haskell
newtype T n a = T_ (Blob n Any)
type role T nominal representational

pattern T :: Blob n a -> T n a
pattern T b <- ((\(T_ bl) -> unsafeCoerce bl) -> b)
  where
    T bl = T_ (unsafeCoerce bl)
```

## Proposed Change Specification

Allow users to supply role signatures for type families and data families.
These will constrain the instances they will be allowed to ensure safety.
A parameter assigned a particular role must be used in that role on every
right-hand side of an instance. Furthermore, a parameter with a phantom
or representational role may not be matched on the left-hand side.

The proposed change would be enabled by a `TypeFamilyRoles` language
extension, which would imply both `TypeFamilies` and `RoleAnnotations`.

## Examples

In the motivating example, we can just write

```haskell
type role Blob nominal representational
```

The type/data family instances don't match on the second parameter, and the
second parameter is used representationally in the right-hand side,
so this would be accepted.

### Prohibited examples

```haskell
type family A a b where
  A Int b = b
  A Bool b = b
type role A phantom representational
```

This family matches on a `phantom` parameter. Even though the result will
be the same whether it's passed `Int` or `Bool`, this is problematic
because of stuck types. It would not be acceptable to `coerce` from
`A Int b` to `A Char b`.

```haskell
type family B (b :: Bool) (n :: Nat) where
  B 'False n = ()
  B 'True n = SNat n
type role B representational
```

The second instance is impermissible because the parameter is used nominally on
the right-hand side, but it is declared to have a representational role.

## Effect and Interactions

I don't see any immediate 

## Costs and Drawbacks

Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.

## Alternatives

I don't know of any alternatives at this time.

## Unresolved Questions

### Matching relaxation

The rule banning all matching on `phantom` and `representational` parameters
seems somewhat unfortunate. Intuitively, one might expect something
like this to be accepted:

```haskell
newtype Yeah = Yeah Int
newtype UhHuh = UhHuh Bool

type family Fish a where
  Fish Int = Bool
  Fish Yeah = UhHuh
type role Fish representational
```

When ``x `Coercible` y``, and both `Fish x` and `Fish y` are defined, we surely
should have ``Fish x `Coercible` Fish y``. But there's a problem that seems
rather hard to get around: coercibility can vary depending on what is in scope.
So there's no reliable way to check that a type family always takes coercible
things to coercible things.

### Data Families

Data families seem to want a little more flexibility than what's described
above. Specific instances seem to want their own roles.

```haskell
data family Foo a b
type role Foo nominal representational

newtype instance Foo Int b = Intish b
data instance Foo Bool b = Boolish
```

The `Foo Bool b` instance is effectively phantom in `b`. It might be nice to
be able to express that:

```haskell
type role Foo Bool phantom
```

In that case, when GHC *knows* that it's dealing with `Foo Bool`, it will treat
the second parameter as phantom, but when it's dealing with an unknown `Foo`
application, it will treat the second parameter as representational.
The challenge here is to work out something sensible to do about overlapping
annotations, such as

```haskell
type role Bar Int nominal representational
type role Bar nominal Int phantom
```

I have no clear idea how to figure this out, so I'll just leave it to
future work.

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

