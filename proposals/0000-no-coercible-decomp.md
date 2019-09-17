---
author: David Feuer
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/276).

# No decomposition of Coercible givens

As described in [Safe Zero-cost Coercions for
Haskell](https://www.microsoft.com/en-us/research/uploads/prod/2018/05/coercible-JFP.pdf),
GHC infers coercibility of certain datatype parameters from coercibility of the
datatypes themselves. For example, given

```haskell
data Foo a b c
type role Foo nominal representational phantom
```

GHC generates an axiom

```haskell
Coercible (Foo a b c) (Foo a' b' c') => (a ~ a', Coercible b b')
```

Indeed, a similar axiom is needed to support the progress proof for GHC Core.

Unfortunately, this rule has limited utility and unpleasant consequences as
applied to Haskell per se, and I propose to remove it from the surface
language.

## Motivation

In `Data.Set`, we decree

```haskell
type role Set nominal
```

This is necessary to prevent "safe" functions from producing `Set` values that
fail to obey the `Set` invariants. If we didn't do that, then someone could
write

```haskell
import Data.Ord

insert (Down (12 :: Int)) $ coerce $ fromList [1,2 :: Int]
```

and produce a bogus set displayed as `fromList [Down 12,Down 1,Down 2]`.

Sometimes, however, people will tolerate an additional proof obligation
in order to improve efficiency. For this purpose, we offer the "unsafe"
function

```haskell
mapMonotonic :: (a -> b) -> Set a -> Set b
```

that simply assumes the passed function is strictly increasing. We would like
to offer a rewrite rule:

```haskell
{-#
"mapMonotonic/coerce" mapMonotonic coerce = coerce
 #-}
```

but this, unfortunately, will not work; GHC believes in the role
annotation for `Set`, so the type of the first `coerce` will be inferred
to be `a -> a`, and the rule is effectively `mapMonotonic/id`.

## Proposed Change Specification

I propose the following:

1. Remove the decomposition axiom from the Haskell type checker.

2. Treat types as having their *inferred* roles wherever all their constructors
are in scope. This affects both `data` and `newtype` declarations. If a type
has no constructors, then to avoid importing/exporting the inferred roles,
it must be given an explicit empty constructor list.

3. When lowering to Core, loosen all roles (transitively) to inferred ones and
reinstate the decomposition axioms. These decomposition axioms may
effectively be *weaker* than the ones currently generated, but they should be
strong enough to support progress.

## Examples

### Example 1

The following will no longer typecheck:

```haskell
f :: Coercible (Maybe a) (Maybe b) => a -> b
f = coerce
```

Fortunately, I believe code like this is unusual in the wild.

### Example 2.1

For `Data.Set`, we can add the desired `RULES` in the module defining
the datatype (or elsewhere, but orphan rules are generally undesirable).
In any module importing the `Set` constructors, we can define

```haskell
unsafeSetCoercion :: Coercible a b => Coercion (Set a) (Set b)
unsafeSetCoercion = Coercion
```

### Example 2.2.1

We can write

```haskell
data A a = A
type role A nominal

data B a = B (A a)
type role B representational
```

This is permitted because the *inferred* role signature of `A` allows it,
and `A`'s constructors are all in scope where `B` is declared.

### Example 2.2.2

If we write

```haskell
module A (A ()) where
data A a
type role A nominal
```

Then no importing module can access the inferred phantom role of `A`.

### Example 2.2.3

We can write

```haskell
module A (A) where
data A a
type role A nominal

module B where
import A (A)
data B a = B (A a)
```

because neither the export nor the import of `A` explicitly excluded constructors.

## Effect and Interactions
I don't foresee any significant interactions.

## Costs and Drawbacks

The main cost I see is that GHC will have to track and record two sets of
roles for each type: one for Haskell and one for Core.

## Alternatives

Rather than removing the decomposition axioms from the source language
altogether, we could weaken them to the ones that would be inferred,
transitively, without role signatures. So users would have access to the axiom

```haskell
Coercible (Set a) (Set b) => Coercible a b
```

I believe this alternative is likely much harder for users to understand and
harder to implement, and that it doesn't add enough power to pay its way.

For types without constructors, an alternative would be to require
an explicit `(..)` to export/import the inferred roles. This is much
more pleasantly explicit, but it interacts poorly with the warning
for such syntax applied to such types. Another option would be to use
some special syntax like `import A (A (roles))`, but that seems
pretty ugly.

## Unresolved Questions

Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?
