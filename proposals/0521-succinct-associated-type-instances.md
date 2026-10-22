---
author: Las Safin
date-accepted: ""
ticket-url: "https://github.com/ghc-proposals/ghc-proposals/pull/521"
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/521>).

# Succint associated type family instance declarations

When declaring associated type family instances in Haskell,
you need to repeat the names of the arguments, unless
you don't use any of the type variables from the instance head.

In that case, you can replace the LHS with a `_`, which is
much more succinct and heavily reduces noise.

This proposal generalises this feature, allowing you to *also* refer
to the type variables bound in the instance head.
This allows you to make use of this succinct syntax no matter
the type instance you want to write:

```haskell
class C (a :: Type) where
  type F a :: Type

-- before
instance C [a] where
  type F [a] = a

-- also before
instance C (Maybe a) where
  type F _ = () -- couldn't refer to `a` before this proposal

-- now
instance C [a] where
  type F _ = a -- can refer to `a` now
```
While seemingly a small change, this can heavily reduce the amount of noise in code
with complex instance declarations.

In addition, this enables the same feature for *default type family instances*,
as reported in https://gitlab.haskell.org/ghc/ghc/-/issues/21702.

## Motivation

There are two main motivations.
If you have a very long instance, for example:
```haskell
instance C [[[[(a, b, c, d, f)]]]] where
  type F [[[[(a, b, c, d, f)]]]] = c
```
In this case, being able to reformulate this as the following is a big improvement:
```haskell
instance C [[[[(a, b, c, d, f)]]]] where type F _ = c
```
Though GHC doesn't consider `c` bound, from the perspective of the programmer,
it is very obvious what is meant.

This also expands to default instances:
```haskell
class C (a :: Type) (b :: Type) | a -> b where
  type F a :: Type
  -- Though this looks like it's illegal, it is legal
  -- at the declaration site.
  -- Without the fundep, some bogus instances would be possible,
  -- but this default instance would just err there and not here.
  type F a = b

-- Succinct instance declaration using a fundep,
-- but you still get a type family for free.
data A
data B
instance C A B
-- implies: F A ~ B
```

## Proposed Change Specification

On the RHS of a (possibly default) associated type instance,
*all* type variables from the instance/class head are in scope,
regardless of what is bound on the LHS.

Such an instance is desugared *at the instance site* (in the case of
a default instance) by considering all type variables as synonyms
of the variables bound on the LHS of the type instance.
If no corresponding variables are bound on the LHS, the instance is illegal.

There would be no language extension for this, since this feature
is very minimal and also fully backward-compatible.

This has previously occurred, in e.g. [9.2.1](https://downloads.haskell.org/ghc/9.2.3/docs/html/users_guide/9.2.1-notes.html),
where the syntax relating to out-of-scope type variables was slightly changed.

In addition, there will be two new warnings:
- `-Wdifferently-named-default-type-instance-variables` that warns when in default instances you name the type variables differently.
- `-Wnamed-associated-type-instance-variables` that warns when you don't use `_` in associated type instances.

The first one will be part of `-Wcompat` (and part of `-Wall` in the future).
The second one will not be part of `-Wcompat`, nor `-Wall`, since it should be opt-in, not opt-out.

## Examples

```haskell
class C (a :: Type) where
  type CF a :: Type

instance C () where
  type CF _ = () -- already legal!

instance C [a] where
  type CF _ = a -- wasn't legal before
  
instance C (Maybe a) where
  type CF (Maybe a) = a -- still legal

{-
instance C (Maybe a) where
  type CF (Maybe b) = b -- still illegal
-}

class D (a :: Type) (b :: Type) where
  type DF a :: Type
  type DF a = b -- legal now! may err at instance site

{-
-- illegal, `type DF a = b` makes no sense
instance D a b
-}

-- legal now!
instance D (Maybe a) [a]

class E (a :: Type) (b :: Type) where
  type EF a b :: Type
  type EF p q = q -- still legal, but arguably should never have been
  
class F (a :: Type) where
  type FF a :: Type
  type FF p = a -- also legal, `a` considered synonym for `p`

class G (a :: Type) (b :: Type) where
  type GF a :: Type

{-
instance G a b where
  type GF a = b -- still illegal
-}
```

## Effect and Interactions

With this you can write the examples in the motivation.
This should interact minimally with other language extensions

## Costs and Drawbacks

Since we don't break backward-compatibility, there are no worries wrt.
that, but with this feature you can write some confusing default instances:
```haskell
class C (a :: Type) (b :: Type) where
  type F a b :: Type
  type F p q = a
```
You could restrict the new type variables to only be in scope if all
type instance arguments are named `_`, but this leads to other confusing
behaviour for non-default instances:
```haskell
class C (a :: Type) (b :: Type) where
  type F a b :: Type

instance C a b where
  type F _ b = a

-- vs

instance C a b where
  type F _ _ = a
```
It would be confusing for people why `a` is out-of-scope in the first
one but not in the second one, even if the second one is "nicer".

Because of this, we introduce the two new warnings that should handle both cases.

Learnability-wise, this should be a big improvement for beginners to Haskell, as it's not
immediately obvious to beginners why the variables aren't in scope.


## Alternatives

Rather than having the instance head type variables be always in scope,
we could have a special form where you only write `type F =` without
any (un)named type arguments at all.
This, however, is potentially confusing since it obscures the truth behind
associated type families, that they're not really part of the type class dictionary,
as they would be in a dependently typed language.

It would also make the associated type family declaration confusing,
since you'd still need to specify the parameters there.

It would also not be clear from the instance whether it's a zero-arity type family or
n-arity for some postive n.

## Unresolved Questions

Should there be a warning that warns against using this feature?
Should there be a language extension for this?

## Implementation Plan

I will implement this within 2-3 months from now (2022-06-22), however, I am unfamiliar with GHC
and unsure of which parts of the source code I would have to change.
A link to the PR for the change related to this in 9.2.1 would be very helpful.
(I could not find it on GitLab).
