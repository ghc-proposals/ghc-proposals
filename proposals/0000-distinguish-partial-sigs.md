---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/491).

# Distinguish different partial signature purposes

A wildcard in a type signature can serve either of two purposes:

1. Let a user ask GHC what type or constraint should go in that hole.
2. Let a user ask GHC to fill in that hole with the appropriate type or
   constraint.

I will call these "don't know" holes and "don't care" holes, respectively.

GHC additionally supports holes in two other contexts:

1. "Don't know" holes are permitted in expressions.
2. "Don't care" holes are permitted in visible type application.

"Don't know" holes in type signatures are supported without language
extensions. "Don't care" holes in type signatures are supported by combining
the `PartialTypeSignatures` extension with the `-Wno-partial-type-signatures`
compiler flag. There is not currently any way to mix "don't know" and "don't
care" holes in the same module. I propose to distinguish these by number of
underscores, and to support "don't know" holes in type applications in the same
manner.

## Motivation

Some styles of programming lead to type signatures that are quite long and dull
to write out in full. In particular, constraint lists can get quite long. When
there is a lot of constraint-level programming, users may not even care exactly
what constraints are being imposed, as long as everything works. The
`PartialTypeSignatures` extension supports this style of "don't care"
programming. To avoid a potentially large number of undesirable warnings,
people using this style of programming must generally enable
`-Wno-partial-type-signatures`. Unfortunately, this leads to a problem: if they
actually *want* an explicit signature or constraint and don't know what that
should be, they can't easily ask GHC. The only workaround is to re-enable
`-Wpartial-type-signatures` and dig through all the warnings they don't care
about to find the one that interests them.

Users of `TypeApplications` may want to specify an explicit type argument even
when one can be inferred; at present, GHC does not help them do so.

## Proposed Change Specification

I propose that the following extensions and warnings be modified as described:

### `PartialTypeSignatures`

1. `__` in a type signature is considered a "don't know" hole. This is a
   breaking change, as `__` is currently a type variable.

2. When `NamedWildCards` is also enabled, any type wildcard beginning with two
   underscores is considered a "don't know" hole.

3. Any other wildcard is considered a "don't care" hole.

### `TypeApplications`

1. `__` in an applied type is considered a "don't know" hole. This is also a
   breaking change for the same reason as for `PartialTypeSignatures` above`.

2. When `NamedWildCards` is also enabled, any type wildcard beginning with two
   underscores is considered a "don't know" hole.

3. Any other wildcard is considered a "don't care" hole (this is not a change).

### Warnings

1. `-Wpartial-type-signatures` will warn about "don't know" holes in either
   type signatures or type applications.

2. A new warning, `-Wall-partial-type-signatures`, which is disabled by
   default, will warn about "don't care" holes as well, but only in type
   signatures. This would support an existing project working to remove
   partial type signatures.

## Examples

Warning for the result type, but not for the constraint:

```haskell
foo :: _ => a -> __
foo = show
```

Warning for the constraint, but not for the result type:

```haskell
bar :: __ => a -> _
bar = show
```

No warnings:

```haskell
foobar :: _ => a -> _
foobar = show
```

Warning for the applied type:

```haskell
print @__ (3 :: Int)
```

No warning:

```haskell
print @_ (3 :: Int)
```

The following examples assume `NamedWildCards`.

Warning for the constraint:

```haskell
baz :: __c => a -> a -> Bool
baz = (==)
```

No warnings:

```haskell
quux :: _c => a -> a -> Bool
quux = (==)
```

## Effect and Interactions

I don't foresee any obvious interactions with existing features. When we get
visible type parameters, we'll want to use the same convention for those:

```haskell
-- 'id' with a visible type argument
expId :: forall a -> a -> a
expId _ a = a

print $ expId _ 'a'  -- no warning
print $ expId __ 'a' -- warning indicating that __ is Char
```

If GHC eventually gets *term* inference as part of `DependentHaskell`, then we
would probably want to do the same.

## Costs and Drawbacks

I imagine the development cost will be low; I know of no obvious drawbacks.

## Alternatives

* An obvious question is whether the single underscore should be a "don't know"
  hole or a "don't care" hole. I have chosen to make it a "don't care" hole to
  minimize the effect on existing code that uses `PartialTypeSignatures` or
  that uses wildcards in type applications, but of course it could be
  otherwise.

* Double underscores in type signatures could be considered holes regardless of
  `PartialTypeSignatures`.  This would lead to a more consistent user
  experience, but would violate the Report requirements without a big benefit.

* On the flip side, we could only consider `__` a "don't know" hole when
  `NamedWildCards` is enabled. That would avoid any breaking changes, but it
  seems like overkill.

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

