---
author: Fumiaki Kinoshita
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Enable `-Wnoncanonical-monad-instances` by default

Currently, `-Wnoncanonical-monad-instances` is not enabled in any of the default, `-Wall` and `-Wcompat`.
I propose enabling the warning by default.

## Motivation

The prerequisite of the Phase 2 of [monad of no return](https://gitlab.haskell.org/ghc/ghc/wikis/proposal/monad-of-no-return) is to wait until "we're confident that the majority of Hackage has reacted to the warning". However, a warning that's disabled unless you specify it individually gives infinitesimally small incentive to the ecosystem, so we can't really expect them to react. In order to avoid catastrophic breakage like what we experienced when `fail` got removed, I think the warning should be enabled by default, not just in `-Wall` or `-Wcompat`.

## Proposed Change Specification

* Add `-Wnoncanonical-monad-instances` to the set of default warnings.

## Examples

On GHC 8.8 (and 8.11.0.20200108), the following code compiles without warnings.

```haskell
data P a = P

instance Functor P where
  fmap _ _ = P

instance Applicative P where
  pure _ = P
  _ <*> _ = P

instance Monad P where
  return _ = P
  _ >>= _ = P
```

With this proposal implemented, it would produce the following warning:

```
/path/to/example.hs:12:3: warning: [-Wnoncanonical-monad-instances]
    Noncanonical ‘return’ definition detected
    in the instance declaration for ‘Monad P’.
    Either remove definition for ‘return’ or define as ‘return = pure’
   |
12 |   return _ = P
   |   ^^^^^^^^^^^^
```

## Effect and Interactions

Once this proposal is implemented, people are more likely to notice non canonical definitions of `return` and fix them.

## Costs and Drawbacks

People are more likely get annoyed when they see the warnings.

## Alternatives

Enable it only in one or more of `-Wcompat` or `-Wall`. Not everyone specifies these flags so more packages are likely to break without a caution beforehand.

## Unresolved Questions

N/A

## Implementation Plan

Once approved, @fumieval can (hopefully) submit code changes.

## Endorsements


