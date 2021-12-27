---
author: Vladislav Zavialov
date-accepted: "2021-05-10"
ticket-url: ""
implemented: ""
---

This proposal was [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/371) and [amended by this one](https://github.com/ghc-proposals/ghc-proposals/pull/460).

# Export `~` from `Data.Type.Equality`


## Motivation

The `~` type operator is magical, in that it's always in scope and does not require `TypeOperators` to use.
But `~~`, `:~:`, and `:~~:` are not magical in the same way, they are
exported from `Data.Type.Equality` and require `TypeOperators`.

This is a historical accident which we propose to correct.

## Proposed Change Specification

* Do not bring `~` into scope magically, export it from `Data.Type.Equality`.

* Re-export `~` from `Prelude`.

* Drop the requieremnt to enable `TypeFamilies` or `GADTs`
  to use the `~` type operator.

* Require `TypeOperators` to use the `~` type operator.

### Migration story

Let `n` be the GHC version that implements this proposal.

Starting with GHC `n`, include a compatibility fallback:

1. When the lookup for `~` fails because it is not in scope,
   assume it refers to `Data.Type.Equality.~`.
   When this happens and `-Wtype-equality-out-of-scope` (included in
   `-Wcompat`) is in effect, emit a warning.

2. When the use of `~` would have been rejected because `TypeOperators` are not
   enabled, accept the program regardless.
   When this happens and `-Wtype-equality-requires-operators` (enabled by
   default) is in effect, emit a warning.

Starting with GHC `n+2`, enable both warnings by default.
Either starting with GHC `n+8` or with the next major compiler version bump (GHC
10), whichever comes first, remove the compatibility fallback.

## Effect and Interactions

* One less quirk.

* The users are allowed to define their own ``~``.

* The compiler internals are simplified, in particular things described in
  ``Note [eqTyCon (~) is built-in syntax]``.

* Haddock documentation for ``~`` will get fixed (it is currently missing in
  the generated HTML).

## Costs and Drawbacks

This is a breaking change:

* Users will need to enable `TypeOperators`
* Authors of custom preludes will likely want to re-export `(~)`

## Alternatives

* Keep `~` special for historical reasons to avoid a breaking change.
* A different migration schedule (e.g. include the warning in `-Wall` first).

## Unresolved Questions

None.

## Implementation Plan

There's a prototype implementation in [MR 4313](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4313).
