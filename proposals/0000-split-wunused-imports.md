---
author: Georgi Lyubenov, Torsten Schmits
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/586).

# Split `-Wunused-imports`

The current iteration of `-Wunused-imports` fulfills two arguably different purposes - warn on "unused imports", and warn on "**duplicate** imports", the latter of which has proven to be a heavy burden on the ecosystem.

We're suggesting that we split off the "duplicate imports" part from `-Wunused-imports` into another warning, called `-Wduplicate-imports`, and crucially, **do not** include `-Wduplicate-imports` into `-Wall`.

## Motivation

`-Wunused-imports` has long been a thorn in the side of people who want both:
* to not have unused imports (in the sense described in the [proposed change specification section](#proposed-change-specification))
* to compile warning-free across different versions of their dependencies

The most often case where this comes up is with `Prelude` and other widely used modules in `base`.
In particular, when `Prelude` gets a new reexport, which previously only lived in another module from `base`,
users of `-Wall` suddenly get new warnings, causing not only noise, but crucially breakage for users of `-Wall -Werror`.

This effectively reduces the usability of the `-Wunused-imports` warning,
because it forces library authors (who in general are doing this work for free) who want to enforce code quality (removing actual unused imports and potentially even unused dependencies)
to also have to do extra work, with questionable benefits.

This not only a theoretic concern:

* Additional work was required in the implementation of the [export `liftA2` proposal](https://github.com/haskell/core-libraries-committee/issues/50):
    * effort in `ghc` itself to maintain warning-freeness
    * additional non-trivial sections in [the migration guide](https://github.com/haskell/core-libraries-committee/blob/main/guides/export-lifta2-prelude.md)
    * two additional non-trivial(requiring `CPP` or turning off the warning entirely) merge requests, to `Cabal-syntax` and `containers` respectively

* The implementation of the AMP proposal required very similar additional work to the `liftA2` case above, see
    * the concerns raised in https://www.yesodweb.com/blog/2016/05/are-unused-import-warnings-harmful
    * the migration guide for ghc 7.10 - https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/7.10#ghc-says-the-import-of-is-redundant

* This is listed as a "bad part" of Haskell - https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/
    * in particular, the article mentions the re-export of `(<>)` from `Prelude` as requiring the exact same `CPP` workarounds as already mentioned

* In the [sized integer types proposal](https://github.com/haskell/core-libraries-committee/issues/156), a critical counterpoint is introducing new emissions of `-Wunused-imports`

* The [export `foldl'` proposal](https://github.com/haskell/core-libraries-committee/issues/167) seems like it would encounter the same implementation issues as the export `liftA2` proposal, and similar concerns to the sized integer types proposal

## Proposed Change Specification

An "unused import" looks like this:
```haskell
import X (f)
import Z
...neither f, nor anything from Z is used, hence they are both unused imports...
```
A "duplicate import" looks like this:
```haskell
import X -- X exports f
import Y (f)
...f is used here...
```
Here, one of the imports is currently marked as unused. Note that `X` and `Y` could be the same module just as well.

The proposal is for `-Wunused-imports` to be changed so that only the "unused import" case emits a warning.

We also propose to introduce another warning, `-Wduplicate-imports`, which warns on only the "duplicate import" case.

Finally, since the majority of uses of `-Wunused-imports` seem to come from `-Wall` uses, we propose to **not** include the new `-Wduplicate-imports` in `-Wall`, so that
library authors can benefit without having to do yet more work.

The wideness of applicability of `-Wduplicate-imports` can also be debated, hence it is unclear if it should be in `-Wall` in general. For example, it makes some sense for app writers, but not so much for library authors.

Expressing the proposed change via the current ghc implementation([source](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/unused-imports)):

Current:

0. `-Wunused-imports` reports
    * warnUnusedModules: import M, where nothing is used from M
    * warnUnusedImports: import M(f), where f is unused, and M doesn't fall under warnUnusedModules
    * warnDuplicateImports: import M + import M(f), even when f is used complain about duplicate import of f
1. `-Wall` includes `-Wunused-imports`

Proposed:

0. `-Wunused-imports` reports
    * warnUnusedModules: import M, where nothing is used from M
    * warnUnusedImports: import M(f), where f is unused, and M doesn't fall under warnUnusedModules
1. `-Wduplicate-imports`
    * warnDuplicateImports: import M + import M(f), even when f is used complain about duplicate import of f
2. `-Wall` includes `-Wunused-imports`, but **not** `-Wduplicate-imports`

## Examples

###

```haskell
import Foo
import Foo (x)

bla = x
```

Current:
* with `-Wunused-imports` - warn that the `Foo` import is unused

Proposed:
* with `-Wunused-imports` - nothing
* with `-Wduplicate-imports` - warn that the `Foo` import is duplicate

###

```haskell
import Foo (x)
import Bar (x)

bla = x
```

Current:
* with `-Wunused-imports` - warn that the `Bar` import is unused

Proposed:
* with `-Wunused-imports` - nothing
* with `-Wduplicate-imports` - warn that the `Bar` import is duplicate

###

```haskell
import Foo
import Bar

bla = x
```

Current:
* with `-Wunused-imports` - warn that the `Bar` import is unused

Proposed:
* with `-Wunused-imports` - nothing
* with `-Wduplicate-imports` - warn that the `Bar` import is duplicate

###

```haskell
import Foo

bla = ()
```

Current:
* with `-Wunused-imports` - warn that the `Foo` import is unused

Proposed:
* with `-Wunused-imports` - warn that the `Foo` import is unused
* with `-Wduplicate-imports` - nothing

###

```haskell
import Foo (x)

bla = ()
```

Current:
* with `-Wunused-imports` - warn that the `Foo` import is unused

Proposed:
* with `-Wunused-imports` - warn that the `Foo` import is unused
* with `-Wduplicate-imports` - nothing

## Effect and Interactions

Unsure what to fill in here, it seems that the [Proposed Change Specification](#proposed-change-specification) covers the effects of this change.

## Costs and Drawbacks

The main cost is changing the behaviour of a warning without notice, even if we explicitly warn users that it has changed.

Is this acceptable? From initial feedback given in the proposal discussion, it seems that it is.

However, for an extension of this proposal which includes some mitigation for this issue, see [the unresolved question section on Meta warnings](#meta-warning).

## Alternatives

### Relaxed redundant imports

We could also instead implement the spec that's suggested in the ["relaxed redundant imports" proposal](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/relaxed-unused-imports), however
We feel that that's an unnecessary complication for several reasons:

1. It will take more time to discuss and implement.
2. More importantly, it will be more confusing to understand when a warning triggers for end users.
3. The spec is simpler, hence easier to maintain.
4. It is not necessary, as splitting up the existing `-Wunused-imports` and not including `-Wduplicate-imports` in `-Wall` achieves the same goal.

They also seem to be mostly orthogonal to me -
if someone wants to have duplicate import warnings as per the "relaxed redundant imports" spec, then we could have another proposal after this one,
potentially amending the new `-Wduplicate-imports` warning instead.

## Unresolved Questions

### Meta warning

One big drawback of the proposed changed is that, as is, the default behaviour will change silently - the user was not asked and has not agreed for this to change.

Therefore, we propose that we introduce a "meta warning", let's say `-Wmeta-unused-imports` (names here are placeholders for now), with the following properties:

0. Whenever a user has `-Wunused-imports` on, and they don't have `-Wno-meta-unused-imports` on, emit a warning that highlights the changed behaviour of `-Wunused-imports`.
1. It is on by default.
2. It **is not made into an error by `-Werror`**. Optionally there could be `-Wmeta-error` or something along those lines for users who want to get errors for potential future meta warnings.

This `-Wmeta-unused-improts` warning is only present for a few release cycles, and is removed afterwards, or alternatively, turned off by default.

There are two issues with this approach:
* On older ghcs, meta warnings are not something that exists, hence if you silence the meta warning with `-Wno-meta-unused-imports`, older ghcs will start warning about an unrecognised warning flag
* In general it seems like quite a sledgehammer to apply here. Meta warnings are a new concept for ghc  in two ways - in being a new type of warning, but also a warning that doesn't error under `-Werror`.
    Initial feedback has indicated that this cost may not be worth it.

### Niche `-Weverything` breakage

Almost directly [quoting Adam Gundry](https://github.com/ghc-proposals/ghc-proposals/pull/586#discussion_r1193415851) here:

An obscure backwards compatibility point: with this proposal, compiling a module with duplicate imports will fail under `-Werror -Weverything -Wno-unused-imports`, whereas previously it would have succeeded (since `-Wno-unused-imports` previously suppressed both).

We could avoid this by making `-Wunused-imports` into a group that includes both `-Wreally-unused-imports` (what this proposal currently calls `-Wunused-imports`, included in `-Wall`) and `-Wduplicate-imports` (in `-Weverything`). This would also mean that users who explicitly ask for `-Wunused-imports` continue to get both.

Is this worth it? I'm not sure. Adding a group feels a bit fiddly for a comparatively rare edge case.

A quick GitHub search for `-Werror` `-Weverything` and `-Wno-unused-imports` in the same filed turned up ~70 results, with almost all of them being in editor plugins
or the ghc user guide.

This is not conclusive or exhaustive, and it relies on the search correctly finding things, but it might be a good indication that this is indeed a niche case.

## Implementation Plan

One of the proposal authors will implement this.

## Endorsements

https://gitlab.haskell.org/ghc/ghc/-/issues/21879
