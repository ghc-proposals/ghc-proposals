---
author: Ollie Charles
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).

# `(No)RecursiveLet`

This proposal changes GHC to distinguish between (a subset of) recursive and non-recursive `let`-bindings with the new `RecursiveLet` extension. When enabled, `let` bindings can refer to themselves by name (this is the current behavior), but when this extension is turned off (with `NoRecursiveLet`), recursive let bindings are only allowed if the author uses `let rec`.

## Motivation

Currently, whenever we write `let` bindings in Haskell we gain the ability to write recursive definitions - a definition can refer to itself. Such definitions are obviously useful, but this power is not always needed. The cost of accidentally using this "power" is significant - a lapse of concentration while writing Haskell can cause a program to be accepted that at runtime simply spins and fails to terminate.

These concerns are not theoretical. This proposal was ultimately motivated by the author making a seemingly innocent change. A small `let` binding began as:

```haskell
let panelisation = flipPanelisation unreflectedPanelisation
```

Later, requirements changed such that we should only call `flipPanelisation` if the `side` variable takes the value `Bot`. Noting this, we go ahead and add a guard and go off on our merry way:

```haskell
let panelisation
      | side == Bot = flipPanelisation unreflectedPanelisation
      | otherwise    = panelisation
```

Can you see the problem? The author certainly couldn't! This program compiles fine, and was happily deployed. This innocent change resulted in a partial outage later, as a batch-processing daemon had got stuck and a server was pinned at 100% CPU usage.

The problem is that when `side /= Bot`, we actually have:

```haskell
let panelisation = panelisation
```

This was not intended at all! The intended code was:

```haskell
let panelisation = unreflectedPanelisation
```

But that's not what was written. The author of the change didn't notice, the compiler didn't notice, and a colleague didn't notice when they did a code review.

With this proposal, if the author had disabled recursive let bindings with `-XNoRecursiveLet`, the definition would have been rejected. The options would be to change `let` to `let rec`, or to pause, reflect, and hopefully write the intended code.

## Proposed Change Specification

* A new language extension is added: `RecursiveLet`. This can be enabled or disabled via the normal mechanisims for extensions. By default the extension is enabled.

* When `RecursiveLet` is _enabled_ (the default, or explicitly with `{-# language RecursiveLet #-}`), there are no changes.

* When `RecursiveLet` is _disabled_ (`{-# language NoRecursiveLet #-}`), compilation of `let` expressions changes. With `NoRecursiveLet`, group of `let`-bound bindings cannot refer to themselves, either directly or mutually. For example, with `NoRecursiveLet`, the following will now fail to compile:

  ```
  let iterate f x = x : iterate f (f x) in ...
  ```

* If the user does want to write such a definition, they need use `let rec` when they create their bindings, rather than just `let`:

  ```
  let rec iterate f x = x : iterate f (f x) in ...
  ```

  By adding `rec` after `let`, all bindings in this `let` expression (or statement, if `let rec` is used within `do`) can refer to each other, enabling self-recursive definitions and mutually recursive definitions.

## Effect and Interactions

One major interaction with this proposal is that it means that `rec` cannot be used as an identifier name. Currently, the following is accepted by GHC:

```
let rec x = x in rec True
```

Here `rec` is just the identity function, albeit with a terribly misleading name. With `NoRecursiveLet`, this program will fail to compile, as the body of the `let` expression doesn't have a variable called `rec` in scope - `rec` is now a keyword, so this `let rec` expression actually binds `x = x` (a recursive, but permitted, definition).

## Costs and Drawbacks

This change appears to be fairly low cost (though this is unverified):

* The change to GHC should be localised to the renamer

* As the extension is turned on by default, all existing code continues to compile.

In terms of drawbacks:

* With `NoRecursiveLet`, a `let` binding can no longer bind the name `rec`, as this becomes a keyword.

* This extension is intentionally simple, and there are many classes of recursion it does _not_ catch. It is intended to catch accidentally self-recursive definitions, but does not rule out non-termination by other means (e.g., calling `fix`).

## Alternatives

### Add a new `letrec` keyword

This proposal currently tries to avoid adding new syntax, but an alternative could be to use `letrec`. This would allow `rec` to be bound as an identifier, as `rec` would no longer parse as a keyword. The downside is that the lexical structure of Haskell increases.

### Use Warnings Instead of a Language Extension

Another suggestion when this propsal was being drafted was to do this with warnings. A "warn about recursive let bindings" warning (name TBC) would be added, and would fire whenever a `let` bound name referred to itself. There remains an open question about what to do with legitimate uses of recursive let bindings, though. That is, if we have `let ones = 1 : ones` and we want this, how do we silence the warning?

## Unresolved Questions

* Should this extend to `where` clauses, too?

## Implementation Plan

The author of this proposal (Ollie Charles) offers to do the work to get this proposal into GHC.

## Endorsements

While not a particularly accurate number, a subset of the community resonated to the frustration of introducing accidentally recursive `let` bindings in this tweet:

https://twitter.com/acid2/status/1346851203599249413
