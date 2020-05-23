---
author: buggymcbugfix
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/335).

# RFC: Expand arrays APIs with efficient primops

## Motivation

Operations that allocate new arrays (`Array#`, `MutableArray#`, `SmallArray#`,
`SmallMutableArray#`) must ensure that the array never contains garbage values
during safe points (when the GC runs); otherwise the GC would interpret these as
pointers and try to follow them and Undefined Behaviour would ensue. Thus, for
safety, many library-defined array operations have to conservatively initialise
array slots at the cost of performance.

We propose to expand the arrays API in
[GHC.Exts](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Exts.html)
by picking the low hanging fruit of missing primitives which would improve the
performance of key libraries, such as
[unordered-containers](https://github.com/tibbe/unordered-containers/tree/d0b78afc028ef307a3cdfe1dd95feaf4ec47f195).

## Proposed Change Specification

Below is the current list of proposed new primops, in order of priority.

- `insert# :: array# a -> Int# -> a -> array# a`
  - Defined [here](https://github.com/tibbe/unordered-containers/blob/d0b78afc028ef307a3cdfe1dd95feaf4ec47f195/Data/HashMap/Array.hs#L357-L374) in
  unordered-containers.
- `pair# :: a -> a -> array# a`
  - Defined [here](https://github.com/tibbe/unordered-containers/blob/d0b78afc028ef307a3cdfe1dd95feaf4ec47f195/Data/HashMap/Array.hs#L266-L271) in
  unordered-containers.
- `singleton# :: a -> array# a`
  - Defined [here](https://github.com/tibbe/unordered-containers/blob/d0b78afc028ef307a3cdfe1dd95feaf4ec47f195/Data/HashMap/Array.hs#L258-L264) in
  unordered-containers.
- `delete# :: array# a -> Int# -> array# a`
  - Defined [here](https://github.com/tibbe/unordered-containers/blob/d0b78afc028ef307a3cdfe1dd95feaf4ec47f195/Data/HashMap/Array.hs#L442-L458) in
  unordered-containers.
- `concat# :: array# a -> array# a -> array# a`
  - Is this useful? Andrew Martin mentioned that the need to concatenate two
    arrays may indicate a flaw in how the arrays are being used, as the two
    arrays should have been created as one in the first place. Can we find a
    counterexample/compelling use case?

## Examples

Please see the links to unordered-containers in the previous section.

## Effect and Interactions

Purely additive and backward-compatible.

## Costs and Drawbacks

There will be more primops to maintain. However, there is only a very small overhead to this.

## Alternatives

Implementing these primops in a separate package, like [array-primops](https://hackage.haskell.org/package/array-primops-0.2.0.1).

Drawbacks of this approach ([courtesy of @treeowl)](https://github.com/treeowl/ghc-proposals/blob/array-creation/proposals/0000-array-creation.rst#11motivation):

1. Its cmm implementations won't be inlined, so they have a performance penalty for very short arrays (like the ones used in Data.HashMap, for example).
2. Since it's written in cmm, it needs to track GHC internals particularly carefully, and it's not available to non-cmm-based backends.

Both of these aspects make it an unattractive dependency.

## Unresolved Questions

Which primops are the most interesting?

## Implementation Plan

@buggymcbugfix will implement this as part of [his Summer of Haskell
project](https://summerofcode.withgoogle.com/projects/#4603859214794752), with
help from @AndreasPK, @andrewthad, @chessai.
