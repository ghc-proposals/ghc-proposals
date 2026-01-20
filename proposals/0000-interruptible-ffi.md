---
author: David Feuer
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Make interruptible foreign calls easier to use

Currently released versions of GHC have quite incomplete documentation of the
meaning of `interruptible` foreign calls. More complete documentation has been
written quite recently, and it highights that using these calls optimally is
quite fussy and verbose. We can make it much less so by adding a more user-friendly
foreign import variant based around the idea of an "interruptible operation"
as defined in `Control.Exception`.

## Motivation

When an exception is received during an `interruptible`
foreign call, the RTS attempts to interrupt the call by sending
`SIGPIPE`. This occurs regardless of the current masking state.
The caller of the foreign function is then responsible for
determining whether the foreign call failed in a way that suggests
a possible exception, and, if so, arranging for said exception
to be delivered. This last part is complicated by the fact that,
in the non-threaded runtime, `interruptible yield` may be required,
rather than just `allowInterrupt`. I believe we can improve most
of this.

## Proposed Change Specification

Add a new type of foreign import, `interruptibleChecking`, to supplement the
existing `unsafe`, `safe`, and `interruptible` types, looking like

```haskell
foreign import ccall interruptibleChecking open_checker
  "open" c_interruptible_open :: CFilePath -> CInt -> CMode -> IO CInt

open_checker :: CInt -> IO (ShouldDeliverExceptions CInt)
open_checker (-1) = pure (DeliverExceptions (-1))
open_checker x = pure (DoNotDeliverExceptions x)

-- data ShouldDeliverExceptions a
--   = DeliverExceptions a
--   | DoNotDeliverExceptions a
```

An `interruptibleChecking` call must have an `IO` result type, because it
has the `IO` effect of suspending a `mask`.

When the masking state is `MaskedUninterruptible`, an `interruptibleChecking`
import will be called just like a `safe` one—no attempt will be made to
interrupt the foreign call.

When the masking state is anything else, the foreign call will be made just
like an `interruptible` one. On return from the foreign call, the
user-providing checking action will be called with the result of the foreign
call. The checking function must take a fixed foreign type and must return a
value of type `IO (ShouldDeliverExceptions a)`, for arbitrary type `a`. If it
returns `DeliverExceptions x`, then any pending exceptions will be delivered
immediately and then the value `x` will be returned. If it returns
`DoNotDeliverExceptions x`, then the value `x` will be returned immediately.

## Examples

This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

## Effect and Interactions

Your proposed change addresses the issues raised in the
motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


## Costs and Drawbacks

Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


## Alternatives

List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

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

(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

