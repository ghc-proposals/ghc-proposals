---
author: Grigorii Gerasev
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/597).

# Proposal title

Allow constraints type synonyms in deriving heads again.

## Motivation

Most of Haskell projects do a lot of `stock` and `anyclass` deriving
for repeating classes. Most of time there is some default list
of classes to be defived.

Making type synonym for that seems like the obvious thing to do,
jet it does not work.

It worked at some moment for any instances,
but was disabled due to wrong behaviour in some cases:
https://gitlab.haskell.org/ghc/ghc/-/issues/13267

Seems like only phantom param case is relevant for deriving instances case.

## Proposed Change Specification

Accept type synonyms in deriving heads, by rewriting it to regular constraints.

## Examples

```
type Synonym x = (Show x, Eq x)

-- Should works exactly the same
derive instance ... => (Synonym x)
derive instance ... => (Show x, Eq x)
```

Non-standalone instances do not have explicit params,
so rewriting implementation may be little different.
But they have same seamantics as standalone instances with holes,
so it should be the same.

## Effect and Interactions

I do not know any.
May be some linters depend on instance head being class.

## Costs and Drawbacks

May be some rewriting implementations may make errors less clear,
due to AST changed inside. Seems not likely and treatible.

## Backward Compatibility

No breakage. This was working before.

## Alternatives

1. Use CPP preprocessor variable, which breaks type-safety
   and may which lead to surprising error messages
2. Use Template Haskell, which have its own problems and is less simple for beginners

## Unresolved Questions

Are there any other "wrong" cases other than phantom params?

## Implementation Plan

GHC does some processing to convert AST into EarlyDeriveSpec.
As part of that before or in `makeDerivSpecs` function
one could rewrite type synonyms. So all types and later logic
will remain the same.

The only question is how to check if rewriting will be correct.
Another question is handling of recursive type synonyms.

1. If the phantom params is the only case, one could forbid only them
2. If there might be complex cases GHC may just rewrite synonims and
   check if result is correct for later stages usual way

I (Gregory Gerasev @uhbif19) could try to implement that.

## Endorsements

