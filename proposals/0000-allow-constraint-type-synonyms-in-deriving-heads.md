---
author: Grigorii Gerasev
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/597).

# Allow constraint tuples and synonyms in typeclass instances

## Motivation

Most of Haskell projects do a lot of `stock` and `anyclass` deriving
for repeating classes. Most of time there is some default list
of classes to be defined.

Making type synonym for that seems like the obvious thing to do,
because jet it does not work.

There are two constructions forbidden now, but required for that usecase:

* Constraint synonyms
* Constraint tuples

Synonyms worked at some moment,
but was disabled due to wrong behaviour in some cases.
Reasoning for that is covered in Note "Instances and constraint synonyms"
and [issue 13267](https://gitlab.haskell.org/ghc/ghc/-/issues/13267).

Main usecase only affect deriving instances head,
but another usecase is usage of tuples/synonims in context of any instance.
It seems to be less common, but natural to cover in same proposal
to allow tuples/synonims anywhere in instance declaration,
leading to more consistent semantics.

## Proposed Change Specification

Proposal requires two purely syntaxic rewrites for in instance declarations
to happen:

1. Apply all type synonyms in instance declarations.
   This should be done recrusively, until no type synonim is present anymore.
2. For any constraint tuple in derived instance head,
   split such declarations into multiple.

After that rewrites happen, existing GHC semantics is applied to result,
with same error messages or produced code.

There are two cases not covered by this rewrites:

1. Using tuples in instance context leads to nested tuples.
   But they already have the same semantics as flatten tuples,
   so this is concern only affecting AST handling.
2. Syntax and need for user-defined instances of class tuples is not clear.
   So tuples in user-defined instances head remain forbidden.

## Examples

Main usecase:

```haskell
type ToFromJSON x = (ToJSON x, FromJSON x)

-- Allowed by this proposal
derive instance ... => (ToFromJSON x)

-- Should work the same with syntax allowed before proposal
derive instance ... => ToJSON x
derive instance ... => FromJSON x
```

Instance context case is simpler:

```haskell
class ToFromJSON x => MyProtocolEncoding x where
   ...

data MyContainer x = ...

-- Allowed by this proposal
instance
   (MyProtocolEncoding x, ToFromJSON (MyContainer x)) =>
   MyContainer x where
   ...

-- Should work the same with syntax allowed before proposal
instance
   (MyProtocolEncoding x, ToJSON (MyContainer x), FromJSON (MyContainer x)) =>
   MyContainer x where
   ...
```

Last example may be not the most natural,
because `MyContainer` would probably have `ToFromJSON` conditional instance
anyway. But my goal was to show instance context changes separately,
while more natural examples involve instance head as well.

Non-standalone instances do not have explicit params,
so rewriting implementation may be little different.
But they have just the same samantics as standalone instances with holes,
covered by examples before, so could be handled the same.

## Effect and Interactions

There is already extension named `TypeSynonymInstances`,
but it does not cover constraint synonym case.
Probably, constraint synonyms should be guarded by this extension as well
and documentation updated on that.

It may be, that some linters depend on current syntaxic restrictions,
but I am not aware on any specific cases.

## Costs and Drawbacks

Main concern is that rewriting implementations may make errors less clear.
I propose to mitigate if this happens by adding context to errors,
same as it done for instances resolving now.

## Backward Compatibility

No breakage. Part of this even was working before.

## Alternatives

For deriving instances there are two alternatives:

1. Use CPP preprocessor variable, which breaks type-safety,
   may lead to surprising error messages and is less composable
2. Use Template Haskell, which have its own problems and is less simple for beginners

For user-defined instances one may define type class instead of synonym:

```haskellb
class (ToJSON x, FromJSON x) => ToFromJSON x

instance (ToJSON x, FromJSON x) => ToFromJSON x
```

While this trick works, Haskell begginers may be not aware of it,
and it makes less clear API.

## Unresolved Questions

* Are there any other "wrong" cases not covered in existing issue?
* Which implementation approach to use?

I think all those questions are not blocking,
because they never lead to bad semantics,
only potential problems are slightly less-clear type errors.
Also implementation paths are similar any may be switched later.

## Implementation Plan

### Main approaches

The main question is how to check that constraint synonym rewriting
will lead to correct code.

There are two principial solutions to that:

1. Check that rewriting will be correct before applying it.
   In this context this probably amounts to checking for each referenced
   type synonym if it belongs to some list of known bad cases.
2. Optimistically apply transformation and check for errors in later stages

Also we can just do both.

### Potential problems

Main potential problems with first approach
would be being unsafe or being too restrictive.

These problems, especially being too restictive,
are amplified by problem of cascading:
handling synonyms application producing jet another synonyms.
This would affect both calculation of referenced type synonyms
and possible generative effects.
In particular this leads to question if to check referenced type synonyms
once or after each reduction pass.
Another concern would be if reduction strategy affects user-facing errors.

Solution 2, on other hand cannot be unsafe or too restrictive by design.
Also it may lead to GHC code simplification,
because now there are multiple codepaths
need to handle type synonims differently.

The potential problem for solution 2 would be error messages clarity.
Since checks are applied to rewritten code,
they might miss original context or show non-original AST.
I believe such problems could be solved by means of
tracking relation to original AST.

### Proposed solution

I believe second approach is more simple to reason and implement.
I (Gregory Gerasev @uhbif19) could try to provide such implementation.

## Endorsements

