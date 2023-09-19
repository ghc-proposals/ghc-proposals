---
author: Grigorii Gerasev
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/597).

# Allow constraint tuples and synonyms in typeclass instance head

## Motivation

Most of Haskell projects do a lot of `stock` and `anyclass` deriving
for same list of common classes. This involves a lot of code duplication.

Such usecase would be natually covered by defining type synonym
for tuple of such reused classes, but GHC does not support deriving
not for constaint synonyms, nor for tuples.

This may be unexpected for user, because both of this constructs
are supported in the instance context, but not in the head.

Another usecase is typeclasses which are semantically same as composition of
multiple classes. They too could be replaced by constraint synonym instances.

Case of constaraint synonyms in instance head worked at some moment,
but was disabled due to wrong user-facing error messages in some cases.
Reasoning for that is covered in Note "Instances and constraint synonyms"
and [issue 13267](https://gitlab.haskell.org/ghc/ghc/-/issues/13267).

Relevant SA question (was provided by @yaitskov):
https://stackoverflow.com/questions/28037837/deriving-clause-with-arbitrary-constraint-aliases

## Proposed Change Specification

Proposal requires two new constructions allowed in instance head:
constraint tuple and constraint synonym.

New semantics can be described by two purely syntaxic rewrites,
translating all new constructions away from instance declaration:

1. Apply all constraint synonyms in instance declaration.
   This should be done recursively,
   until no constraint synonym is present anymore.
2. For any constraint tuple in instance head,
   split such declarations into multiple.

This should work the same for any kind of instances:
stanalone derived instances with or without explicit context,
non-standalone derived instances
and user-defined instances.

Thus proposal do not introduce any changes to instance semantics,
only allowed syntax and user error messages would be improved.

## Examples

### Common derived instances usecase

```haskell
type ToFromJSON x = (ToJSON x, FromJSON x)

-- Allowed by this proposal and working same as example above
data MyData = MkMyData deriving (ToFromJSON)

-- Allowed by this proposal
derive instance ... => (ToFromJSON MyData)

-- Should work the same with syntax allowed before proposal
derive instance ... => ToJSON MyData
derive instance ... => FromJSON MyData
```

Non-standalone instances do not have explicit params,
so rewriting implementation may be little different.
But they have just the same samantics as standalone instances with holes,
covered by examples before, so could be handled the same.

### Tuple of classes instance usecase

Example from @VitWW :

```haskell
type Num x = (Arithm x, Abs x)
type Arithm x = (Add x, Sub x, Mult x)

data MyData

instance Num MyData where
    (+) = ...
    (*) = ...

-- same as
instance Add MyData where
    (+) = ...

instance Mult MyData where
    (*) = ...
```

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

```haskell
class (ToJSON x, FromJSON x) => ToFromJSON x

instance (ToJSON x, FromJSON x) => ToFromJSON x
```

While this trick works, Haskell begginers may be not aware of it.
Also it makes less clear API, because `ToFromJSON` is should be same as
typeclass tuple only by syntaxic convention.

## Unresolved Questions

The only question is, which implementation approach to use.

I think this question is not blocking,
because all implementation paths lead to same semantics,
only potential difference may be slightly less-clear type errors.
Also implementation paths are similar any may be switched later.

## Implementation Plan

### Main approaches

The main question is how to check that constraint synonym rewriting
will lead to correct code.

There are two principal solutions to that:

1. Check that rewriting will be correct before applying it.
   In this context this probably amounts to checking for each referenced
   type synonym if it belongs to some list of known bad cases.
2. Optimistically apply transformation and check for errors in later stages.

Also we can just do both.

Tuple of constraints implementation, on other hand, is clear.
There is slight difference in cases of user-defined and derived instances,
because in user-defined instances we need to keep track of which
method is relevant to each instance,
but overall the only thing needed is to split instances.

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
tracking relation of transformed AST to original AST.

### Proposed solution

I believe second approach is more simple to reason and implement.
I (Gregory Gerasev @uhbif19) could try to provide such implementation.

## Endorsements

