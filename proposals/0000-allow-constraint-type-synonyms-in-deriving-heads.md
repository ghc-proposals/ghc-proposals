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
neither for constraint synonyms, nor for tuples.

This may be unexpected for user, because both of this constructs
are supported in the context of an instance, but not in the head of an instance

Another usecase is typeclasses which are semantically same as composition of
multiple classes. They too could be replaced by constraint synonym instances.

Case of constraint synonyms in instance head worked at some point in time,
but was disabled due to wrong user-facing error messages in some cases.
Reasoning for that is covered in Note "Instances and constraint synonyms"
and [issue 13267](https://gitlab.haskell.org/ghc/ghc/-/issues/13267).

Relevant SA question (was provided by @yaitskov):
https://stackoverflow.com/questions/28037837/deriving-clause-with-arbitrary-constraint-aliases

## Proposed Change Specification

The proposal requires two new declarations to be allowed in head of the instance:
constraint tuple and constraint synonym.

New semantics can be described by two purely syntactic rewrites,
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

Thus, the proposal does not introduce any changes to instance semantics,
only the allowed syntax and error messages would be improved.

## Examples

### Common derived instances usecase

```haskell
type ToFromJSON x = (ToJSON x, FromJSON x)

-- Allowed by this proposal and works the same as the example above
data MyData = MkMyData deriving (ToFromJSON)

-- Allowed by this proposal
derive instance ... => (ToFromJSON MyData)

-- Should work the same with syntax allowed before proposal
derive instance ... => ToJSON MyData
derive instance ... => FromJSON MyData
```

Non-standalone instances do not have explicit params,
therefore rewriting their implementation may be little different.
However they have just the same samantics as standalone instances with holes,
covered by examples before, and could therefore be handled the same.

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

We already have the `TypeSynonymInstances` extension,
but it does not cover the constraint synonym case.
Constraint synonyms should probably be guarded by this extension as well
and the documentation updated to reflect that.

It may be, that some linters depend on current syntaxic restrictions,
the authors however, are not aware on any specific cases.

## Costs and Drawbacks

The primary concern is that rewriting implementations may make errors less clear.
The authors propose to mitigate this by adding context to errors, in cases where this might happen.
The same is already done for instances resolving today.

## Backward Compatibility

No breakage. Part of this even used to work in the past.

## Alternatives

For deriving instances there are two alternatives:

1. Use CPP, which breaks type-safety, and
   may lead to surprising error messages as well as being less composable
2. Use Template Haskell, which has its own set of problems and is harder to learn for beginners

For user-defined instances one may define a type class instead of a synonym:

```haskell
class (ToJSON x, FromJSON x) => ToFromJSON x

instance (ToJSON x, FromJSON x) => ToFromJSON x
```

While this trick works, Haskell beginners may not be aware of it.
It also leads to a less clear API, because `ToFromJSON` should be same as a
typeclass tuple only by syntactic convention.

## Unresolved Questions

The only question is, which implementation approach to use.

The authors do not believe the question of implementation to be blocking,
because all implementation paths lead to same semantics.
A potential difference in implementation paths could lead to slightly sub-optimal errors messages.
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

We could also do both.

Tuple of constraints implementation, on other hand, is clear.
There is a slight difference in cases of user-defined and derived instances,
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
It may also lead to GHC code simplification,
because now there are multiple codepaths
need to handle type synonyms differently.

The potential problem for solution 2 would be error messages clarity.
Since checks are applied to rewritten code,
they might miss original context or show non-original AST.
The authors believe such problems could be solved by means of
tracking relation of transformed AST to original AST.

### Proposed solution

The authors believe the second approach is simpler to reason and implement.
The authors (Gregory Gerasev @uhbif19) would volunteer to provide such implementation.

## Endorsements

