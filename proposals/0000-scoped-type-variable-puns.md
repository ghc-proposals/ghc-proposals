---
author: Georgi Lyubenov
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/507).

# Allow puns to bind type variables

The current language extensions `NamedFieldPuns` and `ScopedTypeVariables` have a (surprisingly) missing interaction:

```haskell
data Thing a = MkThing {field :: a}

f :: Thing a -> a
f MkThing {field :: x} = undefined :: x
```

This currently fails to parse. I believe that a natural extension of the interaction the two extensions
would be to instead have the above mean the same as

```haskell
data Thing a = MkThing {field :: a}

f :: Thing a -> a
f MkThing {field = field :: x} = undefined :: x
```

Dually, we would also allow
```haskell
f :: a -> Thing a
f (field :: x) = MkThing {field :: x}
```
to mean the same as
```haskell
f :: a -> Thing a
f (field :: x) = MkThing {field = field :: x}
```

## Motivation

`NamedFieldPuns` and `ScopedTypeVariables` are both firmly cemented as useful and ubiquitous language extensions.
As we move towards improved type level programming, we should aim to make the UX as seamless as possible.
I think that allowing the `MkField {field :: x}` syntax would be considered "obvious" and "natural" to anyone who has used both extensions,
to the point where when I tried it I had no doubt it would work.

Apart from the obvious convenience that this would provide, it is also less error-prone(as is usual with using field names instead of positional matches) compared to binding type variables in patterns, as described [here](https://gitlab.haskell.org/ghc/ghc/-/issues/18830#note_308891).

While the expression case is less obviously useful, it is nonetheless possible to imagine cases in which a polymorphic field would need a type annotation to type check.

It also makes sense to allow the expression case in order to not add a point of divergence between pattern and expression record syntax.

## Proposed Change Specification

I propose that a pattern match or expression of the form
```haskell
Foo {bar :: baz}
```
be parsed successfully and desugared(in the renamer) to
```haskell
Foo {bar = bar :: baz}
```

Example grammar change from `ghc`'s `Parser.y`:
Current:
```
fbind
        : qvar '=' texp
        | qvar
        ...
```
Proposed addition:
```
fbind
        : qvar '=' texp
        | qvar '::' ctype
        | qvar
        ...
```

Note that here
```haskell
MkRec {x, y, z :: a}
```
the `:: a` signature binds only to the `z`, and not to all of the fields.

## Examples

Pattern example:
```haskell
data Thing a = MkThing {field :: a}

-- x is able to be referred to in the body here, in the same way in which it would be if we wrote
-- MkThing {field = field :: x}
f :: Thing a -> ()
f MkThing {field :: x} = ()
```

Expression example:
```haskell
data Thing a = MkThing {field :: a}

-- field should be checked with the type x, in the same way in which it would be if we wrote
-- MkThing {field = field :: x}
f :: a -> Thing a
f (field :: x) = MkThing {field :: x}
```

## Effect and Interactions

None in particular for this new syntax.

All the interactions and effects should be the same as if `MkThing {field = field :: a}` was written instead of `MkThing {field :: a}`.

## Costs and Drawbacks

The usual increase in GHC code size and grammar complication.

The syntax could possibly be used for something else in the future, but I cannot think of such a thing right now.
I would even argue against it, as it would syntactically be very similar to `{foo = foo :: x}`, but mean a different thing.

The implementation is rather straightforward. The only question I encountered is whether to "abuse"
the already existing `rhs` in `HsFieldBind` to store the type, or to create a new field which stores something like a `Maybe (LHsType pass)`.

## Alternatives

I can't think of other alternatives in this design space, so it seems like the only one is to not have this syntax sugar.

The status quo ("workaround") downstreams the burden(annoyance) of effectively disabling `NamedFieldPuns` when you want to
bind a type variable in a pattern match.

## Unresolved Questions

None

## Implementation Plan

I have implemented a prototype of this [here](https://gitlab.haskell.org/googleson78/ghc/-/commits/sigs-on-puns).

## Endorsements

I have received some positive feedback from Sam Derbyshire and Ben Gamari over at https://gitlab.haskell.org/ghc/ghc/-/issues/18830.
