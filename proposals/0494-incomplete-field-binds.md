---
author: Fumiaki Kinoshita
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/494).

# Warning mechanism for incomplete field bindings

This proposes a new warning `-Wincomplete-field-binds` which would warn a record pattern that silently discards one or more fields.
To garnish it, this proposal also includes a new language extension `RecordDontCarePatterns` which introduces a record "don't care" pattern `_`.

## Motivation

Consider the following record type:

```haskell
data User = User { ident :: Text, name :: Text, email :: Text }
```

It is perfectly valid to bind a part of the fields like `case user of User {ident = i} -> ...`, discarding the rest of the fields.
However, sometimes it is desirable to ensure that each field is bound in a record pattern, when validating values in a record for example.

```haskell
validateUser :: User -> Either String ()
validateUser{ident = ident, name = name, email = email} = do
    validateIdent ident
    validateName name
    validateEmail email
```

Currently, even if a new field is added to `User`, nothing stops from `validateUser` from compiling.

In contrast, Rust and OCaml (missing-record-field-pattern) have warning mechanisms for such patterns.

## Proposed Change Specification

The proposal consists of two parts: a new warning that warns unbound record fields `-Wincomplete-field-binds`, and a new syntax to match rest of the fields without binding any variables.

When `-Wincomplete-field-binds` is enabled, the compiler checks if record field bindings exactly correspond to the set of the fields. If it doesn't, it would produce a warning with a suggestion to bind the remaining fields.

There would be a new record binding syntax `_`, which is similar to `RecordWildCards`'s `..` except that it does not bind anything. This would be available ina a new language extension `RecordDontCarePatterns` (naming subject to bikeshedding), implied by `RecordWildCards` and `NamedFieldPuns`.
More concretely, given a datatype `data Foo = Foo { a :: () }`, a pattern `Foo{ _ }` desugars to `Foo { a = _ }`. Therefore, `_` suppresses a warning `-Wincomplete-field-binds` would otherwise produce.

Note that this has nothing to do with record construction; it would be illegal to use `_` in record construction.

The syntactic treatment for `_` is almost identical to `..`. The following patterns would be _rejected_:

```haskell
User{ _, ident = x } -- field binding after a wildcard
User{ _, _ } -> () -- multiple wildcards
(){ _ } -> () -- the constructor has no labelled fields
```

## Examples

The following code would produce the warning below:

```haskell
{-# OPTIONS -Wincomplete-field-binds #-}
data User = User { ident :: String, name :: String, email :: String }

validateUser :: User -> Either String ()
validateUser User{ident = _ident, name = _name} = pure ()
```

```
hs/example.hs:5:1: warning: [-Wincomplete-field-binds]
    Record field bind(s) are non-exhaustive
    In an equation for ‘validateUser’:
        Field not matched:
            User{ email = _ }
  |
5 | validateUser User{ident = _ident, name = _name} = pure ()

    Suggestion: add a binding for ‘email’ explicitly, or discard the remaining fields by ‘_’
```

This can be suppressed by adding `_` to the list of bindings:

```haskell
{-# OPTIONS -Wincomplete-field-binds #-}
{-# LANGUAGE RecordDontCarePatterns #-}
data User = User { ident :: String, name :: String, email :: String }

validateUser :: User -> Either String ()
validateUser User{ident = _ident, name = _name, _} = pure () -- no warnings!
```

## Effect and Interactions

This won't affect existing code because the warning is not going to be included to -Wall, and a single underscore is not a valid field name.

## Costs and Drawbacks

In general, warnings that are not enabled by `-Wall -Wcompat` are quite obscure and rarely used in the ecosystem. Thus, it would add a bit more code to the compiler without being frequently used.

## Alternatives

There could be a variant of `-Wredundant-record-wildcards` which warns if *any* of the variables bound is unused, instead of *all*.
While that's a lot easier to implement, this proposal has an advantage of allowing arbitrary patterns.

An alternative syntax `.. = _` for `RecordDontCarePatterns` is suggested, however, the author thinks that a single `_` is more reasonable because:

* Simpler syntax: `_` is syntactically the same as `..`. `.. = _` would require an addition to the parser.
* `.. = _` would make sense if it were a specialisation `.. = <pattern>`, but matching multiple fields against the same pattern does not make sense in general.

## Unresolved Questions

* Is there a better name for `RecordDontCarePatterns`? `_` is generally called a wildcard, but `RecordWildCards` has been taken. A few suggestions have been made:
    * SkippedRecordFields
    * UnboundRecordFields
    * RecordFieldTails
    * RecordPatternBlanks

* Should there be a special treatment for single-constructor types? Namely, should `Foo {}` be exempted from `-Wincomplete-field-binds` if the data declaration is `data Foo = Foo { foo :: () }`? While it makes sense as a dual of the semantics of the pattern matching against `Void`-like types, it would require adding a brand-new single-constructorness checker to the compiler.

## Implementation Plan

If accepted, @fumieval is going to implement these features.

## Endorsements

(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
