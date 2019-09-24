---
author: Matthías Páll Gissurarson
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Extended Typed-holes

Typed-holes are a powerful way to interact with the compiler during compilation,
to ask for more information about the context and (recently) to get suggestions
on what could be used in place of the hole. We propose to add a new extensions,
`-XExtendedTypedHoles` to allow users to communicate more efficiently with the
compiler via typed-holes, by using `_(...)`, `_$(...)` and `_$$(...)` where
`...` is a string, a template haskell and a typed template haskell expressions
respectively. 


## Motivation

Typed-holes have a lot of potential for users to really interact with the
compiler during compilation beyond just parsing and type errors. Currently,
typed hole plugins allow users to extend how these holes are dealt with. But,
the only way the user can pass any information to the plugin is via flags or
via the name of the hole, which is not an ideal situation. This means that all
the information must be passed along as alphanumeric strings, and not as 
Haskell datastructures as we'd like.


## Proposed Change Specification

We add `-XExtendedTypedHoles` to the available extensions, which turns
on the lexing of the following tokens:
+ `_(` for extended typed-holes, whose contents are strings
+ `\) $decdigit $decdigit*` for allowing users to enumerate the holes,
  with e.g. `_(...)0`, ..., `_(...)n`.
+  `_$(` opens a extended typed-hole with a template haskell within.
+  `_$$(` opens an extended typed-hole with a typed template haskell within.

We extend the grammer by adding the following constructs:
```
extended_typed_hole
        : '_(' maybe_hole_content hole_close
        | typed_hole_splice

typed_hole_splice
        : '_$('  exp hole_close  
        | '_$$('  exp hole_close 

hole_close
  : ')'    
  | CLOSE_HOLE

maybe_hole_content 
  : STRING 
  | {- empty -}
```

where `CLOSE_HOLE` is the `\) $decdigit $decdigit*` token.

These are parsed into a new `HsExpr`, `HsExtendedHole`, which either contains
the string, or the `LHsExpr` containing the splice. In case of the splice,
it is renamed during renaming and type-checked during type-checking.

When the type-checker encounters a `HsExtendedHole` expression, it emits
an insoluable `CHoleCan` containing a `ExtendedExprHole` constraint with
the string or already run splice as its contents. The string or splice can
then be accessed via the constraint from plugins or the error reporter.

As it is behind an extension flag, it does not impact any existing parsing
nor features, except Haddock and other Haskell AST parsing utilities will 
need to handle the new `HsExpr`.

## Examples

As an example, consider the ExtendedHolesPlugin. By defining a DSL, the plugin
can express how users can interact with it via template haskell expressions.
This allows us to compile the following:

```
{-# OPTIONS -fplugin=ExtendedHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedTypedHoles #-}
module Main where
import Control.Monad
import Language.Haskell.TH
import ExtendedHolesPlugin

f :: (a,b) -> b
f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0

main = return ()
```

where `invoke` and `filterBy` are defined by the plugin itself. This results
in the following output:

```
Main.hs:10:5: error:
    • Found hole: _$(...)0 :: (a, b) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 f :: forall a b. (a, b) -> b
               at Main.hs:9:1-15
      Or perhaps ‘_$(...)0’ is mis-spelled, or not in scope
    • In the expression: _$(...)0
      In an equation for ‘f’: f = _$(...)0
    • Relevant bindings include
        f :: (a, b) -> b (bound at Main.hs:10:1)
      Valid hole fits include
        (\ (_, a) -> a)
        (\ (_, a) -> seq (head (cycle (([]) ++ ([])))) a)
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        f :: (a, b) -> b
        snd :: forall a b. (a, b) -> b
   |
10 | f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```



## Effect and Interactions

By being able to parse any string and the result of template haskell expressions
along to the typed-hole plugins, we enable much richer interaction that the user
can define during development.

## Costs and Drawbacks

As it is mostly a change in how we parse and pass along information, the cost
is not high from GHC's point of view. Further work will be needed from any 
typed hole plugins to make effective use of this new information, but since
this is behind an extension and does not interfere with previous functionality,
this cost is minor.


## Alternatives

+ Do nothing, and use the names of holes as a way of communicating with plugins.
+ Disable extensions to typed-holes alltogether.

## Unresolved Questions

+ Is `_(...)` the right syntax? Other alternatives could be `_{...}` to mirror Agda, or even something
  entirely different like `<...>`. `_(...)` and `_$(...)` matches the current template haskell syntax
  nicely though, and it follows the convention that things on the right hand side that start with an
  underscore are typed-holes.

## Implementation Plan

The implementation will be done by @Tritlo, and is already partly available at [!1766](https://gitlab.haskell.org/ghc/ghc/merge_requests/1766).
