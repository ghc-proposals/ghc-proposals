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
the information must be passed along as alphanumeric strings (without spaces in
case of names!), and not in the type safe manner of Haskell data structures as
we'd like. Strings do allow us to define some language that the user would have
to learn, but does not allow for expressive combinators.

This limits the usefulness of typed-hole plugins and means that IDE features
that interact with holes would either need to pass complex flags or hard to
read names to plugins for any advanced functionality such as limiting synthesis
to specific modules or constraints. 

As an example, consider the [DjinnHoogleModPlugin](https://github.com/Tritlo/ExampleHolePlugin/tree/master/djinn-hoogle-mod-plugin):
```
{-# OPTIONS -fplugin=DjinnHoogleModPlugin
            -funclutter-valid-hole-fits #-}
module Main where
import Control.Monad
f :: (a,b) -> a
f = _invoke_Djinn
g :: [a] -> [[a]]
g = _invoke_Hoogle
h :: [[a]] -> [a]
h = _module_Control_Monad


main :: IO ()
main = return ()
```

Here, the name of the hole is used to invoke different utilities and filter by
specific modules, but the limitation of having only the name makes it impractical
to invoke more than one command at a time, and requires us to use `_` in the
names of the modules we want to filter by.


## Proposed Change Specification

We add `-XExtendedTypedHoles` to the available extensions, which turns
on the lexing of the following tokens:
+ `_(` for extended typed-holes, whose contents are strings
+ `\) $decdigit $decdigit*` for allowing users to enumerate the holes,
  with e.g. `_(...)0`, ..., `_(...)n`.
+ `\) @varid` same as above, but allows arbitrary names for the hole.
+  `_$(` opens a extended typed-hole with a template haskell within.
+  `_$$(` opens an extended typed-hole with a typed template haskell within.

Both `_$(` and `_$$(` require `TemplateHaskell` to be enabled as well as
`ExtendedTypedHoles`.

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

where `CLOSE_HOLE` is the `\) $decdigit $decdigit*` or `\) @varid` token.

These are parsed into a new `HsExpr`, `HsExtendedHole`, which either contains
the string, or the `LHsExpr` containing the splice.

In case of the splice, it is renamed during renaming and type-checked during
type-checking, and generally behave in the same way and adhere to the same
rules as regular template haskell splices, i.e. they are run at the same time.
The only difference is that the resulting expression is not spliced into the code,
but rather passed along with the `ExtendedExprHole` to the constraint solver.

When the type-checker encounters a `HsExtendedHole` expression, it emits
an insoluable `CHoleCan` containing a `ExtendedExprHole` constraint with
the string or already run splice as its contents. The string or splice can
then be accessed via the constraint from plugins or the error reporter.

As it is behind an extension flag, it does not impact any existing parsing
nor features, except Haddock and other Haskell AST parsing utilities will 
need to handle the new `HsExpr`.

## Examples

As an example, consider the [ExtendedHolesPlugin](https://github.com/Tritlo/ExampleHolePlugin/tree/master/extended-holes-plugin).
By defining a DSL, the plugin
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
import Control.Monad


f :: (a,b) -> b
f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0

g :: [[a]] -> [a]
g = _$(invoke "hoogle" & filterBy "Control.Monad")1

main = return ()
```

where `invoke :: String -> Q Exp` and `filterBy :: String -> Q Exp`
and the `(&) :: Q Exp -> Q Expr -> Q Exp` combinator are defined by the
plugin itself. This results in the following output:

```
Main.hs:12:5: error:
    • Found hole: _$(...)0 :: (a, b) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 f :: forall a b. (a, b) -> b
               at Main.hs:11:1-15
      Or perhaps ‘_$(...)0’ is mis-spelled, or not in scope
    • In the expression: _$(...)0
      In an equation for ‘f’: f = _$(...)0
    • Relevant bindings include
        f :: (a, b) -> b (bound at Main.hs:12:1)
      Valid hole fits include
        (\ (_, a) -> a)
        (\ (_, a) -> seq (head (cycle (([]) ++ ([])))) a)
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        f :: (a, b) -> b
        snd :: forall a b. (a, b) -> b
   |
12 | f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Main.hs:15:5: error:
    • Found hole: _$(...)1 :: [[a]] -> [a]
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 g :: forall a. [[a]] -> [a]
               at Main.hs:14:1-17
      Or perhaps ‘_$(...)1’ is mis-spelled, or not in scope
    • In the expression: _$(...)1
      In an equation for ‘g’: g = _$(...)1
    • Relevant bindings include
        g :: [[a]] -> [a] (bound at Main.hs:15:1)
      Valid hole fits include
        Hoogle: Data.List subsequences :: [a] -> [[a]]
        Hoogle: Data.List permutations :: [a] -> [[a]]
        g :: [[a]] -> [a]
        join :: forall (m :: * -> *) a. Monad m => m (m a) -> m a
        msum :: forall (t :: * -> *) (m :: * -> *) a.
                (Foldable t, MonadPlus m) =>
                t (m a) -> m a
        forever :: forall (f :: * -> *) a b. Applicative f => f a -> f b
   |
15 | g = _$(invoke "hoogle" & filterBy "Control.Monad")1
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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

The extended typed-holes are a bit notation heavy, but since they always result in
a type error, complete programs are unlikely to have holes in them.

## Alternatives
+ Do nothing, and use the names of holes as a way of communicating with plugins.

## Unresolved Questions

+ Is `_(...)` the right syntax? Other alternatives could be `_{...}` to mirror Agda, or even something
  entirely different like `<...>`. `_(...)` and `_$(...)` matches the current template haskell syntax
  nicely though, and it follows the convention that things on the right hand side that start with an
  underscore are typed-holes.
+ Is `_(...)varid` a form that is neccessary, or is it enough that users can use `_(...)0`, ..., `_(...)n`
  to disambiguate between holes.
+ Should we make `ExtendedTypedHoles` and the `_(...)`  syntax available without requiring the extension
  flag enabled? This would allow us to use `_` for regular typed-holes (without any suggestions etc.), and
  require users that want suggestions or to use plugins to use  the `_(...)` syntax.
+ Should we define and require that the template haskell splices have a specific type? This could ease the
  interop between different plugins, but we'd also like to give plugin developers as much freedom as possible.

## Implementation Plan

The implementation will be done by @Tritlo, and is already partly available at [!1766](https://gitlab.haskell.org/ghc/ghc/merge_requests/1766).
