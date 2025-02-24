---
author: Matthías Páll Gissurarson
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/280).

# Non-Empty Typed-Holes

Typed-holes are a powerful way for users to interact with the compiler during
compilation, to ask for more information about the context and (recently) to
get suggestions on what could be used in place of the hole. However, the user
can only influence the name of the hole, with the rest being determined by the
hole's context.  We propose to add a new extension, `NonEmptyTypedHoles` to
allow users and IDEs to communicate efficiently with the compiler by adding 3
new syntactic constructs to GHC that represent typed-holes by using `_(...)`,
`_$(...)` and `_$$(...)` where `...` is an expression, a template haskell
expression or a typed template haskell expression respectively.

Using expressions as the contents of the holes not only enables us to
efficiently communicate with any hole plugins, but also allows us to
internalize the notion of the "red underline" that many editors have around
type inconsistencies [[Omar et. al 2019]](https://dl.acm.org/doi/10.1145/3290327).

## Motivation

Typed-holes have a lot of potential for users to really interact with the
compiler during compilation beyond just parsing and type errors. Currently,
typed hole plugins allow plugin developers to extend how these holes are
handled. But, the only way the user can pass any information to the plugin is
via flags or via the name of the hole, which is not an ideal situation.
This means that all the information must be passed along as alphanumeric
strings (without spaces in case of names!), and not in the type safe manner
of Haskell data structures as we'd like. Strings do allow us to define some
language that the user would have to learn, but does not allow for expressive
combinators.

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
specific modules, but the limitation of having only the name makes it
impractical to invoke more than one command at a time, and requires users to
use `_` in the names of the modules we want to filter by.

Passing only the name of the hole also means that we cannot convert
type-inconsistent expressions into typed-holes, which prevents us from dealing
means that we cannot deal with type-inconsistencies beyond reporting them as
type errors.


## Proposed Change Specification

We add `-XNonEmptyTypedHoles` to the available extensions, which turns
on the lexing of the following tokens:
+ `_(` for opening an non-empty typed-hole, whose content is a haskell
  expression (which might be empty)
+ `\) $idchar $idchar*` for allowing users to enumerate the holes, with e.g.
  `_(...)0`, ..., `_(...)n`, or name them e.g. `_(...)a` or `_(...)fix_this`.
+ `_$(` opens a non-empty typed-hole containing a template haskell expression
  inside.
+ `_$$(` opens an non-empty typed-hole containing a typed template haskell
  expression.

Both `_$(` and `_$$(` require `TemplateHaskell` to be enabled in addition to
`NonEmptyTypedHoles`.

We extend the grammar by adding the `non_empty_typed_hole` construct to `aexp2`
and `hole_op`, where `non_empty_typed_hole` is:

```
non_empty_typed_hole
        : '_('       hole_close
        | '_('   exp hole_close
        | '_$('  exp hole_close
        | '_$$(' exp hole_close

hole_close
  : ')'
  | CLOSE_HOLE

```

where `CLOSE_HOLE` is the `\) $idchar $idchar*` lexeme.

These are parsed into a new `HsExpr`, `HsNonEmptyHole`, contains the `LHsExpr`
from the hole or a template haskell splice.

For the splices, they behave the same as template haskell splices in that the
untyped splice is run during renaming, and the typed template haskell splice is
run during  type-checking, and generally behave in the same way as regular
template haskell splices, i.e. they are run at the same time.  The only
difference is that the resulting expression is not spliced into the code, but
rather passed along with the `NonEmptyExprHole` to the constraint solver.

The resulting template haskell expressions are wrapped in a `toDyn` call from
`Data.Dynamic` prior to being type-checked, and a
`runNEHSplice :: LHsExpr GhcTc -> TcM Dynamic` function is provided in
`TcHoleErrors`. This allows plugin developers to easily run the expressions and
obtain a `Dynamic`, which they can then safely try to interpet as whatever type
they expect the user to use in the holes.

For expressions contained in a `_(...)` hole, the contained expression is only
parsed and not renamed nor type-checked, though the
`runNEHRawExpr :: LHsExpr GhcPs -> TcM HValue` and
`runNEHRawExprDyn :: LHsExpr GhcPs -> TcM Dynamic` functions are provided in
`TcHoleErrors` for easy interaction with the expressions. This allows hole
plugin developers to manipulate the contained expressions at will, and allows
users to operate directly on an expression by simply wrapping it in a `_(...)`
hole.

When the type-checker encounters a `HsNonEmptyHole` expression, it emits an
insoluble `CHoleCan` (as we do for typed-holes already), but one that contains
an `NonEmptyExprHole` constraint with the parsed expression (if any) or the
`toDyn` wrapped expression resulting from running the splice.  The expression
can then be accessed via the constraint from plugins or the error reporter, and
will hopefully enable us to move towards [Extensible Type-Directed Editing](http://cattheory.com/extensibleTypeDirectedEditing.pdf)
and [Live Functional Programming with Typed-Holes](https://dl.acm.org/doi/10.1145/3290327)
in Haskell.

As it is behind an extension flag, it does not impact any existing parsing nor
features, except Haddock and other Haskell AST parsing utilities will need to
handle the new `HsExpr`.

## Examples

As an example, consider the [NonEmptyHolesPlugin](https://github.com/Tritlo/ExampleHolePlugin/tree/master/non-empty-holes-plugin).
By defining a DSL, the plugin can express how users can interact with it via
template haskell expressions.


This allows us to compile the following:

```
{-# OPTIONS -fplugin=NonEmptyHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NonEmptyTypedHoles #-}
module Main where
import NonEmptyHolesPlugin
import Control.Monad

f :: (a,b) -> a
f = _([Hoogle])


g :: (a,b) -> b
g = _$( exec $ do
        invoke Hoogle
        filterBy "Control.Monad"
        invoke Djinn)0

h :: (a,b) -> b
h = _$$( execTyped $ do
         filterBy "Prelude"
         invoke Djinn)1

```

where `invoke :: PluginType -> Cmd ()`, `filterBy :: String -> Cmd ()`,
`exec :: Cmd () -> Q Expr`, and the `execTyped :: Cmd () -> Q (TExp [PluginType])`
functions are defined by the plugin itself. This results in the following output:

```
Main.hs:9:5: error:
    • Found hole: _(...) :: (a, b) -> a
      Where: ‘b’, ‘a’ are rigid type variables bound by
               the type signature for:
                 f :: forall a b. (a, b) -> a
               at Main.hs:8:1-15
      Or perhaps ‘_(...)’ is mis-spelled, or not in scope
    • In the expression: _(...)
      In an equation for ‘f’: f = _(...)
    • Relevant bindings include f :: (a, b) -> a (bound at Main.hs:9:1)
      Valid hole fits include
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        f :: (a, b) -> a
        fst :: forall a b. (a, b) -> a
  |
9 | f = _([Hoogle])
  |     ^^^^^^^^^^^

Main.hs:13:5: error:
    • Found hole: _$(...)0 :: (a, b) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 g :: forall a b. (a, b) -> b
               at Main.hs:12:1-15
      Or perhaps ‘_$(...)0’ is mis-spelled, or not in scope
    • In the expression: _$(...)0
      In an equation for ‘g’: g = _$(...)0
    • Relevant bindings include
        g :: (a, b) -> b (bound at Main.hs:13:1)
      Valid hole fits include
        (\ (_, a) -> a)
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        g :: (a, b) -> b
   |
13 | g = _$( exec $ do
   |     ^^^^^^^^^^^^^...

Main.hs:19:5: error:
    • Found hole: _$$(...)1 :: (a, b) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 h :: forall a b. (a, b) -> b
               at Main.hs:18:1-15
      Or perhaps ‘_$$(...)1’ is mis-spelled, or not in scope
    • In the expression: _$$(...)1
      In an equation for ‘h’: h = _$$(...)1
    • Relevant bindings include
        h :: (a, b) -> b (bound at Main.hs:19:1)
      Valid hole fits include
        (\ (_, a) -> a)
        (\ (_, a) -> g (head (cycle (([]) ++ ([]))), a))
        h :: (a, b) -> b
        g :: forall a b. (a, b) -> b
        snd :: forall a b. (a, b) -> b
   |
19 | h = _$$( execTyped $ do
   |     ^^^^^^^^^^^^^^^^^^^...
```

Note that the expressions in `_(...)` are only parsed. Only when we try to
extract the value are the contents type-checked, desugared and compiled.
However, `runNEHSplice`, `runNEHRawExpr` and `runNEHRawExprDyn` all make sure to
propagate any errors encountered to the plugin, which can then handle it
accordingly. E.g. for the NonEmptyHolesPlugin, we can compile the following:

```
{-# OPTIONS -fplugin=NonEmptyHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE NonEmptyTypedHoles #-}
module Main where
import NonEmptyHolesPlugin


data A = A | B | C

b :: A
b = A

j :: ()
j = _([A,b])

main = return ()
```

and get the following output:

```
Main.hs:13:5: error:
    GHC error in desugarer lookup in Main:
      attempting to use module ‘main:Main’ (Main.hs) which is not loaded
   |
13 | j = _([A,b])
   |     ^^^^^^^^

Main.hs:13:5: error:
    GHC error in desugarer lookup in Main:
      Can't find interface-file declaration for variable Main.$tcA
        Probable cause: bug in .hi-boot file, or inconsistent .hi file
        Use -ddump-if-trace to get an idea of which file caused the error
   |
13 | j = _([A,b])
   |     ^^^^^^^^

Main.hs:13:5: error:
    • Found hole: _(...) :: ()
      Or perhaps ‘_(...)’ is mis-spelled, or not in scope
    • In the expression: _(...)
      In an equation for ‘j’: j = _(...)
    • Relevant bindings include j :: () (bound at Main.hs:13:1)
   |
13 | j = _([A,b])
   | 

```

Here the first two errors are internal GHC error being propagated and shown to
the user, it is however up to the discretion of the plugin developer.



## Effect and Interactions

By being able to pass any expression and the result of template haskell expressions
along to the typed-hole plugins, we enable much richer interaction that the user or IDE
can define during development, as well as an internal notion for
type-inconsistent expressions beyond type-errors.

## Costs and Drawbacks

As it is mostly a change in how we parse and pass along information, the cost
is not high from GHC's point of view. Further work will be needed from any
typed hole plugins to make effective use of this new information, but since
this is behind an extension and does not interfere with previous functionality,
this cost is minor.

The non-empty typed-holes are a bit notation heavy, but since they always result in
a type error, complete programs are unlikely to have holes in them.

## Alternatives
+ Do nothing, and use the names of holes as a way of communicating with plugins,
  with no internal notion of inconsistent programs.

## Unresolved Questions

+ Is `_(...)` the right syntax? Other alternatives could be `_{...}` to mirror
  Agda, or even something entirely different like `<...>`. `_(...)` and `_$(...)`
  matches the current template haskell syntax nicely though, and it follows the
  convention that things on the right hand side that start with an underscore
  are typed-holes.
+ Is `_(...)idchars` a form that is neccessary, or is it enough that users can
  use `_(...)0`, ..., `_(...)n` to disambiguate between holes?
+ Should we make `NonEmptyTypedHoles` and the `_(...)`  syntax available
  without requiring the extension flag enabled? This would allow us to
  use `_` for regular typed-holes (without any suggestions etc.), and require users
  that want suggestions or to use plugins to use  the `_(...)` syntax.
+ Should `-XNonEmptyTypedHoles` imply `-XTemplateHaskell`?
+ Should we define and require that the template haskell splices have a specific
  type, and do away with `Dynamic`?  This could ease the interop between different
  plugins, but we'd also like to give plugin developers as much freedom as possible.
+ Should we allow the hole plugins to solve the constraint and result in a splice
  instead of an error? This seems like an avenue that could be explored further in
  the future.

## Implementation Plan

The implementation will be done by @Tritlo, and a prototype is available at [!1766](https://gitlab.haskell.org/ghc/ghc/merge_requests/1766).
