ScopedImports
=============

.. author:: Rashad Gover
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

This GHC proposal introduces the ``ScopedImports`` language extension.
``ScopedImports`` allows import declarations to appear within ``let`` expressions and ``where`` clauses.
Imported names can be scoped to specific expressions, allowing us to declare imports without
polluting a module's namespace.


Motivation
----------

There are only so many names, and many of them overlap across different packages and modules.
Currently, we have the following methods for avoiding naming conflicts within a module.

**Naming Conventions:**

The ``lucid`` HTML library suffixes HTML element names with ``_``
so users avoid conflicts with names exported by ``Prelude``.::

    div_
    head_
    id_
    map_

Lenses generated with TemplateHaskell conflict with record field selectors.
The standard lens generation function, ``makeLenses``, requires users to use a ``_`` prefix when naming field selectors.::

    data User = User
      { _name  :: Text
      , _email :: Text
      }

    makeLenses ''User  -- generates: name, email lenses

These arbitrary naming conventions exist solely for namespace disambiguation and add bloat to our code.

**Hiding:**

The ``clay`` CSS library recommends hiding certain ``Prelude`` names for the same reason.::

    import Prelude hiding ((**))
    import Clay

**Qualifying:**

Modules from the ``containers`` package such as ``Data.Map`` and ``Data.Set`` share many names.
If we want to use these names in the same module we must qualify them.::

  import qualified Data.Map as M
  import qualified Data.Set as S

  memberCount :: Ord a => a -> M.Map a Int -> S.Set a -> Int
  memberCount key map set =
    case M.lookup key map of
      Just n | S.member key set -> n
      _ -> 0

Modules from the ``text`` and ``bytestring`` packages such as ``Data.Text.Lazy`` and ``Data.ByteString`` share many names too.
If we want to use these names in the same module we must qualify them.::

  import qualified Data.Text as T
  import qualified Data.ByteString as BS

  process :: T.Text -> BS.ByteString -> Int
  process text bytes = T.length text + BS.length bytes

**Prior art**

Several languages support locally scoped imports/module opening.

*OCaml* has supported local module opening since its early versions.::

    let list_sum_sq m =
      let open List in
      init m Fun.id |> map (fun i -> i * i) |> fold_left ( + ) 0

*F#* allows ``open`` declarations inside function bodies.::

    let processData() =
        open System.Collections.Generic
        let dict = Dictionary<string, int>()
        dict

*Rust* allows ``use`` declarations inside function bodies.::

    fn process() {
        use std::collections::HashMap;
        let map = HashMap::new();
    }

*Lean 4* supports local ``open``, similar to OCaml.::

    def example : List Nat :=
      open List in
      map (fun x => x + 1) [1, 2, 3]

All four languages allow imports with lexical scoping, proving that this is a useful feature for programmers.

Proposed Change Specification
-----------------------------

This proposal introduces a new language extension, ``ScopedImports``, which
extends the grammar to allow import declarations within ``let`` and ``where``
binding groups.

Syntax
~~~~~~

The ``decls`` production, as defined in the `Haskell 2010 Report §4.4.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-880004.4.3>`_,
is extended as follows:

**Haskell 2010:**

::

  decls  ->  decls ; decl
          |  decl

**Haskell 2010 + ScopedImports:**

::

  decls  ->  decls ; decl
          |  decls ; importdecl    -- (NEW)
          |  decl
          |  importdecl            -- (NEW)

Where ``importdecl`` is the existing import declaration as defined in
`§5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_.

This change affects the following constructs which use ``decls``:

- ``let`` expressions: ``let { decls } in exp``
- ``where`` clauses: ``... where { decls }``

Semantics
~~~~~~~~~

1. **Same semantics as local bindings**: Scoped imports behave like regular
   bindings in ``let`` and ``where`` clauses.

2. **Import semantics**: Scoped import declarations support the same syntax
   and semantics as top-level imports, including:

   - Qualified imports (``import qualified M``)
   - Import lists (``import M (foo, bar)``)
   - Hiding lists (``import M hiding (baz)``)
   - Renaming (``import qualified M as N``)
   - Package imports (``import "package" M``)

3. **Module dependencies**: Modules referenced by scoped imports are treated
   as dependencies of the enclosing module for the purposes of compilation
   order and recompilation checking.

Principles
~~~~~~~~~~

``ScopedImports`` furthers GHC's adherence to the `Lexical Scoping Principle (LSP) <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#221lexical-scoping-principle-lsp>`_.

By allowing imports inside of let statements and where clauses,
it can be made obvious to programmers what imports an expression depends on
and where names in the expression come from without having to look
at the top of the module and keep track of what names are in scope.

Examples
--------

Basic usage
~~~~~~~~~~~

Import in a ``where`` clause:

::

  {-# LANGUAGE ScopedImports #-}
  module TestSum where

  testSum :: Int
  testSum = sum [1, 2, 3, 4, 5]  -- 15
    where import Data.List (sum)

Import in a ``let`` expression:

::

  {-# LANGUAGE ScopedImports #-}
  module TestUpper where

  testUpper :: String
  testUpper = let import Data.Char (toUpper)
              in map toUpper "hello"  -- "HELLO"

Multiple imports
~~~~~~~~~~~~~~~~

Multiple imports can appear in a single binding group, interleaved with
other declarations:

::

  {-# LANGUAGE ScopedImports #-}
  module TestMultiple where

  testMultiple :: (String, Int)
  testMultiple = result
    where
      import Data.Char (toUpper)
      str = map toUpper "abc"
      import Data.List (length, sort)
      result = (str, length (sort [3, 1, 2]))  -- ("ABC", 3)

Mutual recursion
~~~~~~~~~~~~~~~~

Mutually recursive functions can each have their own scoped imports:

::

  {-# LANGUAGE ScopedImports #-}
  module TestMutualRecursion where

  process :: String -> String
  process s = f s
    where
      f [] = []
      f xs = toUpper (head xs) : g (tail xs)
        where import Data.Char (toUpper)

      g [] = []
      g xs = toLower (head xs) : f (tail xs)
        where import Data.Char (toLower)

Qualified imports
~~~~~~~~~~~~~~~~~

Qualified imports work as expected:

::

  {-# LANGUAGE ScopedImports #-}
  module TestQualified where

  testQualified :: [Int]
  testQualified = L.sort [3, 1, 2]  -- [1, 2, 3]
    where import qualified Data.List as L

Guards
~~~~~~

Scoped imports in a ``where`` clause are available in guards:

::

  {-# LANGUAGE ScopedImports #-}
  module TestGuards where

  isSorted :: Ord a => [a] -> Bool
  isSorted xs
    | sort xs == xs = True
    | otherwise     = False
    where
      import Data.List (sort)

Let in do blocks
~~~~~~~~~~~~~~~~

``let`` statements in do blocks desugar to ``let ... in ...``, so scoped
imports work naturally:

::

  {-# LANGUAGE ScopedImports #-}
  module TestDoBlock where

  example :: IO ()
  example = do
    input <- getLine
    let import Data.Char (toUpper)
    putStrLn (map toUpper input)

Note that the import is only visible to statements *later* in the do block.

Per-clause imports
~~~~~~~~~~~~~~~~~~

Each equation of a function can have its own ``where`` clause, and therefore
its own scoped imports.

::

  {-# LANGUAGE ScopedImports #-}
  module TestShowBase where

  showBase :: Int -> Int -> String
  showBase 2 n = showIntAtBase 2 intToDigit n ""
    where
      import Numeric (showIntAtBase)
      import Data.Char (intToDigit)
  showBase 16 n = showHex n ""
    where import Numeric (showHex)
  showBase _ n = show n

The binary case needs ``showIntAtBase`` and ``intToDigit``, the hex case
needs ``showHex``, and the default case needs nothing special.

Effect and Interactions
-----------------------

``ScopedImports`` directly addresses the namespace pollution issues raised in the
Motivation:

- **Naming Conventions**: Libraries like ``lucid`` can potentially export ``div``, ``head``,
  ``id`` without underscores. Lenses can coexist with record selectors by importing the lens
  only where lens operations are needed.::

    {-# LANGUAGE ScopedImports, NoImplicitPrelude, OverloadedStrings #-}
    module LucidExample where

    import Lucid (Html)
    import Prelude (mempty, ($))

    myHtml :: Html ()
    myHtml = div [id "main"] $ mempty
      where import Lucid (div, id)

- **Qualifying**: Functions working with ``Text``, ``ByteString``,
  ``Map``, or ``Set`` can import the operations they need locally and unqualified, avoiding
  the clutter of qualifying ambiguous names.::

    {-# LANGUAGE ScopedImports, NoImplicitPrelude #-}
    module ContainerExample where

    import Prelude (Int, Ord, Maybe, Bool)
    import Data.Map (Map)
    import Data.Set (Set)
    import Data.IntMap (IntMap)

    lookupMap :: Ord k => k -> Map k v -> Maybe v
    lookupMap key m = lookup key m
      where import Data.Map (lookup)

    memberSet :: Ord a => a -> Set a -> Bool
    memberSet x s = member x s
      where import Data.Set (member)

    lookupIntMap :: Int -> IntMap v -> Maybe v
    lookupIntMap key m = lookup key m
      where import Data.IntMap (lookup)

- **Hiding**: Instead of hiding conflicting names at the module level, functions can scope
  both versions. ``Prelude.(**)`` and ``Clay.(**)`` can coexist in the same module.::

    {-# LANGUAGE ScopedImports, NoImplicitPrelude #-}
    module ClayExample where

    import Prelude (Double, ($))
    import Clay (Css, body, p, (?), padding, px, auto)

    squared :: Double
    squared = 2 ** 2
      where import Prelude ((**))

    buttonStyle :: Css
    buttonStyle = body ** p ? padding (px squared) auto auto auto
      where import Clay ((**))

``ScopedImports`` has interesting synergy with other GHC language extensions:

QualifiedDo
~~~~~~~~~~~

Scoped qualified imports can provide the module qualification for ``QualifiedDo``:

::

  {-# LANGUAGE ScopedImports, QualifiedDo #-}
  module TestQualifiedDo where

  testQualifiedDo :: Maybe Int
  testQualifiedDo = M.do
      x <- Just 5
      y <- Just 7
      M.return (x * y)
    where
      import qualified Control.Monad as M

RebindableSyntax
~~~~~~~~~~~~~~~~

``ScopedImports`` works with ``RebindableSyntax``. Different functions in the
same module can have different do-notation semantics, for example:

::

  {-# LANGUAGE ScopedImports, RebindableSyntax, NoImplicitPrelude #-}
  module TestRebindableSyntax where

  import Prelude (Int, Integer)

  -- Identity-style do (just function application)
  pureComputation :: Int
  pureComputation = do
      x <- 1
      y <- 2
      return (x + y)  -- 3
    where
      import Prelude ((+), fromInteger)
      (>>=) :: a -> (a -> b) -> b
      x >>= f = f x
      return :: a -> a
      return x = x

  -- List monad do (nondeterminism)
  combinations :: [Int]
  combinations = do
      x <- [1, 2]
      y <- [10, 20]
      return (x + y)  -- [11, 21, 12, 22]
    where
      import Prelude ((+), (>>=), return, fromInteger)


Costs and Drawbacks
-------------------

**Compilation cost**: When ``ScopedImports`` is enabled, the module graph builder
(downsweep) must parse the entire module to discover scoped import declarations,
rather than stopping after top-level imports. This additional parsing only affects
modules that enable the extension.

**Learnability**: Minimal impact. Scoped imports follow the same scoping rules as
local bindings in ``let`` and ``where`` clauses, which Haskell programmers are
already familiar with.

**Maintenance burden**: Implementation of ``ScopedImports`` touches the parser, renamer, and the
algorithm used in downsweep.
No new type system features, no changes to Core, and no changes to compile-time or runtime semantics.


Backward Compatibility
----------------------

**Level 0: No breakage.**

This is a completely opt-in language extension. Existing programs are unaffected.
The new syntax (import declarations in ``let``/``where``) is currently a parse
error, so no existing code can be broken by this change.


Alternatives
------------

**Qualified top-level imports**: Users can import modules qualified at the top
level and use qualified names throughout. This works but clutters the top-level
namespace and forces all functions in the module to see imports they may not need.

**Modules splitting**: Functionality can be split into separate modules, each with
its own imports. This adds file management overhead and forces programmers to
split code into different modules that may otherwise conceptually make more sense
to be in the same module.

**Records with DuplicateRecordFields**: One can encode "local imports" by
wrapping values in records and using ``DuplicateRecordFields`` to access them.
This is unwieldy, requires boilerplate, and cannot handle certain polymorphic
functions due to monomorphism restrictions on record fields.

Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

A working implementation is available at
`https://gitlab.haskell.org/rgover/ghc <https://gitlab.haskell.org/rgover/ghc>`_
on the ``scoped-imports`` branch.

*The implentation was done by Claude Opus 4.5 (Anthropic) and reviewed by the author of this proposal,
a Haskell programmer with no prior GHC contributions. The implementation may be incorrect.*

The implementation touches the following areas:

1. **Parser** (``GHC/Parser.y``): Extended the grammar to allow ``importdecl``
   within ``decls``. Added productions for ``decls_and_imports`` and updated
   ``let`` and ``where`` clause handling.

2. **AST** (``Language/Haskell/Syntax/Binds.hs``): Added a new constructor
   ``HsLocalBindsWithImports`` to ``HsLocalBindsLR`` that wraps a list of
   import declarations alongside the existing local bindings.

3. **Renamer** (``GHC/Rename/Bind.hs``): Extended ``rnLocalBindsAndThen`` to
   handle scoped imports by renaming them and extending the local environment.
   The extension check happens here, producing a clear error message when
   ``ScopedImports`` is not enabled.

4. **Downsweep** (``GHC/Driver/Downsweep.hs``): Added ``extractScopedImports``
   to traverse the parsed AST and collect scoped import declarations for
   dependency analysis. Uses a generic traversal (``everythingOf``) to find
   all ``HsLocalBindsWithImports`` nodes.

5. **Test suite**: 18 tests covering basic usage, extension interactions,
   scoping behavior, and error cases.


Acknowledgments
---------------

This implementation was developed using AI-assisted programming.
Most of the code was generated by Claude Opus 4.5 (Anthropic) with
some guidance and direction from me. I am a Haskell programmer with
no prior contributions to GHC.

While I had the idea for this proposal, I would have not been able to implement
``ScopedImports`` alone without learning a lot more about GHC's architecture over
a much longer period of time.

I am open to mentorship from experienced GHC contributors so that I may learn more about
GHC and the code that Claude Opus 4.5 (Anthropic) generated for this proposal implementation.
