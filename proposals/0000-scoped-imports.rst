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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/740>`_.
.. contents::

This GHC proposal introduces the ``ScopedImports`` language extension.
``ScopedImports`` allows import declarations to appear within let expressions,
let declarations, and where clauses for bringing module names into local scope, while keeping
module/instance resolution unchanged. Module and instance resolution remain global.

Imported names can be locally scoped to specific expressions, allowing us to declare imports without
polluting a module's namespace.

Motivation
----------

Names overlap across different modules.
If a module imports the same name from 2 or more modules, this results in a **naming conflict**.

Currently, we have the following methods for avoiding naming conflicts.

**Naming Conventions:**

The ``lucid`` HTML library suffixes HTML element names with ``_``
so users avoid conflicts with names exported by ``Prelude``::

    div_
    head_
    id_
    map_

Lenses generated with TemplateHaskell conflict with record field selectors.
The standard lens generation function, ``makeLenses``, requires users to use a ``_``
prefix when naming field selectors::

    data User = User
      { _name  :: Text
      , _email :: Text
      }

    makeLenses ''User  -- generates: name, email lenses

These arbitrary naming conventions exist solely for avoiding naming conflicts and add bloat to our code.
It gets worse the more expressions exported by a module have to abide by the naming convention.
Even short suffix/prefixes like ``_`` add up.

Also, if 2 or more imported modules share the same naming convention, then the naming convention is moot.
It is only useful if each module has a unique naming convention.

**Hiding:**

Since the ``clay`` CSS library exports an operator named ``(**)``,
the library author recommends hiding the ``(**)`` operator defined in the ``Prelude`` module
to avoid a naming conflict::

    import Prelude hiding ((**))
    import Clay

Hiding imports applies to the entire module.
The ``(**)`` operator from the ``Prelude`` module can't be used in any expression within the module,
even if those expressions don't depend on the ``Clay`` module at all.

**Qualifying:**

Modules from the ``containers`` package such as ``Data.Map`` and ``Data.Set`` share many names.
Modules can use import qualified to avoid name collisions::

  import qualified Data.Map
  import qualified Data.Set

  memberCount :: Ord a => a -> Data.Map.Map a Int -> Data.Set.Set a -> Int
  memberCount key map set =
    case Data.Map.lookup key map of
      Just n | Data.Set.member key set -> n
      _ -> 0

  ...

This workaround suffers from the same issues as naming conventions and adds a lot of bloat to our code for every used import.
To alleviate some pain, we can use module renaming to shorten the *modid* of the module::

  import qualified Data.Map as M
  import qualified Data.Set as S

  memberCount :: Ord a => a -> M.Map a Int -> S.Set a -> Int
  memberCount key map set =
    case M.lookup key map of
      Just n | S.member key set -> n
      _ -> 0

  ...

People reading this module may have to scroll up to the top of the module
to remember what *modid* refers to what module.
This can be tedious in large modules depending on the person and the tooling they're using.

**Prior art**

Several languages support locally scoped imports/module opening.

*OCaml* has supported local module opening since its early versions::

    let list_sum_sq m =
      let open List in
      init m Fun.id |> map (fun i -> i * i) |> fold_left ( + ) 0

*F#* allows ``open`` declarations inside function bodies::

    let processData() =
        open System.Collections.Generic
        let dict = Dictionary<string, int>()
        dict

*Rust* allows ``use`` declarations inside function bodies::

    fn process() {
        use std::collections::HashMap;
        let map = HashMap::new();
    }

*Lean 4* supports local ``open``, similar to OCaml::

    def example : List Nat :=
      open List in
      map (fun x => x + 1) [1, 2, 3]

All 4 languages allow imports with lexical scoping, proving that this is a useful feature for programmers.

Proposed Change Specification
-----------------------------

This proposal introduces a new language extension, ``ScopedImports``, which
extends the grammar to allow import declarations within let expressions,
let declarations, and where clauses.

Syntax
~~~~~~

The ``ScopedImports`` language extension extends the ``decl`` production defined in the
`Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html>`_.

**Haskell 2010:**

::

  decl -> gendecl
        | (funlhs | pat) rhs

**Haskell 2010 + ScopedImports:**

::

  decl -> gendecl
        | (funlhs | pat) rhs
        | importdecl

Where ``importdecl`` is an import declaration as defined in
`Section 5.3 of the Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_.

Since

::
  
  decls	-> { decl₁ ; ... ; declₙ } where (n >= 0)

import declarations are allowed in all syntax depending on the extended ``decls`` production rule.

This includes:

- `Let expressions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-440003.12>`_

- Where clauses in `case expressions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13>`_, `function bindings, and pattern bindings <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-830004.4.3>`_

- Let declarations inside of `do expressions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14>`_,
  `list comprehensions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11>`_,
  and `guards <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#dx17-180049>`_

These special import declarations are called *scoped import declarations*.
The names they may bring in scope are called *scoped imports*.

The ``ScopedImports`` language extension does NOT change where other top-level declarations (``topdecl``) such as

- Class declarations

- Instance declarations

- Extensions to the ``topdecl`` production rule such as pattern synonyms

are allowed.

Semantics
~~~~~~~~~

Given module A and module B.

If module A is used in a scoped import declaration in module B,
module B MUST import AT LEAST module A's instances at the top-level.

For example,
if module A is defined as

::

  module A where

    foo = 5

and module B is defined as

::

  {-# LANGUAGE ScopedImports #-}

  module B where

    bar = foo where import A

module B will NOT compile. We did not import the instances of A at the top-level of module B.

If module B is instead defined as

::

  {-# LANGUAGE ScopedImports #-}

  module B where

  import A ()

  bar = foo where import A

module B will compile.

Requiring the import of a module's instances at the top-level
for scoped import declarations does 2 things:

- Brings all instances defined in module A into scope, making it explicit that
  Haskell's global instance semantics are unchanged from Haskell 2010.

- Signals to GHC and other tools that need to know module dependencies,
  that module B depends on module A without parsing the entire module.

Scoped import declarations support the same syntax
and semantics as top-level imports except they are scoped
locally to a specific expression, not the entire module.
This includes hiding, renaming, etc.

All names brought into a local scope via scoped imports have the same semantics as
names declared locally in a let expression, let declaration, or where clause.

Principles
~~~~~~~~~~

``ScopedImports`` furthers GHC's adherence to the `Lexical Scoping Principle (LSP) <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#221lexical-scoping-principle-lsp>`_.

By scoping imports with let expressions, let declarations, and where clauses,
it can be made obvious to programmers what imports an expression depends on.
Programmers can know where names in the expression come from without having to look
at the top of the module and keep track of what names are in scope.

Examples
--------

Basic usage
~~~~~~~~~~~

Import in a where clause:

::

  {-# LANGUAGE ScopedImports #-}

  module TestSum where

  import Data.List ()

  testSum :: Int
  testSum = sum [1, 2, 3, 4, 5]  -- 15
    where import Data.List (sum)

Import in a let expression:

::

  {-# LANGUAGE ScopedImports #-}

  module TestUpper where

  import Data.Char ()

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

  import Data.Char ()
  import Data.List ()

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

  import Data.Char ()

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

  import Data.List ()

  testQualified :: [Int]
  testQualified = L.sort [3, 1, 2]  -- [1, 2, 3]
    where import qualified Data.List as L

Guards
~~~~~~

Scoped imports in a where clause are available in guards:

::

  {-# LANGUAGE ScopedImports #-}

  module TestGuards where

  import Data.List ()

  isSorted :: Ord a => [a] -> Bool
  isSorted xs
    | sort xs == xs = True
    | otherwise     = False
    where
      import Data.List (sort)

Let declarations can also appear directly in pattern guards:

::

  {-# LANGUAGE ScopedImports #-}

  module TestLetInGuard where

  import Data.List ()
  import Data.Char ()

  classify :: String -> String
  classify s
    | let import Data.List (null), null s = "empty"
    | let import Data.Char (isDigit), all isDigit s = "number"
    | otherwise = "text"

Let in do blocks
~~~~~~~~~~~~~~~~

Let declarations in do blocks desugar to let expressions, so scoped
imports work naturally:

::

  {-# LANGUAGE ScopedImports #-}

  module TestDoBlock where

  import Data.Char ()

  example :: IO ()
  example = do
    input <- getLine
    let import Data.Char (toUpper)
    putStrLn (map toUpper input)

Note that the import is only visible to statements *later* in the do block.

Per-clause imports
~~~~~~~~~~~~~~~~~~

Each equation of a function can have its own where clause, and therefore
its own scoped imports.

::

  {-# LANGUAGE ScopedImports #-}

  module TestShowBase where

  import Numeric ()
  import Data.Char ()

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

Interactions with local declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Scoped imports bring names into scope that can be used with other local
declaration forms allowed in let expressions and where clauses.

**Local type signatures:**

Imported types can be used in local type signatures::

  {-# LANGUAGE ScopedImports, ScopedTypeVariables #-}

  module TestLocalTypeSig where

  import Data.Map ()

  example :: Int
  example = size myMap
    where
      import Data.Map (Map, fromList, size)
      myMap :: Map String Int
      myMap = fromList [("a", 1), ("b", 2)]

**List comprehensions:**

Scoped imports work in list comprehension let bindings::

  {-# LANGUAGE ScopedImports #-}

  module TestListComp where

  import Data.Char ()

  uppercaseEvens :: String -> String
  uppercaseEvens str =
    [ toUpper c
    | (i, c) <- zip [0..] str
    , let import Data.Char (toUpper)
    , even i
    ]

**Pattern matching with imported constructors:**

Data constructors can be imported locally for pattern matching::

  {-# LANGUAGE ScopedImports #-}

  module TestPatternMatch where

  import Data.List.NonEmpty ()

  safeHead :: [a] -> Maybe a
  safeHead xs = case nonEmpty xs of
    Just (y :| _) -> Just y
    Nothing -> Nothing
    where
      import Data.List.NonEmpty (nonEmpty, NonEmpty(..))

Effect and Interactions
-----------------------

``ScopedImports`` directly addresses the namespace pollution issues raised in the
Motivation:

- **Naming Conventions**: Libraries like ``lucid`` can potentially export ``div``, ``head``,
  ``id`` without underscores. Lenses can coexist with record selectors by importing the lens
  only where lens operations are needed::

    {-# LANGUAGE ScopedImports, NoImplicitPrelude, OverloadedStrings, BlockArguments #-}
    module LucidExample where

    import Lucid (Html)
    import Prelude ()

    myHtml :: Html ()
    myHtml = do
      let
        import Lucid (div, id)
        import Prelude (mempty)
      div [id "main"] mempty
      div [id "body"] "Hello World"
      div [id "footer"] do
        let
          import Lucid (a, href, head, map)
        head [id "footer-header"] "Goodbye"
        a [href "/links"]
        map [] "A map"

- **Qualifying**: Functions working with ``Text``, ``ByteString``,
  ``Map``, or ``Set`` can import the operations they need locally and unqualified, avoiding
  the clutter of qualifying ambiguous names::

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
  both versions. ``Prelude.(**)`` and ``Clay.(**)`` can coexist in the same module::

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

  import Control.Monad ()

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

**Learnability**: Minimal impact. Names brought into local scope via scoped import declarations follow
the same scoping rules as names defined locally.

**Maintenance burden**: Implementation of ``ScopedImports`` touches the AST and renamer.
No new type system features, no changes to Core, and no changes to runtime semantics or global module/instance resolution.

**Lexical Complexity**: Scoped import declarations can be abused to defined
whacky expressions where it becomes difficult to determine what names are in
the scope of an expression or not.
Users of the ``ScopedImports`` language extension need to be responsible.

Backward Compatibility
----------------------

**Level 0: No breakage.**

This is a completely opt-in language extension. Existing programs are unaffected.
The new syntax (import declarations in let/where) is currently a parse
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

**LocalModules GHC proposal**: The `LocalModules (#283) <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_
proposal subsumes the functionality of ``ScopedImports``.
``LocalModules`` is much more ambitious:

  - **Full module definitions**: LocalModules allows defining entire modules (with data types,
    classes, instances, type families) inside other modules. ScopedImports only allows
    importing existing names.

  - **New namespaces**: LocalModules creates new namespaces for grouping related definitions.
    ScopedImports only controls visibility of names from existing modules.

  - **Qualified re-exports**: LocalModules enables abstracting common import patterns via
    qualified re-exports. ScopedImports has no effect on exports.

  - **Implementation scope**: LocalModules requires changes to the AST, renamer, and interface
    file format. ScopedImports requires only AST and renamer changes.

  - **Complexity**: LocalModules is a significant extension to Haskell's module system.
    ScopedImports is a minimal syntactic convenience.

``ScopedImports`` addresses the most common use case (avoiding name conflicts) with minimal implementation complexity,
while ``LocalModules`` provides a comprehensive solution to module organization.

Unresolved Questions
--------------------

How does the ``ScopedImports`` language extension affect tooling like
Haskell Language Server (HLS)?

Will there need to be changes made to these tools to accomodate scoped import declarations?

Implementation Plan
-------------------

A working implementation of this proposal is available at
`https://gitlab.haskell.org/rgover/ghc <https://gitlab.haskell.org/rgover/ghc>`_
on the ``scoped-imports-v2`` branch.

*The implentation was done by Claude Opus 4.5 (Anthropic) and reviewed by the author of this proposal.
The implementation may be incorrect.*

The implementation touches the following areas:

1. **Parser** (``GHC/Parser.y``): Extended the grammar to allow ``importdecl``
   within ``decls``. Added productions for ``decls_and_imports`` and updated
   let and where clause handling.

2. **AST** (``Language/Haskell/Syntax/Binds.hs``): Added a new constructor
   ``HsLocalBindsWithImports`` to ``HsLocalBindsLR`` that wraps a list of
   import declarations alongside the existing local bindings.

3. **Renamer** (``GHC/Rename/Bind.hs``): Extended ``rnLocalBindsAndThen`` to
   handle scoped imports by renaming them and extending the local environment.
   The extension check happens here, producing a clear error message when
   ``ScopedImports`` is not enabled.

4. **Test suite**: Several tests covering basic usage, extension interactions,
   scoping behavior, and error cases.


Acknowledgments
---------------

The implementation was developed using AI-assisted programming.
Most of the code was generated by Claude Opus 4.5 (Anthropic) with
guidance and direction from me.
