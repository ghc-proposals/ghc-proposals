LocalImports
============

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

This GHC proposal introduces the ``LocalImports`` language extension allowing users
to use import declarations inside of ``let`` and ``where`` binding groups to bring names
into scope.

Motivation
----------

Names overlap across different modules.
If a module imports the same name from 2 or more modules, this results in a **naming conflict**.

Currently, we can use the following methods to avoid naming conflicts.

**Naming Conventions:**

For example, the ``lucid`` HTML library suffixes the names it exports with
``_`` to avoid naming conflicts with ``Prelude``::
  
  div_
  head_
  id_
  map_

Another example is lense names generated using Template Haskell.
The standard lens generation function, ``makeLenses``,
requires users to prefix record field labels with ``_`` to avoid
naming conflicts::

  data User = User
    { _name  :: Text
    , _email :: Text
    }

  makeLenses ''User  -- generates: name, email lenses

These arbitrary naming conventions exist solely for avoiding naming conflicts.
If 2 or more imported modules share the same naming convention, then the
naming convention becomes useless.

**Hiding:**

For example, the ``clay`` CSS library exports an operator named ``(**)``,
which causes a naming conflict with Prelude's ``(**)`` operator.

When using Prelude and Clay in the same module,
the recommendation is to hide ``(**)`` from Prelude::

  import Prelude hiding ((**))
  import Clay

The ``(**)`` operator from ``Prelude`` can't be used in any expression
within the module, even if those expressions don't depend on ``Clay`` at all.

**Qualifying:**

Modules from the ``containers`` package such as ``Data.Map`` and ``Data.Set`` share many names.
Modules can use qualified import declarations to avoid name collisions::

  import qualified Data.Map
  import qualified Data.Set

  memberCount :: Ord a => a -> Data.Map.Map a Int -> Data.Set.Set a -> Int
  memberCount key map set =
    case Data.Map.lookup key map of
      Just n | Data.Set.member key set -> n
      _ -> 0

  ...

This workaround suffers from the same issues as naming conventions.

To alleviate some pain, we can use module renaming to shorten the qualifier::

  import qualified Data.Map as M
  import qualified Data.Set as S

  memberCount :: Ord a => a -> M.Map a Int -> S.Set a -> Int
  memberCount key map set =
    case M.lookup key map of
      Just n | S.member key set -> n
      _ -> 0

  ...

<TODO>
People reading this module may have to scroll up to the top of
the module to remember what *modid* refers to what module.
This can be tedious in large modules depending on the person and the tooling they're using.
<TODO>

Naming conflicts are caused by *namespace pollution*. We generally want to minimize
namespace pollution within our modules, and the ability to *locally scope* names
is the primary way of solving this problem in related GHC proposals and programming languages.

Prior art
~~~~~~~~~

**Other proposals:**

The `LocalModules <>`_ proposal mentions similar motivations for minimizing
namespace pollution and naming conflicts.

These motivations, such as

  When a function ``f`` needs a helper ``h``, we can declare ``h`` in a
  ``where`` clause. However, suppose both ``f`` and ``g`` need ``h``. Now,
  ``h`` must be declared at the top level, meaning all the rest of the
  definitions in the module can see ``h``, even if ``h`` is really very
  specific to ``f`` and ``g``.

  With this proposal, we can model this situation nicely::

    module (f, g) where
      f :: ...
      f = ...

      g :: ...
      g = ...

      h :: ...
      h = ...

and

  If a function or group of functions needs to work with a datatype or class
  locally, there is no way to do this without polluting the namespace of the
  entire file.

  With this proposal, we can do this easily::

    module (f) where
      f :: ...
      f = ...

      data SpecialDataType = ...

      class LocalClass a b c where ...

and

  When importing a library qualified, there are often stretches of code which
  work with the imported type, performing many operations on values of that type.
  In these stretches, the qualifications can become noisy. ::

    module Main where

    import qualified Data.Set as Set

    frobbleSets :: Set.Set Bool -> Set.Set Int -> Set.Set Char
    frobbleSets = ...

  In ``frobbleSets``, there will be many ``Set.`` qualifications.

, can be satisfied with the ``LocalImports``.

**Other programming languages:**

Several languages support a feature similar to ``LocalImports``,
usually known as *locally scoped imports* or *module opening*.

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

All 4 languages allow imports with lexical scoping,
proving that this is a useful feature for programmers in general.

Proposed Change Specification
-----------------------------

This proposal introduces a new language extension, ``LocalImports``, which
extends the grammar to allow import declarations within ``let`` and ``where``
binding groups.

Syntax
~~~~~~

The ``LocalImports`` language extension extends the ``decl`` production defined in the
`Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html>`_.

**Haskell 2010:**::

  decl -> gendecl
        | (funlhs | pat) rhs

**Haskell 2010 + LocalImports:**::

  decl -> gendecl
        | (funlhs | pat) rhs
        | importdecl

Where ``importdecl`` is an import declaration as defined in
`Section 5.3 of the Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_.

If::
  
  decls	-> { decl₁ ; ... ; declₙ } where (n >= 0)

then import declarations are allowed in all syntax depending on the extended ``decls``
production rule.

This includes:

- `Let expressions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-440003.12>`_

- `Where clauses <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13>`_, `function bindings, and pattern bindings <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-830004.4.3>`_

- Let binding groups inside of `do expressions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14>`_,
  `list comprehensions <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11>`_,
  and `guards <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#dx17-180049>`_

These special import declarations are called *local import declarations*. The names they may bring
in scope are called *local imports*.

``LocalImports`` language extension does not change where other top-level declarations
(``topdecl``) such as:

- Class declarations

- Instance declarations

- Extensions to the ``topdecl`` production rule such as pattern synonyms

are allowed.

Semantics
~~~~~~~~~

When the ``LocalImports`` extension is on, a distinction is made between traditional
*top-level import declarations* and *local import declarations*.

Local import declarations may appear in let binding groups::

  f = let import M (p) in ...

and where clauses::

  g = ... where import M (q)

. The *names* they bring into scope are called *local imports*. These may include

- Terms
- Types
- Type synonyms
- Type families
- Data families
- Constructors/Patterns
- Pattern synonyms
- Classes
- Class methods

Required top-level import
#########################

Any module named in local import declarations must be named in a top-level import declaration.

For example, ::

  {-# LANGUAGE LocalImports #-}

  module M where

  f = let import N (p) in ...

  g = ... where import N (q)

is invalid and will result in a compile-time error.

If we want to use ``N`` in local import declarations,
we must use it in a top-level import declaration::

  {-# LANGUAGE LocalImports #-}

  module M where

  import N ()

  f = let import N (p) in ...

  g = ... where import N (q)

. The top-level import declaration may have an empty import list.

A top-level import declaration for local import declarations
is required for 2 reasons:

- Makes it explicit that Haskell's instance resolution semantics are unchanged
- Allows GHC and tooling, like Haskell Language Server, to keep track of module dependencies

The only module that doesn't require a top-level import declaration is Prelude,
since it's imported into every module implicitly.

If ``NoImplicitPrelude`` is enabled,
and you want to use ``Prelude`` in a local import declaration,
then you will need a top-level import declaration for Prelude
like all other modules::

  {-# LANGUAGE NoImplicitPrelude, LocalImports #-}

  module M where

  import Prelude () -- Required, since Prelude isn't in scope by default

  main = print two where
    import Prelude (print, (+), Int)
    two :: Int
    two = 1 + 1

Invalid local import declarations
#################################

Local import declarations must not be empty and must not import any *record field labels*.

Local import declarations of the form::

  import N ()

are invalid and will result in a compile-time error.
If we want to import instances only,
the import declaration must be top-level.

Local import declaration of the form::

  import N (T(l))

and::
  
  import N (T(..))

, where ``T`` is a type and ``l`` is a record field label,
are invalid and will result in a compile-time error.
If an import declaration imports any record field label,
the import declaration must be top-level.

These restrictions on local import declarations
make it explicit to users that instance and record field
label resolution behavior is unchanged when
``LocalImports`` is enabled.

Valid local import declarations
###############################

As long as an import declaration references a module in the
global scope of the module its defined, and it doesn't import record field labels
, it's valid::

  {-# LANGUAGE LocalImports #-}

  module M where

  import N ()

  a = x where import N -- open import

  b = x + y * z where import N (x, y, z) -- explicit import

  c = z where import N hiding (x, y) -- imports everything from N except x, y, and z

  d = N.y where import qualified N -- qualified import

  e = P.y where import qualified N as P

Qualifier shadowing
###################

GHC raises a ``-Wname-shadowing`` warning when a name in an
inner scope overlaps with a name in an outer scope::

  y = 5

  x = let y = 10 in y -- 10, overrides the original y = 5

It's allowed, and the name referenced in the innermost scope will be used,
but it is often discouraged, and can even be made an error depending on the
codebase.

``LocalImports`` adds a new warning called ``-Wqualifier-shadowing`` that does
the same thing, but for module qualifiers::

  import qualified N

  x = let import qualified X as N in N.y -- The qualifier actually refers to the X module!

Like name shadowing, qualifier shadowing is discouraged, but allowed.
Users of ``LocalImports`` can configure GHC to raise an error in these cases
if desired.

Principles
~~~~~~~~~~

``LocalImports`` furthers GHC's adherence to the
`Lexical Scoping Principle (LSP) <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#221lexical-scoping-principle-lsp>`_.
``LocalImports`` allows programmers to scope imports to the exact expression they're used in,
making it clearer what an expression depends on.

``LocalImports`` is also potentially very useful as GHC implements the
`Syntactic Unification Priniciple <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#211syntactic-unification-principle-sup>`_.

If type-level let expressions and where clauses are added to GHC in the future,
this modified `example from the LocalModules proposal <>`_ may be possible.

Consider a data kind called ``Nat``::

  data Nat = Zero | Succ Nat

, and a GADT that shares constructors with the ``Nat`` data kind
and uses the ``Nat`` data kind constructors in its own constructor
type annotations::

  data Fin :: Nat -> Type where
    Zero :: Fin (Succ n)
    Succ :: Fin n -> Fin (Succ n)

Defining ``Nat`` and ``Fin`` in the same module is not possible, and isn't made possible with
``LocalImports`` alone.

I could define ``Fin``, if ``Nat`` and ``Fin`` lived in two different modules,
and I use module qualification. ``Nat`` would look like::

  module Nat where

  data Nat = Zero | Succ Nat

and ``Fin`` would look like::

  module Fin where

  import qualifed Nat

  data Fin :: Nat -> Type where
    Zero :: Fin (Nat.Succ n)
    Succ :: Fin n -> Fin (Nat.Succ n)

Defining ``Fin`` using these names, even in its own module,
is impossible without using module qualification.

With ``LocalImports`` and type-level let expressions, I could define ``Fin`` as::

  module Fin where

  import Nat (Nat)

  data Fin :: Nat -> Type where
    Zero :: let import Nat(Succ) in Fin (Succ n)
    Succ :: let import Nat(Succ) in Fin n -> Fin (Succ n)

This is a potential application of ``LocalImports``.

Examples
--------

Basic usage
~~~~~~~~~~~

Import in a where clause:

::

  {-# LANGUAGE LocalImports #-}

  module TestSum where

  import Data.List ()

  testSum :: Int
  testSum = sum [1, 2, 3, 4, 5]  -- 15
    where import Data.List (sum)

Import in a let expression:

::

  {-# LANGUAGE LocalImports #-}

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

  {-# LANGUAGE LocalImports #-}

  module TestMultiple where

  import Data.Char ()
  import Data.List ()

  testMultiple :: (String, Int)
  testMultiple = (str, length (sort [3, 1, 2]))
    where
      import Data.Char (toUpper)
      str = map toUpper "abc"
      import Data.List (length, sort)

Mutual recursion
~~~~~~~~~~~~~~~~

Mutually recursive functions can each have their own scoped imports:

::

  {-# LANGUAGE LocalImports #-}

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

  {-# LANGUAGE LocalImports #-}

  module TestQualified where

  import Data.List ()

  testQualified :: [Int]
  testQualified = L.sort [3, 1, 2]  -- [1, 2, 3]
    where import qualified Data.List as L

Guards
~~~~~~

Scoped imports in a where clause are available in guards:

::

  {-# LANGUAGE LocalImports #-}

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

  {-# LANGUAGE LocalImports #-}

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

  {-# LANGUAGE LocalImports #-}

  module TestDoBlock where

  import Data.Char ()

  example :: IO ()
  example = do
    input <- getLine
    let import Data.Char (toUpper)
    putStrLn (map toUpper input)

Note that ``toUpper`` is only visible to statements *later* in the do block.

Per-clause imports
~~~~~~~~~~~~~~~~~~

Each equation of a function can have its own where clause, and therefore
its own scoped imports.

::

  {-# LANGUAGE LocalImports #-}

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

  {-# LANGUAGE LocalImports, ScopedTypeVariables #-}

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

  {-# LANGUAGE LocalImports #-}

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

  {-# LANGUAGE LocalImports #-}

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

``LocalImports`` directly addresses the namespace pollution issues raised in the
Motivation:

- **Naming Conventions**: Libraries like ``lucid`` can potentially export ``div``, ``head``,
  ``id`` without underscores::

    {-# LANGUAGE LocalImports, NoImplicitPrelude, OverloadedStrings, BlockArguments #-}

    module LucidExample where

    import Prelude ()
    import Lucid (Html)

    page :: Html ()
    page = do
      header
      main
      footer

    header :: Html ()
    header = header [] $ h1 [] "Hello" where
      import Prelude ($)
      import Lucid (header, h1)

    main :: Html ()
    main = do
      let
        import Prelude (mempty)
        import Lucid (div, id)
      div [id "main"] mempty

    footer :: Html ()
    footer =
      let
        import Prelude (mempty)
        import Lucid (a, div, href, head, id, map, name)
      in
        div [id "footer"] do
          head [id "footer-header"] "Goodbye"
          a [href "/links"]
          map [name "empty_map"] mempty
  
  Lenses can coexist with record selectors by importing the lens
  only where lens operations are needed::

    {-# LANGUAGE TemplateHaskell #-}



- **Qualifying**: Functions working with ``Text``, ``ByteString``,
  ``Map``, or ``Set`` can import the operations they need locally and unqualified, avoiding
  the clutter of qualifying ambiguous names::

    {-# LANGUAGE LocalImports, NoImplicitPrelude #-}
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

    {-# LANGUAGE LocalImports, NoImplicitPrelude #-}
    module ClayExample where

    import Prelude (Double, ($))
    import Clay (Css, body, p, (?), padding, px, auto)

    squared :: Double
    squared = 2 ** 2
      where import Prelude ((**))

    buttonStyle :: Css
    buttonStyle = body ** p ? padding (px squared) auto auto auto
      where import Clay ((**))

``LocalImports`` has interesting synergy with other GHC language extensions:

QualifiedDo
~~~~~~~~~~~

Scoped qualified imports can provide the module qualification for ``QualifiedDo``:

::

  {-# LANGUAGE LocalImports, QualifiedDo #-}

  module TestQualifiedDo where

  import Control.Monad ()

  testQualifiedDo :: Maybe Int
  testQualifiedDo = M.do
      x <- Just 5
      y <- Just 7
      M.return (x * y)
    where
      import qualified Control.Monad as M

QualifiedStrings
~~~~~~~~~~~~~~~~

Qualified local imports can provide the module qualification for
``QualifiedStrings`` (proposal `#723 <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_)::

  {-# LANGUAGE QualifiedStrings, LocalImports #-}

  module Main where

  import Data.ByteString.Qualified.Ascii ()
  import Data.ByteString.Qualified.Utf8 ()
  import Data.ByteString qualified as BS

  main = do
    -- [98,108,97,158]
    print ascii

    -- [98,108,97,232,170,158]
    print utf8

  ascii = BS.unpack Q."bla語"
    where import Data.ByteString.Qualified.Ascii qualified as Q

  utf8 = BS.unpack Q."bla語"
    where import Data.ByteString.Qualified.Utf8 qualified as Q

RebindableSyntax
~~~~~~~~~~~~~~~~

``LocalImports`` works with ``RebindableSyntax``. Different functions in the
same module can have different do-notation semantics, for example:

::

  {-# LANGUAGE LocalImports, RebindableSyntax, NoImplicitPrelude #-}
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

**Maintenance burden**: Implementation of ``LocalImports`` touches the AST and renamer.
No new type system features, no changes to Core, and no changes to runtime semantics or global module/instance resolution.

**Lexical Complexity**: Local import declarations can be abused to defined
whacky expressions where it becomes difficult to determine what names are in
the scope of an expression or not. Users of the ``LocalImports`` language
extension need to be responsible.

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
proposal subsumes the functionality of ``LocalImports``.
``LocalModules`` is much more ambitious:

  - **Full module definitions**: LocalModules allows defining entire modules (with data types,
    classes, instances, type families) inside other modules. LocalImports only allows
    importing existing names.

  - **New namespaces**: LocalModules creates new namespaces for grouping related definitions.
    LocalImports only controls visibility of names from existing modules.

  - **Qualified re-exports**: LocalModules enables abstracting common import patterns via
    qualified re-exports. LocalImports has no effect on exports.

  - **Implementation scope**: LocalModules requires changes to the AST, renamer, and interface
    file format. LocalImports requires only AST and renamer changes.

  - **Complexity**: LocalModules is a significant extension to Haskell's module system.
    LocalImports is a minimal syntactic convenience.

``LocalImports`` addresses the most common use case (avoiding name conflicts) with
minimal implementation complexity, while ``LocalModules`` provides a comprehensive
solution to module organization.

Unresolved Questions
--------------------

How does the ``LocalImports`` language extension affect tooling like
Haskell Language Server (HLS)?

Will there need to be changes made to these tools to accomodate scoped import declarations?

How should ``LocalImports`` work with ``import safe``?

How should ``LocalImports`` interact with import declarations annotated with ``{-# SOURCE #-}``?

Implementation Plan
-------------------

A working implementation of this proposal is available at
`https://gitlab.haskell.org/rgover/ghc <https://gitlab.haskell.org/rgover/ghc>`_
on the ``local-imports-9.10`` branch.

*The implentation was done by Claude Opus 4.5 (Anthropic)
and reviewed by the author, someone with no experience contributing to GHC.
The current implementation may be incorrect.*

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
   ``LocalImports`` is not enabled.

4. **Test suite**: Several tests covering basic usage, extension interactions,
   scoping behavior, and error cases.

5. **Other**: Some other parts of GHC were also touched for adding a new warning
and making sure existing warnings like ``-Wunused-imports`` work with this language
extension. Specifically ``compiler/GHC/Tc/Utils/Backpack.hs`` and ``compiler/GHC/Tc/Utils/Monad.hs``.

Acknowledgments
---------------

The implementation was developed using AI-assisted programming.
Most of the code was generated by Claude Opus 4.5 (Anthropic) with
guidance and direction from me.
