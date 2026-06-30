Qualified Lists
===============

.. author:: Brandon Chinn
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/724>`_.
.. sectnum::
.. contents::

This proposal proposes replicatiing ``-XQualifiedDo`` for literal lists, to enable more ergonomic and more powerful syntax than ``OverloadedLists``. Another way to view this proposal would be replicatiing ``-XRebindableSyntax`` for literal lists, but only within a local scope.

See also:

* `QualifiedStrings <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_
* `QualifiedNumerics <https://github.com/ghc-proposals/ghc-proposals/pull/725>`_

Motivation
----------

Problems with Type Class-driven overloading
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``OverloadedLists`` works by desugaring to a type-class-overloaded function. That leads to certain general shortcomings:

* It is a module-wide setting

  * Anecdotally, people would rather avoid ``OverloadedLists`` than deal with overloaded lists in the entire module.

  * It's possible that this is one reason this extension isn't a default in GHC202X language editions, despite being in GHC for a long time.

* Type inference ambiguity.

  * Consider the following code:

    ::

      output :: Foldable t => t -> IO ()

      main = do
        output [False, True]

        -- output $ V.update myVec [(0, True)]

    This originally works with no extensions, due to the list literal being typed to concrete ``[Bool]``. But say the developer wants to call an API using ``Vector`` and use ``Vector`` literals; adding ``OverloadedLists`` would cause ambiguity to the existing location because it is now no longer concretely ``[Bool]`` but ``(IsList l, Item l ~ Bool) => l``.

This proposal would allow using a module qualifier to say precisely which function to desugar to, rather than using type classes, in a similar manner as ``-XQualifiedDo``. This would allow writing the previous code as

::

  {-# LANGUAGE QualifiedLists #-}

  main = do
    output [False, True]

    output $ V.update myVec V.[(0, True)]

The existing locations would continue working as ``[Bool]``, while the new literal would unambiguously desugar to the equivalent of ``V.fromList [(0, True)]``.

Inability to use heterogeneous lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With ``-XOverloadedLists`` we can never write the literal ``[4, "hello", True]``, because that desugars to ``fromList [4, "hello", True]`` which is ill-typed regardless of ``fromList``. That is annoyingly restrictive, because with heterogeneous lists, it's perfectly fine to write

::

  4 `HCons` "hello" `HCons` True `HCons` HNil :: HList [Int, String, Bool]

and it would be convenient to use list literals instead. This was even explicitly listed as a restriction in the original ``OverloadedLists`` `design <https://gitlab.haskell.org/ghc/ghc/-/wikis/overloaded-lists>`_.

This proposal would desugar list literals to a build-like form instead, so that ``M.[4, "hello", True]`` desugars to

::

  M.buildList 3 (\cons nil -> 4 `cons` ("hello" `cons` (True `cons` nil)))

For a suitable ``M.buildList``, this is enough to support heterogenous list literals: see *Section 4.2 Heterogeneous Lists*.

Proposed Change Specification
-----------------------------

Introduce ``-XQualifiedLists`` that desugars literal list syntax to function calls in a similar way to ``-XQualifiedDo`` (`docs <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html>`_, `proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst>`_), but with a modified desugaring (unlike the proposed ``-XQualifiedStrings``).

As long as the desugared expressions/patterns type check, users are free to define these functions however they want. No whitespace is allowed between the ``.`` and the module name / literal.

Currently, list literals have the following desugaring:

.. list-table::
    :align: left

    * - **Expression**
      - **Enabled extensions**
      - **Desugared expression syntax**
    * - ``[x, y]``
      -
      - ``x Prelude.: y Prelude.: Prelude.[]``
    * - ``[x, y]``
      - ``-XOverloadedLists``
      - ``GHC.Exts.fromListN 2 (x Prelude.: y Prelude.: Prelude.[])``

With ``-XQualifiedLists``, the desugaring is instead as follows:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``M.[]``
      - ``M.buildList 0 (\cons nil -> nil)``
    * - ``M.[x, y]``
      - ``M.buildList 2 (\cons nil -> x `cons` (y `cons` nil))``
    * - ``M.[x ..]``
      - ``M.enumFrom x``
    * - ``M.[x, y ..]``
      - ``M.enumFromThen x y``
    * - ``M.[x .. y]``
      - ``M.enumFromTo x y``
    * - ``M.[x, y .. z]``
      - ``M.enumFromThenTo x y z``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``M.[x, _, y]``
      - ``M.FromListCons x (M.FromListCons _ (M.FromListCons y M.FromListNil))``

``Data.List.Qualified.Experimental`` will initially live in ``ghc-experimental``, eventually merged into ``GHC.Exts`` alongside ``IsList``. It will contain the following definitions:

::

  import qualified GHC.Exts as L
  import qualified GHC.List as L

  buildList :: (IsList l, Item l ~ a) => Int -> ((a -> [a] -> [a]) -> [a] -> [a]) -> l
  buildList n f = L.fromListN n (L.build f)

  enumFrom :: (IsList l, Item l ~ a) => a -> l
  enumFrom x = L.fromList (Prelude.enumFrom x)

  enumFromThen :: (IsList l, Item l ~ a) => a -> a -> l
  enumFromThen x y = L.fromList (Prelude.enumFromThen x y)

  enumFromTo :: (IsList l, Item l ~ a) => a -> a -> l
  enumFromTo x y = L.fromList (Prelude.enumFromTo x y)

  enumFromThenTo :: (IsList l, Item l ~ a) => a -> a -> a -> l
  enumFromThenTo x y  z= L.fromList (Prelude.enumFromThenTo x y z)

It is highly recommended that all types with ``IsList`` instances defined provide a module with the below definitions, to enable locally-scoped overloading over ``-XOverloadedLists``, for example:

::

  import Data.List.Qualified.Experimental qualified as L

  buildList :: Int -> ((a -> [a] -> [a]) -> [a] -> [a]) -> MyList a
  buildList = L.buildList

  enumFrom :: a -> MyList a
  enumFrom L.enumFrom

  enumFromThen :: a -> a -> MyList a
  enumFromThen = L.enumFromThen

  enumFromTo :: a -> a -> MyList a
  enumFromTo = L.enumFromTo

  enumFromThenTo :: a -> a -> a -> MyList a
  enumFromThenTo  z= L.enumFromThenTo

Note that while we could have mirrored ``-XOverloadedLists`` and just done ``M.fromListN 2 [x, y]``, we intentionally decide to use this more general API. This gives us more expressive power, since we no longer need to typecheck an intermediate list. Similar reason for defining ``enumFrom*`` functions instead of reusing Prelude's ``enumFrom*`` functions. See *Section 4.2 Heterogeneous Lists* for a use-case.

We also decide to do ``M.buildList`` instead of something like ``M.fromList (x `M.cons` M.nil)`` so that there's one definition to jump to (e.g. with IDE integrations) instead of three.

To use as patterns, the implementor should define ``FromListCons`` and ``FromListNil`` pattern synonyms, typically with the ``COMPLETE`` pragma specified. We choose to do this instead of ``toList -> [x, _, z]`` because that would also disallow heterogeneous lists.

Parser
~~~~~~

Update `Section 10.5 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ of the Haskell 2010 report as follows.

.. code-block:: abnf

  aexp → qvar
       | ...
       | modid . [ exp_1 , ..., exp_k ]

  apat → var [ @ apat ]
       | ...
       | modid . [ pat_1 , ..., pat_k ]

Module name resolution
~~~~~~~~~~~~~~~~~~~~~~

Module names are resolved immediately, when parsing a quote. This matches the behavior of resolving modules in normal qualified values in quotes.

::

  module A where

  import OneImpl qualified as M

  -- Immediately resolves to OneImpl.[1, 2]
  -- Errors if M is not in scope
  foo = [| M.[1, 2] |]

Proposed Library Change Specification
-------------------------------------

Template Haskell
~~~~~~~~~~~~~~~~

We'll add the following constructors instead of modifying the existing ``ListE`` constructor, to maintain backwards compatibility:

::

  data Exp
    = ...
    | QualListE ModName [Exp]

Examples
--------

Vector
~~~~~~

Currently, if you want to pattern match on ``Vector`` from the `vector package <https://hackage.haskell.org/package/vector>`_, you have to use ``OverloadedLists`` (which enables it for list literals in the entire file) or be verbose:

::

  case user of
    -- guard
    User{tags = tags} | ["a", tag2] <- V.toList tags -> _
    -- with ViewPatterns
    User{tags = (V.toList -> ["a", tag2])} -> _

With ``QualifiedLists``, ``vector`` could define:

::

  module Data.Vector.Qualified where

  import Data.List.Qualified.Experimental qualified as L

  buildList :: Int -> ((a -> [a] -> [a]) -> [a] -> [a]) -> Vector a
  buildList = L.buildList

  pattern FromListCons a b <- (V.uncons -> Just (a, b))
  pattern FromListNil <- (V.uncons -> Nothing)
  {-# COMPLETE FromListCons, FromListNil #-}

And the user could do:

::

  import Data.Vector.Qualified qualified as V

  case user of
    User{tags = V.["a", tag2]} -> _

One scenario this can come up is when parsing ``Aeson.Array``, which stores JSON values in a ``Vector``.

Heterogeneous Lists
~~~~~~~~~~~~~~~~~~~

With ``QualifiedLists``, converting list literals are no longer confined to the list type, enabling list literal syntax for heterogenous lists (aka ``HList``):

::

  module Data.HList.Qualified where

  buildList ::
    Int ->
    ( (forall a as. f a -> HList f as -> HList f (a ': as))
      -> HList f '[]
      -> HList f xs
    ) ->
    HList f xs
  buildList _ f = f HCons HNil

  pattern FromListCons :: () => xs ~ (x0 ': xs0) => f x0 -> HList f xs0 -> HList f xs
  pattern FromListCons a b = HCons a b

  pattern FromListNil :: () => xs ~ '[] => HList f xs
  pattern FromListNil = HNil

  {-# COMPLETE FromListCons, FromListNil #-}

Users could then do

::

  import Data.HList.Qualified qualified as HList

  HList.[Just True, Just 1, Nothing] :: HList Maybe '[Bool, Int, String]

  -- hlist matches all the patterns below
  case hlist of
    HList.[Just True, _, Nothing] -> _
    HList.[_, Just 1, Nothing] -> _
    Just _ HList.: _ -> _

ByteArray
~~~~~~~~~

Example of a ``ByteArray`` implementation, which requires knowing the length of the list in advance.

::

  type Builder s = Int -> MutableByteArray s -> ST s ()

  buildList ::
    forall a. Prim a =>
    Int ->
    ( forall s.
      (a -> Builder s -> Builder s)
      -> Builder s
      -> Builder s
    ) ->
    ByteArray
  buildList n f = createByteArray (n * sizeOfType @a) $ f cons nil
    where
      nil :: Builder s
      nil = \_ _ -> pure ()

      cons :: Prim a => a -> Builder s -> Builder s
      cons x next = \i arr -> writeByteArray arr i x >> next (i + 1) arr

  -- [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40]
  print ByteArray.[1, 2]

Effect and Interactions
-----------------------

With ``QualifiedLists``, there's no more typeclass ambiguity; e.g. if the ``vector`` library provided ``Data.Vector.Qualified`` as described in *Section 4.1 Vector*, users can do

::

  import Data.Vector.Qualified qualified as V

  main = print V.[False, True]

The equivalent code with ``OverloadedLists`` would have failed to compile with ``Couldn't match expected type 'Item a0' with actual type 'Bool'``.


Interactions with other extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Related to `QualifiedStrings <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_ and `QualifiedNumerics <https://github.com/ghc-proposals/ghc-proposals/pull/725>`_, but all three proposals are orthogonal to each other.

* `Allow arbitrary identifiers as fields in OverloadedRecordDot <https://github.com/ghc-proposals/ghc-proposals/pull/668>`_ has similar syntax to the proposed qualified string literal, but as ``M.bar`` is parsed as a qualified identifier even with OverloadedRecordDot, it makes sense that ``M."bar"`` is also parsed as a qualified literal.

Costs and Drawbacks
-------------------

Development and maintenance should be low effort, as the core implementation is in the renamer step, and typechecking would proceed as normal.

The syntax is approachable for novice users and shouldn't be an extra barrier to understand.

Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.

Furthermore, turning on the extension will generally not break existing code. Any existing code written as ``M.[1, 2]`` would be parsed as function composition between a data constructor and a literal, which would only typecheck if someone adds an ``IsList`` instance for a function type.

Alternatives
------------

* Use ViewPatterns for list literals in patterns

  * This prevents marking list patterns as COMPLETE

* Use separate ``M.fromListN`` instead of ``M.buildList``

  * Disallows heterogeneous lists
  * See the discussion in *Section 2 Proposed Change Specification*

Future work
~~~~~~~~~~~

* Some literals are not supported yet (Chars, unboxed literals) due to lack of use-cases, but could be extended in the future.

* Future work could be done to allow compile time logic, e.g. ``$M.[1, 2]`` => ``$(M.buildList 2 $ \cons nil -> [|1|] `cons` [|2|] `cons` nil)``, but that is out of scope of this proposal.

* Future work could be done to allow list comprehensions, e.g. ``M.[x * 10 | x <- [1..10]]`` => ``[1..10] `M.listCompBind` \x -> M.listCompReturn (x * 10)``, but that is out of scope of this proposal.

Unresolved Questions
--------------------

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.

Endorsements
------------
