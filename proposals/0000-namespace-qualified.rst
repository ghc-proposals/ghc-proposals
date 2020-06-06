Namespace-qualified imports
===========================

.. author:: Vladislav Zavialov
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/340>`_.
.. contents::

This proposal introduces a uniform way to specify the namespace (type or data)
from whence a name comes. It does so by reusing an existing mechanism: module
qualification.

Motivation
----------

There's currently a menagerie of ways by which one can specify the namespace
(type or data) of a name. For the following examples assume ``data T = T`` and
``type family a $ b``:

* With ``-XDataKinds``, namespace disambiguation is done by adding ``'`` in
  front of a name:

  * ``p :: Proxy T`` refers to the type constructor
  * ``p :: Proxy 'T`` refers to the data constructor

* With ``-XTemplateHaskell``, namespace disambiguation is done by the choice
  between ``'`` and ``''``:

  * ``$(deriveStuff ''T)`` refers to the type constructor
  * ``$(deriveStuff 'T)`` refers to the data constructor

* In module import and export lists, namespace disambiguation is done by
  (pseudo-)keywords ``type`` and ``pattern``:

  * ``import M (type T)`` refers to the type constructor
  * ``import M (pattern T)`` refers to the data constructor (not necessarily a pattern synonym)

* In fixity declarations, according to `#65
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst>`_,
  namespace disambiguation is done by (pseudo-)keywords ``type`` and ``value``:

  * ``infixr 0 type $`` refers to the type operator
  * ``infixr 0 value $`` refers to the ordinary operator

* In ``WARNING`` pragmas, according to `#65
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst>`_,
  namespace disambiguation is done by (pseudo-)keywords ``type`` and ``value``:

  * ``{-# WARNING type ($) "danger" #-}`` refers to the type operator
  * ``{-# WARNING value ($) "danger" #-}`` refers to the ordinary operator

So we have ``'``, ``''``, ``type``, ``pattern`` and ``value``, where
``pattern`` does not necessarily refer to a pattern, and ``'`` means different
things with ``-XDataKinds`` and ``-XTemplateHaskell``.

What would a unified, simple approach to namespace disambiguation look like? To answer this, observe
that in Haskell, there are other sources of ambiguous names::

  import A.B.C as M (T)
  import D.E.F as N (T)

Here, even though both ``T`` are type constructors, they come from different
modules. Every occurrence must be disambiguated using either the full module
name (``A.B.C.T`` vs ``D.E.F.T``) or an alias (``M.T`` vs ``N.T``).

The idea behind this proposal is to extend this mechanism to introduce separate
aliases for the type namespace and the data namespace. For example::

  module E where
    data X = X

  module M where
    import Data.Proxy
    import E as (D,T)

    p1 :: Proxy D.X
    p2 :: Proxy T.X

Here, ``(D,T)`` is a compound alias, which qualifies every data constructor
from ``E`` with ``D`` and every type constructor from ``E`` with ``T``.

Hence, ``D.X`` refers to the data constructor, and ``T.X`` refers to the type
constructor.

A way to remember the ordering in a compound alias ``(A,B)`` (data namespace on
the left, type namespace on the right) is that in ``A :: B`` we also have data
on the left, types on the right.

It's possible to omit one component of a compound alias, writing ``(D,_)`` or
``(_,T)``.

This syntax is not quite sufficient to achieve feature parity with
``-XExplicitNamespaces``. How do we write an unqualified import of a name from
a specific namespace?::

  import Data.Proxy (type Proxy)

For this, we introduce another minor feature, import from alias::

  import qualified Data.Proxy as (_, T)
  import T

Here, we import ``Data.Proxy`` qualified, with an alias ``T`` for its type
constructors.  And then we do an unqualified import from this alias, so the
user can write ``Proxy`` instead of ``T.Proxy``.

Compound aliases in imports allow us to disambiguate names that come from other
modules. But for names that come from the same module, we need one more piece
of syntax: module aliases.

First, observe that Haskell already allows self-qualification in a module::

  module MyModule where
    x = 5
    main = print MyModule.x

Here, ``MyModule.x`` is a qualified name for a definition that is defined in
the very same module.

We introduce the notion of a module alias::

  module MyModule as M where
    x = 5
    main = print M.x

Here, we can use a shorter name ``M`` for a local reference. And then we extend
this feature with the notion of compound aliases, as before::

  module MyModule as (D, T) where
    data X = X
    p1 :: Proxy D.X
    p2 :: Proxy T.X

With these small additions, the old namespace disambiguation mechanisms almost
become obsolete. There's one more thing: built-in syntax. Does ``[]`` refer to
the list type or its nil constructor? Does ``()`` refer to the unit type or the
unit value? With ``-XDataKinds``, disambiguation is done by ``'``, but our goal
is to make it unnecessary.

The solution is to export built-in type constructors from a new
``Prelude``-like module::

  module Data.BuiltInSyntax
    ( (~)  ()           -- the equality constraint
    , []   ( (:), [] )  -- the list type and its constructors
    , ()   ( ()   )     -- the unit type and its constructor
    , (,)  ( (,)  )     -- the pair type and its constructor
    , (,,) ( (,,) )     -- the triple type and its constructor
    , ...               -- ... and tuples of other arities
    ) where

This module is imported by default, like ``Prelude``. And in the same manner,
it can be imported qualified instead::

  import qualified Data.BuiltInSyntax as (D, T)
  import D

With such an import, ``[]`` unambiguously refers to the nil data constructor,
whereas the list type is written ``T.[]``.

Furthermore, if we are to remove the ``'`` of ``-XDataKinds``, we need a new
way to disambiguate between  ``[a]`` (the type of a list) and ``'[a]`` (a
single-element type-level list); and between ``(a,b)`` (the type of a pair) and
``'(a,b)`` (a pair of types).

We sort this out by somewhat nuanced, but thought-out and backwards-compatible
rules for desugaring of ``[a]`` and ``(a,b)``:

* ``(a, b)`` is desugared as ``(,) a b`` for the ``(,)`` in the given context
  (be it a type constructor or a data constructor).
* When in a given context ``[]`` is a type constructor, ``[a]`` is desugared as
  ``[] a``; and when it is a data constructor, ``[a]`` is desugared as ``a :
  []``.

By default, today's behavior is preserved, and ``[a]`` means a single-element
list when it's to the left of ``::``, and the type of a list when it is to the
right of ``::``. But with the appropriate arrangement of imports, ``[]`` can
unambiguosly refer to the nil data constructor, and ``[a]`` would mean
single-element list both at the term-level and at the type-level. The user is
advised to define ``type List = T.[]`` to write the type of a list as ``List
a``.

Note that Haskell 98 or Haskell 2010 programs are not affected, and only users
of ``-XDataKinds`` will notice.

We propose to deprecate and eventually remove the old namespace disambiguation
methods over the course of 16 releases (8 years). This period will allow for a
graceful migration without ``-XCPP``. It's important to remove the old
mechanisms to keep the language tidy, lest features accrete indefinitely and
make the language too large and too complex to comprehend.

Proposed Change Specification
-----------------------------

1.  Take the Haskell Report context-free grammar as the starting point. Introduce
    new non-terminals::

      alias -> modid
             | [modid] ( modid', modid' )

      modid' -> modid
              | _

    Modify ``impdecl`` as follows::

      - impdecl -> import [qualified] modid [as modid] [impspec]
      + impdecl -> import [qualified] modid [as alias] [impspec]

    Modify ``module`` as follows::

      - module modid            [exports] where body
      + module modid [as alias] [exports] where body

    The use of compound aliases is to be guarded behind a new extension,
    ``-XNamespaceAliases``.  The use of module aliases is to be guarded behind a
    new extension, ``-XModuleAliases``.

2. A module alias ``module MyModule as M`` allows qualification of entities defined
   in the current module.

3. A compound alias ``as (D, T)`` introduces two aliases, ``D`` and ``T``.

   * Names qualified with ``D`` are unambiguously selected from the data
     namespace.

   * Names qualified with ``T`` are unambiguously selected from the type
     namespace.

   One (but not both) parts of a compound alias can be ``_``, which does not
   create a qualifier for that namespace.

4. A compound alias ``as M (D, T)`` introduces both a normal alias ``M`` and
   namespace-specific aliases ``D`` and ``T``.

5. Allow importing from an alias defined in the same module::

    import qualified Data.Proxy as (T, D)
    import qualified Data.Functor as (T, D)
    import D (Proxy, Identity)

6. Extend Template Haskell name quotation ``'T`` to look in both type and data
   namespaces, with priority given to the data namespace in case of ambiguity.

7. Add a new module, ``Data.BuiltInSyntax``, imported by default unless the user
   passes ``-XNoImplicitBuiltInSyntax`` to the compiler. Its behavior mirrors
   that of ``Prelude`` and ``-XNoImplicitPrelude``.

8. Type constructors ``(~)``, ``[]``, ``()``, ``(,)``, ``(,,)``, ``(,,,)``, and
   so on, and their associated data constructors ``[]``, ``(:)``, ``()``,
   ``(,)``, ``(,,)``, ``(,,,)``, and so on, are no longer built-ins, and come
   into scope from ``Data.BuiltInSyntax``. The user is allowed to rebind those
   constructors, e.g. ``data [a] = !a : ![a]`` defines a strict list.

   This change is not observable by users who neither enable ``-XNoImplicitBuiltInSyntax``
   nor write an explicit import declaration for ``Data.BuiltInSyntax``.

9. The ``(a,b)`` syntax means ``(,) a b``, where ``(,)`` is according to the
   scoping rules in the given context. This also applies to tuples of other
   arities.

10. The ``[a]`` syntax is treated as follows:

    1. Look up ``[]`` according to the scoping rules in the given context.
    2. If ``[]`` came from the type namespace, treat ``[a]`` as ``[] a``.
    3. If ``[]`` came from the data namespace, treat ``[a]`` as ``a : []``.

11. Introduce a new warning, ``-Wold-namespace-qualifiers``, which warns on
    ``''`` (of ``-XTemplateHaskell``), ``'`` (of ``-XDataKinds``), ``pattern``
    (of ``-XPatternSynonyms``), ``type`` (of ``-XExplicitNamespaces``). Revert
    the part of #65 that introduces the ``value`` pseudo-keyword.

    * In the next release (0.5 years in), add ``-Wold-namespace-qualifiers`` to ``-Wcompat``.
    * In the next release (1 year in), do nothing.
    * In the next release (1.5 years in), add ``-Wold-namespace-qualifiers`` to ``-Wall``.
      Users who support the last three compiler versions can transition without ``-XCPP``.
    * For six releases (4.5 years in), do nothing.
    * In the next release (5 years in), deprecate the old syntax and enable
      ``-Wold-namespace-qualifiers`` by default.
    * For five releases (7.5 years in), do nothing.
    * In the next release (8 years in), drop the support for the old syntax from GHC.

12. When ``[a]`` is desugared into ``a : []``, and there's a kind mismatch such
    that the expected kind is ``Type``, the error message must account for
    the possibility that the user meant a list type by that, and should include
    a useful hint::

     $ ghci -XDataKinds
     ghci> import qualified Data.BuiltInSyntax as (D, _)
     ghci> import D
     ghci> data D a = MkD a
     ghci> :kind D [Int]

     <interactive>:1:3: error:
         • Expected a type, but ‘[Int]’ has kind ‘[Type]’
         • In the first argument of ‘D’, namely ‘'[Int]’
           In the type ‘D '[Int]’
         • NB. The list type constructor is not in scope,
               so [Int] is a single element list, not the type of a list.

    This is the same as the current message, but note the added "NB".

Examples
--------

``-XDataKinds`` without ``'``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Old::

  module M where
    import Data.Proxy
    p :: Proxy '[ '(Int, 'Proxy) ]

New::

  module M where
    import Data.Proxy as (D, T)
    import qualified Data.BuiltInSyntax as (BuiltInData, _)
    import BuiltInData

    p :: T.Proxy [(Int, D.Proxy)]

``-XTemplateHaskell`` without ``''``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Old::

  module M where
    data Foo = Foo
    $(makeLenses ''Foo)

New::

  module M as (_, T) where
    data Foo = Foo
    $(makeLenses 'T.Foo)

Namespace-specific imports
~~~~~~~~~~~~~~~~~~~~~~~~~~

Old::

  import Data.Proxy as D (pattern Proxy, pattern KProxy)
  import Data.Proxy as T (type Proxy, type KProxy)

New::

  import Data.Proxy as (D, T)

Namespace-specific exports
~~~~~~~~~~~~~~~~~~~~~~~~~~

Old::

  module M (type Foo) where
    data Foo = Foo

New::

  module M as (_, T) (T.Foo) where
    data Foo = Foo

Namespace-specific fixitiy declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Old (with #65)::

  module M where
    type family a $ b
    infixr 0 type $

New::

  module M as (_, T) where
    type family a $ b
    infixr 0 T.$

Effect and Interactions
-----------------------

The users of ``-XDataKinds`` and ``-XTemplateHaskell`` will have to port their
code to use the new feature over the course of 8 years. Other than that, the
proposal is opt-in.

Costs and Drawbacks
-------------------

The users may be reluctant to use the new syntax, as old habits die hard.

Alternatives
------------

* Support old namespace disambiguation syntax indefinitely, if we deem
  backwards-compatibility with 8-year old code more important than a clean and
  simple language.

* `#214 <https://github.com/ghc-proposals/ghc-proposals/pull/214>`_ introduces
  new syntax, ``data.`` and ``type.`` to disambiguate namespaces at every
  occurrence. The main disadvantage is that it leads to verbose code, e.g. ``a
  : data.[]`` instead of ``[a]``.

* Other syntax is possible for compound aliases (an older version of this
  proposal used ``{D,T}``, and ``{data D, type T}`` was also considered).

Unresolved Questions
--------------------

* The proposed syntax provides no way to do an unqualified import of an
  entire namespace. That is, there's no way to say "import all type
  constructors from M, and nothing else".

  It's unclear whether this functionality would be useful.

Implementation Plan
-------------------

I (Vladislav Zavialov) or a close collaborator (Artyom Kuznetsov) will
implement this change.
