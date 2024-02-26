Namespace-specified imports
===========================

.. author:: Adam Gundry, Artyom Kuznetsov, Chris Dornan
.. date-accepted:: 2023-07-15
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/23781
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/581>`_.
.. sectnum::
.. contents::

This proposal supports users who wish to write code without relying on punning,
by allowing imports of all names in either the type or data namespace.
Moreover it clarifies the specification of ``ExplicitNamespaces`` and related
extensions.

This topic has been discussed at length previously (see in particular `proposal
#214 <https://github.com/ghc-proposals/ghc-proposals/pull/214>`_, `proposal #236
<https://github.com/ghc-proposals/ghc-proposals/pull/236>`_ and `proposal #270
<https://github.com/ghc-proposals/ghc-proposals/pull/270>`_).  The current
proposal draws heavily from this previous work and benefits from efforts to
address these issues by Richard Eisenberg, Artyom Kuznetsov and Vladislav
Zavialov (amongst others).  This proposal is an attempt to systematise previous
ideas around ``ExplicitNamespaces`` and namespace-specified imports, primarily
from `proposal #270 <https://github.com/ghc-proposals/ghc-proposals/pull/270>`_.
In order to keep it narrowly focused, it does not address warnings for the use
of punning (since those are covered by `proposal #270
<https://github.com/ghc-proposals/ghc-proposals/pull/270>`_) nor changes outside
of import/exports.


Motivation
----------

Background
~~~~~~~~~~

Haskell has two namespaces:

* the *type namespace*, including the names of type constructors, type
  synonyms, type families and type classes;

* the *data namespace*, including the names of term-level values, functions,
  data constructors and pattern synonyms.

This separation allows us to define data constructors and type
constructors whose names coincide:

.. code:: haskell

   data T = T

(Similarly, with the ``TypeOperators`` extension it is possible to define
term-level operators and type families whose names coincide.)

At use sites, GHC infers which ``T`` is referred to from the context:

.. code:: haskell

   t :: T  -- type-level T
   t = T   -- term-level T

The use of identical names for type-level and term-level entities is called
*punning*.  Haskell makes heavy use of punning in its built-in syntax and common
types.

However, there are various contexts in which an occurrence of a name may refer
either to the type namespace or the data namespace, and it is not always clear
which is meant. In particular, the following may mention both type-level and
term-level entities:

* Import and export lists

* Fixity declarations

* ``WARNING``, ``DEPRECATED`` and ``ANN`` pragmas

* Types, when using the ``DataKinds`` extension to reference a data constructor
  at the type level

* Template Haskell name quotes

The simplest way to avoid namespace ambiguity is to avoid punning entirely, so
there is no need for the context to determine which namespace is meant.
`Proposal #270 <https://github.com/ghc-proposals/ghc-proposals/pull/270>`_
introduces warnings ``-Wpuns`` and ``-Wpun-bindings`` to alert users (who opt in
to the warnings) when they are introducing or relying on punning.

However, given the pervasive use of punning in the Haskell ecosystem, even users
who wish to avoid punning will inevitably end up importing modules which make
use of it. Thus we need mechanisms to disambiguate the namespace on import or at
use sites.


Mechanisms for disambiguating puns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Various mechanisms for disambiguating punned identifiers exist already:

* The ``DataKinds`` extension introduces the tick syntax (``'``) to select
  entities from the data namespace in a type-level context:

  .. code:: haskell

      p1 :: Proxy T   -- Refers to the type constructor T
      p1 = Proxy

      p2 :: Proxy 'T  -- Refers to the data constructor T
      p2 = Proxy

* ``TemplateHaskell`` name quotes use ``'`` for the data namespace and ``''``
  for the type namespace.

* `Proposal #65
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst>`_
  allows fixity declarations, ``WARNING`` and ``DEPRECATED`` pragmas to be
  modified with a ``value`` or ``type`` namespace specifier.  (This proposal
  has been accepted but not yet implemented at the time of writing.)

* ``ANN`` pragmas refer to the data namespace by default, but may use the
  ``type`` keyword to refer to the type namespace.

* The ``ExplicitNamespaces`` extension allows the ``type`` keyword to be used
  in an import or export list to select the type namespace, typically when
  using ``TypeOperators`` to define an operator that would otherwise be
  imported/exported in the data namespace.

* The ``PatternSynonyms`` extension allows the ``pattern`` keyword to be used
  in an import or export list to select the data namespace, typically when
  referring to a pattern synonym.  (However, it may also refer to a data
  constructor without its parent type constructor, a form of import/export
  which is not otherwise possible.)

However, the status quo has some problems:

* It is confusing and inconsistent that a prefix ``'`` has one meaning in terms
  (``TemplateHaskell`` name quotes) and a completely different meaning in types
  (with ``DataKinds``, use the data namespace rather than the type namespace).

* The data namespace is referred to by ``value`` in `proposal #65
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst>`_
  but ``pattern`` in import/export lists when using ``PatternSynonyms``.  We
  should pick a consistent keyword to refer to it, in the interests of
  simplicity.

* Users should not be required to enable ``PatternSynonyms`` and use the
  ``pattern`` keyword if all they actually want is to refer to a data
  constructor in an import/export list.

* The ``ExplicitNamespaces`` extension allows ``type`` to be used with a name
  in the data namespace (if it is unambiguous), see `GHC issue #22581
  <https://gitlab.haskell.org/ghc/ghc/-/issues/22581>`_.

* When users wishing to avoid punning are importing modules that define punned
  entities, they must make careful use of explicit import lists,
  ``ExplicitNamespaces`` and ``PatternSynonyms`` to avoid importing the same
  name into both namespaces.  It would be much simpler if they could
  selectively import "all names in the type namespace" or "all names in the
  data namespace" (perhaps with different module qualifiers).



Solution Overview
~~~~~~~~~~~~~~~~~

To help programmers deal with the external code that uses punning we propose to
extend the ``ExplicitNamespaces`` extension to allow the ``data`` and ``type``
keywords to be used as part of import or export lists, potentially with a ``..``
wildcard. For example:

.. code:: haskell

   import qualified Data.Proxy as T (type ..)   -- import only the type namespace
   import qualified Data.Proxy as D (data ..)   -- import only the data namespace

This avoids needing to name each item individually, but otherwise has the same
effect as writing out an explicit import list, like this:

.. code:: haskell

   import qualified Data.Proxy as T (type Proxy)   -- import only the Proxy type
   import qualified Data.Proxy as D (data Proxy)   -- import only the Proxy constructor

The ``data`` namespace specifier replaces the
existing limited use of ``pattern`` in import lists, guarded by ``PatternSynonyms``.
Moreover, for consistency this proposal modifies `proposal #65
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst>`_
to use ``data`` rather than ``value`` as the keyword for the data namespace in
fixity declarations and pragmas.

This proposal does not directly make changes to the tick syntax, or provide an
equivalent at use sites. However it should reduce the need for disambiguating
promoted data constructors using ticks, because qualified
imports can be used instead.


Proposed Change Specification
-----------------------------

Allow namespace specifiers with wildcards in import/export lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When ``ExplicitNamespaces`` is enabled, anywhere the ``type`` keyword may appear
in an import or export list, the ``data`` keyword may also appear.  Call such an
occurrence a *namespace specifier*. Any import/export of an identifier with a
namespace specifier will be taken to refer only identifiers in the given
namespace.  It is an error to use a namespace specifier if the identifier is not
in scope in the given namespace.

Moreover, a namespace specifier may be followed by a ``..`` wildcard instead of
a single name. This is equivalent to importing or exporting all the available
names in the corresponding namespace.

More precisely, the existing grammar of import/export items accepted by GHC is
essentially the following (after some minor simplifications): ::

      export -> qcname_ext ['(' qcname_ext_w_wildcard_1, ..., qcname_ext_w_wildcard_n ')']
             |  'module' modid
             |  'pattern' qcon  -- with PatternSynonyms

      qcname_ext_w_wildcard -> qcname_ext
                            |  '..'

      qcname_ext -> qvar
                 |  qtycon
                 | 'type' oqtycon  -- with ExplicitNamespaces

This proposal redefines ``qcname_ext`` as follows: ::

      qcname_ext -> qvar
                 |  qtycon
                 | 'type' oqtycon_w_wildcard  -- with ExplicitNamespaces
                 | 'data' qvarcon_w_wildcard  -- with ExplicitNamespaces

      oqtycon_w_wildcard -> oqtycon | '..'
      qvarcon_w_wildcard -> qvarcon | '..'

Notice that:

* ``module`` and ``pattern`` are valid only at the top level of the export,
  whereas ``type`` and ``data`` are valid either at the top or nested inside a
  type constructor or typeclass name.

* ``data`` may be followed by a data constructor name, a variable name
  (including record selectors, in particular), or a ```..`` wildcard.

* Where a parent type constructor or class is exported together with its
  children, any namespace specifier on an individual import/export item will
  apply only to the parent; the children are unrestricted.  For example,
  ``import M (type T(..))`` imports both ``T`` in the type namespace and any
  children in either namespace.


Dodgy import/export warnings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The existing ``-Wdodgy-imports`` and ``-Wdodgy-exports`` flags (part of the
``-W`` warning group) emit warnings if a ``..`` wildcard does not refer to any
names in scope.  For example this arises in an export of ``T(..)`` if ``T`` is a
type synonym or a data type with no constructors.

Similarly, under this proposal a warning will be emitted if a ``type ..`` or
``data ..`` item does not refer to any names in the corresponding namespace.

For example:

* ``import M (data D(..))``, ``import M (data D(type ..))`` and ``import M (data
  D(data ..))`` are syntactically valid, but will always give rise to a warning,
  as it is not currently possible for identifiers in the data namespace to have
  children.  (We might imagine changing this, e.g. for record fields, but doing
  so is outside the scope of this proposal.)

* ``import M (type T (data ..))`` is accepted. When ``T`` is a type its only
  sub-items are in the data namespace so this is somewhat redundant, but a class
  may have both children in either namespace.

* Similarly, ``import M (type T (type ..))`` is accepted but will give rise of a
  warning when ``T`` is a type or a class that does not have any associated
  types.


Deprecate use of ``pattern`` in import/export lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since the ``data`` specifier introduced above subsumes uses of the ``pattern``
keyword in import/export lists that are permitted under ``PatternSynonyms``, we
propose a new warning ``-Wpattern-namespace-specifier`` that warns when the
``pattern`` namespace specifier is used.

Initially this warning will be added to ``-Wcompat``.  Three releases after this
proposal is implemented, the warning will be added to ``-Wall``.

We do not currently propose increasing the severity of the warning beyond
``-Wall``, or removing support for ``pattern`` in import/export lists entirely,
because the simplification to the compiler does not seem worth the backwards
compatibility cost.


Use ``data`` specifier in fixity declarations and ``WARNING``/``DEPRECATED`` pragmas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal changes `GHC Proposal #65
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst>`__
to use the ``data`` namespace specifier instead of ``value``.  (The specific
changes are thanks to Vladislav Zavialov and form part of the PR.)

That proposal has not yet been implemented, so this is not a breaking change.


Examples
---------


Export lists with namespace specifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces, TypeFamilies #-}
   {-# OPTIONS_GHC -Wpattern-namespace-specifier #-}
   module M
     ( D            -- Accepted: exports data family D
     , data D       -- Accepted: exports data constructor D
     , C(type D)    -- Accepted: exports class C and data family D
     , C(type ..)   -- Accepted: exports class C and data family D
     , C(data ..)   -- Accepted: exports class C and method m
     , D(data f)    -- Accepted: exports data family D and field f
     , D(type ..)   -- Accepted: exports data family D; -Wdodgy-exports warning
     , pattern D    -- Accepted: exports data constructor D; -Wpattern-namespace-specifier warning
     , T(data D)    -- Accepted: exports type T and data constructor D
     , data f       -- Accepted: exports field f
     , data v       -- Accepted: exports term v
     , type T (..)  -- Accepted: exports type T and all its data constructors D, D2
     , type T (data ..) -- Accepted: exports type T and all its data constructors D, D2
     , type T (type ..) -- Accepted: exports type T; -Wdodgy-exports warning
     , T(pattern D) -- Rejected: pattern keyword cannot be used in sub-list
     , data T       -- Rejected: T not in scope in data namespace
     , type E       -- Rejected: E not in scope in type namespace
     , type ..      -- Accepted: exports data family D, C, T
     , data ..      -- Accepted: exports data constructor D, m, E, f, D2, v
     ) where

   class C a where
     data D a
     m :: a

   instance C Int where
     data D Int = E { f :: Int }

   data T = D | D2

   v = ()


.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces #-}
   module M
     ( (+)       -- Accepted: exports value-level function
     , data (+)  -- Accepted: exports value-level function
     , type (+)  -- Accepted: exports type family
     ) where

   import Prelude (data (+))
   import GHC.TypeLits (type (+))


.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces #-}
   module M
     ( type (+++) (data X)  -- Accepted: exports data type (+++) and its constructor
     , (+++) (X)            -- Rejected: variable (+++) cannot have a sub-list
     ) where

   (+++) = (+)

   data a +++ b = X


.. code:: haskell

   module M
     ( type .. -- Accepted: exports T
     , data .. -- Accepted: exports MkT
     ) where

   data T = MkT


Imports with namespace specifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following examples, recall that the ``Data.Proxy`` module defines ``data
Proxy t = Proxy``. (Its other exports are ignored for the purposes of these
examples.)

The same module can be imported with different qualifiers for the type namespace
and data namespace:

.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces #-}
   import Data.Proxy as T (type ..)
   import Data.Proxy as D (data ..)

   -- This is accepted:
   f :: T.Proxy Int
   f = D.Proxy

   -- This is accepted too, because both names are in scope unqualified:
   g :: Proxy Int
   g = Proxy

   -- This is rejected, because the type T.Proxy cannot be used at the term level:
   h :: T.Proxy Int
   h = T.Proxy


.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces, ImportQualifiedPost #-}
   import Data.Proxy qualified as T (type ..)
   import Data.Proxy qualified as D (data ..)

   -- This is accepted:
   f :: T.Proxy Int
   f = D.Proxy

   -- This is rejected, because the names are in scope only with qualifiers:
   g :: Proxy Int
   g = Proxy

.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces #-}
   import Data.Proxy (type Proxy(..))       -- Accepted: imports both type constructor and data constructor
   import Data.Proxy (type Proxy(data ..))  -- Accepted: imports both type constructor and data constructor
   import Data.Proxy (type Proxy(type ..))  -- Accepted: imports type constructor; -Wdodgy-imports warning

.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces #-}
   module M
     ( type T (..)  -- Accepted: exports T and MkT
     ) where
     data T = MkT

   {-# LANGUAGE ExplicitNamespaces #-}
   module N where
     import M (T (data MkT))  -- Accepted: imports both T and MkT
     import M (T (data ..))   -- Accepted: imports both T and MkT
     import M (T (type MkT))  -- Rejected: MkT is not in the type namespace
     import M (T (type ..))   -- Accepted: imports T only; -Wdodgy-imports warning
     import M (type T (..))   -- Accepted: imports both T and MkT
     import M (data T (..))   -- Rejected: T is not in the data namespace



Effect and Interactions
-----------------------

This proposal makes ``ExplicitNamespaces`` more coherent and more useful for
avoiding punning via qualified imports.

In either an import or an export list, it is possible to import or export the
entire contents of a module's type namespace or data namespace.
Importing/exporting ``type .., data ..`` is equivalent to omitting the
import/export list, except that it will not emit a ``-Wmissing-import-lists`` or
``-Wmissing-export-lists`` warning.

Since there are two disjoint namespaces, ``import M hiding (type ..)`` is
equivalent to ``import M (data ..)``.  We permit both, however.


``TypeData``
~~~~~~~~~~~~

When `TypeData (proposal #106)
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst>`_
is in use, it introduces both the type constructor name and any data constructor
names into the type namespace only (and does not permit punning).  For example:

.. code:: haskell

   {-# LANGUAGE TypeData #-}
   {-# LANGUAGE ExplicitNamespaces #-}

   module M
     ( type T (type MkT)   -- Accepted: exports both T and MkT
     , data MkT            -- Rejected: MkT not in data namespace
     , type ..             -- Accepted: exports both T and MkT
     ) where

   type data T = MkT


``PatternSynonyms``
~~~~~~~~~~~~~~~~~~~

Referring to pattern synonyms in top-level import/export items requires the
``data`` namespace specifier (or the deprecated ``pattern`` keyword).
Alternatively, pattern synonyms and their record fields can be associated with
parent type constructors by being mentioned in export sub-lists.

.. code:: haskell

   {-# LANGUAGE ExplicitNamespaces #-}
   {-# LANGUAGE PatternSynonyms #-}
   {-# OPTIONS_GHC -W #-}

   module M
     ( type T (data P, data f)  -- Accepted: associates P and f with T
     , data P                   -- Accepted
     , data P (f)               -- Rejected: f is not a child of P
     , P                        -- Rejected: P not in scope in type namespace
     , pattern P                -- Accepted; -Wpattern-namespace-specifier warning
     ) where

     data T = MkT Int

     pattern P {f} = MkT f

   {-# LANGUAGE ExplicitNamespaces #-}
    module N where
      import M (P)               -- Rejected: P not in scope in type namespace
      import M (data P)          -- Accepted
      import M (T(..))           -- Accepted: imports T, P and f
      import M (type T(type ..)) -- Accepted: imports T only; -Wdodgy-imports warning


Costs and Drawbacks
-------------------

This proposal introduces new syntax for namespace specifiers with wildcards,
however its meaning is consistent with the existing namespace specifiers on
individual import items.  By making the ``ExplicitNamespaces`` extension more
consistent it should become easier to learn.

The implementation and maintenance cost of this proposal is expected to be
relatively low.


Backwards Compatibility
-----------------------

This proposal is mostly backwards compatible, except that code may
theoretically rely on using ``type`` with a data constructor, which is
accepted in existing GHC versions but rejected under this proposal (see `GHC
issue #22581 <https://gitlab.haskell.org/ghc/ghc/-/issues/22581>`_).  For
example, the following import is accepted today but will be rejected under
this proposal: ::

  import Data.Functor.Product ( Product ( type Pair ) )

Given that this is essentially a bug, it seems unlikely that user code relies
on it. Moreover it can be fixed by simply removing the bogus ``type``
namespace specifier. Thus we do not expect significant breakage from this
change.

Existing code that uses the ``pattern`` keyword (with ``PatternSynonyms``) in
import/export lists and uses ``-Wcompat`` (or eventually ``-Wall``) will
receive a warning from ``-Wpattern-namespace-specifier`` until it migrates to
use ``data`` instead (with ``ExplicitNamespaces``).  In the future we may wish
to remove support for ``pattern`` as a namespace specifier entirely, but doing
so is not part of this proposal, so this is not a breaking change.


Alternatives
------------

The original version of this proposal extended the import declaration itself
with a namespace specifier (e.g. ``import M type as T``).  The current revision
instead makes this part of the import list (e.g. ``import M as T (type ..)``).
The revised version has several advantages over the original:

* The meaning of ``import M (type ..)`` is arguably more obvious than ``import M type``.

* ``type ..`` or ``data ..`` can now be used in an export list, whereas the original
  proposal did not have a way to export all names in a single namespace.

* The original version had a subtle distinction between ``import M type (T(..))`` and
  ``import M (type T (..))``.

* The original version had redundant constructions such as ``import M type (type T)``
  and had to rule out inconsistencies such as ``import M data (type T)``.

There are various other alternative possibilities:

* We could imagine supporting ``import M (..)`` for consistency, however this
  would be entirely redundant as it is equivalent to ``import M``.  Ideally the
  implementation would issue a sensible error in this case.

* We could use ``value``, ``term``, ``pattern``, or any other keyword instead of
  ``data`` to denote the data namespace.  It seems preferable to use ``data`` as
  it is an existing keyword (unlike ``value`` and ``term``) and unlike
  ``pattern`` it more clearly refers to the data namespace.  However this may
  lead to beginner confusion if expressions like ``import M (data f)`` are used,
  since ``data`` refers to the namespace rather than a datatype.

* There are alternatives to the import syntax proposed here, for example
  `proposal #340 <https://github.com/ghc-proposals/ghc-proposals/pull/340>`_
  proposes ``import M as (T, D)`` syntax. More examples of alternative syntax:

  - ``import M as {T, D}``
  - ``import M type as T data as D``
  - ``import M as (type T, data D)``

* Instead of introducing new syntax we could use the existing syntax for
  explicit import lists:

  .. code:: haskell

     import Data.Proxy( Proxy ) qualified as T
     import Data.Proxy( pattern Proxy ) qualified as D

  However it is significantly less convenient: you can’t import all the
  things at once without manually listing every single one of them.

* This proposal does not introduce a way to disambiguate namespaces at use
  sites, corresponding to the prefix ``'`` syntax used by ``DataKinds`` and the
  prefix ``'`` and ``''`` used by ``TemplateHaskell``.  Instead, disambiguation
  must happen at the level of imports, and cannot be used for definitions within
  the same module. `Proposal #214
  <https://github.com/ghc-proposals/ghc-proposals/pull/214>`_ suggested using
  prefix ``data.`` and ``type.`` systematically for this purpose.  However, that
  would be syntactically noisy and there was difficulty gathering consensus for
  that approach.

* Rather than modifying ``ExplicitNamespaces``,
  we could introduce a new extension.  This would
  make it clearer whether code depends on the new feature, or whether the older
  version of ``ExplicitNamespaces`` was enough.  However the general consensus
  seems to favour reducing the number of extensions over avoiding change to
  existing extensions.

* GHC currently rejects an attempt to import/export a data constructor or
  pattern synonym at the top level, such as in the following, with a "Not in
  scope: type constructor or class" or "Module ... does not export" message:

  .. code:: haskell

     module M ( D ) where
     data T = D

  In principle there is no ambiguity here, so this could be accepted without
  requiring the ``data`` keyword. Similarly, exporting a type operator currently
  requires the ``type`` specifier, even if there is no conflicting term-level
  operator. However these cases are comparatively rare, and the keywords make
  the program clearer to the reader, so we propose continuing to require them.
  GHC's error messages should be improved to point users in the right direction
  (see GHC issues `#20007 <https://gitlab.haskell.org/ghc/ghc/-/issues/20007>`_,
  `#21826 <https://gitlab.haskell.org/ghc/ghc/-/issues/21826>`_).

* ``import M (T(data D))`` is technically redundant as ``import M (T(D))``
  refers to the constructor ``D`` by default. It is currently necessary to use
  ``data`` as a namespace specifier only at the top level.  However it seems
  best to allow ``data`` to be used consistently with ``type``.


Unresolved Questions
--------------------

None


Implementation Plan
-------------------

Support with the implementation of this proposal would be welcome.
