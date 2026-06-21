============
Pun warnings
============

.. author:: Jaro Reinders, Artyom Kuznetsov
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/765>`_.
.. sectnum::
.. contents::

This proposal introduces ``-Wpuns`` and ``-Wpun-bindings``.

These changes should help the users write pun-free code to take advantage of
*Syntactic Unification Principle* described in `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_.

The warnings introduced in this proposal are intended
help programmers enforce a pun-free style of programming in their Haskell
projects if they so desire. It does not claim that one style of programming
is strictly better than the other and acceptance or rejection of this proposal
should not mean that one style is preferred over the other by GHC.

Motivation
==========

----------
Background
----------

Before we move on to laying out the problem statement there are 3 concepts
that the reader is adviced to be familiar with: Punning, Syntactic
Unification Principle and Lexical Scoping Principle.

Punning
-------

Haskell has two namespaces: one for types (the type namespace), and one for
terms (the data namespace).

This separation allows us to define data constructors and type constructors
whose names coincide:

::
  
  data T = T

The use of identical names for type-level and term-level entities is called
*punning*.

At use sites, GHC infers which ``T`` is referred to from context:

::

  t :: T  -- type-level T
  t = T   -- term-level T

Haskell makes heavy use of punning in its built-in syntax and common types:

::

  data [] a = [] | a : [a]
  data (a, b) = (a, b)
  data () = ()
  data Proxy a = Proxy
  newtype Identity a = Identity a
  newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
  newtype ExceptT e m a = ExceptT (m (Either e a))

However, as Haskell's type system evolves, the distinction between types and
terms becomes blurry. For example, the ``-XDataKinds`` extension introduces the
``'`` syntax to select entities from the data namespace in a type-level context:

::

  r :: Rec Const '[ 'T ]   -- vinyl records
  r = Const :& RNil

Note that we had to qualify both the list syntax and the ``T`` data constructor
with a tick.

Syntactic Unification Principle (from `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_)
----------------------------------------------------------------------------------------------------------------------------------------------------

In the absence of punning, there is no difference between type-syntax and
term-syntax.

Syntactic Unification Principle implies that if the user is not using punning
they can simply forget about the distinction between terms and types.

Lexical Scoping Principle (from `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_)
----------------------------------------------------------------------------------------------------------------------------------------------

For every occurrence of an identifier, it is possible to uniquely identify its
binding site, without involving the type system.

-----------------
Problem Statement
-----------------

As we step towards Dependent Haskell (with `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_)
acceptance), the distinction between types and terms becomes blurrier and
blurrier and the need arises to use terms and types interchangeably. Indeed,
we can begin to see this need in `#281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`_,
which introduces "visible ``forall``" syntax that lets us write functions
like this:

::

  sizeOf :: forall a -> Sized a => Int

  sizeOfInt = sizeOf Int

But because of punning, this can result in ambiguity:

::

  data T = T

  sizeOfT = sizeOf T -- is 'T' the data type or the data constructor?

We can't use the type of ``sizeOf`` to determine which ``T`` to use because of
*Lexical Scoping Principle*.

#281 tackles this issue by defaulting ``T`` to a data constructor in this case
(to keep compatibility with existing code) and introduces ``type`` syntactic marker.

However, thanks to *Syntactic Unification Principle* (adhered by `#281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`_), if
the user chooses to not use punning, there is no need to use this syntactic
marker, resulting in less context-dependent and syntax cluttered code.

Unfortunately, GHC has no good support for such style of programming:

* There is no way for the user to know if some code uses punning without
  manually reviewing it, in other words, usage of punning is silent.

* There is no way to avoid punning if the user imports the code that makes the
  use of it.
  Consider this import statement: ``import Data.Proxy``. It is impossible to
  distinguish between ``Proxy`` the type and ``Proxy`` the data constructor.

Beginner confusion
------------------

Even without Dependent Haskell, an argument can be made for pun-free code:
punning can be a source of confusion for beginners. The difference between
the terms namespace and the types namespace can be hard to understand at first,
especially when things like ``()`` or ``[a]`` are used (`as seen in this StackOverflow question <https://stackoverflow.com/questions/16892570/what-is-in-haskell-exactly>`_).

-----------------
Solution Overview
-----------------

We also propose to introduce two new warnings to GHC: ``-Wpuns`` and
``-Wpun-bindings``.

* ``-Wpuns`` warns the user about the usage of punning at use sites.

* ``-Wpun-bindings`` warns the user about the introduction of punning at binding
  sites.

These warnings aim to help the user to avoid using punning in their codebase.

To determine whether some binding or use site takes advantage of punning we
ask the question: "If Haskell had a single unified namespace, would that change
the lookup result?". If the answer is yes, then the code uses punning:

::

  data T = T -- with a single unified namespace, T the data would have a name clash with T the type.

  a = 15

  f :: forall a. a -> a -- no warning, because with a single unified namespace 'forall a.' would shadow the top level 'a', so 'a' in 'a -> a' would still refer to the forall bound 'a'.


There are more detailed examples in Examples section.

Proposed Change Specification
=============================

* Introduce a new warning, ``-Wpun-bindings`` and add it to ``-Weverything``.
  The warning is triggered by any name binding that would be rejected by the
  compiler if Haskell had a single unified namespace. For instance, this would
  include conflicting definitions, but exclude shadowing (for more examples, see
  Examples section).

* Introduce a new warning, ``-Wpuns`` and add it to ``-Weverything``. The warning is
  triggered by using an identifier that would be ambiguous or refer to another
  entity if Haskell had a single unified namespace. This includes syntactic puns
  like ``[]`` and ``()``.

Examples
========

Recall that ``-Wpun-bindings`` is triggered at definition sites that use punning,
and ``-Wpuns`` is triggered at use sites. To see what qualifies as punning, we
will look at the code that works today and analyze the breakage that would
occur if Haskell had a single unified namespace.

----------------------
``-Wpuns``, example #1
----------------------

::

  module A where { data A = T }
  module B where { data T = X }

  module C where

  import A
  import B

  f = T -- -Wpuns warning

If Haskell had a single unified namespace, referring to ``T`` would result in
ambiguity (is it ``A.T`` or ``B.T``?), thus this should trigger the warning.

Same happens if you use ``T`` in the export list:

::

  module C(T) where
      --  ^^^ -Wpuns warning
  import A
  import B

----------------------
``-Wpuns``, example #2
----------------------

::

  a = 15
  f :: a -> a

If Haskell had a single unified namespace, ``a`` instead of referring to
implicitly bound type-variable ``a`` would refer to ``a`` on the type-level. This
means that punning is used and should trigger the warning.

On the contrary:

::

  a = 15

  f :: forall a. a -> a

Does not use punning because if Haskell had a single unified namespace, explicitly bound type variable ``a`` would shadow the top-level ``a``.

----------------------
``-Wpuns``, example #3
----------------------

::
  
  {-# LANGUAGE ScopedTypeVariabels #-}
  a = 15

  f :: forall a. a -> a
  f = \a -> (a :: a)
  --              ^ warning here

In all of the ``a`` uses except for the last one there is no punning, because if
Haskell had a single unified namespace, in the type signature, top-level ``a``
would be shadowed by explicitly bound type variable ``a``, and in the expression
``a`` variable bound in the lambda would shadow the type variable. In the very
last case, however, currently, the ``a`` would refer to the type variable, but if
Haskell had a single namespace it would refer to the term-level variable. Thus the
warning is triggered.

----------------------
``-Wpuns``, example #4
----------------------

::

  f :: [] a -- warning
  g :: [a]  -- warning
  g = []    -- warning
  h = [a]   -- warning
  x = [a,b] -- no warning

Since ``-XListTupleTypeSyntax`` is enabled by default, all of the cases except the
very last one will emit ``-Wpuns`` warning because in all of them it is not clear
whether the data constructor or a type constructor is being referred to, except
in the very last case.

----------------------
``-Wpuns``, example #5
----------------------

::

  f :: ()      -- warning
  f = ()       -- warning
  g :: (a,b)   -- warning
  g = (c,d)    -- warning
  h :: (,) a b -- warning
  h = (,) c d  -- warning

Tuples in this case are very much the same as lists except they will emit a
warning in all cases.

Note that for both lists and tuples if ``-XListTupleTypeSyntax`` is disabled,
the type constructors will not be in scope anymore and no warnings will be
emitted.

------------------------------
``-Wpun-bindings``, example #1
------------------------------

This example shows the interaction with ``TypeAbstractions``.

::

  id :: t -> t
  id @a a = a

Here, when term-level ``a`` is bound it would conflict with the type level ``a`` if
Haskell had a single namespace, thus triggering the warning. This behavior is
similar to conflicting definition error for ``f b b = ...``:

::

  Test.hs:1:3: error:
      • Conflicting definitions for 'b'
        Bound at: Test.hs:1:3
                  Test.hs:1:5
      • In an equation for 'f'

On the contrary, the code below is fine, similarly to ``-Wpuns`` example #2, 
the ``a`` is shadowed instead:

::

  f :: t -> ()
  f @a = \a -> ()

Note how there is no conflicting definition and instead it is just shadowing 
if both were term variables: ``f b = \b -> ...``. 

------------------------------
``-Wpun-bindings``, example #2
------------------------------

::

  data T = T

If Haskell had a single unified namespace, type constructor ``T`` and data
constructor ``T`` would conflict, thus this should trigger the warning.

------------------------------
``-Wpun-bindings``, example #3
------------------------------

::

  data T = MkT
  data B = T | F

Even though type constructor ``T`` and data constructor ``T`` are defined in
different declarations, they would still cause a conflict, same as example #2.

------------------------------
``-Wpun-bindings``, example #4
------------------------------

::

  data J = Bool

This should not cause the warning because ``Bool`` defined here would not
conflict with ``Bool`` imported from ``Prelude``, this declaration is not rejected
by GHC:

::

  import Prelude (Bool)
  data Bool -- no conflict

------------------------------
``-Wpun-bindings``, example #5
------------------------------

This example shows the interaction with pattern signatures 
(part of ``ScopedTypeVariables``).

::

  f :: t -> t
  f @a = \(a :: a) -> a

This will not produce a punning warning, because there are
only two variables being bound, the first with ``@a`` and the second with 
``\(a :: ...) -> ...``.
Note that the ``:: a`` is just a use of the type variable ``a`` that was bound 
by the ``@a`` type abstraction.

Renaming the second binding of ``a`` to ``x`` avoids shadowing:

::

  f :: t -> t
  f @a = \(x :: a) -> x

Effect and Interactions
=======================

* Users will be able to make sure their code is pun-free.

Costs and Drawbacks
===================

* This proposal introduces new warnings, adding a bit to the maintenance burden of GHC. However, we think the benefit significantly outweighs this cost. 

Alternatives
============

* We could suppress ``-Wpuns`` warning for certain kinds of punning. For instance:
  we could suppress it for ``data Foo = Foo`` (when the data constructor is
  related to the type constructor, the most common use of punning) and let users
  disambiguate with module aliases. Or we could suppress ``-Wpuns`` when punning
  is used for records ``Foo { ... }``. However, this doesn't help with backwards
  compatibility much, introduces unintuitive ``-Wpuns`` warning behavior
  (sometimes it warns about puns and sometimes it doesn't)

Unresolved Questions
====================

None

Implementation Plan
===================

Jaro Reinders will implement the change.

There's an (old) merge request with ``-Wpuns`` warning implementation: `!2044 <https://gitlab.haskell.org/ghc/ghc/merge_requests/2044>`_.
This will be rebased or if that is too much work it will be used as inspiration. 