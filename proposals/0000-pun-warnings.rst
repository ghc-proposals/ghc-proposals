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

This proposal introduces ``-Wpun-uses`` and ``-Wpun-bindings``.

These changes should help the users write pun-free code to take advantage of
*Syntactic Unification Principle* described in `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_.

The warnings introduced in this proposal are intended
to help programmers enforce a pun-free style of programming in their Haskell
projects if they so desire. It does not claim that one style of programming
is strictly better than the other and acceptance or rejection of this proposal
should not mean that one style is preferred over the other by GHC.

Motivation
==========

----------
Background
----------

Before we move on to laying out the problem statement,
let us review three background concepts: Punning, the Syntactic
Unification Principle and the Lexical Scoping Principle.

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
terms becomes blurry. For example, the ``DataKinds`` extension introduces the
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
they can simply forget about the distinction between terms and types for name resolution.

Lexical Scoping Principle (from `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_)
----------------------------------------------------------------------------------------------------------------------------------------------

For every occurrence of an identifier, it is possible to uniquely identify its
binding site, without involving the type system.

-----------------
Problem Statement
-----------------

As we step towards Dependent Haskell (with the acceptance of `#378 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_),
the distinction between types and terms becomes blurrier and
blurrier, and the need arises to use terms and types interchangeably. Indeed,
we can begin to see this need with ``RequiredTypeArguments``, which lets us 
write functions like this:

::

  sizeOf :: forall a -> Sized a => Int

  sizeOfInt = sizeOf Int

But because of punning, this can result in ambiguity:

::

  data T = T

  sizeOfT = sizeOf T -- is 'T' the data type or the data constructor?

We can't use the type of ``sizeOf`` to determine which ``T`` to use because of
the *Lexical Scoping Principle*.

Proposal `#281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`_ tackles this issue by defaulting ``T`` to a data constructor in this case
(to keep compatibility with existing code) and introduces the syntactic marker ``type``.

However, thanks to *Syntactic Unification Principle* (adhered by `#281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`_), if
the user chooses to not use punning, there is no need to use this syntactic
marker, resulting in less context-dependent and syntax-cluttered code.

With the acceptance of the namespace-specified imports proposal (`#581 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0581-namespace-specified-imports.rst>`_), we now also have
the ability to specify which scope we want to import from: ::

  {-# LANGUAGE ExplicitNamespaces #-}

  import qualified Data.Proxy as T (type Proxy)   -- import only the Proxy type
  import qualified Data.Proxy as D (data Proxy)   -- import only the Proxy constructor

So it is always possible to avoid punning.

Unfortunately, GHC has no way for the user to know if some code uses punning
without manually reviewing it, in other words, usage of punning is silent.
This proposal addresses that problem: it lets users be confident their code
is pun-free by enabling this warning.

Punning as a source of confusion
--------------------------------

Even without Dependent Haskell, an argument can be made for pun-free code:
punning can be a source of confusion. It requires you to know whether something
is used in a term or a type context before you can mentally resolve a name.
Avoiding puns could slightly reduce the mental effort required to read code.

Newcomers are also not always aware that the same name may refer to two different 
entities, for example in `this StackOverflow question
<https://stackoverflow.com/questions/16892570/what-is-in-haskell-exactly>`_.

Proposed Change Specification
=============================

We propose to introduce two new warnings to GHC: ``-Wpun-uses`` and
``-Wpun-bindings`` and add them both to ``-Weverything``.

* ``-Wpun-uses`` warns the user about the usage of punning at use sites.

* ``-Wpun-bindings`` warns the user about the introduction of punning at binding
  sites.

These warnings aim to help the user to avoid using punning in their codebase.

To determine whether some binding or use site takes advantage of punning we
ask the question: **"If Haskell had a single unified namespace, would that 
change the meaning of the program?"**. If the answer is yes, then the code
uses punning.

Wrinkles:

* **W1** Note that the hypothetical single-namespace version of Haskell would still have
  name shadowing, so the ``-Wpun-bindings`` warnings does not trigger if a name
  would merely be shadowed (i.e., redefined in a separate sub-scope).

* **W2** Furthermore, we include syntactic punning, for example using the ``[]`` or ``()`` syntax
  triggers a warning from ``-Wpun-uses`` unless ``NoListTuplePuns`` is used. The proposal 
  `#475 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst>`_ and 
  `ListTuplePuns GHC documentation <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#extension-ListTuplePuns>`_
  describes how to avoid punning for the built-in lists and tuple syntax.

* **W3** Finally, the ``-Wterm-variable-capture`` (`GHC documentation <https://downloads.haskell.org/~ghc/9.14.1/docs/users_guide/using-warnings.html#ghc-flag-Wterm-variable-capture>`_) warning will become a subset of ``-Wpun-uses`` so warnings from
  ``-Wterm-variable-capture`` are suppressed when both are enabled.

In summary, we propose the following two changes:

* Introduce a new warning, ``-Wpun-bindings`` and add it to ``-Weverything``.
  The warning is triggered by any name binding that would be rejected by the
  compiler if Haskell had a single unified namespace. The ``-Wpun-bindings`` 
  warning includes conflicting definitions in the same scope, but it excludes
  shadowing of names across scopes.

* Introduce a new warning, ``-Wpun-uses`` and add it to ``-Weverything``. The warning is
  triggered by using an identifier that would be ambiguous or refer to another
  entity if Haskell had a single unified namespace. This includes syntactic puns
  like ``[]`` and ``()``. The ``-Wpun-uses`` warnings take precedence over the
  ``-Wterm-variable-capture`` warnings.

-----------------------------
Motivation for ``-Wpun-uses``
-----------------------------

Although ``-Wpun-bindings`` covers many common cases of introducing puns,
``-Wpun-uses`` is still useful in several situations where ``-Wpun-bindings``
does not warn:

* Imported modules might simply not be pun-free. We can still warn when such
  puns are used.

* Multiple different imported modules might individually be pun-free, but when
  combined they can give rise to puns, see `example #1`_. Similar to name clashes,
  we don't plan on checking this by going over all imported names, but instead
  we only warn when a punned name is used.

* Some variables are bound implicitly, for example in type signatures without
  a ``forall``.  These can be puns even though there is no binding site, see
  `example #2`_.

* The built-in list and tuple syntax uses punning, see wrinkle **W2** below
  and also `example #4`_ and `example #5`_. The warning can suggest to enable
  ``NoListTuplePuns``.

Examples
========

Recall that ``-Wpun-bindings`` is triggered at definition sites that use punning,
and ``-Wpun-uses`` is triggered at use sites. To see what qualifies as punning, we
will look at the code that works today and analyze the breakage that would
occur if Haskell had a single unified namespace.

.. _example #1:

--------------------------
``-Wpun-uses``, example #1
--------------------------

::

  module A where { data A = T }
  module B where { data T = X }

  module C where

  import A
  import B

  f = T -- -Wpun-uses warning

If Haskell had a single unified namespace, referring to ``T`` would result in
ambiguity (is it ``A.T`` or ``B.T``?), thus this should trigger the warning.

Same happens if you use ``T`` in the export list:

::

  module C(T) where
      --  ^^^ -Wpun-uses warning
  import A
  import B

.. _example #2:

--------------------------
``-Wpun-uses``, example #2
--------------------------

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

Note that the former example (without the explicit ``forall``) is already covered by the ``-Wterm-variable-capture`` warning.
If both ``-Wpun-uses`` and ``-Wterm-variable-capture`` are enabled the ``-Wpun-uses`` warnings
take precedence and the ``-Wterm-variable-capture`` warnings are suppressed.

--------------------------
``-Wpun-uses``, example #3
--------------------------

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

Note that the ``-Wpun-bindings`` warning also triggers for the ``\a -> ...`` binder.

.. _example #4:

----------------------
``-Wpun-uses``, example #4
----------------------

::

  f :: [] a   -- warning
  g :: [a]    -- warning
  g = []      -- no warning
  x = [a,b]   -- no warning

  h :: (a, b) -- warning
  h = (a, b)  -- no warning

The ``ListTuplePuns`` extension (enabled by default) allows the punned use of
list and tuple syntax at the type level. The `-Wpun-uses` warning will trigger
whenever this happens (see **W2** in the `Proposed Change Specification`_). To
fix this warning, use the non-punned names instead:

::

  import Data.List (List)
  import Data.Tuple.Experimental (Tuple2)
  
  f :: List a
  h :: Tuple2 a b



.. _example #5:

----------------------
``-Wpun-uses``, example #5
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

Note that for both lists and tuples if ``ListTuplePuns`` is disabled,
the type constructors will not be in scope anymore and no warnings will be
emitted (see **W2** in the `Proposed Change Specification`_).

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

On the contrary, the code below is fine, similarly to ``-Wpun-uses`` example #2, 
the ``a`` is shadowed instead:

::

  f :: t -> ()
  f @a = \a -> ()

Note how there is no conflicting definition and instead it would just be
shadowing if both were term variables: ``f b = \b -> ...``. 

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

Currently, pattern signatures, like ``a :: a`` in this case, may or may not
bind type variables depending on whether or not a variable with the same
name was already bound. In this case, the type variable ``a`` was
already bound by the ``@a`` type abstraction, so the pattern signature
is a use of the variable ``a`` and does not bind it as a fresh variable.

This will not produce a pun-use warning, because in the hypothetical pun-free
Haskell, this would simply first bind ``a`` to be the type that ``f`` operates
on and subsequently shadow ``a`` to be the term argument to ``f``. The scopes,
uses, and binding are shown in this diagram:
  
::

  --     ┌──────────────┐ scope of the type variable
  --                  ┌─┐ scope of the term variable (shadowing)
  f @a = \(a :: a) ->  a
  -- ▴     ▴    ▴      ▴
  -- │     │    │      └─ use of the term variable
  -- │     │    └──────── use of the type variable
  -- │     └───────────── binding of the term variable
  -- └─────────────────── binding of the type variable

Renaming the second binding of ``a`` to ``x`` avoids the hypothetical shadowing:

::

  f :: t -> t
  f @a = \(x :: a) -> x

Effect and Interactions
=======================

The intended effect is that users will be able to make sure their code is pun-free.

If ``-Wpun-uses`` is enabled then ``-Wterm-variable-capture`` warnings are suppressed,
because that is a subset of ``-Wpun-uses`` (e.g., ``-Wpun-uses`` example #2).

Costs and Drawbacks
===================

* This proposal introduces new warnings, adding a bit to the maintenance burden of GHC. However, we think the benefit significantly outweighs this cost. 

Alternatives
============

* We could suppress ``-Wpun-uses`` warning for certain kinds of punning. For instance:
  we could suppress it for ``data Foo = Foo`` (when the data constructor is
  related to the type constructor, the most common use of punning) and let users
  disambiguate with module aliases. Or we could suppress ``-Wpun-uses`` when punning
  is used for records ``Foo { ... }``. However, this doesn't help with backwards
  compatibility much, introduces unintuitive ``-Wpun-uses`` warning behavior
  (sometimes it warns about puns and sometimes it doesn't)

Unresolved Questions
====================

None

Implementation Plan
===================

Jaro Reinders will implement the change.

There's an (old) merge request with ``-Wpun-uses`` warning implementation: `!2044 <https://gitlab.haskell.org/ghc/ghc/merge_requests/2044>`_.
This will be rebased or if that is too much work it will be used as inspiration. 