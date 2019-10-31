``-XNoImplicitForAll``
======================

.. author:: John Ericson (@Ericson2314)
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/285>`_.
.. sectnum::
.. contents::

Provide a way to opt out of implicit ``forall`` binding of free variables in type signatures.

Motivation
----------

There are two independent motivations, education and a unified namespace.
Complements the unified namespace proposal (`#270`_) by making all bindings usable in type signatures.

Education
~~~~~~~~~

Some people think that implicit binding is bad for people learning Haskell.
All other variables are explicitly bound, and the inconsistency means more to learn.
Also, implicit syntax in general allows the beginner to not realize what they are doing.
What are tedious tasks for the expert may be helpful learning steps to them.

Further, the most beginnning students may be taught with both ``-XNoImplicictForAll`` and ``-XNoExplicitForAll``.
This means it's impossible to write forall types by any means.
Combine with ``-Wmissing-signatures`` and ``-Wmissing-local-signatures``, so inferred polymorphic types of bindings are also prohibitted, and a monomorphic custom prelude, and forall types are all but expunged entirely.

I don't wish to argue whether these choices do or don't actually help learning, but just state that some people have opinions that they do and there is no technical reason GHC cannot accommodate them.

Unified Namespace
~~~~~~~~~~~~~~~~~

If `#270`_ is accepted, there will be a way to program Haskell with "morally" one namespace for types and terms alike.
However, there is one exception to the unification of namespaces: lower case variables in type signatures bound "like terms" still are treated as free and implicitly bound with a ``forall`` instead::

  t = Int
  x :: t -- sugar for 'forall t. t', no 't ~ Int'
  x = 0

Unlike the other changes done with warnings, this would be breaking change, so we need an extension.
``-XNoImplicitForAll`` alone would *not* cause ``t`` in the signature to be bound from in the above; it would be unbound causing and error.
But, as a newly freed variable, it is now ready to be capture by whatever is proposed in `#270`_.
I think this is a good separation of concerns.

Proposed Change Specification
-----------------------------

Create ``-XImplicitForAll`` to allow automatically capturing free variables in an outer ``forall`` as is always done today.
It is on by default for backwards compatibility.
When using ``-XNoImplicitForAll``, all variables in types must be explicitly bound.

Make ``-XImplicitForAll`` also exclusively allow the implicit binding of non-rigid (unified) variables in pattern signatures.

Examples
--------

All examples assume ``-XExplicitForAll`` and ``-XNoImplicitForAll``, in addition to their own ``LANGUAGE`` pragmas.

::

  f :: k -> ... -- error: k is not bound
  f x = ...

::

  f (x :: k) = ... -- error: k is not bound

::

  {-# LANGUAGE ScopedTypeVariables #-}

  f :: forall k. ...
  f (x :: k) = ... -- OK

Not just terms
~~~~~~~~~~~~~~

Besides top level term bindings, we currently have signatures with implicit quantification for expressions, instances, and data decleration.
This proposal applies to all alike:

::

  data F :: x -> Type where -- error: needs `forall x.`

::

  instance Eq a => X a where -- error: needs `forall a.` (after `instance`)

::

  class Eq a => X (a :: b) where -- error: `b` unbound

When ``-XStandaloneKindSignatures`` is on, it also affects those new standalone signatures as well.
For example all of these would be invalid:

::

  type MonoTagged :: x -> x -> Type -- error: needs `forall x.`
  data MonoTagged t x = ...

::

  type Id :: k -> k -- error: needs `forall k.`
  type family Id x where

::

  type C :: (k -> Type) -> k -> Constraint -- error: needs `forall k.`
  class C a b where

::

  type TypeRep :: forall k. k -> Type -- error: needs `forall k.`
  data TypeRep a where

The other "pattern style" of GADT declarations, like classs declarations, is also restricted::

  data  F (y :: x) (z :: y) ... :: Type where -- error: `x` is unbound, `y` and `y` are OK.
  class F (y :: x) (z :: y)             where -- ditto

Note that ``y`` and ``z`` are deemed explicit bindings analogous to ``f (y :: x) (z :: z) = ...`` and permitted.
However ``x`` is a use, and thus implicit binding today, and not permitted.
There is no way to fix this without rewriting "signature style" as::

  data  F :: forall x. forall (y :: z) -> ... -> Type where

or with ``-XStandaloneKindSignatures``::

  type  F :: forall x. forall (y :: z) -> ... -> Type
  data  F y z where

  type  F :: forall x. forall (y :: z) where
  class F y z

Note that since there is no ``class F :: ...`` syntax analogous to ``data F :: ...``, ``-XStandaloneKindSignatures`` are the only way to write explicitly kind-polymorphic classes.
However maybe in the future we would have something like::

  data  F @x (y :: x) (z :: y) ... :: Type where
  class F @x (y :: x) (z :: y) where

which would be permitted and not require ``-XStandaloneKindSignatures``.

Empty `forall` "desugar"
~~~~~~~~~~~~~~~~~~~~~~~~

It is a little known fact that one can do "empty" ``forall`` quantifications today::

  x :: forall. Int -- same as 'x :: Int'
  x = 0

This has the exact same effect at requiring explicit bounds:

::

  Prelude> x :: forall. t; x = x
  
  <interactive>:21:14: error: Not in scope: type variable ‘t’

::

  Prelude> instance forall. Eq a => Ord a where

  <interactive>:34:21: error: Not in scope: type variable ‘a’

  <interactive>:34:30: error: Not in scope: type variable ‘a’

::

  Prelude> data F :: forall. x -> Type

  -- should complain but there is a bug!

We can imagine then that ``-XNoImplicitForAll`` puts an ``forall.`` at the beginning of every signature, in order to "desugar" the new behavior into the old behavior.

Class declarations
~~~~~~~~~~~~~~~~~~

Notice that today, one cannot even write ``class forall a. Foo a`` though they they can write ``instance forall a. Foo a``.
This is because while the head of an instance is a class applied *arguments*, the head of a class is a class taking *parameters*.
In other words, the ``a`` in ``Foo a`` in ``class forall a. Foo a`` is not a binder, while in ``class forall a. Foo a`` is one. 

Effect and Interactions
-----------------------

As described in the motivation, this opens the door to other means to bind the previously implicitly bound variables.
Other than that, I think this doesn't interact with other features in interesting ways.

Costs and Drawbacks
-------------------

Broadens a stylistic split in the ecosystem between those that like and dislike implicit quantification.
But note that one could already put in the optional ``forall`` if they so please.

Alternatives
------------

Idris has a single namespace, but always does the implicit bindings such that writing the type of an argument with a single lower case identifier is impossible.
Do note that more complicated type expressions with lower case identifiers is fine.

Unresolved Questions
--------------------

No unresolved questions.

seems something that ought to be prohibited because ``b`` is unbound.

Implementation Plan
-------------------

I think this will be easy to implement.
I take responsibility for implementing it, but hope to use the opportunity to mentor someone else rather than do all myself.

.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
