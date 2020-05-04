``-XNoImplicitForAll``, ``-XNoPatternSignatureBinds``
=====================================================

.. author:: John Ericson (@Ericson2314)
.. date-accepted:: 2020-05-04
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/285>`_.
.. contents::

Provide a way to strictly separate bindings and use of varariables, so the distinction never depends on what is in scope.
This means opting out of implicit ``forall`` binding of free variables in type signatures, and opting out of the binding of variables in pattern signatures.
The two can be controlled separately.

Motivation
----------

There are two independent motivations: education and consistency with a unified namespace.

Education
~~~~~~~~~

Some people think that implicit binding is bad for people learning Haskell.
All other variables are explicitly bound, and the inconsistency means more to learn.
Also, implicit syntax in general allows the beginner to not realize what they are doing.
What are tedious tasks for the expert may be helpful learning steps to them.

Further, the most beginnning students may be taught with both ``-XNoImplicictForAll``, ``-XNoExplicitForAll``, and ``-XNoPatternSignatureBinds``.
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
But, as a newly freed variable, it is now ready to be captured by whatever is proposed in `#270`_.
I think this is a good separation of concerns.

Proposed Change Specification
-----------------------------

Create ``-XImplicitForAll`` to allow automatically capturing free variables in an outer ``forall`` as is always done today.
It is on by default for backwards compatibility.
When using ``-XNoImplicitForAll``, all variables in regular signatures, instances, and data declarations must be explicitly bound.

Create ``-XPatternSignatureBinds`` to allow the implicit binding of free variables in pattern signatures.
It is also on by default for backwards compatibility.
When using ``-XNoPatternSignatureBinds``, all variables in pattern signatures must be explicitly bound.

Examples
--------

Basic examples
~~~~~~~~~~~~~~

#. ::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

     f :: t -> ... -- error: `t` is not bound
     f x = ...

   This could be rewritten as::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE ExplicitForAll #-}

     f :: forall t. t -> ...
     f x = ...

#. ::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     f (x :: t) = ... -- error: `t` is not bound

   This could be rewritten as either::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}
     {-# LANGUAGE ScopedTypeVariables #-}

     f :: forall t. ...
     f (x :: t) = ... -- OK

   or, if `#238`_ is accepted::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}
     -- {-# LANGUAGE ScopedTypeVariables #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE ExplicitForAll #-}
     {-# LANGUAGE TypeApplications #-}
     {-# LANGUAGE TypeAbstractions #-} -- from #238

     f :: forall t0. ...
     f @t (x :: t) = ... -- OK

#. ::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}
     {-# LANGUAGE ExistentialQuantification #-}

     data Some where
       MkSome :: forall t. t -> Some

     f (MkSome (x :: t)) = ... -- error: `t` is not bound

   This could be rewritten, once `#126`_ is implemented, as::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}
     {-# LANGUAGE GADTs #-}
     {-# LANGUAGE TypeApplications #-}

     data Some where
       MkSome :: forall t. t -> Some

     f (MkSome @t x) = ... -- OK

Not just term definitions
~~~~~~~~~~~~~~~~~~~~~~~~~

Besides top level term bindings, we currently have signatures with implicit forall quantification for expressions, data declerations, family declarations, and instances [#class-forall]_.
This proposal applies to all alike:

#. ::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     ... (id :: t -> t) -- error: `t` is not bound

   This could be rewritten as::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     ... (id :: forall t. t -> t) -- OK

#. ::

    {-# LANGUAGE NoImplicitForAll #-}
    -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

    data D :: k -> Type where -- error: `k` is not bound

   This could be rewritten as::

    {-# LANGUAGE NoImplicitForAll #-}
    -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

    data D :: forall k. k -> Type where -- OK

#. ::

    {-# LANGUAGE NoImplicitForAll #-}
    -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

    type family F :: k -> Type where -- error: `k` is not bound

   This could be rewritten as::

    {-# LANGUAGE NoImplicitForAll #-}
    -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

    type family F :: forall k. k -> Type where -- OK

#. ::

    {-# LANGUAGE NoImplicitForAll #-}
    -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

    instance Eq t => C t where -- error: `t` is not bound

   This could be rewritten as::

    {-# LANGUAGE NoImplicitForAll #-}
    -- {-# LANGUAGE NoPatternSignatureBinds #-} -- Does not matter whether enabled or disabled

    instance forall t. Eq t => C t where -- OK

When ``-XStandaloneKindSignatures`` is on, these new standalone signatures are affected as well.

#. ::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type F :: k -> Type -- error: `k` is not bound
     data F _ = ...

   This could be rewritten as::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type F :: forall k. k -> Type -- OK
     data F _ = ...

#. ::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type F :: k -> k -- error: `k` is not bound
     type family F where

   This could be rewritten as::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type F :: forall k. k -> k -- OK
     type family F where

#. ::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type C :: (k -> Type) -> Constraint -- error: `k` is not bound
     class C f where

   This could be rewritten as::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type C :: forall k. (k -> Type) -> Constraint -- OK
     class C f where

#. ::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type D :: k -> Type -- error: `k` is not bound
     data D where

   This could be rewritten as::

     {-# LANGUAGE NoImplicitForAll #-}
     -- {-# LANGUAGE PatternSignatureBinds #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE StandaloneKindSignatures #-}

     type D :: forall k. k -> Type -- OK
     data D where

Pattern signatures in GADT declarations, family declarations, and class declarations are also restricted.
I'll first use a hypothetical yet-unproposed ``@``-abstraction syntax to "fix" these examples to demonstrate the analogy to the previous examples.
Then I'll put the inline signature or top-level signature workaround that exist today.

#. ::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     data D (y :: x) (z :: y) where -- error: `x` is not bound, `y` and `z` are fine

   Could *someday* be be rewritten as::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     data D @x (y :: x) (z :: y) where -- OK, someday

   But today we have to this::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     data D :: forall x. forall (y :: x) -> y -> Type where

   Or in 8.10 alternatively this::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     type D :: forall x. forall (y :: x) -> y -> Type
     data D where -- OK

#. ::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     type family F (y :: x) (z :: y) where -- error: `x` is not bound, `y` and `z` are fine

   Could *someday* be be rewritten as::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     type family F @x (y :: x) (z :: y) where -- OK, someday

   But today we have to this::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     type family F :: forall x. forall (y :: x) -> y -> Type where

   or in 8.10 alternatively this::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     type F :: forall x. forall (y :: x) -> y -> Type
     type family F where -- OK

#. ::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     class Eq a => C (y :: x) (z :: y) where -- error: `x` is not bound, `y` and `z` are fine

   Could *someday* be be rewritten as::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     class Eq a => C @x (y :: x) (z :: y) where -- OK, someday

   But in 8.10 we have to this::

     -- {-# LANGUAGE NoImplicitForAll #-} -- Does not matter whether enabled or disabled
     {-# LANGUAGE NoPatternSignatureBinds #-}

     type C :: forall x. forall (y :: x) -> y -> Constraint
     class Eq a => C y z where -- OK

   Note that since there is no ``class F :: ...`` syntax analogous to ``data F :: ...``,
   so ``-XStandaloneKindSignatures`` are the only way to write explicitly kind-polymorphic classes.

Note that the variables to the left of the ``::`` are are deemed explicit bindings analogous to ``f (y :: x) (z :: z) = ...`` and permitted.
However ``x`` to the right of the ``::`` is a use, not otherwise bound, and thus implicit binding today.
It is not permitted as-is, and must be explicitly bound or discarded as done in the working alternatives.

Effect and Interactions
-----------------------

As described in the motivation, this opens the door to other means to bind the previously implicitly bound variables.
Other than that, I think this doesn't interact with other features in interesting ways.

Costs and Drawbacks
-------------------

It is a little known fact that one can do "empty" ``forall`` quantifications today::

  {-# LANGUAGE ExplicitForAll #-}

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
This serves as evidence it wouldn't be costly to implement.

A drawback is that the proposal broadens a stylistic split in the ecosystem between those that like and dislike implicit quantification.
But note that one could already put in the optional ``forall`` if they so please.

Alternatives
------------

#. ``-XImplicitForAll`` and ``-XPatternSignatureBinds`` could be combined into a single ``-XImplicitBinds`` extension.
   The advantage of this is of course fewer extension to implement and learn.
   However, this might obscure that implicit for all quantifiers and implicit pattern bindings work more differently than at first it seems.
   Implicit for all quantifiers in particular have a simple driven-syntax desugaring, whereas implicit pattern bindings are only simple change to the type inference algorithm---not syntax driven.
   I imagine this means some people would prefer the former since its "less magical", while others would prefer the latter as its more "bang for buck".
   There is no easy adjudicating between those two positions, they are aesthetic matters of opinion.
   Furthermore, since one of the main motivations for this proposal is education, I think its good to help teach their difference through independent extensions.
   That said I would certainly rather see 1 extension than no extension---the status quo.

#. Idris has a single namespace, but always does the implicit bindings such that writing the type of an argument with a single lower case identifier is impossible.
   Do note that more complicated type expressions with lower case identifiers is fine.

#. Some people thought ``-XNoImplicitForAll`` should imply ``-XExplicitForAll``, though with the option to opt out of both for education as described above.
   I am sympathetic---this does make common cases more terse---but am wary of making extensions non-monotonic.

#. @Monoidal asks whether ``-XNoImplicitForAll`` should imply ``-fprint-explicit-foralls``.
   I am also sympathetic, but again worried about non-monotonicity.
   That said, warnings are more freeform than extensions so I am less worried than with the above suggestion.

Unresolved Questions
--------------------

No unresolved questions.

Implementation Plan
-------------------

I think this will be easy to implement.
I take responsibility for implementing it, but hope to use the opportunity to mentor someone else rather than do all myself.

.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126

.. _`#238`: https://github.com/ghc-proposals/ghc-proposals/pull/238

.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270

.. [#class-forall]

  Notice that today, one cannot even write ``class forall a. Foo a`` though they they can write ``instance forall a. Foo a``.
  This is because while the head of an instance is a class applied *arguments*, the head of a class is a class taking *parameters*.
  In other words, the ``a`` in ``Foo a`` in ``class forall a. Foo a`` is not a binder, while in ``class forall a. Foo a`` is one.
