Binding type variables in lambda-expressions
============================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/155>`_.
.. sectnum::
.. contents::

.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`#128`: https://github.com/ghc-proposals/ghc-proposals/pull/128
   
Proposal `#126`_ allows us to bind scoped type variables in patterns using an ``@a`` syntax.
However, the new syntax is allowed only in *constructor* patterns ``K @a @b x y``. This proposal
extends this idea to lambda-expressions, allowing ``\ @a x -> ...``. Here are some examples::

  id :: a -> a
  id @a x = (x :: a)                      -- no forall needed

  prefix :: a -> [[a]] -> [[a]]
  prefix @a x yss = map xcons yss
    where xcons :: [a] -> [a]             -- this signature is impossible to write without ScopedTypeVariables
          xcons ys = x : ys

  const :: a -> b -> a
  const @c x _ = (x :: c)                 -- names can change; you do not have to bind every type variable

  fconst :: a -> b -> b
  fconst @_ @d _ x = (x :: d)             -- order matters

  pair :: forall a. a -> (a, a)           -- brings a into scope (just like today)
  pair @b x = (x :: a, x :: b)            -- brings b into scope with the same meaning as a
  
  higherRank :: (forall a. a -> a -> a) -> ...
  higherRank = ...

  ex = higherRank (\ @a x _y -> (x :: a)) -- it works in a higher-rank situation, too

  wrong @a x = x                          -- we can't do this without a type signature

  stillWrong @a (x :: a) = x              -- even here we can't
  
Motivation
----------

There are several motivating factors for this addition:

1. There are cases where a ``Proxy`` is necessary in order for a higher-rank function argument
   to access a type variable, such as::

     type family F a

     higherRankF :: (forall a. F a -> F a) -> ...

     usage = higherRankF (\ (x :: F a) -> ...)

   The ``(x :: F a)`` pattern signature does not work, because ``F`` is not injective. There
   is no way to be sure that the ``a`` in ``usage`` is meant to match the ``a`` in
   ``higherRankF``. Currently, there is simply no way for ``usage`` to get access to the
   type variable written in the signature for ``higherRankF``. This code would have to
   be rewritten to use ``Proxy``. Under this proposal, however, ``usage`` could be simply ::

     usage = higherRankF (\ @a x -> ...)

   Ah. That's better.

2. With `#126`_, we can bind type variables in constructor patterns, allowing us to easily
   capture existentials. The only other place a type variable can enter scope is in a
   function definition, and so it's only logical to extend `#126`_ to do so.

3. ``ScopedTypeVariables``\'s mechanism for binding type variables using a ``forall`` in
   a signature has never sat well with some. (I'm in the some, but I'm not the only one.)
   A type signature can appear arbitrarily far away from a function definition, and
   (to me) the use of ``forall`` to induce scoping over the function definition is far
   from intuitive. Using this new syntax, all the action happens in the function
   definition.

Proposed Change Specification
-----------------------------
GHC's type system is *bidirectional*, meaning that it sometimes is *inferring* a type
and sometimes is *checking* a type. `Practical Type Inference for Arbitrary-Rank Types <http://repository.upenn.edu/cis_papers/315/>`_ is a careful introduction of the ideas, though
GHC's algorithm is currently based on the more recent `Visible Type Applications`_. Essentially,
bidirectionality means that the type system can distinguish (and make decisions based on)
the difference between knowing what type to expect and not.

.. _`Visible Type Applications`: https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1001&context=compsci_pubs

Under this proposal, the new feature is allowed only in *checking* mode. That is, we
always know exactly what type is expected for a function definition or lambda expression.

As always, we can consider a nested lambda ``\ x y z -> ...`` to be an abbreviation for
``\ x -> \ y -> \ z ->``. This does not change if one of the bound variables is a type
variable (preceded by ``@``). We do require, as usual, that we do not bind the same variable
twice in a single lambda; this is true for type variables, too.

Thus, the proposal boils down to one rule:

* ``\ @a -> body``, being checked against the type ``forall a. ty``, binds the type
  variable ``a`` and then checks ``body`` against the type ``ty``. Checking an
  expression ``\ @a -> body`` against a type that does not begin with a ``forall``
  is an error. The token after the ``@`` must be a type variable name or ``_``.

That's it! Note that this specification assumes that the variable name in the lambda
equals the variable name in the ``forall``. If the type begins with a ``forall``, this
correspondence can always be made to happen because we can freely rename the bound
type variable in a ``forall``. (This "free renaming" is entirely internal; a user
can write a different name in the type than in the pattern, always.)

As usual, we can interpret a function defintion ``f <args> = body`` as
``f = \ <args> -> body``, and thus the function-definition case reduces to the lambda-expression
case above.

This new behavior will be available whenever both ``-XTypeApplications`` and
``-XScopedTypeVariables`` are enabled.

This change is specified in the appendix to the `Type variables in patterns <https://cs.brynmawr.edu/~rae/papers/2018/pat-tyvars/pat-tyvars.pdf>`_ paper.

Examples
--------

Here are two real-world examples of how this will help, courtesy of @int-index:

1. It would be useful to eliminate ``Proxy`` in this style of proof::

     class WithSpine xs where
       onSpine ::
         forall r.
         Proxy xs ->
         ((xs ~ '[]) => r) ->
         (forall y ys.
           (xs ~ (y : ys)) =>
           WithSpine ys =>
           Proxy y ->
           Proxy ys ->
           r) ->
         r

   Code taken `from here <https://github.com/int-index/caps/blob/2f46fc6d5480bdef0a17f64359ad6eb29510dba4/src/Monad/Capabilities.hs#L273>`_.

   Compare:

   a. ``@``\-style: ``withSpine @xs (onNil ...) (\ @y @ys -> onCons ...)``
   b. ``Proxy``\-style: ``withSpine (Proxy :: Proxy xs) (onNil ...) (\(Proxy :: Proxy y) (Proxy :: Proxy ys) -> onCons ...)``

2. From `reflection <https://hackage.haskell.org/package/reflection-2.1.4/docs/Data-Reflection.html#v:reify>`_::

     reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r

   Compare:

   a. ``@``\-style: ``reify (\ @s -> ...)``
   b. ``Proxy``\-style: ``reify (\(Proxy :: Proxy s) -> ...)``

Effect and Interactions
-----------------------

* One might worry about parsing. After all, ``@`` already has a meaning in patterns. However,
  this is all OK: whenever ``-XTypeApplications`` is enabled, ``@`` with a preceding
  whitespace character (or comment) is parsed differently from ``@`` without a preceding
  whitespace character (or comment). So ``f x @a`` is a good left hand side for a function
  with type ``Int -> forall a. ...`` and ``f x@a`` simply binds both ``x`` and ``a`` to the
  first argument to ``f``.

* An astute reader will note that I put spaces after all my lambdas. That is because
  ``\@`` is a valid name for a user-defined operator. This proposal does not change that.
  If you want to bind a type variable in a lambda, you must separate the ``\`` from the
  ``@``.

* This proposal is meant to dovetail nicely with other recent proposals in this space
  (`#126`_, `#128`_), but all the proposals are orthogonal. Any can usefully be accepted
  without the others.

* Accepted proposal `26`_ (debated as `#99`_) introduces the possibility of user-written
  specificity annotations (``forall {k} ...``). An *inferred* variable, including one
  written by the programmer using this new notation, is not available for use with
  any form of visible type application, including the one proposed here. If you have
  a function ``f :: forall {k} (a :: k). ...``, you will have to rely on the old behavior
  of ``-XScopedTypeVariables`` to bring ``k`` into scope in ``f``\'s definition. This is
  regrettable but seems an inevitable consequence of the ``{k}`` notation.

.. _`26`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0026-explicit-specificity.rst
.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/pull/99
  
* (technical) The `Visible Type Applications`_ (VTA) paper defines the behavior about what to
  do when checking against a polytype: it says to deeply skolemize. However, eager deep
  skolemization will spell trouble for this extension, as we need the lambdas to see
  the ``forall``\s. The end of the Section 6.1 in the `extended VTA <https://cs.brynmawr.edu/~rae/papers/2016/type-app/visible-type-app-extended.pdf>`_ paper discusses
  why we do eager deep skolemization: essentially, the alternative would be to do
  type generalization at inflection points between checking and inference mode,
  right before doing the subsumption check. Type generalization is hard in GHC, though,
  and so the paper avoided it. In order to implement this proposal, we'll have to work
  out how to do this.

Costs and Drawbacks
-------------------
This is another feature to specify and maintain, and that's always a burden. It will take
some creative thought about how to do generalization properly (last point in previous section),
but I don't actually think the code will be all that challenging there.

There is a potential confusion with as-patterns.

Alternatives
------------
If we want to bind type variables in lambda-expressions, I think this is the only way to do it.
We don't have to, of course, but then there will still be one area in GHC/Haskell that requires
``Proxy``, and that's unfortunate.

One alternative design would be to rearrange the extensions so that users could enable
parts of today's ``ScopedTypeVariables`` without enabling the strange binding behavior of
``forall``. I don't feel the need for this, myself, so I do not plan on working out this
design, but I'm happy to accept contributions toward this end from the community.

Unresolved questions
--------------------
Q: As brought up in the GitHub trail: should we consider changes to the extension structure?
Specifically, do we want a way to enable this feature without also enabling the fact that
a ``forall`` in a type signature binds a type variable in a definition.

A: I say "no". I would prefer that world to the one we're currently in, but I simply don't
think this small rejiggering is worth the transition costs.

Implementation Plan
-------------------
I'm happy to advise and support a volunteer who wishes to implement. I might do it myself
or work with a student on this someday, as well.
