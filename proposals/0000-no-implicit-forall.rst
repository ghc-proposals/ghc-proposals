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

Examples
--------

It is a little known fact that one can do "empty" ```forall`` quantifications today::

  x :: forall. Int -- same as 'x :: Int'
  x = 0

This has the exact same effect at requiring explicit bounds::

  Prelude> x :: forall. t; x = x
  
  <interactive>:21:14: error: Not in scope: type variable ‘t’

We can imagine then that ``-XNoImplicitForAll`` puts an ``forall.`` at the beginning of every signature, in order to "desugar" the new behavior into the old behavior.

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

Implementation Plan
-------------------

I think this will be easy to implement.
I take responsibility for implementing it, but hope to use the opportunity to mentor someone else rather than do all myself.

.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
