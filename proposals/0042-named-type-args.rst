Proposal title
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/196>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Extend type application syntax with record field update syntax.


Motivation
------------
GHC and Haskell ecosystem have been on track to rely more and more on type-level programming. We have TypeInType today and are on the course to have full dependent types in the future. Perhaps the most useful ergonomics extension that GHC got in this domain in the recent years is TypeApplication. It allows to have more readable, succinct code. That being said, type application syntax can be improved by a modest amount in the light of functions with many (say 3+) type parameters. Because Haskell still does and will continue to do a lot of type inference there are situations where one needs to apply a type argument that is not in the first position. Currently it is done as follows

::

  f :: forall m n a . ...
  g = f @_ @_ @Int

This is:

1. Cumbersome.
2. Unclear without context.
3. Susceptible to breakage when signature of ``f`` changes.
4. Produces arcane kind errors on missteps.


Proposed Change Specification
-----------------------------

Extend record update syntax to type applications. I.e. allow to implement ``g`` from above as

::

  g = f @{a = Int}


Effect and Interactions
-----------------------

The new version is:

1. Less cumbersome: length of code is linear in number of given arguments, not number of type variables.
2. More informative without context.
  * Immediately useful because the Haskell programmers have created an ecosystem where even single-letter names of types communicate information about their nature (e.g. `f` is something like a Functor, `m` is something like a monad, etc).
  * It enables programmers to use longer, more descriptive type variable names.
3. More stable with respect to changes in the signature because type variable names changes happen more rarely compared to the addition of new type variables.
4. Produces more obvious error messages: pinpoints exactly the type variable that causes the type checking error.

Costs and Drawbacks
-------------------

Development and maintenance cost: seems low.

Learnability: very low because this is a generalization of currently existing behavior. It runs at a small risk of confusing the users into believing that a similar feature exists for term-level application. This of course is not trivial to design because arguments on term-level functions often lack names i.e. the latter are defined using pattern matching.

We need to consider visual collisions with future extensions. The only examples I'm aware of that come close are:

* Example by Richard Eisenberg: ``data Proxy @{k} (a :: k)`` for explicitly inferred type variables.
* Row polymorphism.
* Record field updates on type level.

To me these examples seem to be sufficiently different either in terms of context or in terms of syntax, hence the cost seems acceptable.

Alternatives
------------

Don't do this.

Unresolved questions
--------------------

1. Interaction with NamedFieldPuns. Should this be legal?

::

  f :: forall len . ...
  g :: forall list len . ...
  g = f @{len}

Leaning **yes** for consistency and least surprise.

2. Does this need to hide behind a separate extension?

Leaning **no** because this is backwards compatible change.

Implementation Plan
-------------------

In absence of volunteers I could implement. N.B. This would be my first GHC CL.
