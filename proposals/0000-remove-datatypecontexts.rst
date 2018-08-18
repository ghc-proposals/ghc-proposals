Remove DatatypeContexts
=======================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/161>`_.
.. sectnum::
.. contents::

I propose to remove datatype contexts from GHC.


Motivation
------------
DatatypeContexts is a Haskell feature that allows to define a constrained datatype:

::

 data Num a => T a = MkT a

As a result, ``MkT`` has type ``Num a => a -> T a``. Pattern matching on ``MkT`` requires the presence of the ``Num a`` dictionary: the function

::

 f (MkT x) = x

has type ``Num a => T a -> a``. This dictionary is discarded and not used during construction nor during pattern matching.

(This can be contrasted with GADT pattern matching: in ``data T2 a = Num a => MkT2 a``, pattern matching on ``MkT2`` provides the ``Num a`` constraint, so ``f2 (MkT2 x) = x + x`` has type ``T2 a -> a`` even though uses addition in the function body.)

This extension is commonly described as a misfeature - it forces type signatures to have constraints without giving anything in return.

DatatypeContexts is a part of Haskell 98 and 2010 standards. In 2011, the Haskell language committee `accepted <https://mail.haskell.org/pipermail/haskell-prime/2011-January/003335.html>`_ its removal as a new baseline for the next language standard. Starting from GHC 7.2 (released in 2011), DatatypeContexts is disabled by default and gives a deprecation warning when enabled.

DatatypeContexts costs to maintain. It can be combined with newtypes, GADTs and data families. It interacts with roles, `record polymorphism <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=DatatypeContexts#solving-hasfield-constraints>`_, `deriving Functor <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=DatatypeContexts#deriving-functor-instances>`_, `partial type signatures <https://github.com/ghc/ghc/commit/d831b6f41b3b89dc4a643069d5668c05a20f3c37#diff-301d72a6bf8b97cff7a0d225b9876cea>`_ and others. It's known as "stupid theta" in GHC source: `git grep stupid` shows 80+ places where it occurs.

Proposed Change Specification
-----------------------------
I propose that in GHC 8.8:

- The flag ``-XDatatypeContexts`` continues to exist, but it has no effect other than printing a deprecation message. This behavior is the same as the current flag ``-XGenerics``.
- Attempting to write a datatype context gives an error message stating that this feature was removed.
- Currently, the TH constructors ``DataD``, ``NewtypeD``, ``DataInstD``, ``NewtypeInstD`` allow to declare a context. For backwards compatibility, this is not changed. Instead, attempting to splice with a nonempty context gives an error.
- The `known bugs and infelicities <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/bugs.html>`_ list states that DatatypeContexts is not supported.


Effect and Interactions
-----------------------
The proposal simplifies the language and the compiler.


Costs and Drawbacks
-------------------
This will be another deviation from the Haskell 2010 standard. Note that GHC already does not enable DatatypeContexts by default; this proposal is about removing the possibility to go back.

Packages using DatatypeContexts will break. I downloaded the entire Hackage repository, used different regexes to search for usage and manually filtered out false positives. Out of 12907 hackage packages, I counted 181 in total using DatatypeContexts; out of those, 14 are deprecated, 120 didn't have an upload within the last 2 years, there are 47 remaining. `See details <https://gist.github.com/monoidal/77c6dd7490f7c9398db35b35273cc030>`_.

In general, programs with `DatatypeContexts` can be updated just by removing the context. In rare cases, programs depending on type defaulting can `change semantics <https://prime.haskell.org/wiki/NoDatatypeContexts>`_. I don't expect this to occur in practice; worst-case, type defaulting can be detected with ``ghc -Wall``.


Alternatives
------------
A more gradual plan is to change the tone of the deprecation warning in GHC 8.6 or 8.8, and only later remove the extension. I opted for GHC 8.8 in the proposal, since we are paying interest to maintain, and the feature has been deprecated 7 years ago.


Unresolved Questions
--------------------


Implementation Plan
-------------------
I volunteer to implement.
