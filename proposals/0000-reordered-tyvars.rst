Allow binding type variables out of dependency order
====================================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/131>`_.
.. sectnum::
.. contents::

GHC currently requires us to write kind variables before type variables that
mention those kind variables. This proposal suggests dropping that requirement,
allowing the type/kind variables listed in one ``forall`` to be listed in any
order, as long as there are no cycles.

For example, it is more useful to instantiate the type variable of ``typeRep :: forall k (a :: k). Typeable a => TypeRep a``
than the kind variable, but the rules for type applications require (if ``typeRep`` had that signature) us to instantiate
the kind first. Under this proposal, we could write ``typeRep :: forall (a :: k) k. Typeable a => TypeRep a`` and then
use ``typeRep @Int`` to get the representation for ``Int``. Someone who wanted to could write ``typeRep @Int @Type``
as well, if they wanted to be fully explicit.

Motivation
------------
Listing variables in dependency order is all well and good in an internal language, but there seems to be
no great need for this restriction in surface Haskell, where we're all quite used to declaring things out
of order. (See the ``where`` syntax for an example.) The lack of ability to declare variables out of
dependency order has led to proposal `#99`_ (introducing "explicit specificity)
and to complications in `#96`_ / `#126`_ (allowing the binding of type variables via an ``@`` notation,
but where one might have to skip over kind variables with annoying uses of ``@_``). Let's just lift
this restriction.

.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/pull/99
.. _`#96`: https://github.com/ghc-proposals/ghc-proposals/pull/96
.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`#102`: https://github.com/ghc-proposals/ghc-proposals/pull/102

Proposed Change Specification
-----------------------------
All type variables brought into scope in a ``forall`` or in a type declaration (e.g., the variables
``a b`` in ``class C a b``) would scope over the entire set of such variables. Call the set of variables
a *pantelescope* (as opposed to a normal telescope, where later variables can depend on earlier ones).
A pantelescope is well-formed if and only if it is a permutation of a well-formed telescope. This would
be checked by the type checker.

This feature would not have a new extension, but instead would become part of ``-XPolyKinds``.

Effect and Interactions
-----------------------
This would likely kill the primary motivation for `#99`_.

If `#102`_ is ever accepted, ``foreach`` could also introduce a pantelescope.

Costs and Drawbacks
-------------------
Would this make Haskell easier or harder to understand? It's not obvious to me.

I imagine the implementation would be very challenging, given GHC's current practice of using Core types
as Haskell types. Instead, we would need to store a translation from one to the other, possibly by storing
a permutation with every (nested) ``forall``. I also don't relish sorting out type-checking in the presence
of this extension.

Alternatives
------------
I think `#99`_ is the best alternative.


Unresolved questions
--------------------
Is this at all feasible? I actually think it isn't, but I still felt it worth articulating. GHC didn't become
what it is today by dismissing ideas simply because they seemed impossible to implement. I *do* think this idea
is well-specified and would be useful. Perhaps if it is well liked it can be accepted with a milestone of
⊥.

Implementation Plan
-------------------
I do *not* volunteer to implement.
