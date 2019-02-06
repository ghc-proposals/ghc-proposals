Fix the treatment of type variables in quotations
=================================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

There is a `current bug <https://ghc.haskell.org/trac/ghc/ticket/15437>`_ in
Template Haskell which leads to out of scope type variables in generated
programs. We solve this problem by applying the
same principles of cross stage persistence to types as well as values.


Motivation
----------

Consider this simple Template Haskell program.::

   import Language.Haskell.TH.Syntax (Q, TExp)

   get :: forall a. Int
   get = 1

   foo :: forall a. Q (TExp Int)
   foo = [|| get @a ||]

In the definition of ``foo`` we quote the function ``get`` and apply it to
a single type argument ``a``. However, when we splice in ``foo``, we find
that GHC issues an error as ``a`` has escaped its scope.

.. code::

   Test.hs:6:8: error:
    • The exact Name ‘a’ is not in scope
        Probable cause: you used a unique Template Haskell name (NameU),
        perhaps via newName, but did not bind it
        If that's it, then -ddump-splices might be useful
    • In the result of the splice:
        $foo
      To see what the splice expanded to, use -ddump-splices
      In the Template Haskell splice $$(foo)
      In the expression: $$(foo)
     |
   6 | f = $$(foo)
     |        ^^^

Now analysing ``foo``, we see that ``a`` is bound at stage 0 but used in stage
1. The problem is that we don't check for stage violations in types like we do
for variables. In the value case, a cross stage reference to ``x`` is interpreted
as ``$(lift x)``. The idea is to do precisely the same for types as well.

Proposed Change Specification
-----------------------------

The proposal is that we apply the principle of cross-stage to types. In order
to do this we implement a new class called ``LiftT`` which is the type
level analogue of ``Lift``. It's definition is as follows::

   class LiftT (t :: k) where
      liftTyCl :: Q Type

What this means is that given a type, we can return the Template Haskell
representation of that type.

Then, when we see a variable which violates the stage principle we insert an
implicit splice just as with terms. So now in pseudo-haskell, the original
program would be modified as follows after renaming::

   foo :: forall a. LiftT a => Q (TExp Int)
   foo = [|| get @$$(liftTyCl @a) ||]

There are two details to notice.

1. The addition of the ``LiftT`` constraint.
2. The stage correction by replacing ``a`` with ``$(liftTYCl @a)``.

Unlike ``Lift`` we magically solve all instances of ``LiftT``. It is forbidden
to write user defined instances as the compiler can generate representations
for all the types it can represent. The solving works recursively, emitting
constraints for all free type variables in the type.

For example there are instances for the following type::

   LiftT ()
   LiftT Int
   LiftT "abc"
   LiftT 5
   LiftT a => LiftT (Eq a)
   LiftT (forall (a :: Type) . a -> a)
   (LiftT a, LiftT b) => LiftT (a, b)

.. note::
   This idea is very similar to how type variables are already handled by
   the ``StaticPointers`` extension. In that case, free variables give rise to
   ``Typeable`` constraints rather than ``LiftT`` constraints but the principle
   is the same.


Effect and Interactions
-----------------------

The primary question of the proposal is why to use ``LiftT`` rather than ``Typeable``.
The problem with ``Typeable`` is that it only supports monotypes because
the representation is type indexed. We want to be able to represent polytypes
as well because it's pefectly possible to represent them. As our representation
is not type indexed, and doesn't claim support for comparing equality of type
representations, it is easier to implement representations for complicated types.

There is also the question of why we only allow implicit splices rather than
explicit splices in these positions. With a kind-indexed type representation
(like ``Q (TExp a)`` but for types), it would be possible to support typed
type splicing as well. For now, the proposal is merely concerned with implicit
lifting.


Costs and Drawbacks
-------------------

There are likely quite a lot of user programs will break when this change is
enabled but it will fix a long standing bug.

Invisible arguments such as kind variables need also be subject to the cross
stage restriction lest they also escape their scope. This can lead to quite
hard to understand failures when the ``PolyKinds`` extension is enabled.


Alternatives
------------

The alternative is to use ``Typeable`` instead but this is wholly unappealing
due to the monotype restriction.


Unresolved Questions
--------------------

There are none.


Implementation Plan
-------------------

I have already `implemented the proposal <https://gitlab.haskell.org/ghc/ghc/merge_requests/166>`_.
