Clean up printing of foralls
============================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/179>`_.
.. sectnum::
.. contents::

GHC has two infelicities around the way it prints ``forall`` types around visible type application.
This proposes a fix for both. The two
fixes are entirely separable, but it seems sensible to debate them together.

Motivation
----------
With visible type application, we have a distinction between *specified* variables and *inferred* ones.
In brief, a specified variable is one that has been written by the user, whereas an inferred variable
is invented by GHC. Here is an example::

  id :: a -> a
  id x = x

  id2 x = x

The type of ``id`` contains a specified variable, ``a``, written by the user. Though ``id2`` will be
inferred to be polymorphic, its type variable is inferred, not specified.

Visible type application works only with specified variables. This is because we can rely on the ordering
and existence of specified variables only; inferred variables might be reordered between different minor
versions of GHC. Thus, ``id @Int 5`` is accepted, while ``id2 @Int 5`` is rejected. None of this is new.

There are two problems with the status quo:

1. GHC prints braces around inferred variables, but only with ``-fprint-explicit-foralls`` on. This is
   sometimes confusing. For example::

     GHCi, version 8.7.20181028: http://www.haskell.org/ghc/  :? for help
     Prelude> :set -XPolyKinds
     Prelude> import Type.Reflection
     Prelude Type.Reflection> :t +v typeRep
     typeRep :: forall k (a :: k). Typeable a => TypeRep a
     
     Prelude Type.Reflection> :set -fprint-explicit-foralls
     Prelude Type.Reflection> :t +v typeRep
     typeRep :: forall {k} (a :: k). Typeable a => TypeRep a

   Even without ``-fprint-explicit-foralls``, we get an explicit ``forall`` because the type is quantified
   over a poly-kinded type variable. (This is described in the manual. We had to turn on ``-XPolyKinds`` to
   prevent GHCi from defaulting the kind variable to ``Type``.) But, lo and behold, the ``k`` is actually
   *inferred*, not *specified*, only witnessed after ``-fprint-explicit-foralls`` is enabled.

2. The type printed by ``:type <expr>`` is the type that would be inferred for ``it`` if we said
   ``let it = expr``. This means that ``<expr>``\'s type is instantiated and regeneralized. This
   instantiating/regeneralizing allows us to solve some constraints. For example::

     Prelude> :set -XTypeApplications
     Prelude> :t (+) @Int
     (+) @Int :: Int -> Int -> Int

   Without instantiating, we would still see the ``Num Int =>`` constraint in the printed type.

   However, this is awfully confusing with visible type application::

     Prelude> :t map
     map :: forall {a} {b}. (a -> b) -> [a] -> [b]

   Both of ``map``\s type variables are *specified*, yet GHC does not print it accordingly. This is
   because the varibales are instantiated and then rengeneralized. In the process, GHC declares them
   to be *inferred*, because they were regeneralized. To override this behavior, we must use ``:t +v``::

     Prelude Type.Reflection> :t +v map
     map :: forall a b. (a -> b) -> [a] -> [b]
     
Proposed Change Specification
-----------------------------
1. Whenever printing variables quantified in a ``forall``, print inferred variables with braces.

2. If the expression passed to ``:type`` is a single identifier (including symbolic identifiers in
   parentheses), do not instantiate. Instead, treat ``:type`` just like ``:type +v`` in this case.

Effect and Interactions
-----------------------
This change will fix the two infelicities described in the motivation.

Costs and Drawbacks
-------------------
Both changes are trivial to implement.

The drawback to change (1) is that it means GHC is printing more fancy widgets in types. Without
``-XTypeApplications``, users do not care about the inferred/specified distinction and may be
unfamiliar with the new notation.

The drawback to change (2) is that it requires a special case, and users might stumble over the
special case. For example, if we define ::

  constMap :: x -> (a -> b) -> [a] -> [b]
  constMap _ = map

and then ask ``:type constMap undefined``, we'll get a different printed type than we would from ``:type map``;
the latter would be the special case while the former wouldn't be. However, this drawback seems
smaller than the drawback of the status quo, where one might reasonably not be aware of the importance of
using ``:type +v`` when doing visible type application.

Alternatives
------------
These are free design decisions. No specific alternatives come to mind, but there's a wide array of
possibilities here.


Unresolved Questions
--------------------
None at this time.


Implementation Plan
-------------------
This is all great stuff for a newcomer if you'd like to try your hand.
