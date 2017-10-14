.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Deprecate ``-XTypeInType``
==========================

GHC 8.0 came with a new extension, ``-XTypeInType``. This extension activated several new features in GHC,
including the ability to write kind-indexed GADTs (``data G (a :: k) where MkG1 :: G Maybe; MkG2 :: G Int``)
explicit kind quantification (``foo :: forall k (a :: k). Proxy a -> ()``), kind families, and more.
However, it is properly seen as a generalization of ``-XPolyKinds``. Currently, GHC has to go to some
lengths to detect when users are accessing features unique to ``-XTypeInType`` but not ``-XPolyKinds``,
only to tell those users to turn on ``-XTypeInType``.

This proposal moves to deprecate ``-XTypeInType`` by expanding the meaning of ``-XPolyKinds`` to
cover the new features of ``-XTypeInType``.


Motivation
------------

* This is a simplification over the status quo, with two closely related extensions and an arbitrary
  distinction between them.

* In truth, GHC always has ``Type :: Type``, whether you say ``-XTypeInType`` or no. Thus, the real
  extension name should be ``-XPolyKinds``, because it's kind polymorphism that the user wants, not
  the always-true ``Type :: Type``.

* The reason for a distinction between the extensions was because ``-XTypeInType`` started out as
  rather buggy and experimental, whereas ``-XPolyKinds`` had settled down by GHC 8. There was the
  possibility that ``-XTypeInType`` would allow you to shoot the gorillas (my suggestion for an
  update of "launch the rockets"; the latter seems just a bit too poignant these days) while ``-XPolyKinds``
  wouldn't. That possibility has not come to fruition (happily), and so the distinction isn't
  really paying its way.

Note that what we're doing here is very much like the merger between ``-XRankNTypes`` and ``-XRank2Types``.
  
Proposed Change Specification
-----------------------------
Make ``-XPolyKinds`` and ``-XTypeInType`` be synonyms (adopting the latter's current behavior).
In time, deprecate the latter in favor of the former.

Effect and Interactions
-----------------------
This will effectively create two different versions of ``-XPolyKinds``, which could be problematic
for users who want tooling to choose compilers based on extension names. Is this a problem in practice?
I don't know. Even without this change, ``-XPolyKinds`` evolved significantly during the GHC 7 releases,
as do various other extensions, so users already have to resort to measures other that just looking
at extensions when choosing a compiler version.

Costs and Drawbacks
-------------------
This is a simplification to the implementation and description of GHC. Hooray!


Alternatives
------------
Come up with a new extension name that encompasses both ``-XTypeInType`` and ``-XPolyKinds``. All
three would be synonymous.


Unresolved questions
--------------------
None right now.


Implementation Plan
-------------------
I or a close collaborator volunteers to implement.
