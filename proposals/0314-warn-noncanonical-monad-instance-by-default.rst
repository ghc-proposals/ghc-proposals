Enable ``-Wnoncanonical-monad-instances`` and ``-Wnoncanonical-monoid-instances`` by default
============================================================================================

.. author:: Fumiaki Kinoshita
.. date-accepted:: 2020-04-26
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3174
.. implemented:: GHC-9.2
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/314>`_.
.. contents::


Currently, ``-Wnoncanonical-monad-instances`` and
``-Wnoncanonical-monoid-instances`` are not enabled in any of the
default, ``-Wall`` and ``-Wcompat``. I propose enabling the warning by
default.

Motivation
----------

The prerequisite of the Phase 2 of `monad of no
return <https://gitlab.haskell.org/ghc/ghc/wikis/proposal/monad-of-no-return>`__
is to wait until “we’re confident that the majority of Hackage has
reacted to the warning”. However, a warning that’s disabled unless you
specify it individually gives infinitesimally small incentive to the
ecosystem, so we can’t really expect them to react. In order to avoid
catastrophic breakage like what we experienced when ``fail`` got
removed, I think the warning should be enabled by default, not just in
``-Wall`` or ``-Wcompat``.

I also propose enabling ``-Wnoncanonical-monoid-instances`` for the same
reason.

Proposed Change Specification
-----------------------------

-  Add ``-Wnoncanonical-monad-instances`` and
   ``-Wnoncanonical-monoid-instances`` to the set of default warnings.

Examples
--------

On GHC 8.8 (and 8.11.0.20200108), the following code compiles without
warnings.

.. code:: haskell

   data P a = P

   instance Functor P where
     fmap _ _ = P

   instance Applicative P where
     pure _ = P
     _ <*> _ = P

   instance Monad P where
     return _ = P
     _ >>= _ = P

   instance Semigroup (P a) where
     _ <> _ = P

   instance Monoid (P a) where
     mempty = P
     mappend _ _ = P

With this proposal implemented, it would produce the following warning:

::

   /path/to/example.hs:12:3: warning: [-Wnoncanonical-monad-instances]
       Noncanonical ‘return’ definition detected
       ‘return’ will eventually be removed from the ‘Monad’ class in favour of ‘pure’.
       Either remove definition for ‘return’ (recommended) or define as ‘return = pure’
       See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
      |
   12 |   return _ = P
      |   ^^^^^^^^^^^^

   /path/to/example.hs:12:3: warning: [-Wnoncanonical-monoid-instances]
       Noncanonical ‘mappend’ definition detected
       in the instance declaration for ‘Monoid (P a)’.
       ‘mappend’ will eventually be removed from the ‘Monoid’ class in favour of ‘(<>)’.
       Either remove definition for ‘mappend’ (recommended) or define as ‘mappend = (<>)’
       See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/semigroup-monoid
      |
   20 |   mappend _ _ = P

Note that it also updates the warning messages, adding a URL and a
notice that the method will be removed.

Effect and Interactions
-----------------------

Once this proposal is implemented, people are more likely to notice non
canonical definitions of ``return`` and ``mappend``.

Costs and Drawbacks
-------------------

People are more likely get annoyed when they see the warnings.

Alternatives
------------

Enable it only in one or more of ``-Wcompat`` or ``-Wall``. Not everyone
specifies these flags so more packages are likely to break without a
caution beforehand.

Unresolved Questions
--------------------

N/A

Implementation Plan
-------------------

Once approved, @fumieval can (hopefully) submit code changes.

Endorsements
------------
