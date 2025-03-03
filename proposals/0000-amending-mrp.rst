Amending Monad of No Return Proposal
==============

.. author:: Benjamin
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/687>`_.
.. sectnum::
.. contents::

The proposed migration strategy for the proposal `Monad of No Return 
<https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return>`_ 
uneccessarily splits warning and error phases, and requires the movement of 
methods before it is neccessary. Recent discussion has also revealed a flaw in
the original design with regards to the performance of the default 
implementation of ``(*>)``.

Motivation
----------
The Monad of No Return proposal has stagnated for many years, and it would be 
good to move to the next stages of this long term proposal. To that end, we
should streamline the next steps so that they can be completed quickly, while
still respecting people's breakage requirements.

It has also been brought to light that the default implementation of ``(*>)``
has a space leak, and replacing ``(>>)`` with ``(*>)`` without thought could 
result in performance implications for many programs.

Recent discussion has taken place `on the discourse <https://discourse.haskell.org/t/monad-of-no-return-next-steps/11443/>`_.
Other links of interest include the `current proposal <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return>`_,
the `original proposal <https://mail.haskell.org/pipermail/libraries/2015-September/026121.html>`_,
rhendric's `current implementation <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3174>`_
of the amended phase 2 (see below) and associated `issue <https://gitlab.haskell.org/ghc/ghc/-/issues/25783>`_.
There is also the enablement of the warnings by default on `gitlab <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3174>`_
and its `own proposal <https://github.com/ghc-proposals/ghc-proposals/pull/314>`_.


Proposed Change Specification
-----------------------------
To summarise the proposal to date:

* When making ``Applicative`` a superclass of Monad, there are now redundant 
  methods in ``Monad`` in ``return`` and ``(>>)``, as these can always (modulo
  performance) be replaced with ``pure`` and ``(*>)`` respectively
* The first phase was completed in 2015, that of adding a warning (off by 
  default) that warns when definitions of ``return`` and ``(>>)`` are present
  that aren't just making them equivalent to ``pure`` and ``(*>)`` respectively.
  This is ``-Wnoncanonical-monad-instances``
* In ~2020 this warning was enabled by default in 9.2.1

The next phases of the proposal have not been moved forward until very recently.

* Phase 2 requires that the previous warning is now an error and that 
  ``return`` and ``(>>)`` are top level bindings, but also that canonical
  definitions are ignored
* Phase 3 would then start warning about canonical ``return`` and ``(>>)``
  definitions
* Phase 4 would remove support for any sort of override, making that an error

A lawful method is simply a method that follows the laws of the typeclass.
A canonical method is one that is always definitionally the same, and has no 
greater power. For example, ``return`` and ``pure`` must always be equivalent, 
and there is no reason that they should not be defined in terms of each other. 
The proposal seeks to eliminate the presence of the excess canonical methods
that can be defined in ``Monad``.

This proposal would adjust the next phases to reduce the number of changes
neccessary in GHC as follows:

* Phase 2 makes ``-Wnoncanonical-monad-instances`` a compiler error by default,
  and adds a ``-Wredundant-canonical-monad-method`` warning that makes canonical
  method definitions (only ``return = pure``) warn
* Phase 3 would then move ``return`` and ``(>>)`` to the top level, and remove
  the original and new warnings (after removing these methods from ``Monad`` the
  compiler will emit an error like for any other extraneous typeclass instance
  method definition)

  * Phase 3 shouldn't be  (fully) implemented until research is done as to what 
    we want the eventual definition of ``(>>)`` to be

This reduces the number of steps neccessary, and means that we wouldn't need
some special casing about ignoring canonical definitions while we have a top
level one.

Another adjustment to the plan is to adjust the warnings and errors about
``(>>)`` to encourage a performant definition of ``(*>)`` instead of blindly
replacing its definition. We wish to do this because the default
implementation of ``(*>)`` (that of ``(*>) a b = (id <$ a) <*> b``) leaks space,
and thus any and every ``Monad`` that currently uses the more space efficient
``(>>)`` would start leaking space and degrading program performance.

In addition to the phase changes above, it would be good to accelerate the
Semigroup-Monoid proposal under a similar schedule.
To that end, I ask that we complete steps for both at the same time (both get 
warnings and errors, both get methods moved, etc), since the breakage and change
requirements in GHC will be very similar.

Proposed Library Change Specification
-------------------------------------

One library change that is not outlined fully in the original proposal
is that currently the default implementation of ``(>>)`` uses ``Monad``'s
``(>>=)`` internally, but when we move it to the top level users will no longer
be able to overwrite it. We should be specific that we intend to leave ``(>>)``
as it is for now, or to use ``(*>)`` if that has good performance at that point.

Examples
--------
Example file:
::
  {-#LANGUAGE DeriveAnyClass#-}

  newtype Id a = MkId a deriving (Functor, Applicative)
  newtype Id2 a = MkId2 a deriving (Functor, Applicative)

  instance Monad Id where
    return = MkId
    (>>) _ b = b
    (>>=) = undefined

  instance Monad Id2 where
    return = pure
    (>>) = (*>)
    (>>=) = undefined

Current warnings:
::
  example.hs:6:3: warning: [-Wnoncanonical-monad-instances]
      Noncanonical ‘return’ definition detected
      in the instance declaration for ‘Monad Id’.
      ‘return’ will eventually be removed in favour of ‘pure’
      Either remove definition for ‘return’ (recommended) or define as ‘return = pure’
      See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
    |
  6 |   return = MkId
    |   ^^^^^^^^^^^^^

  example.hs:7:3: warning: [-Wnoncanonical-monad-instances]
      Noncanonical ‘(>>)’ definition detected
      in the instance declaration for ‘Monad Id’.
      ‘(>>)’ will eventually be removed in favour of ‘(*>)’
      Either remove definition for ‘(>>)’ (recommended) or define as ‘(>>) = (*>)’
      See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
    |
  7 |   (>>) _ b = b
    |   ^^^^^^^^^^^^

Eventual warnings and errors (expected):
::
  example.hs:7:5: error: [-Wnoncanonical-monad-instances]
      Noncanonical ‘return’ definition detected
      in the instance declaration for ‘Monad Id’.
      ‘return’ will eventually be removed in favour of ‘pure’
      Remove definition for ‘return’
      See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
    |
  7 |     return = MkId
    |     ^^^^^^^^^^^^^

  example.hs:8:5: error: [-Wnoncanonical-monad-instances]
      Noncanonical ‘(>>)’ definition detected
      in the instance declaration for ‘Monad Id’.
      ‘(>>)’ will eventually be removed in favour of ‘(*>)’
      Remove definition for ‘(>>)’, and implement ‘(*>)’ with an efficient definition.
      See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
    |
  8 |     (>>) _ b = b
    |     ^^^^^^^^^^^^

  example.hs:12:5: error: [-Wredundant-canonical-monad-instances]
      ‘return’ definition detected
      in the instance declaration for ‘Monad Id’.
      ‘return’ will eventually be removed in favour of ‘pure’
      Remove definition for ‘return’
      See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
    |
  12|     return = pure
    |     ^^^^^^^^^^^^^

  example.hs:13:5: error: [-Wredundant_canonical-monad-instances]
      ‘(>>)’ definition detected
      in the instance declaration for ‘Monad Id’.
      ‘(>>)’ will eventually be removed in favour of ‘(*>)’
      Remove definition for ‘(>>)’, and implement ‘(*>)’ with an efficient definition.
      See also: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return
    |
  13|     (>>) = (*>)
    |     ^^^^^^^^^^^

Effect and Interactions
-----------------------
Speeding up the phases of the proposal means that we will eventually be rid of
the warnings and errors we are building up in service to this proposal.
Encouraging users to implement ``(*>)`` efficiently will also mean that more
programs are likely to be performant.

The alternative is that we warn against a change that we are not intending on 
making.


Costs and Drawbacks
-------------------
rhendric has already made a MR to perform phase 2 of the amended proposal, which
can be found `here <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13999>`_.
As such the only code cost would be for stage 3 of the amended proposal.

Old tutorials may become more incorrect as they will no longer be able to define
``Monad`` fully.

Backward Compatibility
----------------------
This amendment would mean that we have breakage in uncommon cases. In one 
attempt to compile Stackage with a patched GHC, ~94% of packages that were 
attempted compiled. However, there are a handful of packages that do not want
to accept fixes that would make them compatible with the changes in this 
proposal which are depended upon by hundereds of other packages, which makes
further analysis hard.

However, the compiler error that is coming as a result of this proposal has been
a warning since 2015, and has been on by default since 9.2.1 in 2020, so there
shouldn't need to be more lead in.

Alternatives
------------
Instead of amending the MRP proposal, we could try to do the proposal as 
written.

Unresolved Questions
--------------------
See `Proposed Library Change Specification` on the question of the eventual
definition of ``(>>)``.

As suggested by Teo on the Discourse thread, we could put the breaking changes
behind a language extension. This language extension would be added to the next 
GHCXXXX language edition. New code would therefore be disallowed from giving
definitions of these methods, while old code would continue to compile. This
comes with the disadvantage that we would have to keep the methods in the
typeclass.

However, I believe this is the incorrect move as we then have an increasingly
complex combination of states to support, instead of cleaning up historical 
warts.

Implementation Plan
-------------------
rhendric has already done the work for the amended phase 2 of this proposal. I'm
happy to continue urging this forward for now.

Endorsements
-------------
None yet.
