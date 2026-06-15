Import And
==========

.. author:: Akhra Mellivora Gannon
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/764>`_.
.. sectnum::
.. contents::

An extension allowing multiple import declarations to be collapsed into one.


Motivation
----------
(todo: fill this out) briefly, to reduce verbosity/repetition in imports


Proposed Change Specification
-----------------------------
The ``ImportAnd`` extension implies ``ImportQualifiedPost`` and revises `Haskell 2010 Report Section 5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_ as follows.

In 5.3, *impdecl* becomes:

  | *impdecl* 	→ 	``import`` [``qualified``] *modid* [*impparams*]
  | *impparams*  	→ 	  [``qualified``] [``as`` *modid*] [*impspec*] [``and`` *impparams*]

The list of *varid* symbols in the final paragraph is updated to include ``and``.

``and`` serves as syntactic sugar, heralding an additional import declaration for the same *modid*. No clauses of the parent declaration other than *modid* are reproduced.


Examples
--------
(todo: more in-depth?)

Haskell 2010 + ``ImportQualifiedPost``:
::
  import Data.Map qualified as Map
  import Data.Map (Map)

As proposed:
::
  import Data.Map (Map) and qualified as Map

Or, equivalently:
::
  import Data.Map qualified as Map and (Map)
 

Effect and Interactions
-----------------------
(todo)


Costs and Drawbacks
-------------------
This should be implementable as syntactic sugar for multiple actual ``import`` lines, which hopefully reduces both the direct costs and the learning/reading load of additional syntax.

Admitting multiple orderings with the same meaning may raise the reading load slightly, but this a wash against status quo where multiple imports of the same module might not even be consecutive.

``import M qualified and`` is a legal declaration with potentially surprising behavior (full unqualified import). However, both ``-Wmissing-import-lists`` and ``-Wunused-imports`` should catch this.

It should also be acknowledged that any new syntax has a downstream impact on tooling.


Backward Compatibility
----------------------
Impact level 0: ``ImportAnd`` admits all existing programs with no change in behavior.


Alternatives
------------

Guards Syntax
^^^^^^^^^^^^^
`impparams` could also be heralded by guards:
::
  import Data.Map
    | qualified as Map
    | (Map)

Unless empty guards are permitted (unprecedented?) there is at least one realistic use case this does not trivially support: a full unqualified import coupled with a selective aliased import (e.g. for re-export).

Without permitting empty guards, this case can still be covered with a spurious unqualified alias. Also, modern tooling generally nudges the user toward specifying imports rather than pulling a whole module unqualified.

Import Exposing (#763)
^^^^^^^^^^^^^^^^^^^^^^
Proposal `#763 <https://github.com/ghc-proposals/ghc-proposals/pull/763>`_ enables what may be the most common use case for this proposal, the combination of a qualified import with a specified set of top-level imports; and for that case, it is more concise. It also resembles `a much older proposal <https://gitlab.haskell.org/ghc/ghc/-/issues/10478>`_ which was initially popular but lost momentum due to conflicts with existing syntax, which #763 avoids.

Speaking as the author of both proposals: ``ImportAnd`` is more expressive, seems easier to implement, and doesn't smell of special-case. However, ``ImportExposing`` may be conceptually simpler for users, and received enough initial interest to justify leaving it open as an alternative pending further feedback on both.


Unresolved Questions
--------------------


Implementation Plan
-------------------
Tentatively and with a likely need for guidance, the author can attempt implementation if nobody else finds it an exciting project.


Endorsements
-------------


Acknowledgments
---------------
- `tek <https://github.com/tek>`_ for the guards syntax alternative `suggestion <https://github.com/ghc-proposals/ghc-proposals/pull/764#issuecomment-4702415913>`_
