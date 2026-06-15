Import Exposing (implicitly qualified)
======================================

.. author:: Akhra Mellivora Gannon
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/763>`_.
.. sectnum::
.. contents::

An extension allowing qualified import declarations to selectively expose top-level identifiers.


Motivation
----------
This proposal hybridizes parts of (now closed) `#758 Import Exposing <https://github.com/ghc-proposals/ghc-proposals/pull/758>`_ and `#760 Default Import Qualified <https://github.com/ghc-proposals/ghc-proposals/pull/760>`_, sharing core motivations with both but compromising toward streamlined impact. In particular, it posits that likely the most common point point of redundancy in imports is the pattern:
::
  import Data.Map (Map)
  import qualified Data.Map as Map

Thus it aims to address this in isolation, without reaching for broader impact.


Proposed Change Specification
-----------------------------
Adds the ``ImportExposing`` extension, which revises `Haskell 2010 Report Section 5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_ as follows.

In 5.3, *impspec* is extended with an ``exposing`` form:

  | *impspec* 	→ 	…
  |   | 	``exposing`` ( *import*:subscript:`1` , … , *import*:subscript:`n` [ , ] ) 	    *(n ≥ 0)*

The list of *varid* symbols in the final paragraph is updated to include ``exposing``.

In 5.3.1, the following is inserted as list item 3:

  | 3. Using the form ``exposing`` (*import*:subscript:`1` , … , *import*:subscript:`n`) causes the import declaration to be evaluated as if it had used the ``qualified`` keyword (per 5.3.2) with **no** *impspec*, and additionally extends the top-level environment with the specified identifiers. (Presence or absence of ``qualified`` is ignored in this form.)


Effect and Interactions
-----------------------
Under this proposal, the example in the motivation can be expressed as:
::
  import Data.Map as Map exposing (Map)

Notably this is very similar to `an older proposal <https://gitlab.haskell.org/ghc/ghc/-/issues/10478>`_, but does not suffer the same conflicts with existing syntax.


Costs and Drawbacks
-------------------
Special behavior obviating a separate clause of the import declaration (``qualified``) is unprecedented and may carry extra implementation cost, as well as being a unique pattern for users to learn.

``import M exposing ()`` becomes a synonym for ``import qualified M``. Despite being longer, this is likely desirable for consistent style when using the extension; but as above, it is another pattern to learn. It is also considerably worse than `#760 <https://github.com/ghc-proposals/ghc-proposals/pull/760>`_ for this case; this proposal presumes that the case it improves is more common (or at least more commonly irritating) than the one it doesn't.

It should also be acknowledged that any new syntax has a downstream impact on tooling.


Backward Compatibility
----------------------
Impact level 0: ``ImportExposing`` admits all existing programs with no change in behavior.


Alternatives
------------

#760+#764
^^^^^^^^^
In combination, these two proposals do everything this one does and more. However, `#760 <https://github.com/ghc-proposals/ghc-proposals/pull/760>`_ has level 5 compatibility impact. Meanwhile `#764 <https://github.com/ghc-proposals/ghc-proposals/pull/764>`_ is considerably more expressive, but less concise on its own for the ``ImportExposing`` use case.

Warn for ``qualified``
^^^^^^^^^^^^^^^^^^^^^^
Because ``qualified`` and ``exposing`` are redundant it may be appropriate to issue a warning if they are used together. (This could probably fit under ``-Wunused-imports``, despite not being strictly that.)


Unresolved Questions
--------------------


Implementation Plan
-------------------
Tentatively and with a likely need for guidance, the author can attempt implementation if nobody else finds it an exciting project.


Endorsements
-------------


Acknowledgments
---------------
- `VitWW <https://github.com/VitWW>`_, for proposing this syntax in `discussions on #760 <https://github.com/ghc-proposals/ghc-proposals/pull/760#issuecomment-4661677360>`_.
