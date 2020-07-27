Negative Literals Improved
==========================

.. author:: Vladislav Zavialov
.. date-accepted:: 2020-07-22
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3624
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/344>`_.
.. contents::


At the moment, ``NegativeLiterals`` has the following issue, documented
in the User’s Guide:

   One pitfall is that with ``NegativeLiterals``, ``x-1`` will be parsed
   as ``x`` applied to the argument ``-1``, which is usually not what
   you want. ``x - 1`` or even ``x- 1`` can be used instead for
   subtraction.

We propose to fix this.

Additionally, unboxed literals (e.g. ``-1#``) have the same issue even
without the ``NegativeLiterals`` extension. We propose to fix this, too.

Motivation
----------

1. Having ``x-1`` parsed as ``x (-1)`` may violate users’ expectations.
   As evidence, see
   `#18022 <https://gitlab.haskell.org/ghc/ghc/-/issues/18022>`__.

2. Fixing this infelicity would make ``NegativeLiterals`` a subset of
   ``LexicalNegation``.

Proposed Change Specification
-----------------------------

When lexing a negative literal, require that it is *not preceded by a
closing token*. See
`#229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`__
for the definition of a closing token.

Effect and Interactions
-----------------------

-  Code that relies on ``x-1`` being parsed as ``x (-1)`` will break.

-  Enabling ``-XLexicalNegation -XNegativeLiterals`` has the same effect
   as enabling ``-XLexicalNegation`` alone; however, enabling
   ``-XNegativeLiterals`` alone does not have the same effect, as it
   does not affect expressions such as ``-x``. In other words,
   ``NegativeLiterals`` becomes a subset of ``LexicalNegation``.

Costs and Drawbacks
-------------------

Changing the behavior of an existing extension may not be the best
practice.

Alternatives
------------

-  Don’t change anything.
-  Deprecate ``NegativeLiterals`` in favor of ``LexicalNegation``.

Unresolved Questions
--------------------

None at the moment.

Implementation Plan
-------------------

Implemented in `Merge Request
3624 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3624>`__.
