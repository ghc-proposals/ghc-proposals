Warn on prefix/suffix occurrences of operators
==============================================

.. author:: Richard Eisenberg
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

Proposal `#229`_ specifies that operators can occur in one of four contexts:
*prefix* (like ``a +1``), *suffix* (like ``a+ 1``), *tight infix* (like ``a+1``),
and *loose infix* (like ``a + 1``). Various constructs steal syntax from prefix
and suffix occurrences of operators (examples below). This proposal suggests
warnings on prefix or suffix uses of operators, so that Haskellers are forewarned
that the syntax might be stolen in the future.

.. _`#229`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst


Motivation
----------
There are currently a number of symbols which change meaning (with certain
extensions enabled) depending on
whether they occur prefix, suffix, or infix (adapted from `#229`_):

  +-------------------+---------------------+--------------------------------------------+
  | Operator          | Occurrence          | Meaning override                           |
  +===================+=====================+============================================+
  | ``!``, ``~``      | prefix              | strictness annotation in types,            |
  |                   |                     | bang/lazy pattern in term-level patterns   |
  +-------------------+---------------------+--------------------------------------------+
  | ``$``, ``$$``     | prefix              | untyped/typed Template Haskell splice      |
  +-------------------+---------------------+--------------------------------------------+
  | ``@``             | prefix              | type application                           |
  +-------------------+---------------------+--------------------------------------------+
  | ``@``             | tight infix         | as-pattern                                 |
  +-------------------+---------------------+--------------------------------------------+
  | ``@``             | suffix              | parse error                                |
  +-------------------+---------------------+--------------------------------------------+
  | ``-``             | prefix              | negation                                   |
  +-------------------+---------------------+--------------------------------------------+
  | ``%``             | prefix              | syntax ``a %1 -> b`` is a linear function  |
  +-------------------+---------------------+--------------------------------------------+
  | ``#``             | prefix              | overloaded label                           |
  +-------------------+---------------------+--------------------------------------------+
  | ``#``             | suffix              | the "magic hash"                           |
  +-------------------+---------------------+--------------------------------------------+

I'm confident that, in time, we will want to add to this list. So we should
start warning users not to write new code with prefix/suffix occurrences of
operators. (Note that the only operator with a special *tight infix* meaning
is ``@``, which cannot be used as a user-definable operator.)

The very concrete motivation for this proposal is a `question <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4020#note_302858>`_ on a ticket about the implementation of
the syntax for linear types. The question is how GHC should treat e.g. ``Int %1 -> Int``
if ``-XLinearTypes`` is not enabled. GHC could suggest enabling ``-XLinearTypes``,
or it could complain that no type operator ``%`` is in scope. If we were more
confident that users would avoid prefix uses of ``%``, then it would be a clearer
decision of what to do: we should just suggest enabling ``-XLinearTypes``.

Proposed Change Specification
-----------------------------
Introduce a new warning ``-Woperator-spacing`` that will be reported whenever
an operator is used in either a prefix or suffix position, in either types
or terms.

The new warning will be part of the ``-Wall`` and ``-Wcompat`` warning
groups, but not on by default.

Examples
--------
The following will warn::

  a +b
  b- a
  c *(4)

The following will not warn::

  a + b
  b-a
  c*(4)

Effect and Interactions
-----------------------
This new warning will encourage users to avoid prefix and suffix uses
of binary operators, making future attempts to steal their syntax easier.

Prefix and suffix uses of operators already look strange (to me), and
so this is probably a style improvement, anyway.

Note that the stealing of prefix and suffix syntax has been going on
for a long time in GHC, such as for Template Haskell and the ``-XMagicHash``
extension.

Costs and Drawbacks
-------------------
There will be a small maintenance burden.

Some users may find their preferred style is now warned against.

Alternatives
------------
Do nothing. But doing nothing means more debates later when we need
to steal more syntax.

Unresolved Questions
--------------------
Should this warning be extended to operator-like syntax, like ``::``,
``<-``, and ``->``? This non-exhaustive list includes syntax that operates
something like binary operators, but are not considered binary operators
in Haskell's syntax. It is a free choice to whether this proposal should
extend to these. I have decided not to do so, in thinking that these key
bits of syntax are less likely to be stolen. However, it may be sensible
to include them in the warning.
