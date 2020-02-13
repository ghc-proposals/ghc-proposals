Unrestricted Overloaded Labels
==============================

.. author:: howtonotwin
.. date-accepted:: 2018-11-05
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/11671
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/170>`_
.. contents::

``OverloadedLabels`` are a useful feature, but they have somewhat unexpected syntax restrictions on what is a valid label. This proposal exists to alleviate this.

Motivation
------------
``#foo`` is a valid expression with ``-XOverloadedLabels``, but ``#Foo`` is not. This is somewhat surprising, as ``#foo`` is only syntax for ``fromLabel @"foo"``. This can mean anything, so it doesn't make sense to enforce a case distinction as done for constructors and functions. Similarly, ``#3`` is not valid, nor is any other number. There isn't much reason against this, either.

There are real world cases in the ticket, usually because libraries want to define the syntax ``#Foo`` to do something constructor related.

Proposed Change Specification
-----------------------------
Immediately following a ``#``, with no intervening whitespace, allow any nonempty string of characters that is composed out of the same characters that make up variable names and numeric literals. Also, allow strings of characters that look like string literals. In the language of the Report, labels are defined by:

| *labelChar* → *small* | *large* | *digit* | ``'`` | ``.``
| *label* → ``#``\ (*string* | *labelChar* {*labelChar*})

A "quoted" label ``#``\ ⟨string⟩ translates to ``fromLabel @``\ ⟨string⟩, and a "bare" label ``#``\ ⟨labelChars⟩ translates to ``fromLabel @"``\ ⟨labelChars⟩\ ``"``. This is a table of some example labels and their translations.

========================== ==================================== ============
Label syntax               Translation                          Works today?
========================== ==================================== ============
``#a``                     ``fromLabel @"a"``                   Yes
``#number17``              ``fromLabel @"number17"``            Yes
``#do``                    ``fromLabel @"do"``                  Yes
``#type``                  ``fromLabel @"type"``                Yes
``#Foo``                   ``fromLabel @"Foo"``                 No
``#"Foo"``                 ``fromLabel @"Foo"``                 No
``#3``                     ``fromLabel @"3"``                   No
``#"3"``                   ``fromLabel @"3"``                   No
``#199.4``                 ``fromLabel @"199.4"``               No
``#17a23b``                ``fromLabel @"17a23b"``              No
``#"The quick brown fox"`` ``fromLabel @"The quick brown fox"`` No
``#f'a'``                  ``fromLabel @"f'a'"``                Yes
``#'a'``                   ``fromLabel @"'a'"``                 No
``#":"``                   ``fromLabel @":"``                   No
``#'``                     ``fromLabel @"'"``                   No
``#"\""``                  ``fromLabel @"\""``                  No
========================== ==================================== ============

Effect and Interactions
-----------------------
``(#)`` is a valid name for an operator. The current ``-XOverloadedLabels`` steals syntax, as ``a#b`` means ``(#) a b`` without it and ``a (fromLabel @"b")`` with it (the difference is even more catastrophic for e.g. ``a#do ...``). This proposal steals more syntax in the same manner. E.g. the meanings of ``a#3``, ``a#"foo"``, ``a#'b'``, and ``a#Foo`` all change.

Costs and Drawbacks
-------------------
The syntax stealing above is one drawback. There is also the somewhat confusing possibility of things like ``#'a'``. The actual implementation should be simple and easy to maintain.

Alternatives
------------
Do nothing and just let libraries keep using tricks like ``#_Foo``.

Unresolved questions
--------------------
None.

Implementation Plan
-------------------
TBA
