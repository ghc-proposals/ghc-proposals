Unrestricted Overloaded Labels
==============================

.. proposal-number:: 
.. trac-ticket:: 11671
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/170>`_
.. sectnum::
.. contents::

``OverloadedLabels`` are a useful feature, but they have somewhat unexpected syntax restrictions on what is a valid label. This proposal exists to alleiviate this.

Motivation
------------
``#foo`` is a valid expression with ``-XOverloadedLabels``, but ``#Foo`` is not. This is somewhat surprising, as ``#foo`` is only syntax for ``fromLabel @"foo"``. This can mean anything, so it doesn't make sense to enforce a case distinction as done for constructors and functions. Similarly, ``#3`` is not valid, nor is any other number. There isn't much reason against this, either.

There are real world cases in the Trac ticket, usually because libraries want to define the syntax ``#Foo`` to do something constructor related.

Proposed Change Specification
-----------------------------
Immediately following a ``#``, allow any nonempty alphanumeric string, including things that may otherwise look like numbers (e.g. ``#3``, ``#199.4``, ``#17a23b``), keywords (e.g. ``#do``, ``#type``), constructors (e.g. ``#Foo``, ``#List``), or variables (e.g. ``#a``, ``#number17``). Also allow strings that look like string literals, e.g. ``#"3"``, ``#"The quick brown fox"``, ``#":"``, or ``#"\""``, which are translated into ``fromLabel @"3"``, ``fromLabel @"The quick brown fox"``, ``fromLabel #":"``, and ``fromLabel @"\""``, respectively.

Effect and Interactions
-----------------------
``(#)`` is a valid name for an operator. The current ``-XOverloadedLabels`` steals syntax, as ``a#b`` means ``(#) a b`` without it and ``a (fromLabel @"b")`` with it (the difference is even more catastrophic for e.g. ``a#do ...``). This proposal steals more syntax in the same manner. E.g. the meanings of ``a#3``, ``a#"b"``, and ``a#Foo`` all change.

Costs and Drawbacks
-------------------
The syntax stealing appears to be the only substantial drawback. The actual implementation should be simple and easy to maintain.

Alternatives
------------
Do nothing and just let libraries keep using tricks like ``#_Foo``.

Unresolved questions
--------------------
None.

Implementation Plan
-------------------
TBA
