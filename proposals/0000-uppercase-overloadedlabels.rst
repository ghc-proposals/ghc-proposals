Uppercase Overloaded Labels
===========================

.. proposal-number:: 
.. trac-ticket:: 11671
.. implemented::
.. highlight:: haskell
.. header::
.. sectnum::
.. contents::

``OverloadedLabels`` are a useful feature, but they have a somewhat unexpected restriction in that they cannot be uppercase. This proposal exists to eliminate this.

Motivation
------------
``#foo`` is a valid expression with ``-XOverloadedLabels``, but ``#Foo`` is not. This is somewhat surprising, as ``#foo`` is only syntax for ``fromLabel @"foo"``. This can mean anything, so it doesn't make sense to enforce a case distinction as done for constructors and functions. There are real world cases in the Trac ticket, usually because libraries want to define the syntax ``#Foo`` to do something constructor related.

Proposed Change Specification
-----------------------------
Allow the string after a ``#`` to start uppercase. Currently, we allow a ``#`` followed by something that looks like a normal variable name. After this proposal, we allow a ``#`` followed by something that either looks like a variable name or a constructor name.

Effect and Interactions
-----------------------
None.

Costs and Drawbacks
-------------------
None.

Alternatives
------------
Do nothing and just let libraries keep using tricks like ``#_Foo``.

Unresolved questions
--------------------
None.

Implementation Plan
-------------------
`Done with two lines in the parser. <https://github.com/ghc/ghc/pull/192>`_
