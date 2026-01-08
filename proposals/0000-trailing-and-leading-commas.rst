Add Support for Trailing and Leading Commas in Lists
====================================================

.. author:: Adrian Sieber
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/658>`_.
.. sectnum::
.. contents::

This proposal suggests extending the Haskell syntax to support trailing and leading commas in list notation.
This change aims to improve code readability and maintainability by allowing more flexibility in formatting lists, particularly in scenarios involving version control, code reviews, and automated code generation.


Motivation
----------

In many programming languages, including JavaScript, Python, and Rust, trailing commas in lists
(and other collection types) are a common feature.
This feature provides several benefits:

1. **Improved Diff Quality**: When adding or removing items in a list, having a trailing comma means only the relevant lines are changed in version control systems, reducing the noise in diffs and making reviews easier.

.. code-block:: diff

  --- example.hs	2024-06-10 12:00:00 +0000
  +++ example.hs	2024-06-10 12:01:00 +0000
  @@ -2,5 +2,5 @@
      "apple",
      "banana",
  -   "cherry"
  +   "cherry",
  +   "peach"
      ]

.. code-block:: diff

  --- example.hs	2024-06-10 12:00:00 +0000
  +++ example.hs	2024-06-10 12:01:00 +0000
  @@ -2,5 +2,5 @@
      "apple",
      "banana",
      "cherry",
  +   "peach",
      ]

2. **Ease of Code Modification**: Developers can add new elements to the end of a list without having to modify the previous last line, which reduces the likelihood of syntax errors.
3. **Consistency in Formatting**: When generating code automatically or formatting lists in a specific way, leading and trailing commas can simplify the process.

Currently, Haskell already supports trailing commas in import statements,
but neither trailing nor leading commas in lists,
leading to potential issues in code maintainability and readability.


Proposed Change Specification
-----------------------------

This proposal introduces the following syntactical changes to Haskell lists:

1. **Trailing Commas**: Allow a comma after the last element in a list.

   .. code-block:: haskell

      myList = [1, 2, 3,]

2. **Leading Commas**: Allow a comma before the first element in a list.

   .. code-block:: haskell

      myList = [, 1, 2, 3]

The changes to the Haskell grammar are as follows:

- The list production rule in the Haskell grammar will be modified to optionally accept a trailing comma.
- The list production rule will also be modified to optionally accept a leading comma.

The formal grammar changes:

.. code-block:: bnf

   list ::= '[' elems ']' ;
   elems ::= (',' element)* ','? (element (',' element)*)? ;
   element ::= exp ; -- as per existing grammar rules for list elements


Proposed Library Change Specification
-------------------------------------

This is a fully backwards-compatible syntax extension,
so no changes to user libraries are required.

Libraries fromt the GHC ecosystem,
like `GHC API <https://hackage.haskell.org/package/ghc>`
and `ghc-exactprint <https://github.com/alanz/ghc-exactprint>`
may need to be updated to handle the new syntax.


Examples
--------

1. **Trailing Commas**

   .. code-block:: haskell

      numbers = [
        1,
        2,
        3,
      ]

   This would be equivalent to:

   .. code-block:: haskell

      numbers = [1, 2, 3]

2. **Leading Commas**

   .. code-block:: haskell

      fruits = [
        , "apple"
        , "banana"
        , "cherry"
      ]

   This would be equivalent to:

   .. code-block:: haskell

      fruits = ["apple", "banana", "cherry"]

3. **Combined Leading and Trailing Commas**

   .. code-block:: haskell

      mixed = [
        , 1, 2
        , 3, 4, -- 5
      ]

   This would be equivalent to:

   .. code-block:: haskell

      mixed = [1, 2, 3, 4]


Effect and Interactions
-----------------------

TODO


Costs and Drawbacks
-------------------

TODO


Backward Compatibility
---------------------------------

This change is backward compatible with existing Haskell code,
as it introduces new syntactical permissiveness without altering the existing valid syntax.
All current Haskell programs will remain valid and unchanged in their behavior.


Implementation
--------------

The implementation requires modifying the Haskell parser to accept the proposed grammar changes. This involves:

1. Updating the parser definition to handle optional leading and trailing commas.
2. Ensuring that the list construction logic correctly interprets lists with these commas.


Alternatives
------------

The primary alternative is to maintain the current syntax without allowing trailing or leading commas.
However, this would forgo the benefits in readability, maintainability,
and ease of code modification that the proposed change aims to provide.


Unresolved Questions
--------------------

TODO


Implementation Plan
-------------------

TODO


Endorsements
-------------

TODO
