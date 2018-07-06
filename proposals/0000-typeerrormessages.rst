Improved Type Error Messages
==================================

.. proposal-number:: 
.. trac-ticket:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

To improve GHC’s current type error messages by removing statements beginning with “In the …” and modifying the format of the current “expected vs. actual” statements for improved clarification.

Motivation
------------
For the most part, GHC’s current type error messages are visually ineffective. The messages are often too long and can be difficult for users to process, even when the problem at hand is actually a simple one. This is largely due to their inclusion of redundant information, and their overall structure. With some rewording, reformatting, and the removal of a few phrases, GHC’s type error messages will better facilitate the troubleshooting and overall development processes for users of all levels.

**Example #1**

Input code
::
 case1 :: a a
 case1 = undefined
Original error message:
::
     * Occurs check: cannot construct the infinite kind: k0 ~ k0 -> *
     * In the first argument of `a', namely `a'
       In the type signature: case1 :: a a
    |
 13 | case1 :: a a
    |            ^
New error message:
::
     * Expected something of kind [E] but the expression below has kind [A].
       [E] k0 -> *
       [A] k0
    |
 13 | case1 :: a a
    |            ^
     * I got stuck because k0 would be infinite for type checking to succeed.


Proposed Change Specification
-----------------------------
The implemented change would involve the following:
1.) Removal of context phrases beginning with “In the…”
           Ex. “In the expression…”
           Ex. “In the equation…”
2.) Reformatting and rewording of expected vs. actual phrases to use tags. The general format would look something like this for each error message:
           Expected something of type [E] but the expression below has type [A].
           [E] (insert expected type here, for example: [Char])
           [A] (insert actual type here, for example: Char)
Notes:
•   Relevant bindings will be printed as usual.
•   Expression in question will still be printed as usual at the bottom of the message.


Effect and Interactions
-----------------------
The removal of the extra context phrases will significantly shorten the type error messages, making for more digestible feedback for GHC users. The new tagging format of the “expected vs. actual” phrases serves to establish a clearer distinction between the two mismatched types in question, allowing for easier debugging and tool integration.


Costs and Drawbacks
-------------------
If anyone finds the "In the..." context phrases helpful or in general likes the current error messages as they are, they will probably be unhappy with this change. However, I believe that this change will greatly improve the learnability of haskell for beginners.


Alternatives
------------
GHC ticket #9173 discusses several possibilities for better type error messages. Most of the comments on this ticket mentioned simplifying the structure of the messages and rewording them to some degree, which is what this change aims to accomplish with the new tagged format and removal of repetitive statements.

Unresolved questions
--------------------
1.) Does anyone find value in the expressions proposed for removal?
2.) Are there words other than “expected” and “actual” that would be better for avoiding user confusion? 

Implementation Plan
-------------------
If approved, the change will be implemented by Nadine Adnane and Dorothy Feng, research students in Dr. Richard Eisenberg’s lab.
