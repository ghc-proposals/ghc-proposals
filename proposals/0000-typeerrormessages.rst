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

To improve the clarity of GHC’s current type error messages by modifying their structure and by completely removing statements beginning with “In the …”.

Motivation
------------
For the most part, GHC’s current type error messages are visually ineffective. The messages are often too long and can be difficult for users to process, even when the problem at hand is actually a simple one. This is largely due to their inclusion of redundant information, and their overall structure. With some rewording, reformatting, and the removal of a few phrases, GHC’s type error messages will better facilitate the troubleshooting and overall development processes for users of all levels. This change also has the potential to improve tool integration.

**Example #1**

Input code:
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

**Example #2**

Input code:
::
 case2 :: IO Int#
 case2 = return 1#
Original error message:
::
     * Expecting a lifted type, but ‘Int#’ is unlifted
     * In the first argument of ‘IO’, namely ‘Int#’
       In the type signature: case2 :: IO Int#
    |
 22 | case2 :: IO Int#
    |             ^^^^
New error message:
::
     * Expected a kind [E] but the expression below has a kind [A]
       [E] 'LiftedRep
       [A] 'IntRep
    |
 22 | case2 :: IO Int#
    |             ^^^^

**Example #3**

Input code:
::
 data ExpectsUnlifted (a :: TYPE 'UnliftedRep) = ExpectsUnlifted
 case3 :: ExpectsUnlifted Int
 case3 = undefined

 class ExampleClass (a :: TYPE 'UnliftedRep)  where
     thisIsAMethod :: a -> Bool
     case3b :: ExampleClass Int
     case3b = "xx"
     
Original error message:
::
     * Expecting an unlifted type, but ‘Int’ is lifted
     * In the first argument of ‘ExampleClass’, namely ‘Int’
       In the type signature: case3 :: ExpectsUnlifted Int
       In the class declaration for `ExampleClass`
    |
 26 | case3 :: ExampleClass Int
    |                       ^^^
New error message:
::
     * Expected a kind [E] but the expression below has kind [A]
       [E] ‘'UnliftedRep’
       [A] ‘'LiftedRep’
    |
 26 | case3 :: ExampleClass Int
    |                       ^^^

**Example #4**

Input code:
::
 case4 :: Maybe
 case4 xs = True
     
Original error message:
::
     * Expecting one more argument to `Maybe'
       Expected a type, but `Maybe' has kind `* -> *'
     * In the type signature: case4 :: Maybe
    |
 32 | case4 :: Maybe
    |          ^^^^^
New error message:
::
     * Expected a kind [E] but the expression below has kind [A]
       [E] *
       [A] * -> *
     
     * Expecting one more argument to 'Maybe'
    |
 32 | case4 :: Maybe
    |          ^^^^^
     
**Example #5**

Input code:
::
 data HighKind :: (* -> *) -> *
 case5 :: HighKind Either
 case5 = undefined
     
Original error message:
::
     * Expecting one more argument to `Either'
       Expected kind `* -> *', but `Either' has kind `* -> * -> *'
     * In the first argument of `HighKind', namely `Either'
       In the type signature: case5 :: HighKind Either
    |
 36 | case5 :: HighKind Either
    |                   ^^^^^^
New error message:
::
     * Expected a kind [E] but the expression below has kind [A]
       [E] *
       [A] * -> *
    |
 36 | case5 :: HighKind Either
    |                   ^^^^^^

**Example #6**

Input code:
::
 case6:: Int Bool
 case6 = undefined
     
Original error message:
::
     * Expecting one fewer arguments to `Int'
       Expected kind `* -> *', but `Int' has kind `*'
     * In the type signature: case6 :: Int Bool
    |
 40 | case6 :: Int Bool
    |          ^^^^^^^^
New error message:
::
     * Expected a kind [E] but the expression below has kind [A]
       [E] * -> *
       [A] *
    |
 40 | case6 :: Int Bool
    |          ^^^^^^^^

**Example #7**

Input code:
::
 case7 :: (a,b,c) -> (a,b)
 case7 (x,y,z) = (x)
     
Original error message:
::
     * Couldn't match expected type `(a, b)' with actual type `a'
       `a' is a rigid type variable bound by
         the type signature for:
           case7 :: forall a b c. (a, b, c) -> (a, b)
     * In the expression: (x)
       In an equation for `case7': case7 (x, y, z) = (x)
     * Relevant bindings include
         y :: b
         x :: a
         case7 :: (a, b, c) -> (a, b)
    |
 44 | case7 (x,y,z) = (x)
    |                  ^
New error message:
::
     * Expected type [E] but the expression below has type [A]
       [E] (a, b)
       [A] a
       where `a' is a rigid type variable bound by
         the type signature for:
           case7 :: forall a b c. (a, b, c) -> (a, b)
     * Relevant bindings include
         y :: b
         x :: a
         case7 :: (a, b, c) -> (a, b)
    |
 44 | case7 (x,y,z) = (x)
    |                  ^

**Example #8**

Input code:
::
 case8 :: (a,b) -> (a,b,c)
 case8 (x,y) = (x, y,"")
     
Original error message:
::
     * Couldn't match expected type `c' with actual type `[Char]'
       `c' is a rigid type variable bound by
         the type signature for:
           case8 :: forall a b c. (a, b) -> (a, b, c)
     * In the expression: ""
       In the expression: (x, y, "")
       In an equation for `case8': case8 (x, y) = (x, y, "")
     * Relevant bindings include
         case8 :: (a, b) -> (a, b, c)
    |
 54 | case8 (x,y) = (x, y,"")
    |                     ^^
New error message:
::
     * Expected type [E] but the expression below has type [A]
       [E] c
       [A] [Char]
       where `c' is a rigid type variable bound by
         the type signature for:
           case8 :: forall a b c. (a, b) -> (a, b, c)
     * Relevant bindings include
         case8 :: (a, b) -> (a, b, c)
    |
 54 | case8 (x,y) = (x, y,"")
    |                     ^^

**Example #9**

Input code:
::
 case9 :: String -> String
 case9 xs = True
     
Original error message:
::
     * Couldn't match type `Bool' with `[Char]'
       Expected type: String
         Actual type: Bool
     * In the expression: True
       In an equation for `case9': case9 xs = True
    |
 57 | case9 xs = True
    |            ^^^^
New error message:
::
     * Expected type [E] but the expression below has type [A]
       [E] String
       [A] Bool
    |
 57 | case9 xs = True
    |            ^^^^

Proposed Change Description
-----------------------------
The implemented change would involve the following:

**1.) Removal of all context phrases beginning with “In the…”**

•   Ex. “In the expression…”, “In the equation…”, "In the argument...", "In the type signature...", "In the class declaration...", etc.

**2.) Reformatting and rewording of all statements to use tags. This is meant to unify the many different types of messages.** 

•   Ex. “Expecting an unlifted type, but ‘Int’ is lifted”

The general format would look something like this for each error message:
::
 Expected something of type [E] but the expression below has type [A].
 [E] (expected type goes here, for example: [Char])
 [A] (actual type goes here, for example: Char)

**2.) Removal of occurs check** 

•   Ex. "Occurs check: cannot construct the infinite kind: k0 ~ k0 -> *"

**Notes:**
•   Relevant bindings will be printed as usual.

•   Expression in question will still be printed as usual at the bottom of the message.


Effect and Interactions
-----------------------
The removal of the extra context phrases will significantly shorten the type error messages, making for more digestible feedback for GHC users. The new tagging format of the “expected vs. actual” phrases serves to establish a clearer distinction between the two mismatched types in question, allowing for easier debugging and tool integration. Additionally, this change will shorten the compiler code by removing several functions currently involved in the printing of the "In the..." context expressions.


Costs and Drawbacks
-------------------
If anyone finds the "In the..." context phrases helpful or in general likes the current error messages as they are, they will probably be unhappy with this change. However, I believe that this change will greatly improve the learnability of haskell for beginners, as well as the overall experience of using GHC for most users after the initial disorientation.

Alternatives
------------
GHC ticket #9173 discusses several possibilities for better type error messages. Most of the comments on this ticket mentioned simplifying the structure of the messages and rewording them to some degree, which is what this change aims to accomplish with the new tagged format and removal of repetitive statements.

Unresolved questions
--------------------
1.) Does anyone find value in the expressions proposed for removal?
2.) Are there words other than “expected” and “actual” that would be better for avoiding user confusion? 

Implementation Plan
-------------------
If approved, the change will be implemented by Nadine Adnane, a research student in Richard Eisenberg’s lab.
