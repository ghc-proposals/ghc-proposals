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
----------
For the most part, GHC’s current type error messages are visually ineffective. The messages are often too long and can be difficult for users to process, even when the problem at hand is actually a simple one. This is largely due to their inclusion of redundant information, and their overall structure. With some rewording, reformatting, and the removal of a few phrases, GHC’s type error messages will better facilitate the troubleshooting and overall development processes for users of all levels. This change also has the potential to improve tool integration. To be clear, this proposal describes changes to GHC's error messages, not to the Haskell Language itself. It should also be noted that this proposal is intended primarily as a means of gathering feedback from the GHC community, and thus does not involve the same level of specificity as is standard in GHC proposals. 

**Type Error Example #1**

Input code:
::
 example1 :: String -> String
 example1 xs = True
     
Original error message:
::
     * Couldn't match type `Bool' with `[Char]'
       Expected type: String
         Actual type: Bool
     * In the expression: True
       In an equation for `example1': example1 xs = True
    |
 57 | example1 xs = True
    |               ^^^^
New error message:
::
     * Expected type [E] but the underlined code below has type [A]
       [E] String
       [A] Bool
    |
 57 | example1 xs = True
    |               ^^^^

**Type Error Example #2**

Input code:
::
 example2 :: (a,b,c) -> (a,b)
 example2 (x,y,z) = (x)
     
Original error message:
::
     * Couldn't match expected type `(a, b)' with actual type `a'
       `a' is a rigid type variable bound by
         the type signature for:
           example2 :: forall a b c. (a, b, c) -> (a, b)
         at C:\Users\Example\Documents\Examples.hs:42:1-25
     * In the expression: (x)
       In an equation for `example2': example2 (x, y, z) = (x)
     * Relevant bindings include
         y :: b
           (bound at C:\Users\Example\Documents\Examples.hs:43:10)
         x :: a
           (bound at C:\Users\Example\Documents\Examples.hs:43:8)
         example2 :: (a, b, c) -> (a, b)
           (bound at C:\Users\Example\Documents\Examples.hs:43:1)
    |
 44 | example2 (x,y,z) = (x)
    |                     ^
New error message:
::
     * Expected type [E] but the underlined code below has type [A]
       [E] (a, b)
       [A] a
       where `a' is a rigid type variable bound by
         the type signature for:
           example2 :: forall a b c. (a, b, c) -> (a, b)
         at C:\Users\Example\Documents\Examples.hs:42:1-25
     * Relevant bindings include
         y :: b
           (bound at C:\Users\Example\Documents\Examples.hs:43:10)
         x :: a
           (bound at C:\Users\Example\Documents\Examples.hs:43:8)
         example2 :: (a, b, c) -> (a, b)
           (bound at C:\Users\Example\Documents\Examples.hs:43:1)
    |
 44 | example2 (x,y,z) = (x)
    |                     ^

**Type Error Example #3**

Input code:
::
 example3 :: (a,b) -> (a,b,c)
 example3 (x,y) = (x, y,"")
     
Original error message:
::
     * Couldn't match expected type `c' with actual type `[Char]'
       `c' is a rigid type variable bound by
         the type signature for:
           example3 :: forall a b c. (a, b) -> (a, b, c)
         at C:\Users\Example\Documents\Examples.hs:60:1-25
     * In the expression: ""
       In the expression: (x, y, "")
       In an equation for `example3': example3 (x, y) = (x, y, "")
     * Relevant bindings include
         example3 :: (a, b) -> (a, b, c)
           (bound at C:\Users\Example\Documents\Examples.hs:61:1)
    |
 54 | example3 (x,y) = (x, y,"")
    |                        ^^
New error message:
::
     * Expected type [E] but the underlined code below has type [A]
       [E] c
       [A] [Char]
       where `c' is a rigid type variable bound by
         the type signature for:
           example3 :: forall a b c. (a, b) -> (a, b, c)
         at C:\Users\Example\Documents\Examples.hs:60:1-25
     * Relevant bindings include
         example3 :: (a, b) -> (a, b, c)
           (bound at C:\Users\Example\Documents\Examples.hs:61:1)
    |
 54 | example3 (x,y) = (x, y,"")
    |                        ^^
 
**NOTE: Additional examples can be found** `here <#additional-examples>`_

Proposed Change Description
---------------------------
The implemented change would involve the following:

**1.) Removal of all context phrases beginning with “In the…”**

•   Ex. “In the expression…”, “In the equation…”, "In the argument...", "In the type signature...", "In the class declaration...", etc.

**2.) Reformatting and rewording of all statements to use tags. This is meant to unify the many different types of messages.** 

The general format would look something like this for each error message:
::
 Expected type [E] but the underlined code below has type [A].
 [E] (expected type goes here)
 [A] (actual type goes here)
 (print the line in question and its number here as usual)

**3.) Move away from using technical terms in error messages!** 

•   Ex. Remove the occurs check statements. See additional example #6.

**4.) Introduce the use of "I" in some error messages**

•   Ex. See additional example #6.

**Notes:**

•   Relevant bindings will be printed as usual.

•   The expression in question will still be printed as usual at the bottom of the error message.

Effect and Interactions
-----------------------
The removal of the extra context phrases will significantly shorten the type error messages, making for more digestible feedback for GHC users. The new tagging format of the “expected vs. actual” phrases serves to establish a clearer distinction between the two mismatched types in question, allowing for easier debugging and tool integration. Additionally, this change will shorten the compiler code by removing several functions currently involved in the printing of the "In the..." context expressions.

Costs and Drawbacks
-------------------
If anyone finds the "In the..." context phrases helpful or in general likes the current error messages as they are, they will probably be unhappy with this change. However, I believe that this change will greatly improve the learnability of haskell for beginners, as well as the overall experience of using GHC for most users after the initial disorientation.

Alternatives
------------
`GHC ticket #9173 <https://ghc.haskell.org/trac/ghc/ticket/9173>`_ discusses several possibilities for better type error messages. Most of the comments on this ticket mentioned simplifying the structure of the messages and rewording them to some degree, which is what this change aims to accomplish with the new tagged format and removal of repetitive statements.

Unresolved questions
--------------------
1.) Does anyone find value in the expressions proposed for removal?

2.) Are there words other than “expected” and “actual” that would be better for avoiding user confusion? 

3.) How do the majority of GHC users feel about the compiler presenting some (or parts of) error messages in the first person? (see additional example #6)

Implementation Plan
-------------------
If approved, the change will be implemented by Nadine Adnane, a research student in Richard Eisenberg’s lab.

Additional Examples
-------------------
**Additional Example #1**

Input code:
::
 aexample1 :: IO Int#
 aexample1 = return 1#
Original error message:
::
     * Expecting a lifted type, but `Int#’ is unlifted
     * In the first argument of `IO’, namely `Int#’
       In the type signature: aexample1 :: IO Int#
    |
 22 | aexample1 :: IO Int#
    |                 ^^^^
New error message:
::
     * Expected kind [E] but the underlined code below has kind [A]
       [E] *
       [A] TYPE 'IntRep
    |
 22 | aexample1 :: IO Int#
    |                 ^^^^

**Additional Example #2**

Input code:
::
 data ExpectsUnlifted (a :: TYPE 'UnliftedRep) = ExpectsUnlifted
 aexample2 :: ExpectsUnlifted Int
 aexample2 = undefined
     
Original error message:
::
     * Expecting an unlifted type, but ‘Int’ is lifted
     * In the first argument of ‘ExpectsUnlifted’, namely `Int’
       In the type signature: aexample2 :: ExpectsUnlifted Int
    |
 26 | aexample2 :: ExpectsUnlifted Int
    |                              ^^^
New error message:
::
     * Expected kind [E] but the underlined code below has kind [A]
       [E] TYPE 'UnliftedRep
       [A] *
    |
 26 | aexample2 :: ExpectsUnlifted Int
    |                              ^^^

**Additional Example #3**

Input code:
::
 aexample3 :: Maybe
 aexample3 xs = True
     
Original error message:
::
     * Expecting one more argument to `Maybe'
       Expected a type, but `Maybe' has kind `* -> *'
     * In the type signature: aexample3 :: Maybe
    |
 32 | aexample3 :: Maybe
    |              ^^^^^
New error message:
::
     * Expecting one more argument to 'Maybe'
     * Expected kind [E] but the underlined code below has kind [A]
       [E] *
       [A] * -> *
    |
 32 | aexample3 :: Maybe
    |              ^^^^^

**Additional Example #4**

Input code:
::
 aexample4:: Int Bool
 aexample4 = undefined
     
Original error message:
::
     * Expecting one fewer arguments to `Int'
       Expected kind `* -> *', but `Int' has kind `*'
     * In the type signature: aexample4 :: Int Bool
    |
 40 | aexample4 :: Int Bool
    |              ^^^^^^^^
New error message:
::
     * Expecting one fewer argument to `Int'
     * Expected kind [E] but the underlined code below has kind [A]
       [E] * -> *
       [A] *
    |
 40 | aexample4 :: Int Bool
    |              ^^^
**NOTE: For those wondering why both Int and Bool are underlined in the original error message - it appears to be a bug, which will hopefully also be remedied by this change.

**Additional Example #5**

Input code:
::
 data HighKind :: (* -> *) -> *
 aexample5 :: HighKind Either
 aexample5 = undefined
     
Original error message:
::
     * Expecting one more argument to `Either'
       Expected kind `* -> *', but `Either' has kind `* -> * -> *'
     * In the first argument of `HighKind', namely `Either'
       In the type signature: aexample5 :: HighKind Either
    |
 36 | aexample5 :: HighKind Either
    |                       ^^^^^^
New error message:
::
     * Expected kind [E] but the underlined code below has kind [A]
       [E] * -> *
       [A] * -> * -> *
    |
 36 | aexample5 :: HighKind Either
    |                       ^^^^^^

**Additional Example #6**

Input code:
::
 aexample6 :: a a
 aexample6 = undefined
Original error message:
::
     * Occurs check: cannot construct the infinite kind: k0 ~ k0 -> *
     * In the first argument of `a', namely `a'
       In the type signature: aexample6 :: a a
    |
 13 | aexample6 :: a a
    |                ^
New error message:
::
     * Expected kind [E] but the underlined code below has kind [A].
       [E] k0 -> *
       [A] k0
    |
 13 | aexample6 :: a a
    |                ^
     * I got stuck because k0 would be infinite for type checking to succeed.
