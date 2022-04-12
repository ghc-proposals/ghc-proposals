Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose the text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Reduce type family application via precompiled term-level functions
==============

.. author:: Rinat Striungis
.. date-accepted:: 
.. ticket-url:: `https://gitlab.haskell.org/ghc/ghc/-/issues/19634`
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

The current mechanism of reducing type families applications has a few drawbacks: 
 1. Cumbersome and restricted implementation of complex type families 
 2. Slow reduction of complex type families 
 3. There are useful families which can't be implemented as usual families.
    This leads to a grown built-in families set or lack of useful families. 
    (paste here a link to the discussion about type families for char kind types)

We propose to add a possibility to define type families which applications 
will be reduced via an application of precompiled term-level functions.   


Motivation
----------
There are two major motivations:

 1. To allow users to write basic functions over built-in types, like type literals. 
    Without proposed changes, such functions also must be built-in into the compiler growing its 
    codebase. A great example is `https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3598`: 
    most of these type families could be written using the proposed way without expanding GHC. 

 2. To allow using complex type families like parsers. Although a user can write such families now 
    in most cases, but in reality, a set of necessary families would be so complicated and a process of their 
    reduction would be so slow that no one would write it, otherwise, a user would get hard-to-read and huge piece of code 
    and unacceptably slow compilation.     


Proposed Change Specification
-----------------------------
We propose a new language extension ``TypeFunctions``. 
Enabling this extension allows a user to declare a new type of closed type families: 
::
  type family F a | F = fn

It must have zero equations and ``fn`` must be imported from another module.  
A type of the term-level function must be the same as a kind of the type family. 

Every mentioned type in the type of function must be promotable, to have a corresponding kind. 

The evaluation order of such families is strict by their arguments. 
Meeting with type variable or unevaluated type family application causes giving up 
and returning an unevaluated application. 

These families work ok with variables in constraints:: 

   f :: forall a. (F a ~ 123) => ...     -- Ok 
   

   type family IdNat (a :: Nat) | IdNat = id

   g :: forall (a :: Nat). a -> IdNat a  -- Not ok, ``IdNat a`` would not be evaluated.  
   g = id 

Failures and exceptions thrown by applied term-level functions will be turned into type errors.

All the machinery works via compiling term-level functions at the beginning of type-checking and applying 
pre-compiled function to types turned into usual Haskell values. 
A result of such application is interpreted as a type. 

Examples
--------
Examples of basic functions over built-in types: 
::
  type family ToUpper (a :: Char) | ToUpper = toUpper 

  type family IsDigit (a :: Char) | IsDigit = isDigit 

Examples of complex type families: 
A path parser has been used as a performance benchmark.
It parses a string as a path and tries to detect if it 
is a windows or posix path, absolute or relative  
if it is a path to a file or folder. It also returns a list of 


A time spent for reducing by application of pre-compiled function 
is several orders of magnitude less than in the case of reducing via usual way. 
Of course, there is also time spent on a compilation of the function itself 
but it is still very small and moreover, it should be done only once while 
usual reducing of type family application takes a lot of time every time being applied 
to different arguments.  

Benchmarks: 

An example of the usual reduction: 

`Profile.hs <https://gist.github.com/Haskell-mouse/b05db12de9e9fdc8cfa9b02f436eccc0/raw/4ccd8089677ccec6597949df56de8ddc2bfb41ad/2Profile.hs>`_
`ExamplesPath <https://gist.github.com/Haskell-mouse/b05db12de9e9fdc8cfa9b02f436eccc0/raw/4ccd8089677ccec6597949df56de8ddc2bfb41ad/3ExamplesPath.hs>`_

and a result: 
`timings-old <https://gist.github.com/Haskell-mouse/b05db12de9e9fdc8cfa9b02f436eccc0/raw/a56b9207664703eeb7bab16609fc30a14982f181/timings-dump.txt>`_


An `example <https://gist.github.com/Haskell-mouse/08f44cc208f01a1c6e89346bae00cb6f>`_ of the proposed new type of reduction.

and a result: 
`timings-new <https://gist.github.com/Haskell-mouse/08f44cc208f01a1c6e89346bae00cb6f/raw/2becc41652f358e34950d9b7c7a01bd20ab52873/timings-dump.txt>`_

A short benchmark via ghci:

1. The usual reduction:
:: 

   ghci> runParserFinal @PathParser @"D:\\test\\stack\\test\\rrrrr\\gggg\\bbb\\ddd\\vvvv\\nnnn\\nnnn\\yyyyy\\ddddd\\jjjjj\\mmmm\\eeee\\mmmm\\yyyyyy\\aaaaaaaaaa"
   Path (PathParam Windows Dir Absolute) ("D" :| ["test","stack","test","rrrrr","gggg","bbb","ddd","vvvv","nnnn","nnnn","yyyyy","ddddd","jjjjj","mmmm","eeee","mmmm","yyyyyy","aaaaaaaaaa"])
   (31.15 secs, 243,096 bytes)

2. The new reduction: 
::
   ghci> path @(ShowPathResult (MkPathFst (SymbolToString ("D:\\test\\stack\\test\\rrrrr\\gggg\\bbb\\ddd\\vvvv\\nnnn\\nnnn\\yyyyy\\ddddd\\jjjjj\\mmmm\\eeee\\mmmm\\yyyyyy\\aaaaaaaaaa"))))
   "((Windowss,Dir,Absolute),[\"D\",\"test\",\"stack\",\"test\",\"rrrrr\",\"gggg\",\"bbb\",\"ddd\",\"vvvv\",\"nnnn\",\"nnnn\",\"yyyyy\",\"ddddd\",\"jjjjj\",\"mmmm\",\"eeee\",\"mmmm\",\"yyyyyy\",\"aaaaaaaaaa\"])"
   (0.31 secs, 247,984 bytes)

Another example is type-safe printf: 
::
   example :: String
   example = formatS @"Person's name is %s and age is %d and height is %f" "Danya" 26 8.1

1. The usual reduction:
::
   ghci> formatS @"Person's name is %s and age is %d and height is %f" "Danya" 26 8.1
   "Person's name is Danya and age is 26 and height is 8.100000381469727"
   (11.71 secs, 1,331,464 bytes)


2. The new reduction: 
::
   ghci> formatS @"Person's name is %s and age is %d and height is %f" "Danya" 26 8.1
   "Person's name is Danya and age is 26 and height is 8.100000381469727"
   (0.25 secs, 1,166,296 bytes)


Effect and Interactions
-----------------------
1. The change makes possible real-world using complex type families without 
   slow compilation. 

2. The change should be backward compatible. 

3. Enabling the extension implies ``TypeFamilies``

Costs and Drawbacks
-------------------
The change probably wouldn't add much code and make the compiler much more complicated. 
The most significant drawback is probably a slow building of large projects 
because using this new type of the families creates additional building 
dependencies between modules like Template Haskell does. 

It also slightly slows reducing of the usual TFs applications, but it is 
really minor. 

Alternatives
------------
There could be many syntax alternatives with the same internal machinery. 
An example:: 

   module M where 
   ... 
   {-# PROMOTE f #-}
   f :: Nat -> Bool

Such definition creates an implicit type family F in all modules importing ``M``. 
This variant has a few drawbacks: 
1. It confuses user which would try to understand a code filled with such 
   implicitly defined families. 
2. It leads to recompiling ALL the modules importing ``M``. Even if they 
   don't use ``F`` at all. 

There are also alternatives with different reduction algorithms. 
An example: 

A definition of function like 

  f True = 1
  f False = 2

will also implicitly create a type family: 

   type family F a where
     F True = 1
     F False = 2
   
It should be very similar to how ``Singletons`` library does the similar thing, generating 
type families via Template Haskell. 

The main drawback is obvious: it will cause a very slow compilation: generating new code via TH + usual 
slow reduction.  


Unresolved Questions
--------------------
The main questions that are unresolved now are 
1. Support laziness by arguments. 
   It is easy to implement it partially, but such an implementation 
   would not support returning unsubstituted type variables or unreduced type family applications. 
   For example ``Fst (1,a)`` will be reduced to ``1`` but ``Fst (a,1)`` will not be reduced at all. 
   Such a behavior looks unclear and fragile, but proper implementation of laziness will probably 
   require analyzing term-level functions for neutrality. Implementing this analysis looks 
   very complicated and requires RTS modification. 

2. Support user-defined axioms. This question is related with a previous one. 
   Currently reducing is possible only with fully reduced specific types as arguments. 
   But what if a user would be allowed to describe invariants of a term-level function 
   by hands ? 
   Something like... 

   type FstMod :: (Nat, Bool) -> Nat 
   type family FstMod a | FstMod = fstMod
     axioms: 
       FstMod (a, True) = a -- both sides must contain type variables 

Reducing via an axiom should be tried if the type checker meets a variable or unreduced TF application while trying 
to feed a precompiled function by given arguments.  
The main drawback is a possibility of a significant changing behavior of the type family in comparison with 
an original term-level function by writing wrong axioms.  

Implementation Plan
-------------------
Proposed changes are partially implemented in a prototype. 
Currently such functions works, but only with a limited set of types both as arguments and 
a result: ``Nat``, ``Char``, ``[a]``, ``Either a b``, ``()``, tuples with all supported lengths. 
That is because conversion between these types and corresponding Haskell values and vice versa is 
hardcoded. We need to do a forward conversion because we can apply a precompiled function only to a Haskell value. 
And we need to convert the result of such application into a corresponding type. 

So, the next big task to implement is custom types support.


Endorsements
-------------
(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
