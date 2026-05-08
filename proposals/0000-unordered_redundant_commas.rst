Extra commas in UnOrdered Structures
=========================================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/748>`_.
.. sectnum::
.. contents::

This proposal suggests extending the Haskell syntax to support trailing and leading commas 
in unordered structures (records, import and export lists and sublists, derivation and default clauses).

This change aims to improve code readability and maintainability by allowing more flexibility 
in formatting unordered structures (records, import and export lists and sublists, derivation and default clauses). 

First of all, particularly important in scenarios involving version control, code reviews, and automated code generation.


Motivation
----------

In many programming languages, including JavaScript, Python, and Rust, trailing commas in lists
(and other collection types) are a common feature.

The problem code is repeated::

    module Foo (
    #ifdef TESTING
    #ifdef USE_PATTERN_SYNONYMS
      Foo (Foo, Pat1, Pat2)
    #else
      Foo (Foo)
    #endif
    #elif USE_PATTERN_SYNONYMS
      Foo (Pat1, Pat2)
    #else
      Foo
    #endif

Given trailing and leading commas, one could instead write::

    module Foo (
      Foo(
    #ifdef TESTING
        , Foo
    #endif
    #if USE_PATTERN_SYNONYMS
        , Pat1
        , Pat2
    #endif
    )

This feature provides several benefits:

1. **Improved Diff Quality**: When adding or removing items in a list, having a trailing comma means only
   the relevant lines are changed in version control systems, reducing the noise in diffs and making reviews easier.
  
2. **Ease of Code Modification**: Developers can add new elements to the end of a list without having 
   to modify the previous last line, which reduces the likelihood of syntax errors.

3. **Consistency in Formatting**: When generating code automatically or formatting lists in a specific way, 
   leading and trailing commas can simplify the process.

4. **Use a different style of coding**: Extra commas allow for different styles to write code.

5. **Simplicity of conditional meta-programming**: Extra commas allow to write much simpler code when conditional meta-programming is used.


History
~~~~~~~~~~~~

Adding trailing commas (and more rarely leading commas) is a frequently asked feature to add in Haskell.

But this task is highly divisive in the Haskell community.

Original Proposal #87 
`ExtraCommas (was: Trailing and leading commas in sub-export lists) <https://github.com/ghc-proposals/ghc-proposals/pull/87>`__ 
was discussed several years and that discussion was so controversial,
that author withdrawn own proposal just before the final Acceptation was received (with minor changes).

However, the tension in the Haskell community was so high that the duplicate of that proposal was never proposed 
and had never succeeded in the next 6 years.

This proposal is an attempt to allow extra commas where everyone agrees to have them - 
in unordered structures (records, import and export "lists" and sublists, derivation and default clauses).


Proposed Change Specification
-----------------------------

This proposal does not cover order-matter structures (including lists, tuples, constraint tuples).

This proposal introduces the following syntactical changes to Haskell:

1. **New extension**: Add language extension ``UnorderedExtraCommas`` with a simple rule where
   extra commas are allowed: in unordered structures only.

2. **Trailing Commas**: Allow a comma after the last element in place-unordered clauses:

   - module export lists(already supported) and sub-lists
   - module import lists(already supported) and sub-lists
   - deriving and default clauses
   - record-like occurrences (declarations, patterns, constructions)

   ::
   
       module Foo
		  ( -- * Types
		    Foo,
		    Bar (C, D,),
			-- * Functions
		    mkFoo,
		  ) where 

       data Example a = ....
			deriving (
				Functor,
				Foldable,
				Applicative,
			)
				
       data family URec a p

       data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#,    }
       data instance URec Char     p = UChar   { uChar#   :: Char#,   uInt# :: Int#, }
       data instance URec Double   p = UDouble { uDouble# :: Double#, uInt# :: Int#, uFloat#  :: Float#, }

	  
3. **Leading Commas**: Allow a comma before the first element in place-unordered clauses:

   - module export lists and sub-lists
   - module import lists and sub-lists
   - deriving and default clauses
   - record-like occurrences (declarations, patterns, constructions)

   ::
   
       module Bar
		  ( -- * Types
		  , Foo
		  , Bar  (
			, C
			, D
		  )
			-- * Functions
		  , mkFoo
		  ) where 

4. **Trailing AND Leading Commas**: Allow both Trailing AND Leading Commas in one structure.


Syntax
~~~~~~~~~~~~

The formal grammar changes for ``UnOrderedExtraCommas`` for Trailing AND Leading Commas WITHOUT Repetetive Commas:

.. code:: none

    exports ::= ( [,] export1 , ... , exportn [,] )                       -- upd

    export ::= qvar
        | qtycon[(..)| ( [,] cname_1, ... , cname_n [,] ) ]  (n >= 0)     -- upd
        | qtycls[(..)| ( [,] var_1, ... , var_n [,] ) ]      (n >= 0)     -- upd
        | module modid
        | ......
   
    impspec ::= ( [,] import1 , ... , importn [,] )         (n ≥ 0)       -- upd
        | hiding ( [,] import1 , ... , importn [,] )        (n ≥ 0)       -- upd

    import ::= qvar
        | qtycon[(..)| ( [,] cname_1, ... , cname_n [,] ) ]  (n >= 0)     -- upd
        | qtycls[(..)| ( [,] var_1, ... , var_n [,] ) ]      (n >= 0)     -- upd
        | ......

    deriving ::= deriving dclass
            | deriving (  
                 ( [,] dclass1 , ... , dclassn [,] )    -- upd
              )

    topdecl ::= type simpletype = type
        | default ( [,] type1 , ... , typen [,] )       -- upd
        | ......

    constr ::= con [!] atype1 ... [!] atypek                (arity con = k, k>=0)
        | (btype | ! atype) conop (btype | ! atype)                 (infix conop)
        | con { [,] fielddecl1 , ... , fielddecln [,] }            (records n>=0)     -- upd
        | ......

    aexp ::= qvar                                                        (variable)
        | ......
        | qcon { [,] fbind1 , ... , fbindn [,] }      (labeled construction, n ≥ 0)   -- upd
        | aexp_(qcon) { [,] fbind1 , ... , fbindn [,] }   (labeled update, n  ≥  1)   -- upd

These changes allow extra commas in next unordered structures:

- module export "lists"
- module export sub-"lists"
- module import lists
- module import sub-"lists"
- deriving clauses
- default clauses
- record-like occurrences (declarations, patterns, constructions)

This proposal does not include yet using extra commas in next unordered structures:

- fixity "lists"


Examples
--------
1. **Improved Diff Quality**:

   Right adding or removing a thing often needs changing 2 lines of code

   .. code-block:: diff

     --- example.hs	2024-06-10 12:00:00 +0000
     +++ example.hs	2024-06-10 12:01:00 +0000
     @@ -2,5 +2,5 @@
         baz1,
         baz2,
     -   foo
     +   foo,
     +   bar
         ) where

   insted of just 1 line

   .. code-block:: diff

     --- example.hs	2024-06-10 12:00:00 +0000
     +++ example.hs	2024-06-10 12:01:00 +0000
     @@ -2,5 +2,5 @@
         baz1,
         baz2,
         foo,
     +   bar,
         ) where

2. **Simplicity of conditional meta-programming**

   We could simplify conditional meta-programming:
   ::

    module Foo (
      Foo(
    #ifdef TESTING
        , Foo
    #endif
    #if USE_PATTERN_SYNONYMS
        , Pat1
        , Pat2
    #endif
    )

3. **Use a different style of coding**
   ::

       -- style: comma after element
       module Foo
          ( -- * Types
            Foo,
            Bar (
                   C, 
                   D,
                 ),
            Baz,
            -- * Functions
            mkFoo,
            mkBar,
            mkBaz,
          ) where 

       -- style: comma before element
       module Foo
          ( -- * Types
            , Foo
            , Bar (
                  , C
                  , D
                  )
            , Baz
            -- * Functions
            , mkFoo
            , mkBar
            , mkBaz
          ) where 


Effect and Interactions
-----------------------

None.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``UnorderedExtraCommas`` has medium difficulty.

Second, all tooling which parses Haskell code will need to be updated to be compatible with the extended syntax.


Backward Compatibility
---------------------------------

This change is backward compatible with existing Haskell code,
as it introduces new syntactical permissiveness without altering the existing valid syntax.

All current Haskell programs will remain valid and unchanged in their behavior.


Alternatives
------------

The primary alternative is "status quo".

Alternative adding extra commas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. The proposal suggests to allow Trailing **AND** Leading Commas, but Committee could choose instead
   Trailing **OR** Leading Commas, a bit stricter version of extra commas.

   The main benefit of OR-version - Cabal already support such liberalisation.

   The main disadvantage  of OR-version is disallowing to mix code-styles. 
   Also stricter version has more complex parsing ::

      lead_OR_trail  ::= ( , subList ) | ( subList [,] )

      lead_AND_trail ::= [,] subList [,]

2. The proposal suggests to allow Extra Commas **WITHOUT** Repetetive Commas, but Committee could choose instead
   Extra Commas **WITH** Repetetive Commas, a much more liberal version of extra commas.

   **Repetetive Commas**: Allow multiple commas instead of one in place-unordered clauses:

   - module export lists and sub-lists
   - module import lists and sub-lists
   - deriving and default clauses
   - record-like occurrences (declarations, patterns, constructions)

   ::
   
       module BarFoo
		  ( 
		  , -- Foo    -- temporary
		  , -- Bar    -- temporary
		  , BarFoo
		  ) where 

   The main disadvantage of WITH-version is allowing to write very "dirty" code. 
   Haskell is known as a language with "pretty looking code".

   The main benefit of WITH-version - the maximum liberalisation of using extra commas. 
   Also more liberal version has almost the same parsing ::

     lead_AND_trail_WITHOUT_repeats ::=  [,] { elem_i , } elem_max [,] 

     lead_AND_trail_WITH_repeats    ::=  {,} { elem_i , {,} } elem_max {,} 

3. The proposal suggests to allow Extra Commas in **PURE** Code, but Committee could allow also Extra Commas in **Pragmas**.

Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

It is unclear. I cannot implement this plan.


Acknowledgments
----------------

Thanks to all contributors of `ExtraCommas (was: Trailing and leading commas in sub-export lists) 
<https://github.com/ghc-proposals/ghc-proposals/pull/87>`__.


Endorsements
-------------
