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

This is particularly helpful in scenarios involving version control, code reviews, and automated code generation.


Motivation
----------

In many programming languages, including JavaScript, Python, and Rust, trailing commas in lists
(and other collection types) are a common feature.

This causes the problem that code needs to be repeated::

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

If trailing and leading commas were allowed, one could instead write::

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

1. **Improved Diff Quality**: When adding or removing items in a structure, having a trailing or leading comma means only
   the relevant lines are changed, reducing the noise in diffs and making reviews easier.
  
2. **Ease of Code Modification**: Developers can add new elements to the end / beginnging of a structure without having 
   to modify the previous last / first line, which reduces the likelihood of syntax errors.

3. **Consistency in Formatting**: When generating code automatically or formatting structures in a specific way, 
   leading and trailing commas can simplify the process.

4. **Use a different style of coding**: Extra commas allow for different styles to write code.

5. **Simplicity of conditional meta-programming**: Extra commas allow to write much simpler code when conditional meta-programming is used.


History
~~~~~~~~~~~~

Adding trailing commas (and more rarely leading commas) is a frequently asked for feature to add to Haskell.

But, this task is highly divisive in the Haskell community.

The original proposal #87 
`ExtraCommas (was: Trailing and leading commas in sub-export lists) <https://github.com/ghc-proposals/ghc-proposals/pull/87>`__ 
was discussed several years. The discussion was so controversial
that the author withdrew the proposal just before the final acceptance (with minor changes).

However, the tension in the Haskell community was so high that a new attempt of that proposal has not been proposed in the following 6 years.

This proposal is an attempt to allow extra commas where everyone agrees to have them - 
in unordered structures (records, import and export "lists" and sublists, derivation and default clauses, multi-name signatures).


Proposed Change Specification
-----------------------------

This proposal does not cover structures in which order matters (including lists, tuples, constraint tuples).

This proposal introduces the following syntactical changes to Haskell:

1. **New extension**: Add a new language extension ``UnorderedExtraCommas`` which allows leading and trailing commas in unordered structures.

2. **Trailing Commas**: Allow a comma after the last element in enumeration clauses where order does not matter:

   - module export lists(already supported) and sub-lists
   - module import lists(already supported) and sub-lists
   - deriving and default clauses
   - record-like occurrences in terms and types (declarations, patterns, constructions)
   - multi-name signatures (including nested in records)

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

	  
3. **Leading Commas**: Allow a comma before the first element in enumeration clauses where order does not matter:

   - module export lists and sub-lists
   - module import lists and sub-lists
   - deriving and default clauses
   - record-like occurrences in terms and types (declarations, patterns, constructions)
   - multi-name signatures (including nested in records)

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

4. **Trailing AND Leading Commas**: Allow both trailing (2) **and** leading (3) commas simultaneously in a single structure. ::

      data Example a = ....
                 deriving (
                        , Functor
                        , Foldable,
                          Applicative,
                          Traversable,
                 )


Syntax
~~~~~~~~~~~~

The formal grammar changes for ``UnorderedExtraCommas`` for trailing **and** leading commas **without** repetitive commas:

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

    fielddecl ::= vars :: (type | ! atype)

    aexp ::= qvar                                                        (variable)
        | ......
        | qcon { [,] fbind1 , ... , fbindn [,] }      (labeled construction, n ≥ 0)   -- upd
        | aexp_(qcon) { [,] fbind1 , ... , fbindn [,] }   (labeled update, n  ≥  1)   -- upd

    gendecl ::= vars :: [context =>] type       (type signature)
        | fixity [integer] ops	            (fixity declaration)
        |                                    (empty declaration)

    vars ::= [,] var1 , ... , varn [,]                   (n ≥ 1)        -- upd

These changes allow extra commas in the following unordered structures:

- module export "lists"
- module export sub-"lists"
- module import lists
- module import sub-"lists"
- deriving clauses
- default clauses
- record-like occurrences in terms and types (declarations, patterns, constructions)
- multi-name signatures (including nested in records)

This proposal **does not include yet** using extra commas in the following unordered structures:

- fixity "lists"
- fundeps clauses

This proposal does not cover structures in which the order of the elements matters, 
such as lists, tuples, and constraint tuples.


Examples
--------
1. **Improved Diff Quality**:

   Adding or removing an element often requires changing 2 lines of code

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

   now we can do this in 1 line

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

   This proposal would simplify conditional meta-programming like this:
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

This proposal purposefully dodges interacting with ``TupleSections`` extension.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``UnorderedExtraCommas`` to have medium difficulty.

Second, all tooling which parses Haskell code will need to be updated to be compatible with the extended syntax.


Backward Compatibility
---------------------------------

This change is backward compatible with existing Haskell code,
as it introduces new syntactical permissiveness without altering the existing valid syntax.

All current Haskell programs will remain valid and unchanged in their behavior.


Alternatives
------------

The primary alternative is the "status quo".

Alternative adding extra commas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. The main non-controversial alternative of **Unordered** Extra Commas is **NonData** Extra Commas (allowing extra commas in non-data structures), 
   which is in the same places as "Unordered" without records.

   The author didn't see anyone against extra commas in records.

2. The proposal suggests to allow trailing and leading commas at the same time, but the committee could choose instead to only allow one of
   trailing or leading commas at the same time. This would be a bit stricter.

   The main benefit of the OR-version is that Cabal already supports this liberalisation.

   The main disadvantage of the OR-version is disallowing the mixing of code-styles. 
   Also, the stricter version needs more complex parsing: ::

      lead_OR_trail  ::= ( , subList ) | ( subList [,] )

      lead_AND_trail ::= [,] subList [,]

3. The proposal suggests to allow extra commas **WITHOUT** repetitive commas, but the committee could choose instead to allow
   extra commas **WITH** repetitive commas. This would be more lenient.

   **Repetitive Commas**: Allow multiple commas instead of one in enumeration clauses where order does not matter:

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

   The main disadvantage of the WITH-version is allowing to write very "dirty" code. 
   Haskell is known as a language with "pretty looking code".

   The main benefit of the WITH-version is the maximum liberalisation of using extra commas. 
   Also, the more lenient version has almost the same parsing ::

     lead_AND_trail_WITHOUT_repeats ::=  [,] { elem_i , } elem_max [,] 

     lead_AND_trail_WITH_repeats    ::=  {,} { elem_i , {,} } elem_max {,} 

4. The proposal suggests to allow extra commas in code, but the committee could also allow extra commas in **pragmas**.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Acknowledgments
----------------

Thanks to all contributors of `ExtraCommas (was: Trailing and leading commas in sub-export lists) 
<https://github.com/ghc-proposals/ghc-proposals/pull/87>`__.


Endorsements
-------------
