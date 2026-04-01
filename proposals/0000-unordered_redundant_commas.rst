Redundant commas in UnOrdered Structures
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
in unordered structures (records, import and export lists and sublists, derivation clauses).

This change aims to improve code readability and maintainability by allowing more flexibility 
in formatting unordered structures (records, import and export lists and sublists, derivation clauses). 

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


History
~~~~~~~~~~~~

Adding trailing commas (and more rarely leading commas) is a frequently asked feature to add in Haskell.

But this task is highly divisive in the Haskell community.

Original Proposal #87 `ExtraCommas (was: Trailing and leading commas in sub-export lists) <https://github.com/ghc-proposals/ghc-proposals/pull/87>`__ 
was discussed several years and that discussion was so controversial,
that author withdrawn own proposal just before the final Acceptation was received (with minor changes).

However, the tension in the Haskell community was so high that the duplicate of that proposal was never proposed 
and had never succeeded in the next 6 years.

This proposal is an attempt to allow redundant commas where everyone agrees to have them - 
in unordered structures (records, import and export "lists" and sublists, derivation clauses).

This proposal does not cover order-matter structures (including lists, tuples, constraint tuples).

This proposal introduces the following syntactical changes to Haskell:

1. **New extension**: Add language extension ``UnorderedRedundantCommas``

2. **UnOrdered Trailing Commas**: Allow a comma after the last element in place-unordered clauses:

   - module export lists(already supported) and sub-lists
   - module import lists(already supported) and sub-lists
   - deriving clauses
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

	  
9. **Qualified Leading Commas**: Allow a comma before the first element in place-unordered clauses:

   - module export lists and sub-lists
   - module import lists and sub-lists
   - deriving clauses
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


Syntax
~~~~~~~~~~~~

The formal grammar changes for ``UnOrderedRedundantCommas`` :

:: 

    export ::= qvar
        | qtycon[(..)| [,] (cname_1, ..., cname_n) [,]]  (n >= 0)     -- upd
        | qtycls[(..)| [,] (var_1, ..., var_n) [,]]      (n >= 0)     -- upd
        | module modid
	    | ......

    import ::= qvar
        | qtycon[(..)| [,] (cname_1, ..., cname_n) [,]]  (n >= 0)     -- upd
        | qtycls[(..)| [,] (var_1, ..., var_n) [,]]      (n >= 0)     -- upd
	    | ......

    deriving ::= deriving (
              [,] dclass [,]                     -- upd
	        | [,] (dclass1, ... , dclassn) [,]   -- upd
        )

    constr ::= con [!] atype1 ... [!] atypek	(arity con = k, k>=0)
        |	(btype | ! atype) conop (btype | ! atype)	(infix conop)
        |	con { [,] fielddecl1 , ... , fielddecln [,] }	(records n>=0)  -- upd
        | ......

These changes allow redundant commas in next unordered structures:

- module export "lists"
- module export sub-"lists"
- module import lists
- module import sub-"lists"
- deriving clauses
- record-like occurrences (declarations, patterns, constructions)

This proposal does not include yet using redundant commas in next unordered structures:

- fixity "lists"
- default clauses


Examples
--------


Effect and Interactions
-----------------------

None.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``UnorderedRedundantCommas`` has medium difficulty.


Backward Compatibility
---------------------------------

This change is backward compatible with existing Haskell code,
as it introduces new syntactical permissiveness without altering the existing valid syntax.
All current Haskell programs will remain valid and unchanged in their behavior.


Alternatives
------------

The primary alternative is "status quo".


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

It is unclear.


Acknowledgments
----------------

Thanks to all contributors of `ExtraCommas (was: Trailing and leading commas in sub-export lists) 
<https://github.com/ghc-proposals/ghc-proposals/pull/87>`__.


Endorsements
-------------
