Extra NonTuple Commas
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
in most structures (pragmas, lists, records, import and export lists and sublists, derivation and default clauses,
fixity, guards and fundeps declaration, different multi-name signatures) except tuple-like structures.

This change aims to improve code readability and maintainability by allowing more flexibility 
in formatting all structures but not in tuple-like.

This is particularly helpful in scenarios involving version control, code reviews, and automated code generation.


Motivation
----------

In many programming languages, including JavaScript, Python, and Rust, trailing commas in lists
(and other collection types) are a common feature.

For example some features causes the problem that code needs to be repeated::

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


Proposed Change Specification
-----------------------------

This proposal introduces the following syntactical changes to Haskell.

Add a new language extension ``ExtraNonTupleCommas`` which allows both leading
and trailing commas in **any** syntactic construct in which enumeration with a comma as separator is allowed, 
**except** those that looks like tuples: which are delimited by round parentheses in either terms or types (or constraints).

Sure, we allow extra comma if and only if enumeration is not empty:

- an extra leading comma must precede a "list" item
- and extra trailing comma must follow an item



    x  = [,]                   -- forbidden
    y  = [,,]                  -- forbidden

    data T =  MkT deriving (,) -- forbidden

Note that (because of the exception) this proposal does not cover tuple-like structures
(including tuples and unboxed tuples, constraint tuples, class context simplified and non-simplified), 
delimited with round parentheses.  
For example ``(,,3,4)`` is not covered by this proposal - rather, it is a tuple section 
if extension ``-XTupleSections`` is on.

More precisely, this extension allows both leading and trailing commas 
(but not adjacent commas) in the following structures:

- module export "lists" and sub-"lists"
- module import "lists" and sub-"lists"
- deriving clauses
- default clauses
- list-comprehensions 
- literal list (expressions and patterns) 
- records in terms and types (declarations, patterns, constructions)
- multi-name function signatures (including nested fields in records)
- multi-name patten signatures
- multi-name type-synonym signatures
- fixity "lists"
- fundeps clauses
- guard clauses
- pragmas clauses

  - ``COMPLETE``
  - ``DEPRECATED``
  - ``INLINE``
  - ``NOINLINE``
  - ``LANGUAGE``
  - ``MINIMAL``
  - ``SPECIALIZE``
  - ``SPECIALIZE INLINE``
  - ``WARNING``


Syntax
~~~~~~~~~~~~

The formal grammar changes for ``ExtraNonTupleCommas`` for trailing **and** leading commas **without** adjacent commas:

Trailing commas in module export and import lists (but not in sub-lists) 
are already supported without this proposal.

Grammar changes in export and import "lists" and sub-"lists":

.. code:: abnf

    exports ::= '(' [','] export1 ',' … ',' exportn [','] ')'                   ;-- upd

    export  ::= qvar
        | qtycon [ ('..') | ( [','] cname1 ',' … ',' cnamen [','] ) ]  (n ≥ 0)  ;-- upd
        | qtycls [ ('..') | ( [','] var1 ',' … ',' var_n [','] ) ]     (n ≥ 0)  ;-- upd
        | 'module' modid
        | ……
   
    impspec ::= '(' [','] import1 ',' … ',' importn [','] ')'          (n ≥ 0)  ;-- upd
        | 'hiding' '(' [','] import1 ',' … ',' importn [','] ')'       (n ≥ 0)  ;-- upd

    import  ::= qvar
        | qtycon [ ('..') | ( [','] cname1 ',' … ',' cnamen [','] ) ]  (n ≥ 0)  ;-- upd
        | qtycls [ ('..') | ( [','] var1 ',' … ',' var_n [','] ) ]     (n ≥ 0)  ;-- upd
        | ……

Grammar changes in deriving and default clauses:

.. code:: abnf

    deriving ::= 'deriving' dclass
            | 'deriving' '('  
                 ( [','] dclass1 ',' … ',' dclassn [','] )               ;-- upd
              ')'

    topdecl  ::= 'type' simpletype '=' type
        | 'default' '(' [','] type1 ',' … ',' typen [','] ')'            ;-- upd
        | 'class' [scontext '=>'] tycls tyvar [fundep] ['where' cdecls]
        | ……

Grammar changes in records and lists, including expressions, patterns, declarations:

.. code:: abnf

    constr ::= con ['!'] atype1 … ['!'] atypek                          (arity con = k, k ≥ 0)
        | '(' btype | '!' atype) conop (btype | '!' atype ')'                    (infix conop)
        | con '{' [','] fielddecl1 ',' … ',' fielddecln [','] '}'              (records n ≥ 0)  ;-- upd
        | ……

    fielddecl ::= vars '::' (type | '!' atype)

    aexp ::= qvar                                                                   (variable)
        | ……
        | '[' [','] exp1 ',' … ',' expk [','] ']'                                (list, k ≥ 1)  ;-- upd
        | '[' exp '|' [','] qual1 ',' … ',' qualn [','] ']'        (list comprehension, n ≥ 1)  ;-- upd
        | ……
        | qcon '{' [','] fbind1 ',' … ',' fbindn [','] '}'       (labeled construction, n ≥ 0)  ;-- upd
        | aexp_(qcon) '{' [','] fbind1 ',' … ',' fbindn [','] '}'    (labeled update, n  ≥  1)  ;-- upd

    apat ::= var [ '@' apat]                                                      (as pattern)
        | ……
        | '[' [','] pat1 ',' … ',' patk [','] ']'                        (list pattern, k ≥ 1)  ;-- upd
        | ……
        | qcon '{' [','] fpat1 ',' … ',' fpatk [','] '}'              (labeled pattern, k ≥ 0)  ;-- upd

Grammar changes in fixity "lists", multi-name signatures, fundeps & guard clauses:

.. code:: abnf

    gendecl ::= vars '::' [context '=>'] type                  (type signature)
        | fixity [integer] ops                             (fixity declaration)
        |                                                   (empty declaration)

    fixity  ::= 'infixl' | 'infixr' | 'infix'

    topdecl ::= 'type' simpletype '=' type            (simple type declaration)
        | 'type' qtycons '::' type                     (multi-type declaration)
        | 'pattern' pats '::' type ['where' pdecls]       (pattern declaration)
        | ……

    vars ::= [','] var1 ',' … ',' varn [',']                   (n ≥ 1)  ;-- upd

    ops  ::= [','] op1 ',' … ',' opn [',']                     (n ≥ 1)  ;-- upd

    pats ::= [','] pat1 ',' … ',' patn [',']                   (n ≥ 1)  ;-- upd

    qtycons ::= [','] qtycon1 ',' … ',' qtyconn [',']          (n ≥ 1)  ;-- upd

    fundep  ::= '|' [','] fdp1 ',' … ',' fdpn [',']            (n ≥ 1)  ;-- upd

    fdp     ::= tyvar1 … tyvarn '->' tyvarm                    (n ≥ 1)

    guards  ::= '|' [','] guard1 ',' … ',' guardn [',']        (n ≥ 1)  ;-- upd

Grammar changes in pragmas:

.. code:: abnf

    inlprgmdcl   ::= '{-#' 'INLINE' qvars '#-}'

    noinlprgmdcl ::= '{-#' 'NOINLINE' qvars '#-}'

    deprprgmdcl  ::= '{-#' 'DEPRECATED' qvars txt '#-}'

    wrngprgmdcl  ::= '{-#' 'WARNING' qvars txt '#-}'

    qvars ::= [','] var1 ',' … ',' varn [',']                   (n ≥ 1)  ;-- upd

    specprgmdcl    ::= '{-#' 'SPECIALIZE' specs '#-}'

    specinlprgmdcl ::= '{-#' 'SPECIALIZE' 'INLINE' specs '#-}'

    specs ::= [','] spec1 ',' … ',' specn [',']                 (n ≥ 1)  ;-- upd

    spec  ::= vars [','] '::'  type                                      ;-- upd

    langprgmdcl ::= '{-#' 'LANGUAGE' lanexts '#-}'

    lanexts     ::= [','] lext1 ',' … ',' lextn [',']           (n ≥ 1)  ;-- upd

    cmplprgmdcl ::= '{-#' 'COMPLETE' cmplpats '#-}'

    cmplpats    ::= [','] pat1 ',' … ',' patn [',']             (n ≥ 1)  ;-- upd

    mingprgmdcl ::= '{-#' 'MINIMAL' mindef '#-}'

    mindef ::= [','] var1 ',' … ',' varn [',']                  ;-- upd
        |  [','] '(' mindef ')' [',']                           ;-- upd
        |  mindef '|' mindef


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

   With a leading comma, a standard style for formatting a line with a comma before and a single element per line can be improved:
   ::

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

   With a trailing comma, an alternative style for formatting a line with a comma after and a single element per line can be used:
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

       lst :: [Int]
       lst = [ 
              1, 
              2, 
              3, 
              4,
            ]

   A mix of styles also could be used:
   ::

      -- mix of styles
      data Example4 a = ....
                 deriving (
                        , Functor
                        , Foldable,
                          Applicative,
                          Traversable,
                 )

      lst4 :: [Int]
      lst4 = [ , 1, 2, 3, 4, 5, 6, 7, 8, ]

4. **Pragmas**
   ::

     {-# LANGUAGE ExtraNonTupleCommas #-}
     {-# LANGUAGE Haskell2010,
                  StandaloneTypeSignatures,
                  PatternSynonyms,
                  TupleSections,
     #-}

     factorial :: Num a => a -> a  
     factorial 0 = 0  
     factorial n = n ⋆ factorial (n-1)  
     {-# SPECIALIZE factorial :: Int -> Int,  
                    factorial :: Integer -> Integer,
     #-}

     foo1, bar, baz, :: forall a b. a -> a -> b -> b -> (a, b)
     {-# INLINE foo1, 
                bar, 
                baz, 
     #-}

5. Different structures:
   ::
   			
       data family URec a p

       data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#,   }

       data instance URec Char     p = UChar   { uChar#   :: Char#,   uInt# :: Int#, }

       data instance URec Double   p = UDouble { , uDouble# :: Double#, uInt#, :: Int#,  uFloat#  :: Float#, }

       infixr 5 +, -,

       class C a b | a -> b, b -> a,  where ....

       foo x y |  p x, q y, = expr

       ,f, g, h, :: Int -> Int

       type , Foo, Bar, Baz, :: Int -> Int

       pattern , pat1, pat2, :: Int -> Int

       default (
                 Int,
                 String,
               )

Effect and Interactions
-----------------------

Tuple Sections
~~~~~~~~~~~~~~

This proposal purposefully dodges interacting with ``TupleSections`` extension.
With ``TupleSection`` it is impossible to distinguish syntexically section ``(, a)`` from extra comma ``(, a)``.

That's why this proposal do not cover tuples, unboxed tuples, constraint tuples, class context siplified and non-simplified.

Standalone Type Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~

We allow to use extra commas in multi-name type-synonym signatures with the ``StandaloneTypeSignatures`` extension.

Pattern Synonyms
~~~~~~~~~~~~~~~~

We allow to use extra commas in multi-name pattern signatures with ``PatternSynonyms`` extension.

CPP
~~~

This proposal does not directly affect the ``CPP`` extension, 
but it allows you to write fewer conditional CPP directives in code involving comma separated lists. 
See the example in the motivation.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``ExtraNonTupleCommas`` to have medium difficulty.

Second, all tooling which parses Haskell code will need to be updated to be compatible with the extended syntax.


Backward Compatibility
---------------------------------

This change is backward compatible with existing Haskell code,
as it introduces new syntactical permissiveness without altering the existing valid syntax.

All current Haskell programs will remain valid and unchanged in their behavior.


Alternatives
------------

The primary alternative is the "status quo".

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
in all unordered structures (records, import and export "lists" and sublists, derivation and default clauses, multi-name signatures).

Alternative rules for adding extra commas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. The main non-controversial alternative of **NonTuple** Extra Commas is **Unordered** Extra Commas (allowing extra commas in unordered structures)
   and **NonData** Extra Commas (allowing extra commas in non-data structures), which is in the same places as "Unordered" without records.
  
   Other alternatives includes **Im(Ex)port** Extra Commas and **Everywhere** Extra Commas.

2. The proposal suggests to allow trailing and leading commas at the same time, but the committee could choose instead to only allow one of
   trailing or leading commas at the same time. This would be a bit stricter.

   The main benefit of the OR-version is that Cabal already supports this liberalisation.

   The main disadvantage of the OR-version is disallowing the mixing of code-styles. 
   Also, the stricter version needs more complex parsing: ::

      lead_OR_trail  ::= ( ',' subList ) | ( subList [','] )

      lead_AND_trail ::= [','] subList [',']

3. The proposal suggests to allow extra commas **WITHOUT** adjacent commas, but the committee could choose instead to allow
   extra commas **WITH** adjacent commas. This would be more lenient.

   **Adjacent Commas**: Allow multiple commas instead of one in enumeration clauses:
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

     lead_AND_trail_WITHOUT_adjacent ::=  [','] { elem_i ',' } elem_max [','] 

     lead_AND_trail_WITH_adjacent    ::=  {','} { elem_i ',' {','} } elem_max {','} 


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
