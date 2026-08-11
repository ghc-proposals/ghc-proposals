Comma Tuples
============

.. author:: Viktor WW
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/749>`_.
.. sectnum::
.. contents::

This proposal suggests extending the Haskell syntax by adding alternative syntax 
for tuple-like structures with build support of trailing and leading commas.

This change aims to improve code readability and maintainability by allowing more flexibility 
in formatting boxed and unboxed tuples, constraint tuples and class context simplified and not. 

First of all, particularly important in scenarios involving version control, code reviews, and automated code generation.


Motivation
----------

In many programming languages, including JavaScript, Python, and Rust, trailing commas in lists
(and other collection types) are a common feature.

This feature provides several benefits:

1. **Improved Diff Quality**: When adding or removing items in a tuple structure, having a trailing or leading comma means only
   the relevant lines are changed, reducing the noise in diffs and making reviews easier.
  
2. **Ease of Code Modification**: Developers can add new elements to the end / beginning of a structure without having 
   to modify the previous last / first line, which reduces the likelihood of syntax errors.

3. **Consistency in Formatting**: When generating code automatically or formatting structures in a specific way, 
   leading and trailing commas can simplify the process.

4. **Use a different style of coding**: Extra commas allow for different styles to write code.

5. **Simplicity of conditional meta-programming**: Extra commas allow to write much simpler code when conditional meta-programming is used.


Now git-diffs often looks like this:

.. code-block:: diff

  --- example.hs	2024-06-10 12:00:00 +0000
  +++ example.hs	2024-06-10 12:01:00 +0000
  @@ -2,5 +2,5 @@
      "apple",
      "banana",
  -   "cherry"
  +   "cherry",
  +   "peach"
      )

but we wish to write insted:

.. code-block:: diff

  --- example.hs	2024-06-10 12:00:00 +0000
  +++ example.hs	2024-06-10 12:01:00 +0000
  @@ -2,5 +2,5 @@
      "apple",
      "banana",
      "cherry",
  +   "peach",
      ) data
      

Proposed Change Specification
-----------------------------

Main idea is:  To create an alternating syntax for tuple-like structures 
that has built support for trailing and leading commas and are not affected by ``TupleSection`` extension.

This proposal introduces the following syntactical changes to Haskell:

1. Add language extension ``CommaTuples``

2. Add language extension ``ExtraCommas`` which is just unification of 2 extensions: ``ExtraCommas = CommaTuples + ExtraNonTupleCommas``

3. **Comma qualified tuples**: Allow to write ``data`` keyword after tuple close bracket `)` in tuples, 
   unboxed-tuples, constraint tuples and class context at terms and types.

   Comma qualified tuples (and tuple-like strucures) by meaning are indistinguishable 
   from ordinary tuples, but they are different in syntax.
   
   Tuple ``(a, b, c)`` is the same as ``(, a, b, c,) data`` in types or class context;
   and ``(x, y, z)`` is the same as ``(, x, y, z,) data`` in terms. 

   The only difference between comma-qualified and ordinary tuples (and tuple-like strucures) is:

   - ordinary tuples don't allow extra commas, but allow curried tupling constructors 
     and ``TupletSections`` (if values are not Constraint kind)
   
   - comma-qualified tuples allow extra commas, but ignore ``TupletSections``
   
     - Allow a comma before the first element after opening bracket ``(`` or ``(#``
	
     - Allow a comma after the last element before closing bracket ``)``  or ``#)``
	
     - Allow both trailing & leading commas in same structure
   
       ::
  
         myTuple1 :: (Int, String, Char)
         myTuple1 = (1, "2abc", 'd') data

         myTuple2 :: (, Int, Int, String, Char) data
         myTuple2 = (42, 43, "xyz", 'w',) data

         myTuple3 :: (,Int, String, Char,) data
         myTuple3 = (,1, "2abc", 'd') data

4. **Comma Qualified solo-tuples**: Allow to write solo-tuples 
   (and tuple-like strucures) with ``data`` keyword ::

       mySoloTuple :: (Int) data
       mySoloTuple  = (5,) data

5. **Comma Qualified unit-tuples**: It is allowed to write unit-tuples 
   (and tuple-like strucures), but not in constructors 
   with ``data`` keyword, but without any extra comma::

       myUnitTuple  :: () data
       myUnitTuple   = () data

       myUnitTuple2  = (,) data   -- forbidden


Syntax
~~~~~~~~~~~~
	  
The formal grammar changes for ``CommaTuples``:

Syntax for tuples, unboxed tuples, constraint tuples:

.. code:: abnf

    ;-- kmax = if 'data' then 1 else 2

    atype ::= gtycon
        | '(' ')'     'data'                                                      (comma empty tuple type)           ;-- new
        | '('  ',' type1 ',' … ',' typek [',']  ')'  'data'                       (comma tuple type, k ≥ 1)          ;-- upd
        | '('      type1 ',' … ',' typek (( ',' ')'  'data' )|( ')' ['data'] ))   (tuple type, k ≥ kmax)             ;-- new
        | '(#' '#)'   'data'                                                      (comma empty unboxed tuple type)   ;-- new
        | '(#'  ',' type1 ',' … ',' typek [',']  '#)' 'data'                      (unboxed comma-tuple type, k ≥ 1)  ;-- upd
        | '(#'      type1 ',' … ',' typek (( ',' '#)' 'data' )|( '#)' ['data'] )) (unboxed tuple type, k ≥ 1)        ;-- new
        | ……

    aexp ::= qvar                                                                 (variable)
        | ……
        | '(' exp ')'                                                             (parenthesized expression)
        | '(' ')'    'data'                                                       (comma empty tuple)                ;-- new
        | '('  ',' exp1 ',' … ',' expk [',']  ')'  'data'                         (comma tuple, k ≥ 1)               ;-- upd
        | '('      exp1 ',' … ',' expk (( ',' ')'  'data' )|( ')' ['data'] ))     (tuple, k ≥ kmax)                  ;-- new
        | '(#' '#)'  'data'                                                       (comma empty unboxed tuple)        ;-- new
        | '(#'  ',' exp1 ',' … ',' expk [',']  '#)' 'data'                        (unboxed comma-tuple, k ≥ kmax)    ;-- upd
        | '(#'      exp1 ',' … ',' expk (( ',' '#)' 'data' )|( '#)' ['data'] ))   (unboxed tuple, k ≥ 1)             ;-- new
        | '(' infixexp qop ')'                                                    (left section)
        | '(' qop⟨-⟩ infixexp ')'                                                  (right section)

    apat ::= var [ @ apat]                                                        (as pattern)
        | ……
        | '(' pat ')'                                                             (parenthesized pattern)
        | '(' ')'     'data'                                                      (comma empty tuple pattern)             ;-- new
        | '('  ',' pat1 ',' … ',' patk [',']  ')'  'data'                         (comma tuple pattern, k ≥ 1)            ;-- upd
        | '('      pat1 ',' … ',' expk (( ',' ')'  'data' )|( ')' ['data'] ))     (tuple pattern, k ≥ kmax)               ;-- new
        | '(#' '#)'   'data'                                                      (comma empty unboxed tuple pattern)     ;-- new
        | '(#'  ',' pat1 ',' … ',' patk [',']  '#)' 'data'                        (unboxed comma-tuple, k ≥ kmax pattern) ;-- upd
        | '(#'      pat1 ',' … ',' patk (( ',' '#)' 'data' )|( '#)' ['data'] ))   (unboxed tuple pattern, k ≥ 1)          ;-- new


Syntax for class content and class simplified content:

.. code:: abnf

    topdecl ::= 'type' simpletype '=' type
        | 'data'     [context '=>']  simpletype  ['=' constrs] [deriving]
        | 'newtype'  [context '=>']  simpletype  '=' newconstr [deriving]
        | 'class'    [scontext '=>'] tycls tyvar ['where' cdecls]
        | 'instance' [scontext '=>'] qtycls inst ['where' idecls]
        | ……

    gendecl ::= vars '::' [context '=>'] type                   (type signature)
        | fixity [integer] ops                                  (fixity declaration)
        |                                                       (empty declaration)

    exp ::= infixexp '::' [context '=>'] type                   (expression type signature)
        | infixexp

    context ::= class
        | '(' ')'  ['data']                                               ;-- upd
        | '(' ',' cntclasses [','] ')' 'data'                             ;-- upd
        | '(' cntclasses ((')' ['data']) | (',' ')' 'data'))              ;-- upd


    scontext ::= simpleclass
        | '(' ')'  ['data']                                               ;-- upd
        | '(' ',' scntclasses [','] ')' 'data'                            ;-- upd
        | '(' scntclasses ((')' ['data']) | (',' ')' 'data'))             ;-- upd
		
    cntclasses ::= class1 ',' … ',' classn                       (n ≥ 1)  ;-- upd

    class ::= qtycls tyvar
        | qtycls '(' tyvar atype1 … atypen ')'                   (n ≥ 1)

    scntclasses ::= simpleclass1 ',' … ',' simpleclassn          (n ≥ 1)  ;-- upd

    simpleclass ::= qtycls tyvar
 
    simpletype  ::= tycon tyvar1 … tyvark                        (k ≥ 0)


These changes allow extra commas in the all comma-tuple-like structures:

- tuples
- unboxed tuples
- constraint tuples
- class content
- class simplified content

This proposal does not cover constructors for obvious reasons.


Proposed Library Change Specification
-------------------------------------

The core library ``base`` must be changed.

First of all must be updated ``Read a`` instances 
for supporting reading comma qualified tuples.

Second, we update ``Data.Tuple`` to:
::

    data Solo a = (a) data
	
    pattern MkSolo a = (a) data


Examples
--------

1. **Trailing Commas**

   Instance with content with trailing comma::

       instance ( 
                  GSerialize a, 
                  GSerialize b,
                ) data 
            => 
                GSerialize (a :+: b) 
            where
               ...


      
2. Mix of comma-tuples and ordinary tuples

   Unboxed tupleles, class context ::

       unlftTuple1 = (# 1#, 'x'#, 3.2## #) data


       myfun1 :: forall a s. (
                    C1 a,
                    C2 a s,
                    C3 s,
                ) data =>
                     (# 
                       SuperLongType a,
                       SuperPuperLongType a s,
                       MegaPuperLongType (Maybe a),
                     #) data
                     -> Int#
                     -> Int#
                     -> Int#
       myfun1 = ....



Effect and Interactions
-----------------------

We choose the **postfix variant** ``(x,y,z) data`` over **prefix variant** ``data (x,y,z)`` to avoid injections 
for ``BangPatterns``, ``AsPattern``, ``StrictPattern``,  ``Irrefutable Patterns``, data declaration ::

    -- Bang Patterns
    let !(,p,q,) data = e in body
    
    let (!x, !(y,) data) = e in body

    -- As Pattern
    foo1 :: (a, b) -> a
    foo1 t@(,p,q,) data = t
   
    -- StrictPattern
    data T = MkT ~(,Int, Int, Int,) data
    
    -- Irrefutable Patterns
    let ~(a,b,) data = expr in e0 a b

    -- Data declaration	vs function declaration
    (a1, b1,) data `op` (a2, b2,) data = expr

Tuple Section
~~~~~~~~~~~~~~~~~~

``TupleSection`` extension do not interact with alternative syntax of comma-tuples.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``CommaTuples`` has medium difficulty.


Backward Compatibility
---------------------------------

This change is backward compatible with existing Haskell code,
as it introduces new syntactical permissiveness without altering the existing valid syntax.

All current Haskell programs will remain valid and unchanged in their behavior.

Alternatives
------------

The primary alternative is "status quo".

History
~~~~~~~~~~~~

Adding trailing commas (and more rarely leading commas) is a frequently asked feature to add in Haskell.

But this task is highly divisive in the Haskell community.

Original Proposal #87 `ExtraCommas (was: Trailing and leading commas in sub-export lists) <https://github.com/ghc-proposals/ghc-proposals/pull/87>`__ 
was discussed for several years, and that discussion was so controversial 
that the author withdrew their own proposal just before the final Acceptation was received (with minor changes).

Unfortunately, redundant commas contradict with ``TupleSection`` 
(and presumably/conceivable ``ListSections``) extension's notation. 
This is inconstancy.

This proposal is an attempt to allow redundant commas more universally and consistently.

Alternative Syntax
~~~~~~~~~~~~~~~~~~

Alternative to ``(x, y, z) data`` syntax we could choose alternative syntax,
like ``q(x, y, z)`` or ``(x, y, z)q`` or ``%(x, y, z)`` and ``(x, y, z)%``. 

Or alternative keywords could be chosen insted of ``data``, like ``qualified``.

Alternative Rule for Commas
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal has synchronized rules with ``ExtraNonTupleCommas`` extension:

  We allow Leading Comma AND Trailing Comma (in single Structure) but WITHOUT Adjacent Commas.

Alternative to this are different rules, which are different from described one.


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

Thanks to all contributors of `Add Support for Trailing and Leading Commas in Lists 
<https://github.com/ghc-proposals/ghc-proposals/pull/658>`__.

Thanks to all contributors of `Allow Trailing Comma in List Constructor Syntaxs 
<https://github.com/ghc-proposals/ghc-proposals/issues/653>`__.

Thanks to all contributors of `Extra NonTuple Commas 
<https://github.com/ghc-proposals/ghc-proposals/issues/748>`__.


Endorsements
-------------
