New syntax for View Patterns
==================================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/767>`_.
.. sectnum::
.. contents::

We change the syntax from ``(expr -> pat)`` to ``(pat <- expr)`` in ``ViewPatterns`` 
to avoid any conflicts with ``RequiredTypeArguments`` syntax ::

    size (Unit        <- view) = 1
    size (Arrow t1 t2 <- view) = size t1 + size t2


Motivation
----------

Depended Haskell and ``RequiredTypeArguments`` extension especially 
plays more and more significant role in modern haskell code.

But due similar structure it is became harder and harder to distinguish
type-terms from view patterns now if we use both extension together.
::

 --     ---------- Is it View Pattern?
 --     v
 foo (b -> Int) = some_expr
 --     ^
 --     ---------- Is it Type Argument?

So EACH time the compiler find an argument, it always checks
if it consists exactly one ``->`` and if it has,
then EACH time heuristic is applied is it just a type or expression with pattern.

So resolving this ambiguity make parsing much slower!
So right now it is much easier to switch off one of 2 extension.

Second, the current syntax looks **flipped** / wrong relative to guards, 
do-expressions, comprehension generators and pattern synonym declarations.

We change the syntax from ``(expr -> pat)`` to ``(pat <- expr)`` 

- to avoid any conflicts with future of Depended Haskell 
- to ``->`` arrow pointing like in pattern synonym declarations, 
  guards and comprehension generators

::

    endpoints (begin : (end : _ <- reverse) <- sort) = Just (begin, end)
    endpoints _ = Nothing

Even if it looks too radical, but it is better to change to something that looks right, 
than to remain that looks wrong-looking.

And yes, this proposal suggest a **breaking change** - we change a syntax for ``ViewPatterns``
in favor of ``RequiredTypeArguments``.


Proposed Change Specification
-----------------------------

First, we change the syntax ``(expr -> pat)`` to ``(pat <- expr)`` for ``ViewPatterns`` extension.

We allow to use old syntax ``(expr -> pat)`` as Deprecated one with a Warning.


The formal grammar changes for assigned a pattern type signatures:

.. code:: none

    apat ::= var [ @ apat]              --;     (as pattern)
        | ……
        | '(' pat '<-' expr ')'         --; upd (view pattern)
	

Examples
--------

An example with requested constraint
::

    lensDownloads :: Map HaskellPackage Int -> Int
    lensDownloads (Just n <- M.lookup "lens") = n
    lensDownloads _ = 0

    email :: String -> Maybe EMail
    email (x <- parse) = x

    nonzero :: (a -> Int) -> a -> Maybe a
    nonzero f (0 <- f) = Nothing
    nonzero _ x = Just x


Effect and Interactions
-----------------------

This change ``(expr -> pat)`` to ``(pat <- expr)`` for ``ViewPatterns`` extension 
was made to avoid conflicts with ``RequiredTypeArguments`` syntax
and future development of Depended Haskell.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of separator change has low difficulty.


Backward Compatibility
----------------------

Some libs and code became Deprecated a bit and lately will cause a breakage.

Hopefully it affects very complex syntax, which is not widely uses.

But this change will allow to write a code in a more universal way - with  less conflicts.


Alternatives
------------

The primary alternative is "status quo".

Alternative separator instead of alternative syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could choose different separator  
``(expr |- pat)`` and unicode ``(expr ⊢ pat)`` 
instead of proposed different syntax.
::

    size (view |- Unit       ) = 1
    size (view |- Arrow t1 t2) = size t1 + size t2
	
But we must to add this new separator as a keyword 
(or at least term-keyword) which could broke some packages.

Why ``|-`` could be choosen? **Pro**: it looks mathematical. 
**Cons**: it is already used as operator in several packages:
*uniplate*, *numhask*, *sbv*, *tidal*, *tidal-core*, *one-liner*.

Use different *operator* for separator, for example:

#. Use ``|--`` and same unicode ``⊢`` for separator. 
   **Pro**: it looks mathematical. It is unused in all packages.
   **Cons**: 3 letter instead of 2.

#. Use ``||-`` and unicode ``⊩`` (Forces, U+22A9) for separator. 
   **Pro**: it looks mathematical. 
   **Cons**: 3 letter instead of 2. It is already used as operator in several packages:
   *tidal*, *tidal-core*.

#. Use ``|->``  and unicode ``↦`` (RIGHTWARDS ARROW FROM BAR, U+21A6) for separator. 
   **Pro**: Similar to already used one. 
   **Cons**: 3 letter instead of 2.
   It is already used as operator in several packages:
   *clash-prelude*, *sbv*, *liquid-fixpoint*, *HMock*.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Acknowledgments
---------------


Endorsements
-------------
