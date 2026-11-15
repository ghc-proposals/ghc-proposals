New separator for View Patterns
==================================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/767>`_.
.. sectnum::
.. contents::

We change the separator from ``->`` to ``|-`` in ``ViewPatterns`` 
to avoid any conflicts with ``RequiredTypeArguments`` syntax ::

    size (view |- Unit) = 1
    size (view |- Arrow t1 t2) = size t1 + size t2


Motivation
----------

Depended Haskell and ``RequiredTypeArguments`` extension especially 
plays more and more significant role in modern haskell code.

But due similar structure it is became harder and harder to distinguish 
type-terms from view patterns now if we use both extension together.

This proposal suggest a breaking change - change a syntax for ``ViewPatterns``
in favor of ``RequiredTypeArguments``.

We change the separator from ``->`` to ``|-`` to avoid 
any conflicts with future of Depended Haskell ::

    endpoints (sort |- begin : (reverse |- end : _)) = Just (begin, end)
    endpoints _ = Nothing


Proposed Change Specification
-----------------------------

First, we change the operator ``->`` for ``ViewPatterns`` extension to a new one ``|-`` separator.

Second, we make operator ``|-`` as keyword in terms.

Third, we allow to use old separator ``->`` as Deprecated one with a Warning.


Syntax
~~~~~~~~~~~~

The formal grammar changes for assigned a pattern type signatures:

.. code:: abnf

    apat ::= var [ @ apat]              --;     (as pattern)
        | ……
        | '(' expr '|-' pat ')'         --; upd (view pattern)
	

Examples
--------

An example with requested constraint
::

    lensDownloads :: Map HaskellPackage Int -> Int
    lensDownloads (M.lookup "lens" |- Just n) = n
    lensDownloads _ = 0

    email :: String -> Maybe EMail
    email (parse |- x) = x

    nonzero :: (a -> Int) -> a -> Maybe a
    nonzero f (f |- 0) = Nothing
    nonzero _ x = Just x


Effect and Interactions
-----------------------

This change ``->`` to ``|-`` keyword for separator for ``ViewPatterns`` extension 
was made to avoid conflicts with ``RequiredTypeArguments`` syntax
and future development of Depended Haskell.

UnicodeSyntax
~~~~~~~~~~~~~~~~

We wish to preserve ``⊢`` (Right Tack, U+22A2) symbol to represent ``|-`` requested separator in pattern signatures.
::
    email ∷ String → Maybe EMail
    email (parse ⊢ x) = x


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

Alternative keywords
~~~~~~~~~~~~~~~~~~~~~~~~

We could choose different separator instead of proposed ``|-`` and unicode ``⊢`` keywords.

Why ``|-`` is choosen? **Pro**: it looks mathematical. 
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
