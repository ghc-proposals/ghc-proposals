
Unicode Ellipsis
================

.. author:: Ignat Insarov
.. date-accepted:: 2022-03-30
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/477>`_.
.. contents::

Motivation
----------

We already have beautiful Unicode make-up for all reserved symbols. And
they look great!

But no, not for all. The poor ellipsis ``..`` is left without. It wants
to look like this: ``…``.

Proposed Change Specification
-----------------------------

When the ``UnicodeSyntax`` extension is enabled:

1. Make ``…`` accepted wherever ``..`` is accepted.

2. Allow ``…`` in situations like ``[False…]`` where ASCII syntax
   ``[False..]`` would be a parse error. *(This is automatically the
   case with the patch proposed below.)* *(Thanks to Jakob Brünker
   for*\ `pointing this
   out <https://github.com/ghc-proposals/ghc-proposals/pull/477#issuecomment-1000255004>`__\ *.)*

Examples
--------

.. code:: haskell

   {-# language UnicodeSyntax #-}
   {-# language RecordWildCards #-}

   module X where

   import Prelude (Bool (…))

   data Record = Record {value ∷ Bool}

   function ∷ Record → Bool
   function Record {…} = value

   booleans ∷ [Bool]
   booleans = [False…]

Effect and Interactions
-----------------------

Ellipsis gets its beautiful Unicode make-up.

Costs and Drawbacks
-------------------

There are no costs and no drawbacks.

Alternatives
------------

There are no alternatives.

Unresolved Questions
--------------------

There are no unresolved questions.

Implementation Plan
-------------------

Apply this patch:

.. code:: diff

   diff --git a/compiler/GHC/Parser/Lexer.x b/compiler/GHC/Parser/Lexer.x
   index d74d17be8f..4135372d31 100644
   --- a/compiler/GHC/Parser/Lexer.x
   +++ b/compiler/GHC/Parser/Lexer.x
   @@ -1063,6 +1063,7 @@ reservedSymsFM = listToUFM $
           ,("-<<", ITLarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
           ,(">>-", ITRarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
    
   +       , ("…",  ITdotdot,                   UnicodeSyntax,  0 )
           ,("∷",   ITdcolon UnicodeSyntax,     UnicodeSyntax, 0 )
           ,("⇒",   ITdarrow UnicodeSyntax,     UnicodeSyntax, 0 )
           ,("∀",   ITforall UnicodeSyntax,     UnicodeSyntax, 0 )

Endorsements
------------

There are no endorsements.
