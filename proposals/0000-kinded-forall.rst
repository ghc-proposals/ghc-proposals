Kinded Forall
=============

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/774>`_.
.. sectnum::
.. contents::

We propose to update ``forall`` syntax to write less verbose 
and less parentheses kinds for type variables.


Motivation
----------

GHC has very limited capacity to inference kinds of type variables, especially if it not ``Type``.

So we could explicitly add kinds to type variables with ``KindSignatures`` extension.
But for many type variables signatures could become very verbose and less readable.

This proposal suggest to add kind with much simpler, prettier and more readable syntax.

For example next 2 signatures are the same ::

    -- now and it will be remain valid
    foo1 :: forall (n :: Nat). Vec n Int -> ... sometype
	
    -- new syntax
    foo2 :: forall n :: Nat.   Vec n Int -> ... sometype

And next 2 signatures are the same ::

    -- now and it will be remain valid
    bar1 :: forall (n :: Nat) (m :: Nat) (r :: Nat). Vec n Int -> Vec m Int -> Vec r Int -> ... sometype
	
    -- new syntax
    bar2 :: forall n m r :: Nat. Vec n Int -> Vec m Int -> Vec r Int -> ... sometype

And next 3 signatures are the same ::

    -- now and it will be remain valid
    baz1 :: forall (n :: Nat) (m :: Nat) (a :: Type). Vec n a -> Vec m a -> ... sometype
    -- now alternate and valid
    baz2 :: forall (n :: Nat) (m :: Nat) a. Vec n a -> Vec m a -> ... sometype
	
    -- new syntax
    baz3 :: forall n m (a :: Type) :: Nat. Vec n a -> Vec m a -> ... sometype


This proposal syntax do not cover "visible forall" quantifier. 
Because it is unclear how. And because current syntax for "visible forall" 
could be changed by another proposal.

Proposed Change Specification
-----------------------------

Introduce a new extension ``KindedForall``.

With this extension we could easily explicitly add kind near ``forall`` quantifier 
before the "dot" ``.`` using classical ``::`` signature separator:

- for one type variable without parentheses
- for all type variables at ones without repletions and without parentheses
- for several type variables at ones, if they are not already kinded

Syntax
~~~~~~~~~~~~

The formal grammar changes for multi-kinded ``forall``:

.. code:: abnf

    foralls ::= 'forall' tv_bndrs [ '::' ktype ] '.'      ;-- upd
              | 'forall' tv_bndrs '->'
              | ;-- empty

    tv_bndrs ::= tv_bndr1 { tv_bndr1 }
	
    tv_bndr1 ::= tyvar
              | '(' tyvar '::' ktype ')'
	
	
Examples
--------

Several examples with new syntax
::

    foo :: forall a :: Nat. a -> ...
	
    bar :: forall a b c :: Nat. a -> b  -> c  -> ...
	
    baz :: forall m n r (a :: Type) :: Nat. a -> Vec m a  -> Vec n a  -> ...


Effect and Interactions
-----------------------

Extension implies ``KindSignatures`` extension.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of separator change has low difficulty.


Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.


Alternatives
------------

The primary alternative is "status quo".

Next we could extend ``KindSignatures`` extension instead of creating a new extension ``KindedForall``.


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
