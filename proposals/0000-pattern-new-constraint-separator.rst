New requested separator for Pattern Synonyms
===============================================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/767>`_.
.. sectnum::
.. contents::

We change the inner requested separator from ``=>`` to ``|-`` in pattern signatures of ``PatternSynonyms`` 
to avoid any conflicts with curried constraint syntax ::

    pattern P :: CReq |- CProv => t1 -> t2 -> ... -> tN -> t

And we also allow to write more general Requested and Provided signatures: ::

    pattern P :: SignReq |- SignProv

Motivation
----------

Several ideas and proposals and feature requests appear to allow to write 
Curried Constrains and Context in different places: 

- Proposal: `Curried Class Contexts <https://github.com/ghc-proposals/ghc-proposals/pull/666>`__
- Feature Request: `Curried contexts in prefix GADT constructors <https://gitlab.haskell.org/ghc/ghc/-/work_items/27424>`__
- Feature Request: `Refactor ConDeclGADT to allow nested foralls and contexts <https://gitlab.haskell.org/ghc/ghc/-/work_items/27425>`__
- Feature Request: `Support TypeAbstractions in PatternSynonyms <https://gitlab.haskell.org/ghc/ghc/-/work_items/27642>`__

They suggest to see, for example that several constraints ``C1 a => C2 a b => (C3 a, C3 b)`` are the same 
as ``C1 a => (C2 a b, C3 a, C3 b)``
and are the same as ``(C1 a, C2 a b, C3 a, C3 b)``


This syntax is very universal and curried and it looks promising.

Unfortunately, the syntax on ``PatternSynonyms`` has a conflict syntax 
with curried context/constraint,
so if we change inner constraint separator in pattern signatures from ``=>`` to ``|-``,  
then we will avoid any future conflicts ::

    pattern P :: CReq |- CProv => t1 -> t2 -> ... -> tN -> t

Second, unfortunately right now we have strange situation, when
pattern signatures without requested signature has different syntax if provided signature
has constraint or not: 
::

    pattern P1 :: t1 -> t2 -> ... -> tN -> t

    pattern P2 :: () => (C1 a, ...) => t1 -> t2 -> ... -> tN -> t

And vice-versa if we have requested constraint, but without provided constraint, 
then we also have special syntax:
::

    pattern P3 :: (C1 a, ...) => () => t1 -> t2 -> ... -> tN -> t

Third, current requested separator is not flexible enough. 
Currently it is not supported visible forall in the Req/Prov parts of the pattern signature at all.
Even function arguments are not part of this proposal, 
current syntax blocks to extend it for use them in the Req part. 
In general, the Req/Prov split shouldn't be solely about constraints.


Proposed Change Specification
-----------------------------

Main idea is: to have much simpler and more flexible syntax with ``|-`` requested separator in pattern signatures
::

    -- signature WITHOUT Requested parameters
    pattern P :: SignProv

    -- signature WITH Requested parameters
    pattern P :: SignReq |- SignProv

We should be able to have any of the following both in the Reqested and the Provided parts of a patsyn:

- Invisible forall ``forall (a::k).`` or ``forall {k}.``
- Visible forall forall ``(a::k) ->``
- Constraints ``(C a, D b) =>``

Note: Function arguments ``T ->`` in Requested part is not a part of this proposal
(mainly because it is unclear what such FunArg in Req could represent), 
even this could be extended lately if it would be necessary.

Second, we make operator ``|-`` as keyword in signatures. At least as Constraint name.

Third, we allow to use old requested separator ``=>`` as Deprecated one with a Warning 
till ``CurriedContext`` (or similar) extension in not developed and is not active.


Syntax
~~~~~~~~~~~~

The formal grammar changes for assigned a pattern-synonym signatures:

.. code:: abnf

    pattern_synonym_sig ::= 'pattern' patslist '::' [ signreq ] sigtype     --; upd

    signreq  ::= [ foralls ] context '|-'                                   --; upd
               | foralls '|-'                                               --; upd


Examples
--------

An example with requested constraint in pattern
::

    data T a where
        MkT :: (Show b) => a -> b -> T a

    f1 :: (Num a, Eq a) => T a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: (Num a, Eq a) |- (Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

    f2 :: (Eq a, Num a) => T a -> String
    f2 (ExNumPat x) = show x

Pattern signature with just Provided Constraint
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An example without requested Constraint, but with provided Constraint
::

    data S a where
        S1 :: Bool -> S Bool

    pattern P1 :: Bool -> Maybe Bool
    pattern P1 b = Just b

    -- old separator
    -- pattern P2 :: () => (b ~ Bool) => Bool -> S b

    pattern P2 :: (b ~ Bool) => Bool -> S b
    pattern P2 b = S1 b

    f :: Maybe a -> String
    f (P1 x) = "no no no"     -- Type-incorrect

    g :: S a -> String
    g (P2 b) = "yes yes yes"  -- Fine

Pattern signature with just Requested Constraint
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An example without provided Constraint, but with requested Constraint
::

    fromUniTuple :: Eq a => (a, a) -> Maybe a
    fromUniTuple (x, y)
        | x == y    = Just x
        | otherwise = Nothing

    -- old separator
    -- pattern Double ::  (Eq a) => () => a -> (a, a)

    pattern Double ::  (Eq a) |- a -> (a, a)
    pattern Double a <- (fromUniTuple -> Just a)
        where Double a = (a, a)

Pattern signature with Curried Constraint
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Theorethically if Curried Constraint become valid in Haskell, 
then next will be possible to write:
::

    data T2 a where
        MkT :: Show b => Read b => a -> b -> T2 a

    f1 :: Num a => Eq a => T2 a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: Num a => Eq a |- Show b => Read b => b -> T2 a
    pattern ExNumPat x = MkT 42 x

Pattern signature with Requested just Forall
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An example without requested Constraint, but with requested ``forall``
::

    data T3 a where
        MkT3 :: Bool -> b -> (b -> Int) -> a -> T a

    pattern P ::  forall b. |- forall a. b -> (b -> Int) -> a -> T3 a
    pattern P x y v <- MkT3 True x y (v :: a)

Sure, we could use Visible forall ``forall b ->`` instead of invisible ``forall`` 
in both data declaration and requested part.


Effect and Interactions
-----------------------

This change ``=>`` to ``|-`` keyword for inner constraint/requested separator for pattern signatures
on ``PatternSynonyms`` extension was made to avoid conflicts with Curried Context syntax
and to allow more liberal requested signatures.

UnicodeSyntax
~~~~~~~~~~~~~~~~

We wish to preserve ``⊢`` (Right Tack, U+22A2) symbol to represent ``|-`` requested separator in pattern signatures.
::

    pattern ExNumPat :: (Num a, Eq a) ⊢ (Show b) => b -> T a


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of separator change has low difficulty.


Backward Compatibility
----------------------

Some libs and code became Deprecated a bit and lately will cause a breakage.

Hopefully it affects very complex syntax, which is not widely uses.

But this change will allow to write a code in a more universal way - with curried constraint.


Alternatives
------------

The primary alternative is "status quo".

Alternative keywords
~~~~~~~~~~~~~~~~~~~~~~~~

We could choose differen requested separator in pattern signatures
instead of proposed ``|-`` and unicode ``⊢`` keywords.

Why ``|-`` is choosen? **Pro**: it looks mathematical. 
**Cons**: it is already used as operator in several packages:
*uniplate*, *numhask*, *sbv*, *tidal*, *tidal-core*, *one-liner*.

- Use a *word* as separator, for example ``then``. 
  **Pro**: it is already a keyword for terms, looks mathematical. 
  **Cons**: it would be a second word as keyword in types

- Use different *operator* as separator, for example:

  #. Use ``|--`` and same unicode ``⊢`` for separator. **Pro**: it looks mathematical. 
     **Cons**: 3 letter instead of 2.

  #. Use ``||-`` and unicode ``⊩`` (Forces, U+22A9) for separator. 
     **Pro**: it looks mathematical. 
     **Cons**: 3 letter instead of 2. It is already used as operator in several packages:
     *tidal*, *tidal-core*.

  #. Use ``=>=>`` for separator. 
     **Pro**: Similar to already used one. 
     **Cons**: 4 letter instead of 2. 
     Unclear which unicode could be used. Do not looks mathematical.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Acknowledgments
---------------

- `int-index <https://github.com/int-index>`_, for proposing generalized syntax in the pull discussion.


Endorsements
-------------
