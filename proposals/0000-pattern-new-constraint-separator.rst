New constraint separator for Pattern Synonyms
=============================================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/767>`_.
.. sectnum::
.. contents::

We change the inner constraint separator from ``=>`` to ``then`` in ``PatternSynonyms`` 
to avoid any conflicts with curried constraint syntax ::

    pattern P :: CReq then CProv => t1 -> t2 -> ... -> tN -> t

And we also allow to write more general Request signatures: ::

    pattern P :: SignReq then SignProv

Motivation
----------

Several ideas and proposals and feature requests appear to allow to write 
Curried Constrains and Context in different places: 

- Proposal: `Curried Class Contexts <https://github.com/ghc-proposals/ghc-proposals/pull/666>`__
- Feature Request: `Curried contexts in prefix GADT constructors <https://gitlab.haskell.org/ghc/ghc/-/work_items/27424>`__
- Feature Request: `Refactor ConDeclGADT to allow nested foralls and contexts <https://gitlab.haskell.org/ghc/ghc/-/work_items/27425>`__

They suggest to see, for example that ``C1 a => C2 a b => (C0 a, C0 b)`` is the same 
as ``(C1 a, C2 a b, C0 a, C0 b)``

This syntax is very universal and curried and it looks promising.

Unfortunately, the syntax on ``PatternSynonyms`` has a conflict syntax 
with curried context/constraint,
so it is required to change ``=>`` to ``then`` as inner constraint separator 
to avoid any future conflicts ::

    pattern P :: CReq then CProv => t1 -> t2 -> ... -> tN -> t

    pattern P :: SignReq then SignProv

New separator helps to have more liberal and diversed Requested params, for example here:

- Feature Request: `Support TypeAbstractions in PatternSynonyms <https://gitlab.haskell.org/ghc/ghc/-/work_items/27642>`__

Proposed Change Specification
-----------------------------

Main idea is:  to make the syntax with ``=>`` inner separator 
deprecated and lately removed in signatures like this: ::

    pattern P :: CReq => CProv => t1 -> t2 -> ... -> tN -> t

And change inner constraint separator from ``=>`` to ``then`` here ::

    pattern P :: CReq then CProv => t1 -> t2 -> ... -> tN -> t

This proposal does not address the following challenges:

Currently it is not supported visible forall in the Req/Prov parts of the pattern signature at all.
Function arguments in the Req part is also do not supported. Currently all function arguments are in the Prov part.

In general, the Req/Prov split shouldn't be solely about constraints.
We should be able to have any of the following both in the Req and the Prov parts of a patsyn:

- Invisible forall ``forall (a::k).`` or ``forall {k}.``
- Visible forall forall ``(a::k) ->``
- Constraints ``(C a, D b) =>``
- Function arguments ``T ->``

So, we also allow to write more general Request ans Prov signatures ::

    pattern P :: SignReq then SignProv


Syntax
~~~~~~~~~~~~

The formal grammar changes for assigned a pattern type signatures:

.. code:: abnf

    topdecl ::= 'type' simpletype '=' type                           (simple type declaration)
        | 'pattern' pats '::' type ['where' pdecls]                      (pattern declaration)   --; upd
        | 'pattern' pats '::' signreq 'then' signprov ['where' pdecls]   (pattern declaration)   --; new
        | ……

    signreq  ::= ()                                              --; new
        | { {forquantiferes} {context '=>'} } [type]             --; new

    signprov ::= { {forquantiferes} {context '=>'} } type        --; new

Examples
--------

An example with requested constraint
::

    data T a where
        MkT :: (Show b) => a -> b -> T a

    f1 :: (Num a, Eq a) => T a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: (Num a, Eq a) then (Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

    f2 :: (Eq a, Num a) => T a -> String
    f2 (ExNumPat x) = show x

An example without requested constraint
::

    data S a where
        S1 :: Bool -> S Bool

    pattern P1 :: Bool -> Maybe Bool
    pattern P1 b = Just b

    pattern P2 :: () then (b ~ Bool) => Bool -> S b
    pattern P2 b = S1 b

    f :: Maybe a -> String
    f (P1 x) = "no no no"     -- Type-incorrect

    g :: S a -> String
    g (P2 b) = "yes yes yes"  -- Fine


Effect and Interactions
-----------------------

This change ``=>`` to ``then`` keyword for inner constraint separator for pattern signatures
on ``PatternSynonyms`` extension was made to avoid conflicts with Curried Context syntax.


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


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Endorsements
-------------


Acknowledgments
---------------

- `int-index <https://github.com/int-index>`_, for proposing generalized syntax in the pull discussion.
