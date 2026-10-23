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

We change the inner constraint separator from ``=>`` to ``|-`` in ``PatternSynonyms`` 
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

They suggest to see, for example that ``C1 a => C2 a b => (C3 a, C3 b)`` is the same 
as ``C1 a => (C2 a b, C3 a, C3 b)``
and is the same as ``(C1 a, C2 a b, C3 a, C3 b)``


This syntax is very universal and curried and it looks promising.

Unfortunately, the syntax on ``PatternSynonyms`` has a conflict syntax 
with curried context/constraint,
so if we change inner constraint separator from ``=>`` to ``|-``,  
then we will avoid any future conflicts ::

    pattern P :: CReq |- CProv => t1 -> t2 -> ... -> tN -> t

Second, unfortunately right now we have strange situation, when
pattern signatures without requested signature has different syntax if provided signature
has constraint or not: 
::

    pattern P1 :: t1 -> t2 -> ... -> tN -> t

    pattern P2 :: () => (C1 a, ...) => t1 -> t2 -> ... -> tN -> t

Third, current separator is not flexible enough. 
Currently it is not supported visible forall in the Req/Prov parts of the pattern signature at all.
Function arguments in the Req part is also do not supported. 
Currently all function arguments are in the Prov part.
In general, the Req/Prov split shouldn't be solely about constraints.


Proposed Change Specification
-----------------------------

Main idea is: to have much simpler and more flexible syntax with ``|-`` inner separator 
::

    pattern P :: SignProv

    pattern P :: SignReq |- SignProv

We should be able to have any of the following both in the Reqested and the Provided parts of a patsyn:

- Invisible forall ``forall (a::k).`` or ``forall {k}.``
- Visible forall forall ``(a::k) ->``
- Constraints ``(C a, D b) =>``
- Function arguments ``T ->``

Second, we make operator ``|-`` as keyword in signatures.

Third, we allow to use old requested separator ``=>``, but with Warning 
when ``CurriedContext`` (or similar) extension in not active.


Syntax
~~~~~~~~~~~~

The formal grammar changes for assigned a pattern type signatures:

.. code:: abnf

    topdecl ::= 'type' simpletype '=' type                              (simple type declaration)
        | 'pattern' pats '::' [signreq '|-'] signprov ['where' pdecls]    (pattern declaration)   --; upd
        | ……

    signreq  ::= [foralls '.'] [context '=>'] type       --; new
        | [foralls  '.'] context                         --; new
        | foralls                                        --; new

    signprov ::= [foralls '.'] [context '=>'] type       --; upd

Examples
--------

An example with requested constraint
::

    data T a where
        MkT :: (Show b) => a -> b -> T a

    f1 :: (Num a, Eq a) => T a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: (Num a, Eq a) |- (Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

    f2 :: (Eq a, Num a) => T a -> String
    f2 (ExNumPat x) = show x

An example without requested constraint
::

    data S a where
        S1 :: Bool -> S Bool

    pattern P1 :: Bool -> Maybe Bool
    pattern P1 b = Just b

    --pattern P2 :: () => (b ~ Bool) => Bool -> S b
    pattern P2 :: (b ~ Bool) => Bool -> S b
    pattern P2 b = S1 b

    f :: Maybe a -> String
    f (P1 x) = "no no no"     -- Type-incorrect

    g :: S a -> String
    g (P2 b) = "yes yes yes"  -- Fine


Effect and Interactions
-----------------------

This change ``=>`` to ``|-`` keyword for inner constraint separator for pattern signatures
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

We could choose differen separator instead of proposed ``|-`` and unicode ``⊢`` keywords.
Why? **Pro**: it looks mathematical. **Cons**: it already used as operator in several packages

- Use a *word* for separator, for example ``then``. **Pro**: it is already a keyword for terms, looks mathematical. 
  **Cons**: it would be a second word as keyword in types

- Use different *operator* for separator, for example:

  #. Use ``|--`` and same unicode ``⊢`` for separator. **Pro**: it looks mathematical. 
     **Cons**: 3 letter insted of 2.

  #. Use ``||-`` and unicode ``⊩`` (Forces, U+22A9) for separator. **Pro**: it looks mathematical. 
     **Cons**: 3 letter insted of 2.

  #. Use ``=>=>`` for separator. **Pro**: Similar to already used one. 
     **Cons**: 4 letter insted of 2. Unclear which unicode could be used. Do not looks mathematical.


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
