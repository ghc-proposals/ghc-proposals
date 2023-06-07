Class Instances of Injective Synonyms 
====================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
.. sectnum::
.. contents::

GHC Type Families has lack of class instances.

This proposal gives reason why changing this is important.


Background
----------

"Main reason" why class instances of Type Families are forbidden is written in the GHC documentation:

`6.4.9.6. Type families and instance declarations <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_families.html#type-families-and-instance-declarations>`_

*Type synonym families may not appear (at all) in an instance head*

*The reason for the latter restriction is that there is no way to check for instance matching. Consider* ::

    type family F a
    type instance F Bool = Int

    class C a

    instance C Int
    instance C (F a)

*Now a constraint* ``(C (F Bool))`` *would match both instances.* 

*The situation is especially bad because the type instance for* ``F Bool`` *might be in another module, 
or even in a module that is not yet written.*

From our point of view, this is just a technical difficulties, but not a reason. 

Main reason - no useful cases was found for such instances.

Finally useful cases are found!


Motivation
----------

Sometimes we wish to have specific behavior without ``newtype``. 

Let we wish to write ::

    instance Semigroup Int where
        (<>) = ????

    instance Monoid Int where
        mempty = ????

We can't write instances of ``Int`` for ``Semigroup`` and ``Monoid``.

The reason of this is not because ``Int`` is not a semigroup or monoid, but because it has plenty of them!

Sure, we could use newtypes for that. Or we could use type families!


Proposed Change Specification
-----------------------------

Syntax
~~~~~~

Non-Injective Type Family types are not strong enough, so this proposal limits usage of to Injective Type Family only.

We write two quotes before name `''F` both in instances and constraint to distinguish usage real type from synonym type ::

    data SP = Sum | Prod

    type family Distr (a :: SP) b = r | r -> b where
        Distr a b = b

    instance Semigroup (''Distr Sum Int) where
        (<>) = (+)

    instance Semigroup (''Distr Prod Int) where
        (<>) = (*)

    (<+>) :: Semigroup (''Distr Sum a)  => Distr Sum a -> Distr Sum a -> Distr Sum a
    (<+>) = (<>)

    (<*>) :: Semigroup (''Distr Prod a) => Distr Prod a -> Distr Prod a -> Distr Prod a
    (<*>) = (<>)

    class (Semigroup (''Distr Sum a), Semigroup (''Distr Prod a)) => Distributive a

    instance Distributive Int


Semantics
~~~~~~~~~

Since 2 quotes ``''F`` usually mean *"type unlifted to data"*, we expected no conflict in usage.

And we extend meaning of ``''F`` in this special case to *"type unlifted to data and lifted as type"*, like ``'''F``.
But since 3 quotes looks annoying, we cut one.

For ``class C a`` and ``type family F a = r | r -> a`` 

* constraint ``C (F a) =>`` means declaration of ``instance C r`` (or ``deriving C r`` ), 
* constraint ``C (''F a) =>`` means declaration of ``instance C (''F a)``.


Effect and Interactions
-----------------------

Any Effect and Interactions are unknown.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.

Backward Compatibility
----------------------

This proposal is fully backward compatible.

Alternatives
------------

An alternative is status-quo, to remain as is.

Implementation Plan
-------------------

It is unclear.
