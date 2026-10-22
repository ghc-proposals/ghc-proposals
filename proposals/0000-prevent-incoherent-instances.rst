Prevent Incoherent Instances
============================

.. author:: Daniel Smith
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/279>`_.
.. sectnum::
.. contents::

Currently it is possible to cause incoherent instance behavior without ``-Worphans`` giving any warning and without even
enabling ``Overlaps``/``Overlappable`` let alone ``Incoherent``. You only need `FlexibleInstances`.

This proposal introduces a flexible and general mechanism that prevents this incoherence.

Motivation
----------
As seen `here <https://pastebin.com/wyVMdRkc>`_ and `here <https://pastebin.com/MQ4wd17Y>`_, it is currently possible to break
coherence without the use of the ``Incoherent`` pragma, without ``-Worphan`` warnings, and even without any usage of
``Overlaps``/``Overlappable``. The only extension needed to cause these problems to occur is `FlexibleInstances`.

As seen above this incoherence can break various invariants without warning, and cause unintuitive and antimodular behavior,
and thus we should prevent it.

Proposed Change Specification
-----------------------------
We define a new pragma, currently named ``Refine``, that can be attached to ``class`` and ``instance`` declarations to specify
how [overlapping-]instances should be declared.

``Refine`` takes in a list of zero or more type variables from the class/instance head.

Instances of this class, or instances that overlap with this instance, must match on a top level concrete type constructor
for all of the above type variables, and they must have ownership of at least one of those type constructors. Any type
variables not mentioned in the ``Refine`` clause have no restrictions and can be left bare if desired.

For completeness a ``noinstance`` declaration will be added that does not actually define an instance, but exists solely to
attach a ``Refine`` pragma too.

By default classes and noinstances will have a ``Refine`` that specifies all type variables in the class/instance head,
instances however will default to an empty ``Refine`` which disallows any overlapping instances.

Instances that violate the above must be closely tracked to prevent incoherence, as soon as any two instances have
unintentional overlap not declared with ``Refine``, compilation should fail. Unless the ``Incoherent`` pragma is explicitly used.
``Overlaps``/``Overlappable`` would no longer be needed and can be phased out.

Instances that violate the above should ideally not be defined in the first place, and should warn with ``-Worphans`` to avoid
having to constantly watch out for unintentional overlap. However they should not be completely prevented to allow for
situations like ``data-default`` and ``data-default-class`` where all the rules would be followed except for the rule that one
of the concrete type constructors must be owned.

The above prevents any unintentional overlap, leaving only intentional overlap from defining ``Refine`` on an ``instance``.

Currently the way ``Overlaps``/``Overlappable`` is handled we can cause incoherence even with only intentional overlapping.
This is because the type checker is willing to commit too early to an instance which is later overlapped. Since we now
must always specify in advance that an instance can be overlapped, we know exactly when to not commit too early to an
instance. In those cases where an instance can be overlapped, constraints on top level values must not commit to that instance
and must instead keep a FlexibleContexts-style expanded constraint that will be able to receive any overlapped instance.

Examples
--------

Refine example indicating which instances would be considered orphans:

::

 module Foo where

 class Foo a where

 instance Foo [a]

 [no]instance Foo (a -> b)
    {-# Refine b #-}

 [no]instance Foo (a, b)
     {-# Refine a b #-}

 class Baz a b
     {-# Refine b #-}

 class Qux a b
     {-# Refine a b #-}

::

 module Bar where

 import Foo

 data Bar

 instance Foo Bar

 instance Foo Int -- orphan

 instance Foo a -- orphan

 instance Foo [Bar] -- orphan

 instance Foo (a -> Bar)

 instance Foo (Bar -> a) -- orphan

 instance Foo (Int, Bar)

 instance Foo (Bar, Int)

 instance Foo (a, Bar) -- orphan

 instance Foo (Bar, a) -- orphan

 instance Baz a Bar

 instance Baz Bar a -- orphan

 instance Qux Int Bar

 instance Qux Bar Int

 instance Qux a Bar -- orphan

 instance Qux Bar a -- orphan

Example of adjusted type checking:

::

 instance Show a => Show [a]
     {-# Refine a #-}

 -- Rejected to prevent incoherence
 showInList :: Show a => a -> String
 showInList x = show [x]

 -- Accepted and prevents incoherence
 showInList :: Show [a] => a -> String
 showInList x = show [x]

Effect and Interactions
-----------------------
This proposal would prevent incoherence whilst keeping a large amount of flexibility and expressiveness.

Costs and Drawbacks
-------------------
Some classes would require ``Refine`` to be specified immediately to continue being usable. For example ``IsLabel`` and ``HasField``
would want to specify that the ``Symbol`` type variable is not in the ``Refine`` list to avoid requiring owning a given Symbol.

It is not as flexible as the status quo of allowing incoherence.

Alternatives
------------
Doing nothing and accepting that instances are not guaranteed to be coherent once you enable FlexibleInstances.

Unresolved Questions
--------------------
Is there a better pragma name than Refine?

Is the above proposal truly coherent in all cases?

Is there any reasonable and desirable existing code which cannot be easily adjusted to fit this proposal?
