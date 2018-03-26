Unboxed coercions
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/116>`_.
.. sectnum::
.. contents::

Expose unboxed coercions to the source language.


Motivation
------------
This type is useful for playing certain tricks with traversals: ::

 data Mag a b r where
   Ap :: Mag a b (x -> y) -> Mag a b x -> Mag a b y
   Pure :: r -> Mag a b r
   One :: a -> Mag a b b

GHC infers ::

  type role Mag representational nominal nominal

In these applications, we don't actually need the ``One`` constructor to
carry *equality* evidence. All we really need is *coercibility* evidence.
This gives a rather friendlier type role: ::

 data MagC a b r where
   Ap :: MagC a b (x -> y) -> MagC a b x -> MagC a b y
   Pure :: r -> MagC a b r
   One :: Coercible b r => a -> MagC a b r
 
 type role MagC representational representational representational

Unfortunately, there's a performance cost: the ``One`` constructor gets
larger, and we have to follow a pointer to the coercion at runtime. Since
there are likely to be many ``One`` constructors, that's unfortunate.

Proposed Change Specification
-----------------------------
I have thought of three basic approaches to addressing this problem. I'll
list them all here, and let the discussion guide which becomes primary.
Some of these can be combined.

An unboxed coercion type
========================
This approach seems the simplest.

Add a primitive type::

 Coercion# :: k -> k -> TYPE ('TupleRep '[])

and a primitive operation::

 coercion# :: a ~R# b => Coercion# a b

Having something of type ``Coercion# a b`` in scope would provide
the primitive constraint ``a ~R# b``. So we could write::

   One :: Coercion# b r -> a -> MagC a b r

   one :: Coercible b r => a -> MagC a b r
   one a = One coercion# a

   extr :: MagC a b r -> Maybe (Coercion b r)
   extr (One _ _) = Just Coercion
   extr _ = Nothing

Banged constraints
==================

Perhaps we could add syntax for strict constraints on data constructors: ::

   One :: !(Coercible b r) => a -> Mag a b r

I'm guessing this won't play nicely with tuple constraints, so we'd probably
need to do things like::

   Foo :: !(C1 x) => !(C2 y) => ...

A magical constraint
====================

This approach could go well with the ``Coercion#`` one. We could expose
``~R#`` directly. Since its name is highly non-standard, it would probably
need to be renamed to ``Coercible#``. Like more general banged constraints,
I suspect tupling will be an issue, and we might have to work around that
specially. if we do this, it might also pay to expose ``~N#``.

Effect and Interactions
-----------------------
We'd probably want to write ``Coercion`` differently: ::

 data Coercion a b where
   Coercion :: Coercion# a b -> Coercion a b     -- unboxed coercion option
   Coercion :: !(Coercible a b) => Coercion a b  -- unboxed constraint option
   Coercion :: Coercible# a b => Coercion a b    -- magical constraint option

Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------
List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.


Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
