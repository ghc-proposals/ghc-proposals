DYSFUNCTIONAL per-instance pragma
=================================

.. author:: Andrzej Rybczak
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/374>`_.
.. contents::

Provide a per-instance ``{-# DYSFUNCTIONAL #-}`` pragma that allows disabling
the coverage condition for a specific instance of a class with functional
dependencies.

Motivation
----------

Before GHC 7.8 the coverage condition for functional dependencies was completely
disabled by the ``UndecidableInstances`` extension. It was considered a
bug/misfeature and fixed (by using the liberal version of the coverage condition
in such case instead of turning it off completely), yet it was quite useful on
occasion and its removal generated several complaints on the GHC issue tracker:

- `Coverage Condition cannot be turned off <https://gitlab.haskell.org/ghc/ghc/-/issues/9227>`_
- `Relax functional dependency coherence check ("liberal coverage condition") <https://gitlab.haskell.org/ghc/ghc/-/issues/8634>`_
- `hackage's type-level-0.2.4 fails to compile <https://gitlab.haskell.org/ghc/ghc/-/issues/9103>`_


It needs to be noted that there still exists a way to disable the coverage
condition on a per-instance basis for all type variables by simply including the
instance head in its context, e.g.

.. code-block:: haskell

  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE FunctionalDependencies #-}
  {-# LANGUAGE UndecidableInstances #-}

  class F a b | a -> b
  instance F Int a => F Int a -- compiles

However, since the definition of the ``F Int a`` instance is circular, it
*sometimes* makes GHC loop during type inference (e.g. when it tries to
determine a type of a local binding without type signature with
``MonomorphismRestriction`` turned on). Sometimes, because usually GHC caches
solved constraints to not repeat the work it already did.

The proposed ``{-# DYSFUNCTIONAL #-}`` pragma does exactly what the circular
trick does:

.. code-block:: haskell

  class F a b | a -> b
  instance {-# DYSFUNCTIONAL #-} F Int a -- compiles

in a reliable way.

Proposed Change Specification
-----------------------------

The ``{-# DYSFUNCTIONAL #-}`` pragma appears in an instance declaration after
the ``instance`` keyword and optional pragma specifying the overlap mode, but
before the instance context:

.. code-block::

  instdecl → instance overlap_mode [ {-# DYSFUNCTIONAL #-} ] [context =>] type [where]

Examples
--------

Basic examples
**************

.. code-block:: haskell

  class F (a :: k1) (b :: k2) | a -> b

  -- a and b are exempt from the coverage condition
  instance {-# DYSFUNCTIONAL #-} F Int (a, b)

  -- interaction with the overlap mode pragma
  instance {-# OVERLAPPABLE #-} {-# DYSFUNCTIONAL #-} F Char (a, Int)

  -- instance involving coverage of a kind variable
  instance {-# DYSFUNCTIONAL #-} F () (Proxy (a :: k))

HasField
********

Consider the `HasField` type class from `GHC.Records`:

.. code-block:: haskell

  class HasField (name :: Symbol) s a | name s -> a where
    getField :: s -> a

The functional dependency `name s -> a` is crucial for effective type inference
when abstracting over the constraint:

.. code-block:: haskell

  -- won't compile without the functional dependency
  f :: (HasField "inner" b c, HasField "outer" a b) => a -> c
  f = getField @"inner" . getField @"outer"

However, here are instances currently impossible to write without the circular
trick or the pragma:

1) Instance that provides a custom type error:

   .. code-block:: haskell

     data Opaque

     instance {-# DYSFUNCTIONAL #-}
       ( TypeError ('Text "Can't access fields of the Opaque data type")
       ) => HasField name Opaque a where
       getField = error "unreachable"

2) Instance that gets a polymorphic field (variation of a problem from `#8634
   <https://gitlab.haskell.org/ghc/ghc/-/issues/8634>`_):

   .. code-block:: haskell

     newtype X = X { x :: forall a. a -> a }

     instance {-# DYSFUNCTIONAL #-} HasField "x" X (a -> a) where
       getField X{x} = x

Extended HasField
*****************

Consider the improved version of the `HasField` type class that also allows to
update the field and change the type of the structure:

.. code-block:: haskell

  class HasField (name :: Symbol) s t a b | name s -> a
                                          , name t -> b
                                          , name s a -> t
                                          , name t b -> s where
    hasField :: s -> (b -> t, a)

The functional dependencies mean:

- `name s -> a` - the field `name` in `s` has a type `a`
- `name t -> b` - the field `name` in `t` has a type `b`
- `name s a -> b` - by replacing the field `name` of a type `a` in `s` we get `t`
- `name t b -> s` - by replacing the field `name` of a type `b` in `t` we get `s`

They are needed, as before, for effective type inference either when getting
nested fields or updating multiple fields:

.. code-block:: haskell

  setField :: forall name s t a b. HasField name s t a b => b -> s -> t
  setField b s = fst (hasField @name s) b

  -- won't compile without the last two functional dependencies
  g :: (HasField "name" s u a1 String, HasField "age" u t a2 Int) => s -> t
  g = setField @"age" 21 . setField @"name" "Tom"

It's now impossible (without the circular trick or the ``{-# DYSFUNCTIONAL #-}``
pragma) to write:

1) Instance that changes a phantom type parameter:

   .. code-block:: haskell

     newtype Phantom ph = Phantom { phantom :: Int }

     instance {-# DYSFUNCTIONAL #-}
       HasField "phantom" (Phantom ph1) (Phantom ph2) Int Int where
       hasField ph = (\n -> ph { phantom = n }, phantom ph)

2) Instance that changes a type parameter that is applied to a non-injective
   type family:

   .. code-block:: haskell

     type family Fam a

     newtype FamRec a = FamRec { fam :: Fam a }

     instance {-# DYSFUNCTIONAL #-}
       ( x ~ Fam a
       , y ~ Fam b
       ) => HasField "fam" (FamRec a) (FamRec b) x y where
       hasField fr = (\x -> fr { fam = x }, fam fr)

Resolution of `#9103 <https://gitlab.haskell.org/ghc/ghc/-/issues/9103>`_
*************************************************************************

.. code-block:: haskell

  type family Failure x :: Constraint
  data PredecessorOfZeroError x
  data D0

  class Succ' xh xl yh yl (yz::Bool) | xh xl -> yh yl yz, yh yl yz -> xh xl
  instance {-# DYSFUNCTIONAL #-}
    ( Failure (PredecessorOfZeroError x)
    ) => Succ' (x,x) (x,x) D0 D0 'True

Effect and Interactions
-----------------------

The change provides a reliable way to lift the coverage condition on a
per-instance basis without relying on the circular trick and the internal
details of GHC for termination of the type checking process.

Moreover, having the pragma (apart from the resolution of `#8634
<https://gitlab.haskell.org/ghc/ghc/-/issues/8634>`_) gives a future possibility
for fixing the following tickets:

- `GHC does not check the functional dependency consistency condition correctly <https://gitlab.haskell.org/ghc/ghc/-/issues/10675>`_

- `"overlapping instances" through FunctionalDependencies <https://gitlab.haskell.org/ghc/ghc/-/issues/9210>`_

- `Instances do not respect functional dependency, yet are accepted <https://gitlab.haskell.org/ghc/ghc/-/issues/18400>`_

by tightening the so-called `bogus consistency check
<https://gitlab.haskell.org/ghc/ghc/-/blob/0abe3ddf85a915ab99ae4f87a85faf6ee5466ad3/compiler/GHC/Tc/Instance/FunDeps.hs#L610>`_. However,
the exact details of doing so are considered out of scope for this proposal, as the
coverage condition check and consistency check are orthogonal.

Costs and Drawbacks
-------------------

The implementation is straightforward and doesn't significantly increase the
maintenance cost of GHC (see `!4356
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4356>`_ for the
proof-of-concept).

While most of the code using `DYSFUNCTIONAL` instances won't lead to any
surprising results, it's possible to construct contrived examples that
demonstrate e.g. loss of confluence. It needs to be noted however that loss of
confluence can be currently obtained even without the circular trick or the
`DYSFUNCTIONAL` pragma, as demonstrated `here
<https://gitlab.haskell.org/ghc/ghc/-/issues/18851#note_310088>`_. This is the
consequence of unresolved `#10675
<https://gitlab.haskell.org/ghc/ghc/-/issues/10675>`_, which as explained in
`Effect and Interactions`_ could be fixed if this proposal is accepted.

Moreover, the same can be said about using `INCOHERENT` or `OVERLAPS`
pragmas. Most of the time their usages is perfectly fine, yet when abused might
lead to extreme confusion.

In any case, existing Haskell tooling can adapt to the proposed change, detect
usage of `DYSFUNCTIONAL` pragma and warn users (or outright reject these
instances). It's also worth noting that it's much harder (if not impossible) for
the tooling to detect the circular trick (which can be freely used as of today)
than the pragma.

Alternatives
------------

1. Do nothing and keep using the almost-always-working circular trick when
   needed.

2. Use a different syntax instead of a pragma, e.g.

   .. code-block:: haskell

     instance forall (%covered a). C Int a

   instead of

   .. code-block:: haskell

      instance {-# DYSFUNCTIONAL #-} C Int a

3. Introduce new language extension and/or syntax for "dysfunctional
   dependencies" and use them on a per-class basis.

Answers:

- The first point is not an enticing perspective because of the "almost" bit.

- I'd argue that the second point will unnecessarily complicate the
  implementation without a substantial gain and is inconsistent with existing
  `INCOHERENT` and `OVERLAPS` pragmas.

- As for the third point, there are cases when marking dependencies
  "dysfunctional" class-wide is too big of a hammer, e.g. when:

  - `DYSFUNCTIONAL` instances are used for custom type errors, or

  - functional dependencies are morally correct, yet this cannot be proved to
    GHC (or doing so would incur a major compile time performance loss).

  This is also similar to the situation with `OverlappingInstances` and
  `IncoherentInstances` language extensions that were deprecated and
  reintroduced as per-instance pragmas.


Unresolved Questions
--------------------

None for now.

Implementation Plan
-------------------

It's already implemented by me (Andrzej Rybczak) (see `!4356
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4356>`_), all that remains
is adding documentation and Template Haskell support.
