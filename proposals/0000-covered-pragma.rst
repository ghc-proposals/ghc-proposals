COVERED per-instance pragma
===========================

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

Provide a per-instance ``{-# COVERED #-}`` pragma that allows disabling the
coverage condition for specified type variables.

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

  class Covered a | -> a
  instance Covered a => Covered a -- compiles

What is more, ``Covered`` can now be used in other instances to lift the
coverage condition for a specific type variable:

.. code-block:: haskell

  class F a b | a -> b
  instance Covered a => F Int a -- compiles

However, since the definition of the ``Covered`` instance is circular, it
*sometimes* makes GHC loop during type inference (e.g. when it tries to
determine a type of a local binding without type signature with
``MonomorphismRestriction`` turned on). Sometimes, because usually GHC caches
solved constraints to not repeat the work it already did.

The proposed ``{-# COVERED #-}`` pragma does exactly what the ``Covered`` class
does:

.. code-block:: haskell

  class F a b | a -> b
  instance {-# COVERED a #-} F Int a -- compiles

in a reliable way.

Proposed Change Specification
-----------------------------

The ``{-# COVERED #-}`` pragma appears in an instance definition after the ``instance``
keyword and optional pragma specifying the overlap mode, but before the instance
context.

After ``{-# COVERED`` there is a white-space separated list of type variables
from the instance head and its context that will be exempt from the coverage
condition. If the list of type variables is empty, the pragma is a no-op.

Examples
--------

Basic examples:

.. code-block:: haskell

  class F (a :: k1) (b :: k2) | a -> b

  -- a and b are exempt from the coverage condition
  instance {-# COVERED a b #-} F Int (a, b)

  -- interaction with the overlap mode pragma
  instance {-# OVERLAPPABLE #-} {-# COVERED a #-} F Char (a, Int)

  -- instance involving coverage of a kind variable
  instance {-# COVERED a k #-} F () (Proxy (a :: k))

  -- no-op pragma
  instance {-# COVERED #-} F (a, Int) a

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

     instance {-# COVERED a #-}
       ( TypeError ('Text "Can't access fields of the Opaque data type")
       ) => HasField name Opaque a where
       getField = error "unreachable"

2) Instance that gets a polymorphic field (variation of a problem from `#8634
   <https://gitlab.haskell.org/ghc/ghc/-/issues/8634>`_):

   .. code-block:: haskell

     newtype X = X { x :: forall a. a -> a }

     instance {-# COVERED a #-} HasField "x" X (a -> a) where
       getField X{x} = x

Now, consider the improved version of the `HasField` type class that also allows
to update the field and change the type of the structure:

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

It's now impossible (without the circular trick or the ``{-# COVERED #-}``
pragma) to write:

1) Instance that changes a phantom type parameter:

   .. code-block:: haskell

     newtype Phantom ph = Phantom { phantom :: Int }

     instance {-# COVERED ph1 ph2 #-}
       HasField "phantom" (Phantom ph1) (Phantom ph2) Int Int where
       hasField ph = (\n -> ph { phantom = n }, phantom ph)

2) Instance that changes a type parameter that is applied to a non-injective
   type family:

   .. code-block:: haskell

     type family Fam a

     newtype FamRec a = FamRec { fam :: Fam a }

     instance {-# COVERED a b #-}
       ( x ~ Fam a
       , y ~ Fam b
       ) => HasField "fam" (FamRec a) (FamRec b) x y where
       hasField fr = (\x -> fr { fam = x }, fam fr)

These are issues that I personally encountered. For completeness there's also the one from `#9103 <https://gitlab.haskell.org/ghc/ghc/-/issues/9103>`_:

.. code-block:: haskell

  type family Failure x :: Constraint
  data PredecessorOfZeroError x
  data D0

  class Succ' xh xl yh yl (yz::Bool) | xh xl -> yh yl yz, yh yl yz -> xh xl
  instance {-# COVERED x #-}
    ( Failure (PredecessorOfZeroError x)
    ) => Succ' (x,x) (x,x) D0 D0 'True

Effect and Interactions
-----------------------

The change merely provides a reliable way to lift the coverage condition on a
per-instance, per-variable basis without relying on internal details of GHC for
termination of the type checking process.

Costs and Drawbacks
-------------------

The implementation is straightforward and doesn't significantly increase the
maintenance cost of GHC (see `!4356
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4356>`_ for the
proof-of-concept).

People not using the pragma are not affected.

It can be argued that this change takes us further from the word `Functional` in
`FunctionalDependencies`, but:

- The desired behavior can already be obtained without the pragma (just not 100%
  reliably).
- Functional dependencies as implemented in GHC aren't really functional in the
  mathematical sense as the following code is accepted:

  .. code-block:: haskell

    class C a b | a -> b

    instance C Int Int
    instance {-# OVERLAPPABLE #-} a ~ Char => C Int a

  They simply guide type inference.

Alternatives
------------

Do nothing and keep using the almost-working `Covered` type class when needed.

Unresolved Questions
--------------------

None for now.

Implementation Plan
-------------------

It's already implemented (see `!4356
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4356>`_), all that remains
is adding documentation and Template Haskell support.
