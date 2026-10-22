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

Provide a per-instance ``LIBERAL`` and ``DYSFUNCTIONAL`` pragmas for controlling
the coverage condition check for a specific instance of a class with functional
dependencies.

Motivation
----------

Instances of a class with functional dependencies are subject to the coverage
condition check. In case the check is too strict, `UndecidableInstances`
extension can be used to relax it to the liberal coverage condition module-wide.

Doing so has a few drawbacks:

1) `UndecidableInstances` extension relaxes the condition for all instances in
   the module and prevents fine-grained control over each instance (similar to
   the now deprecated `OverlappingInstances` and `IncoherentInstances`
   extensions).

2) There exist decidable instances which need the liberal coverage condition in
   order to be accepted by GHC. An example:

   .. code-block:: haskell

     class Functor f => FunctorWithIndex i f | f -> i
     instance FunctorWithIndex i f => FunctorWithIndex i (Identity f)

Therefore it'd be beneficial to decouple these two and provide a ``LIBERAL``
pragma for relaxing the coverage condition on a per-instance basis instead of
having to rely on `UndecidableInstances` for that.

What's more, before GHC 7.8 the coverage condition for functional dependencies
used to be completely disabled by the `UndecidableInstances` extension. It was
considered a bug/misfeature and fixed (by using the liberal version of the
coverage condition in such case instead of turning it off completely), yet it
was very useful on occasion and its removal generated several complaints on the
GHC issue tracker:

- `Coverage Condition cannot be turned off <https://gitlab.haskell.org/ghc/ghc/-/issues/9227>`_
- `Relax functional dependency coherence check ("liberal coverage condition") <https://gitlab.haskell.org/ghc/ghc/-/issues/8634>`_
- `hackage's type-level-0.2.4 fails to compile <https://gitlab.haskell.org/ghc/ghc/-/issues/9103>`_

Crucially, there still exists a way to disable the coverage condition on a
per-instance basis by simply including the instance head in its context, e.g.

.. code-block:: haskell

  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE FunctionalDependencies #-}
  {-# LANGUAGE UndecidableInstances #-}

  class F a b | a -> b
  instance F Int a => F Int a -- compiles

However, since the definition of the ``F Int a`` instance is circular, its
termination depends on implementation details of the GHC constraint solver. For
example, with GHC 8.* it works fine the majority of time because GHC usually
caches solved constraints to not repeat the work it already did.

The proposed ``DYSFUNCTIONAL`` pragma reliably does what the circular trick
does, i.e. it disables the coverage condition check on a per-instance basis.

.. code-block:: haskell

  class F a b | a -> b
  instance {-# DYSFUNCTIONAL #-} F Int a -- compiles

Why both ``LIBERAL`` and ``DYSFUNCTIONAL``?

1) Having only ``LIBERAL`` would makes it impossible to reliably disable the
   coverage condition, which is immensely useful on occasion.

2) Having ``DYSFUNCTIONAL`` only would lead to a subpar user experience - truly
   ``DYSFUNCTIONAL`` instances are rare and the vast majority of the instances
   work perfectly fine under the liberal coverage condition. Taking the liberal
   version away would make it harder for users to verify that their ``LIBERAL``
   instances are correct.

Proposed Change Specification
-----------------------------

The ``LIBERAL`` or ``DYSFUNCTIONAL`` pragma appears in an instance declaration
after the ``instance`` keyword:

.. code-block::

  instdecl → instance [ {-# LIBERAL #-} | {-# DYSFUNCTIONAL #-} ] overlap_mode [context =>] type [where]

If the class has functional dependencies, the behaviour of the pragmas is as
follows:

1) No pragma - the coverage condition check will be applied to the instance.
2) ``LIBERAL`` - the liberal coverage condition check will be applied to the
   instance.
3) ``DYSFUNCTIONAL`` - no coverage condition check will be applied to the
   instance.

Moreover, ``UndecidableInstances`` no longer relaxes the coverage condition to
its liberal version. Since it's a breaking change, there will be a transition
period for three major GHC releases (9.2 - 9.6), during which instances relying
on the liberal coverage condition, yet not annotated with the ``LIBERAL`` (or
``DYSFUNCTIONAL``) pragma will generate a warning. After that GHC will stop
accepting them.

Examples
--------

Most of the examples will focus on the ``DYSFUNCTIONAL`` pragma as there is
nothing new about the behaviour of the ``LIBERAL`` one.

Basic examples
**************

.. code-block:: haskell

  class F (a :: k1) (b :: k2) | a -> b

  -- | No pragma needed.
  instance F Integer Integer

  -- | Relies on the liberal coverage condition.
  instance {-# LIBERAL #-} F Bool a => F Bool [a]

  -- | No coverage condition, a and b are not covered.
  instance {-# DYSFUNCTIONAL #-} F Int (a, b)

  -- | Interaction with the overlap mode pragma (a is not covered).
  instance {-# DYSFUNCTIONAL #-} {-# OVERLAPPABLE #-} F Char (a, Int)

  -- | No coverage condition, a and k are not covered.
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

However, here are instances currently impossible to write without the
``DYSFUNCTIONAL`` pragma:

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

   Note: it's not possible to solve this with type families because of `#9262
   <https://gitlab.haskell.org/ghc/ghc/-/issues/9269>`_, an open problem.

Extended HasField
*****************

Consider an improved version of the `HasField` type class that also allows to
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

It's now impossible (without the ``DYSFUNCTIONAL`` pragma) to write:

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

The change provides:

1) A way to decouple the liberal coverage condition from `UndecidableInstances`.

2) A reliable way to lift the coverage condition on a per-instance basis without
   relying on the circular trick and the internal details of GHC for termination
   of the type checking process.

Moreover, these pragmas (apart from the resolution of `#8634
<https://gitlab.haskell.org/ghc/ghc/-/issues/8634>`_) give a future possibility
for fixing the following tickets:

- `GHC does not check the functional dependency consistency condition correctly <https://gitlab.haskell.org/ghc/ghc/-/issues/10675>`_

- `"overlapping instances" through FunctionalDependencies <https://gitlab.haskell.org/ghc/ghc/-/issues/9210>`_

- `Instances do not respect functional dependency, yet are accepted <https://gitlab.haskell.org/ghc/ghc/-/issues/18400>`_

by tightening the so-called `bogus consistency check
<https://gitlab.haskell.org/ghc/ghc/-/blob/0abe3ddf85a915ab99ae4f87a85faf6ee5466ad3/compiler/GHC/Tc/Instance/FunDeps.hs#L610>`_. However,
the exact details of doing so are considered out of scope for this proposal, as
the coverage condition check and consistency check are orthogonal.

Costs and Drawbacks
-------------------

The implementation is straightforward and doesn't significantly increase the
maintenance cost of GHC (see `!4356
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4356>`_ for the
proof-of-concept).

While most of the code using ``DYSFUNCTIONAL`` instances won't lead to any
surprising results, it's possible to construct contrived examples that
demonstrate e.g. `loss of confluence
<https://gitlab.haskell.org/ghc/ghc/-/issues/18851>`_ (however, as demonstrated
`here <https://gitlab.haskell.org/ghc/ghc/-/issues/18851#note_318471>`_ and
`here <https://gitlab.haskell.org/ghc/ghc/-/issues/18851#note_319766>`_ it seems
to be a more general issue in the GHC constraint solver, not a direct effect of
lifting the coverage condition).

Moreover, a similar argument can be made about ``OVERLAPS`` pragmas. Most of the
time their usages is perfectly fine, yet when abused might lead to extreme
confusion. Even the ``INCOHERENT`` pragma has its rare, legitimate uses.

In any case, existing Haskell tooling can adapt to the proposed change, detect
usage of ``DYSFUNCTIONAL`` pragma and warn users (or outright reject these
instances). It's also worth noting that it's much harder (if not impossible) for
the tooling to detect the circular trick (which can be freely used as of today)
than the pragma.

Alternatives
------------

Note: as the main drive for this proposal is the ``DYSFUNCTIONAL`` pragma and
the ``LIBERAL`` one is just a straightforward extension of the idea, the
following considers only alternatives to the former.

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

The core of the proposal is already implemented by me (Andrzej Rybczak) (see
`!4356 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4356>`_), all that
remains is adding documentation and Template Haskell support.
