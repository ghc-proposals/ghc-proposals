Deriving Via
============

.. author:: Icelandjack
.. date-accepted:: 2018-05-22
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15178
.. implemented:: 8.6
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/120>`_.
.. contents::

We propose ``DerivingVia``, a new
`deriving strategy <https://downloads.haskell.org/~ghc/8.4.1/docs/html/users_guide/glasgow_exts.html#extension-DerivingStrategies>`_
that significantly increases the expressive power of Haskell's ``deriving`` construct.

One example of a use case for this is the following. Given the following
``Sum`` newtype (taken from ``Data.Monoid``): ::

    newtype Sum a = Sum a
    instance Num a => Monoid (Sum a) where
      Sum x `mappend` Sum y = Sum (x + y)
      mempty = Sum 0

Then we can leverage ``Sum`` to derive an instance for a *different* data type
like so: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

One can interpret this as "derive a ``Monoid`` instance for ``T`` by *reusing*
the existing ``Monoid`` instance for ``Sum Int``".

The paper `Deriving Via; or, How to Turn Hand-Written Instances into an Anti-Pattern
<https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf>`_ describes this feature
in great detail, with many accompanying examples, and so is a primary reference source
for this proposal.

An implementation is already available in our GHC fork
`here <https://github.com/RyanGlScott/ghc/tree/deriving-via-8.5>`_.

Motivation
----------

Broadly speaking, the purpose of ``DerivingVia`` is to facilitate the ability
to capture programming patterns that often arise in type class instances and
reuse them. As one example, the ``Monoid`` instances for ``IO`` and ``ST``
follow the exact same pattern: ::

    instance Monoid a => Monoid (IO a) where
      mempty  = pure mempty
      mappend = liftA2 mappend

    instance Monoid a => Monoid (ST s a) where
      mempty  = pure mempty
      mappend = liftA2 mappend

Namely, these ``Monoid`` instances are defined by lifting ``mempty`` and
``mappend`` through an ``Applicative`` instance. Instead of having to tediously
copy-paste these method implementations in every instance that needs them, we
can first explicitly codify this pattern by means of a newtype: ::

    newtype App f a = App (f a)
    instance (Applicative f, Monoid a) => Monoid (App f a) where
      mempty = App (pure mempty)
      mappend (App f) (App g) = App (liftA2 mappend f g)

Now, we can use ``DerivingVia`` to deploy this pattern where it's needed.
Assuming we had access to the original definitions for ``IO`` and ``ST``,
we could have instead defined their ``Monoid`` instances like so: ::

    data IO a = ...
      deriving Monoid via (App IO a)
    data ST s a = ...
      deriving Monoid via (App (ST s) a)

Here, we are reusing the ``Monoid`` instance for ``App IO a`` to derive the
``Monoid`` instance for ``IO``. (And similarly for ``ST``.) This works because
``App IO a`` and ``IO a`` are *representationally equal* types. That is to say,
one can safely coerce values of type ``App IO a`` to type ``IO a``, as they are
the same modulo newtypes (in this example, the newtype being ``App``). As a
result, GHC can figure out how to take any instance for ``App IO a`` and
safely repurpose it to be an instance for ``IO a``.

There are many other use cases for this language extension.
See Section 4 of
`the paper <https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf>`_ for
a broad survey of some of the more interesting applications, including:

* A generalization of ``DefaultSignatures``, which can allow for the
  coexistence of *multiple* defaults.
* A way to make it easier to adapt to superclass changes (such as the
  ``Applicative``–``Monad`` Proposal).
* A technique to reuse instances from types that are *isomorphic*, not just
  representationally equal.
* A trick which can eliminate the need for orphan instances in certain
  situations.

Aside from the paper itself, here is a list of other sources about this idea:

* The `original blog post <https://gist.github.com/Icelandjack/d258b88a0e0b3be2c0b3711fdd833045>`_ proposing this idea, and the `accompanying Reddit discussion <https://www.reddit.com/r/haskell/comments/6ksr76/rfc_part_1_deriving_instances_of/>`_.

* A `Reddit post <https://www.reddit.com/r/haskell/comments/8aa81q/deriving_via_or_how_to_turn_handwritten_instances/>`_ discussing the paper.

Proposed Change Specification
-----------------------------
We propose a new language extension, ``DerivingVia``. ``DerivingVia`` will imply
``DerivingStrategies``, as ``DerivingVia`` requires using deriving strategy
syntax.

Syntax changes
--------------
Currently, there are three deriving strategies in GHC: ``stock``, ``newtype``,
and ``anyclass``. For example, one can use the ``stock`` strategy in a
``deriving`` clause like so: ::

    data Foo = MkFoo
      deriving stock Eq

Or in a standalone ``deriving`` declaration: ::

    deriving stock instance Eq Foo

We propose a fourth deriving strategy, which requires enabling the
``DerivingVia`` extension to use. This deriving strategy is indicated by using
the ``via`` keyword. Unlike other deriving strategies, ``via`` requires
specifying a type (referred to as the ``via`` type) in addition to a derived
class. For instance, here is how one would use ``via`` in a ``deriving``
clause: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

Or in a standalone ``deriving`` declaration: ::

    deriving via (Sum Int) instance Monoid T

As is the case with ``stock`` and ``anyclass``, the ``via`` identifier is
only treated specially in the context of ``deriving`` syntax. One will still
be able to use ``via`` as a variable name in other contexts, even if the
``DerivingVia`` extension is enabled.

Note that in ``deriving`` clauses, we put the ``via`` keyword *after* the
derived class instead of before it. We do so primarly because we find it
makes the distinction between the derived class and the ``via`` type more
obvious. If we had put the ``via`` type *before* the derived class, as
in the following two examples: ::

    deriving via X (Y Z)
    deriving via (X Y) Z

Then the distinction is harder to see from a glance, and we would
have two type expressions directly adjacent to each other, which looks
like a type application but is not.

Code generation
---------------
The process by which ``DerivingVia`` generates instances is a strict
generalization of ``GeneralizedNewtypeDeriving``. For instance, the
following ``Age`` newtype, which has an underlying representation type
of ``Int``: ::

    newtype Age = MkAge Int
      deriving newtype Enum

Would generate the following instance: ::

    instance Enum Age where
      toEnum   = coerce @(Int -> Int)   @(Int -> Age)   toEnum
      fromEnum = coerce @(Int -> Int)   @(Age -> Int)   fromEnum
      enumFrom = coerce @(Int -> [Int]) @(Age -> [Age]) enumFrom
      ...

Here, each method of ``Enum`` is derived by taking the implementation of
the method in the ``Enum Int`` instance and coercing all occurrences of
``Int`` to ``Age`` using the ``coerce`` function from
`Data.Coerce <http://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Coerce.html>`_.

The context of the derived instance is determined by taking the derived class,
applying it to the representation type to obtain a context, and simplifying
that context as much as possible. In the example above, this would entail
simplifying the context ``Enum Int``. Since there is an ``Enum Int`` instance,
this simplifies to just ``()``. In a more complicated example, like: ::

    newtype Z a = MkZ (Identity a) deriving Enum

We would have a derived context of ``Enum a`` leftover after simplifying
``Enum (Identity a)``.

This algorithm need only be tweaked slightly to describe how ``DerivingVia``
generates code. In ``GeneralizedNewtypeDeriving``:

1. We start with an instance for the representation type.
2. GHC coerces it to an instance for the newtype.
3. The derived context is obtained from simplyfing the class applied to the
   representation type.

In ``DerivingVia``, however:

1. We start with an instance for a ``via`` type.
2. GHC coerces it to an instance for the data type.
3. The derived context is obtained from simplifying the class applied to the
   ``via`` type.

For instance, this earlier example: ::

    newtype T = MkT Int
      deriving Monoid via (Sum Int)

Would generate the following instance: ::

    instance Monoid T where
      mempty  = coerce @(Sum Int) @T mempty
      mappend = coerce @(Sum Int -> Sum Int -> Sum Int)
                       @(T       -> T       -> T)
                       mappend

To make it evident that ``DerivingVia`` is in fact a generalization of
``GeneralizedNewtypeDeriving``, note that this: ::

    newtype Age = MkAge Int
      deriving newtype Enum

Is wholly equivalent to this: ::

    newtype Age = MkAge Int
      deriving Enum via Int

Another feature that ``GeneralizedNewtypeDeriving`` supports, which is the
ability to derive instances of classes with associated type families, is
similarly generalized in `DerivingVia`. Given the following example: ::

    class C a where
      type T a

    instance C Int where
      type T Int = Bool

    instance C (Sum a) where
      type T (Sum a) = Sum (T a)

Then a ``newtype``-derived instance of ``C`` would look like this: ::

    newtype Age1 = MkAge1 Int
      deriving newtype C
    -- This generates:
    instance C Age1 where
      type T Age1 = T Int

And a ``via``-derived instance of ``C`` would like this: ::

    newtype Age2 = MkAge2 Int
      deriving C via (Sum Int)
    -- This generates:
    instance C Age2 where
      type T Age2 = T (Sum Int)

Note that while ``GeneralizedNewtypeDeriving`` has a strict requirement that
the data type for which we're deriving an instance must be a newtype, there
is no such requirement for ``DerivingVia``. For example, this is a perfectly
valid use of ``DerivingVia``: ::

    newtype BoundedEnum a = BoundedEnum a
    instance (Bounded a, Enum a) => Arbitrary (BoundedEnum a) where ...

    data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
      deriving (Enum, Bounded)
      deriving Arbitrary via (BoundedEnum Weekday)

``DerivingVia`` only imposes the requirement that the generated code
typechecks. (See the "Typechecking generated code" section for more on this.)

Renaming source syntax
----------------------
``DerivingVia`` introduces a new place where types can go (the ``via`` type),
and as a result, introduces a new place where type variables can be bound. To
understand how this works, consider the following example that uses a
``deriving`` clause: ::

    data Foo a = ...
      deriving (Baz a b c) via (Bar a b)

* ``a`` is bound by ``Foo`` itself in the declaration ``data Foo a``.
  ``a`` scopes over both the ``via`` type, ``Bar a b``,
  as well as the derived class, ``Baz a b c``.
* ``b`` is bound by the ``via`` type ``Bar a b``. Note that ``b`` is bound
  here but ``a`` is not, as it was bound earlier by the ``data`` declaration.
  ``b`` also scopes over the derived class ``Baz a b c``.
* ``c`` is bound by the derived class ``Baz a b c``, as it was not bound
  earlier.

For ``StandaloneDeriving``, the scoping works similarly.
In the following example: ::

    deriving via (V a) instance C a (D a b)

* ``a`` is bound by the ``via`` type ``V a``, and scopes over the instance
  type ``C a (D a b)``.
* ``b`` is bound the instance type ``C a (D a b)``, as it was not bound
  earlier.

Note that ``DerivingVia`` requires that all type variables bound by a ``via``
type must be used in each derived class (for ``deriving`` clauses) or
in the instance type (for ``StandaloneDeriving``). If a ``via`` type binds
a type variable and does not use it accordingly, then it is *floating*,
and rejected with an error. To see why this is the case, consider the
following example: ::

  data Quux
    deriving Eq via (Const a Quux)

This would generate the following instance: ::

  instance Eq Quux where
    (--) = coerce @(Quux         -> Quux         -> Bool)
                  @(Const a Quux -> Const a Quux -> Bool)
                  (--)
    ...

This instance is ill-formed, as the ``a`` in ``Const a Quux`` is unbound! One
could conceivably "fix" this by explicitly quantifying the ``a`` at the top
of the instance: ::

  instance forall a. Eq Quux where ...

But this would not be much better, as now the ``a`` is ambiguous. We avoid
these complications by making floating type variables in ``via`` types an
explicit error.

Typechecking source syntax
--------------------------
In this example: ::

  newtype Age = MkAge Int
    deriving Eq

GHC requires that the kind of the argument to the class must unify with the
kind of the data type. (In this example, both of these kinds are ``Type``, so
it passes this check.) This is done to ensure that the generated code makes
sense. For instance, one could not derive ``Functor`` for ``Age``, as the
kind of the argument to ``Functor`` is ``Type -> Type``, which does not
unify with ``Age``'s kind (``Type``).

``DerivingVia`` extends this check ever-so-slightly. In this example: ::

  newtype Age = MkAge Int
    deriving Eq via (Sum Int)

Not only must the kind of the argument to ``Eq`` unify with the kind of
``Age``, it must also be the case that those two kinds unify with the kind
of the ``via`` type, ``Sum Int``. (``Sum Int :: Type``, so it passes that
check.)

We must also have that ``Age`` and ``Sum Int`` have the same runtime
representation. This is checked after the code for the instance itself has
been generated (see the "Typechecking generated code" section).

More formally, if the data declaration we have is: ::

  data D1 d1 ... dm
    deriving (C c1 ... cn) via (V v1 ... vp)

Then the following must hold:

1. The type ``C c1 ... cn`` must be of kind ``(k1 -> ... -> kr -> *) -> Constraint``
   for some kinds ``k1``, ..., ``kr``.
2. The kind ``V v1 ... vp``, the kind ``D d1 ... di``, and the kind of the
   argument to ``C c1 ... cn`` must all unify, where *i* is an index (less than or
   equal to *m*) determined by dropping arguments from the end of ``D1 d1 ... dm``
   according to the kind of ``C c1 ... cn``. The use of *i* here instead of *m*
   is what allows us to support higher-kinded scenarios, such as: ::

      newtype I a = MkI a
        deriving Functor via Identity

   Wherein the generated instance, ``instance Functor I``, we have dropped the ``a``
   from ``I a``.
   For more details on how this aspect works, refer to Section 3.1.2
   of `the paper <https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf>`_.

Typechecking generated code
---------------------------
Once ``DerivingVia`` generates instances, they are fed back into GHC's
typechecker as one final sanity check. In order for the generated code to
typecheck, the original data type and the ``via`` type must have the same
runtime representations. The use of ``coerce`` is what guarantees this.

For instance, if a user tried to derive ``via`` a type that was not
representationally equal to the original data type, as in this example: ::

    newtype UhOh = UhOh Char
      deriving Ord via Int

Then GHC will give an error message stating as such: ::

    • Couldn't match representation of type ‘Char’ with that of ‘Int’
        arising from the coercion of the method ‘compare’
          from type ‘Int -> Int -> Ordering’
            to type ‘UhOh -> UhOh -> Ordering’
    • When deriving the instance for (Ord UhOh)

Fortunately, GHC has invested considerable effort into making error messages
involving ``coerce`` easy to understand, so ``DerivingVia`` benefits from this
as well.

Effect and Interactions
-----------------------
Other ``deriving``-related language extensions, such as
``GeneralizedNewtypeDeriving`` and ``DeriveAnyClass``, are selected
automatically in certain cases, even without the use of explicit ``newtype``
or ``anyclass`` deriving strategy keywords. This is not the case with
``DerivingVia``, however. One *must* use the ``via`` keyword to make use of
``DerivingVia``. That is to say, GHC will never attempt to guess a ``via``
type, making this extension strictly opt-in.

As a result, ``DerivingVia`` has the nice property that it is orthogonal to
other language features. No existing code will break because of
``DerivingVia``, as programmers must consciously choose to make use of it.

It is worth noting that like all other forms of ``deriving``, a standalone
``DerivingVia`` declaration only ever targets the *last* argument to a
class. In other words, in the following code: ::

    class Triple a b c where
      triple :: (a, b, c)
    instance Triple () () () where
      triple = ((), (), ())

    newtype A = A ()
    newtype B = B ()
    newtype C = C ()

    deriving via () instance Triple A B C

The generated instance would ``coerce`` through the ``Triple A B ()`` instance,
instead of, say, the ``Triple () () ()`` instance. This is because the standalone
instance above would be the same as if a user had written: ::

    newtype C = C ()
      deriving (Triple A B) via ()

This makes it consistent, if not a bit limited, since there are other ways one could
conceivably implement this ``Triple A B C`` instance. As noted in Section 6.2 of
`the paper <https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf>`_,
we do not attempt to generalize ``DerivingVia``'s interaction with multi-parameter
type classes any further than this, since it would likely require devising a new
syntax to say which combination of parameters to a class one would prefer to
``coerce`` through. (For instance, in the ``Triple A B C`` instance above, there are
seven different combinations to choose from!)

Costs and Drawbacks
-------------------
There are currently no known drawbacks to this feature. Implementing this
feature was a straightforward extension of the machinery already in place
to support ``deriving``, so it will not impose significant maintenance costs.
(Moreover, the maintainer of this part of the codebase,
`@RyanGlScott <https://github.com/RyanGlScott>`_, is also the person who wrote
much of the code for ``DerivingVia``.)

Alternatives
------------
The closest existing alternatives to this feature are various preprocessor hacks
that people have cooked up to "copy-and-paste" code patterns in various places,
such as in Conal Elliott's
`applicative-numbers <http://hackage.haskell.org/package/applicative-numbers>`_
package. But this is far from a satisfying solution to the problem.

The syntax for ``StandaloneDeriving`` we have chosen is slightly different from
the syntax for ``deriving`` clauses in the sense that in ``StandaloneDeriving``: ::

    deriving via B instance Foo A

The ``via`` part comes before the derived class, whereas in a ``deriving`` clause: ::

    data A
      deriving Foo via B

Thr ``via`` part comes *after* the derived class. One could conceivably put the
``via`` at the end in ``StandaloneDeriving`` to regain some consistency, like in: ::

    deriving instance Foo A via B

We have chosen not to, since:

1. ``StandaloneDeriving`` syntax is always different enough from the syntax
   for ``deriving`` clauses (the ``instance`` keyword, the presence of an
   explicit instance context, etc.) that this additional slight deviation is
   not so bad.
2. It's significantly more difficult to implement. GHC will want to parse
   ``Foo A via B`` as a single type, which means that ``via`` will have to be
   made a special identifier in the ``inst_type`` production rule in GHC's
   grammar. However, we do not want ``via`` to be a special identifier in other
   type-related production rules, or else we would lose the ability to
   write ``f :: via -> via; f = id``!

   Currently, GHC's parser is not sophisticated enough to make identifiers
   "locally special" as in the above example, so it would take a nontrivial
   amount of engineering to allow this. This is not to say that we should let
   the difficulty of implementation dictate the design of the feature, but
   it is a cost worth considering.

Unresolved questions
--------------------

Implementation Plan
-------------------
There is feature is fully implemented in our GHC fork
`here <https://github.com/RyanGlScott/ghc/tree/deriving-via-8.5>`_. I
(`@RyanGlScott <https://github.com/RyanGlScott>`_) volunteer to work to get
this fork into GHC proper.
