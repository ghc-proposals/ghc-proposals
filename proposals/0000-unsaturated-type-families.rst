Unsaturated Type Families
=========================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/242>`_.
.. sectnum::
.. contents::

This is a proposal to allow partial application of type families, as
described in the paper `Higher-Order Type-level Programming in Haskell <https://www.microsoft.com/en-us/research/publication/higher-order-type-level-programming-in-haskell>`_.


Motivation
----------

GHC provides rich tools for type-level programming, but the type
language is quite different from the term language, which makes
using these tools a challenging task. In particular, type functions
are required to be always fully applied (saturated), restricting users
to first-order programming.

What this means is that while it's possible to write a type-level
``Map`` function ::

   type family Map (f :: a -> b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

it is not possible to pass another type function as an argument,
because that would require partial (unsaturated) application of the
function argument.
It is, however, possible to pass a type constructor, such as ``Maybe``.
``Map Maybe '[Int, Bool]`` evaluates to ``'[Maybe Int, Maybe Bool]``.
This is because unlike type families, type constructors can be
unsaturated.

The aim of this proposal is to bring the type language closer to the
term language by enabling partial application of type functions,
thereby making the type language higher-order. Thanks to this
feature, type-level programs are easier to understand and
maintain, as common abstractions like ``Map`` can be defined in
a standard library.

Recap: saturation restriction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It might not be obvious, but there is in fact a very good reason why
type families must be saturated today. GHC's type inference engine
relies on syntactic equalities of type constructor applications.
For example, GHC can solve equalities of the shape ``f a ~ g b``
by decomposing them to ``f ~ g`` and ``a ~ b`` then producing the
substitutions. This yields a unique solution only if ``f`` and ``g``
are type constructors (so ``f a`` and ``g b`` are syntactically equal).
To make sure that ``f`` and ``g`` are not type families, GHC disallows
unsaturated type families.

For more background and examples see
`Section 2 of the paper <https://www.microsoft.com/en-us/research/publication/higher-order-type-level-programming-in-haskell>`_.

Proposed Change Specification
-----------------------------

The following sections describe a new GHC extension, which can be
enabled with the pragma ``{-# LANGUAGE UnsaturatedTypeFamilies #-}``.
The pragma implies ``TypeFamilies``.

Matchabilities
~~~~~~~~~~~~~~

The proposed change is to distinguish between type constructors and
type functions in the *kind system*. That is, a type family such as identity ::

  type family Id a where
    Id a = a

will have kind ``k ~> k``, instead of ``k -> k``, which is
reserved for type constructors such as ``Maybe``.
``~>`` is called an *unmatchable* arrow, while ``->`` is *matchable*

Then equalities of the shape ``f a ~ g b`` are only solved by
decomposition when ``f :: k -> j`` and ``g :: k -> j``.

With this, it is now possible to define a version of ``Map`` that
abstracts over type families ::

   type family Map (f :: a ~> b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

The kind of ``Map`` itself becomes ``(a ~> b) ~> [a] ~> [b]``.

Matchability polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~

This new version of ``Map`` can now only be applied to type families,
but not type constructors, whose kind is ``a -> b``. To rectify this,
we make matchability a first-class type, and abstraction over
matchabilities. Thus ::
  data Matchability = Matchable | Unmatchable

  type (->) = (->{'Matchable})
  type (~>) = (->{'Unmatchable})


Finally, ``Map`` can be defined to be *matchability polymorphic* ::

   type family Map (f :: a ->{m} b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

Accepting both ``Map Id`` and ``Map Maybe``.

Inference
~~~~~~~~~

To retain backwards compatibility, matchabilities are **not**
generalised over, instead, they are defaulted to ``'Matchable``. That
is, any matchability-polymorphic definition must be manually defined
so. ::
   foo :: f a -> f a                                                    -- (f :: * -> *)
   bar :: HList xs -> HList (Map f xs)                                  -- (f :: * -> *)
   baz :: forall m xs (f :: * ->{m} *).  HList xs -> HList (Map f xs)   -- (f :: * ->{m} *)

Note that in ``baz``, ``f`` is explicitly marked to be polymorphic,
whereas in ``bar``, it defaults to matchable.

More generally, all unsolved matchability metavariables are defaulted
to ``'Matchable``. This helps inference by disambiguating type
variables at the call sites. Consider the following function ::

   qux :: forall m (f :: * ->{m} *) a. f a -> f a                       -- (f :: * ->{m} *)

In ``qux (Just False)``, we need to solve ``f a ~ Maybe Bool``. Since
``f`` is polymorphic, we are stuck. Here, ``f`` gets defaulted to matchable,
and type inference can proceed by setting ``f := Maybe`` and ``a := Bool``.

What if the user wishes to use a type family instead? They can use
visible type applications: ``qux @_ @Id (Just False)``. Now, ``f`` is
set to ``Id``, and ``a`` is inferred to be ``Maybe Bool``. (Note the
wildcard ``@_`` standing in for the matchability; it can be inferred
from the kind of ``Id``).

Arity of type families
~~~~~~~~~~~~~~~~~~~~~~

Consider the following two type families ::

  type family Foo (a :: *) :: *
  type family Bar :: * ~> *

Both have the same kind, namely ``* ~> *``, but there is a crucial
difference: the arity of ``Foo`` is 1, whereas ``Bar`` is nullary.
That is, it is possible to define ::
  type family Foo (a :: *) :: * where
    Foo Int  = Bool
    Foo Char = Int

but ``Bar`` can only be defined without arguments and a type family on its RHS ::

  type family Bar :: * ~> * where
    Bar = Foo

This is because type families can only be reduced when they are fully
saturated.

Effects and interactions
------------------------

Term-level functions
~~~~~~~~~~~~~~~~~~~~

Since ``TypeInType``, the types of terms and the kinds of types share
the same arrow ``->``. Consequently, as a result of this proposal, term-level
functions inevitably need to have a matchability. They are assigned
the unmatchable arrow ``~>`` (though this should not be visible in
surface Haskell, the arrow is rendered as ``->`` in terms).

Inferred arguments
~~~~~~~~~~~~~~~~~~

When supplying type arguments to matchability-polymorphic functions such as ::

   qux :: forall m (f :: * ->{m} *) a. f a -> f a

the user needs to provide either a concrete matchability or a wildcard before
supplying the instantiation for ``f``, as in ``qux @_ @Id``. This is tiresome,
because ``m`` can *always* be inferred from the kind of ``f``, so it would be
preferable to write ``qux @Id`` instead.
The `explicit specificity <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_
proposal would make this possible.

Linear Haskell
~~~~~~~~~~~~~~

Under the Linear Haskell proposal, the arrow type is decorated with a
different kind of information: multiplicity. Happily, multiplicities
are only used in *types*, whereas matchabilities only appear in
*kinds*. As a result, these features are completely orthogonal.


Costs and Drawbacks
-------------------

There is no serious maintenance cost of this feature, as the change to
the constraint solver is modest (taking into account matchability
information when decomposing type applications).

A potential drawback is that users will now need to be aware of the
arrow dichotomy. However, this only concerns advanced users, and
the feature is backwards-compatible. Notably, before this feature,
the kind of a type family never shows up in source Haskell, and is only
printed by GHCi with the ``:kind`` command.

Alternatives
------------

There are a number of alternative decisions regarding the specific
details of the proposal. Instead of matchability polymorphism,
a subsumption relationship could be considered between the two arrows.
This approach has been fully formalised by Richard Eisenberg in his `thesis <http://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf>`_,
and it would simply allow ``Map :: (a ~> b) ~> [a] ~> [b]`` to be applied to both
constructors and functions. The main drawback of that approach is that
inference would suffer compared to the scheme outlined above.
Matchability polymorphism also fits more cleanly into the existing
constraint solver mechanism.

Secondly, type inference with the matchability defaulting scheme is
incomplete. Take following program ::
  nested :: a b ~ c Id => b Bool
  nested = False

initially, the matchabilities of ``a``, ``b`` and ``c`` are all
instantiated with unification variables, and there are no further
steps. So they are all defaulted to be matchable, at which point
the equality can be decomposed, and we learn that
``(b :: * -> *) ~ (Id :: * ~> *)``. This way, ``nested`` cannot be called,
because no such ``b`` exists.

Instead, we could do something more clever by defaulting
matchabilities in dependency order, but it's not obvious if this
additional complexity would be worth it.

Unresolved Questions
--------------------
Syntax. Using ``~>`` for the unmatchable arrow would steal a very
commonly used operator.

Implementation Plan
-------------------
I have implemented a
`prototype <https://gitlab.haskell.org/kcsongor/ghc/tree/unsaturated_type_families>`_
of this feature.
