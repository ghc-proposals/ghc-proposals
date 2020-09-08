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

This is a proposal to allow partial application of type families. The idea is
described in the paper
`Higher-Order Type-level Programming in Haskell <https://www.microsoft.com/en-us/research/publication/higher-order-type-level-programming-in-haskell>`_,
and is presented here with a few tweaks and refinements.

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

it is not possible to pass another type function as the first argument
to ``Map`` because that would require partial (unsaturated) application of the
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

would have kind ``k -> @U k`` instead of ``k -> k`` -- the kind that GHC would
infer today. The ``U`` means "unmatchable". Type constructors such as ``Maybe``
or ``[]`` would instead have kind ``Type -> @M Type``, meaning they are
matchable. Matchability is a property of the arrow that appears in the kind.
The saturation restriction for ``Map`` from earlier can now essentially be
summed up by stating that its first argument must have kind ``a -> @M b``.

Then equalities of the shape ``f a ~ g b`` are only solved by
decomposition when ``f :: k -> @M j`` and ``g :: k -> @M j``.

With this distinction, it is now possible to define a version of ``Map`` that
abstracts over type families ::

   type family Map (f :: a -> @U b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

The kind of ``Map`` itself becomes ``(a -> @U b) -> @U [a] -> @U [b]``.

Matchability is a first-class type, and is defined as:::

  data Matchability = Matchable | Unmatchable

We could even define ``M`` and ``U`` as synonyms of ``'Matchable`` and
``'Unmatchable`` respectively.::

  type M = 'Matchable
  type U = 'Unmatchable

We could export these types from a new ``GHC.Matchability`` module,
or perhaps ``GHC.Types``.

The full kind of the ``(->)`` constructor becomes:::

  (->) :: forall (m :: Matchability)
                 {q :: RuntimeRep} {r :: RuntimeRep}.
          TYPE q -> TYPE r -> Type

The matchability part of the arrow can be instantiated using visible type
application in types, a recent addition to GHC.

The ``a -> @m b`` syntax is thus syntactic sugar for ``(->) @m a b``.
This proposed syntax hints at the optionality of manually specifying the
matchability, a point discussed later in this proposal.

Matchability polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~

The version of ``Map`` above can only be applied to type families
(which have kind ``-> @U``) but not type constructors (which have kind
``-> @M``). Since matchabilities are a first-class type, they can be
quantified over, thus enabling polymorphism in the matchability of arrows.

This way, ``Map`` can be defined to be *matchability polymorphic* (in
its first argument) ::

   type family Map (f :: (->) @m a b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

As a matter of convenience for writing matchability-polymorphic arrow
kinds, we allow matchability variables (i.e type variables of kind
``Matchability``) in ``@`` annotations, in addition to ``U`` and ``M``.
Thus, ``Map`` can alternatively be written as ::

   type family Map (f :: a -> @m b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

These two variants of ``Map`` support taking both ``Id`` (a type family) and
``Maybe`` (a type constructor) as the first argument. The complete kind of
``Map`` is ``forall (m :: Matchability) a b. (a -> @m b) -> @U [a] -> @U [b]``.

In fact, as evidenced by the change suggested in the *Syntax* section, we
can even allow matchabilities to be applications of type families, e.g:::

  type family Flip (m :: Matchability) :: Matchability where
    Alternate 'Matchable = 'Unmatchable
    Alternate 'Unmatchable = 'Matchable

  type ArrFlip (m :: Matchability) a b = a -> @(Flip m) b

  -- F only accepts 'f's with a matchable arrow kind.
  type family F (f :: ArrFlip 'Unmatchable i j) (a :: i) :: j where
    F f a = f a

We could optionally add reserved operators for ``-> @U`` and ``-> @M``, e.g
``a $-> b`` and ``a |-> b`` respectively. These would have to be baked in
because GHC would have to treat them like ``->`` and GHC's parser handles this
operator in a special way
(see `here <https://gitlab.haskell.org/ghc/ghc/issues/10056#note_157509>`_).

.. _Inference:

Meaning of an annotation-free ``->``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Even though this proposal introduces a way to annotate arrows, we do not want
to force every single arrow (written or inferred) to be annotated with a
matchability. Therefore, let's consider the meaning that we want to give to an
annotation-free ``->``, what matchability should be given by default? We could
just default to matchable in all arrow kinds and unmatchable elsewhere. However,
some contexts seem to be particularly adapted to a matchability-polymorphic
interpretation, while others require a more rigid interpretation, constraining
pieces of code to only accept type constructors with matchable arrow kinds.
A few examples are given below.::

  -- Here, we want:
  --   Map1 :: forall (a :: Type) (m :: Matchability) (b :: Type).
  --           (a -> @m b) -> @U [a] -> @U [b]
  type family Map1 (f :: a -> b) (xs :: [a]) where
    Map1 f       '[] =             '[]
    Map1 f (x ': xs) = f x ': Map1 f xs

  -- Same here:
  --   Map2 :: forall (a :: Type) (m :: Matchability) (b :: Type).
  --           (a -> @m b) -> @U [a] -> @U [b]
  type family Map2 f xs where
    Map2 f       '[] =             '[]
    Map2 f (x ': xs) = f x ': Map2 f xs

  -- Below, we want the type application '@(Type -> Type)'
  -- to be interpreted as instantiating 'k' to 'Type -> @m Type',
  -- so that both 'p1' and 'p2' typecheck, allowing us to construct proxies to
  -- types with matchable and unmatchable arrow kinds.
  data P k (a :: k) = P
  type family F a
  p1 = P @(Type -> Type) @Maybe
  p2 = P @(Type -> Type) @F

  -- Here, we only want to allow proper type constructors:
  --   Functor1 :: (Type -> @M Type) -> @M Constraint
  class Functor1 (f :: Type -> Type) where
    fmap1 :: (a -> b) -> f a -> f b

  -- Same here:
  --   Functor2 :: (Type -> @M Type) -> @M Constraint
  class Functor2 f where
    fmap2 :: (a -> b) -> f a -> f b

This suggests that the meaning of explicitly written or inferred arrow kinds
should depend on the context from which they originated. We however are unlikely
to interpret any annotation-free ``->`` as an unmatchable arrow. There is a
balance to be found between the flexibility granted by a
matchability-polymorphic interpretation and the inference problems that the said
flexibility would cause. In the case of arrow kinds for type family arguments,
the matchability-polymorphism seems desirable and might even end up accepting
all the programs accepted today, and then some, without inducing any breakage.
Likewise, it seems desirable to accept the definitions for ``p1`` *and* ``p2``.
We therefore propose the following plan:

1. Figure out all the different contexts where we would not want to default to
   a matchable arrow, quite likely extending ``UserTypeCtxt`` along the way or
   defining a dedicated type.

2. Implement the desired behaviour in the compiler, for each context, by
   allocating matchability variables or interpreting as matchable as
   appropriate, for both explicitly written and inferred kind arrows.
   In the matchability variable allocation case, the constraint solver would
   then be responsible for unifying the variable with a specific matchability
   or figuring out that we can just keep the matchability polymorphism. Being
   able to discriminate on the context from which an annotation-free ``->``
   comes will require that we associate to each of them some information about
   the context in which they were written, earlier in the pipeline, or the
   context in which they were inferred, during typechecking.

3. Document and summarize the final behaviour in a specification that would be
   included in the GHC manual.

Without claiming completeness, we mention some of these special cases:

Term-level arrows
#################

Term-level arrows are always unmatchable. ::

  id :: a -> a

always means ``id :: a -> @U a``.

It is an error to write ::

  id :: a -> @M a

Kind-arrows in type signatures
##############################

Whenever an arrow kind arises from the type signature of a term, they are
defaulted to matchable ::

  foo :: forall (m :: Type -> Type) a. m a
  foo = undefined

Here ``m :: Type -> @M Type``. The rule is that matchability variables are never
generalised in terms: if it's a "term-level" arrow, it's assigned unmatchable,
if it's a "type-level" arrow, it's assigned matchable. This happens regardless
of whether the arrow is spelled out, viz: ::
  bar :: m a
  bar = undefined

This behaviour is the most conservative, as we don't trigger ambiguity errors,
and still allow decomposition of equality constraints. Users can override this behaviour
by specifying an explicit matchability annotation::

  bar :: forall (m :: Type -> @U Type) a. m a

Note that this type signature is now ambiguous.

Kind-arrows in classes
######################

When an arrow kind arises from a type class parameter, it's assumed to be
matchable ::

  class Functor f where
    fmap :: (a -> b) -> f a -> f b

Similarly in instances ::

  instance Functor f
  instance Show (g a)

both ``f`` and ``g`` are inferred to have matchable kinds.

Arrow-type in type class instances
##################################

When defining an instance, the arrow type can turn up directly
in the instance head, for example::

  instance Monad ((->) r)
  instance Category (->)
  instance Semigroup (a -> b)

To retain compability, all of these arrows are assumed to mean the term-level
arrow, in other words unmatchable. This default can be overridden ::

  instance Foo ((->) @M)

Kind-arrows in type family patterns
###################################

In the pattern::

  type family UnApp a where
    UnApp (f x) = x

``f`` is inferred to have a matchable kind. Indeed, it must have a matchable
kind, and declaring otherwise is an error.

RHS of type synonyms
####################

When writing::

  type Arrow = (->)

the arrow is defaulted to mean ``(->) @U``.

.. _Syntax:

Syntax changes
~~~~~~~~~~~~~~

The `Haskell 2010 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_
standard defines the syntax of types as follows ::

  type ::= btype [-> type]

This proposal changes the syntax by adding an optional matchability annotation
slot to ``->``, and defines what those matchability annotations can be
(``U``, ``M`` or a (type) variable, the result of applying a type family to a
type, ...).::

    type ::= btype [-> [@btype] type]

The syntax described above is a mere application of the
"infix type application" idea discussed in
`#12363 <https://gitlab.haskell.org/ghc/ghc/issues/12363>`_.

Arity of type families
~~~~~~~~~~~~~~~~~~~~~~

Consider the following two type families ::

  type family Foo (a :: *) :: *
  type family Bar :: * -> @U *

Both have the same kind, namely ``* -> @U *``, but there is a crucial
difference: the arity of ``Foo`` is 1, whereas ``Bar`` is nullary.
That is, it is possible to define ::

  type family Foo (a :: *) :: * where
    Foo Int  = Bool
    Foo Char = Int

but ``Bar`` can only be defined without arguments and a type family on
its RHS ::

  type family Bar :: * -> @U * where
    Bar = Foo

This is because type families can only be reduced when they are fully
saturated.

Effects and interactions
------------------------

Type synonyms
~~~~~~~~~~~~~

While the proposal's main focus is type families, it also enables partial
application of type synonyms, treated in the same way as type families.

Term-level functions
~~~~~~~~~~~~~~~~~~~~

Since ``TypeInType``, the types of terms and the kinds of types share
the same arrow ``->``. Consequently, as a result of this proposal, term-level
functions inevitably need to have a matchability. They are assigned
the unmatchable arrow ``-> @U``.

Visible dependent quantification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another place where the ``->`` shows up today is visible dependent
quantification, for example in the kind of ::

  -- P :: forall k -> k -> Type
  data P k (a :: k) = P

We propose to annotate such arrows with matchabilities too, so the full
kind of ``P`` becomes ``forall k -> @M k -> @M Type``.

Explicit specificity
~~~~~~~~~~~~~~~~~~~~

When supplying type arguments to matchability-polymorphic functions such as ::

   qux :: forall m (f :: * -> @m *) a. f a -> f a

the user needs to provide either a concrete matchability or a wildcard before
supplying the instantiation for ``f``, as in ``qux @_ @Id``. This is tiresome,
because ``m`` can *always* be inferred from the kind of ``f``, so it would be
preferable to write ``qux @Id`` instead.

The `explicit specificity <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_
feature greatly improves the usability of unsaturated type families, as now the signature
can be written as ::

   qux :: forall {m} (f :: * -> @m *) a. f a -> f a

Standalone Kind Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``StandaloneKindSignatures`` feature has the largest user-facing interaction
surface with the current proposal. Before ``StandaloneKindSignatures``, the kind
of a type family was never written in the source language, and was only hinted at by
GHCi. Today this is no longer the case ::

  type Id :: Type -> Type
  type family Id a where
    Id a = a

The meaning of this kind signature is ``type Id :: Type -> @U Type``. However,
requiring the user to spell out the full signature incurs a considerable amount
of mental burden, so instead we propose to infer it from the definition.

Another example ::

  type Foo :: Type -> Type
  type family Foo where
    Foo = Maybe

  type Bar :: Type -> Type
  type family Bar where
    Bar = Id

will get inferred to be ``Foo :: Type -> @M Type`` and ``Bar :: Type -> @U Type`` respectively.
We can also infer polymorphism, for example ::

  type Map :: (a -> b) -> [a] -> [b]
  type family Map f xs where
    Map f       '[] =             '[]
    Map f (x ': xs) = f x ': Map f xs

can be inferred the kind ``forall {t :: Matchability} {k :: Type} (a b :: k). (a -> @t b) -> @U [a] -> @U [b]``.

When such inference is not possible, we propose to use a defaulting strategy
that is in line with existing behaviour. For example, in the case of open type
families ::

  type Open :: Type -> Type
  type family Open

we can not tell up front which arrow the user meant, so we default it to mean
matchable. Doing so disallows defining ``type instance Open = Id``, but such
instances are already disallowed today due to the saturation restriction.

The proposed solution requires some inference to happen even in checking
mode. Kind inference is notoriously hard due to issues like kind-indexed type
families and polymorphic recursion. However, we propose only to infer
matchabilities and not arbitary kinds, which is a conservative extension of the
status quo. In particular, we never infer non-parametric polymorphism so the following
is rejected ::

  type FooBar :: Type -> Type
  type family FooBar where
    FooBar = Maybe
    FooBar = Id

To be accepted, an annotation is required::

  type FooBar' :: Type -> @m Type
  type family FooBar' where
    FooBar' = Maybe
    FooBar' = Id

It means that in type families, users only need to provide an explicit signature
if they desire the matchability-indexed definition, which we predict is going to
be restricted to a small fraction of advanced users.

CUSKs
~~~~~

In the proposed solution the requirements to have a CUSK_ changes.
Today, the following type is considered to have a CUSK::

  type family Cusk :: Type -> Type where
    Cusk = Maybe

Under this proposal, ``Cusk`` requires an explicit annotation on its
matchability to be considered having a CUSK::

  type family Cusk :: Type -> @m Type where
    Cusk = Maybe


.. _CUSK: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion

DataKinds
~~~~~~~~~

Promoted data constructors get matchable kinds. This means that ::

  :type Just  :: a -> @U a
  :kind 'Just :: Type -> @M Type

Linear Haskell
~~~~~~~~~~~~~~

Under ``LinearTypes``, the arrow type is decorated with a different kind of
information: multiplicity. Happily, multiplicities are only used in *types*,
whereas matchabilities only appear in *kinds*. As a result, these features are
in theory completely orthogonal.

In practice, the arrow annotation requires careful thought. In ``LinearTypes``,
the ``(->)`` type is defined as a synonym for a more general constructor ``FUN``
that takes a multiplicity argument: ::

  type FUN :: forall (n :: Multiplicity) ->
              forall {q :: RuntimeRep} {r :: RuntimeRep}. TYPE q -> TYPE r -> Type

  type (->) :: forall {q :: RuntimeRep} {r :: RuntimeRep}. TYPE q -> TYPE r -> Type
  type (->) =
    FUN 'Many

Here we propose yet another annotation, which turns the kinds of the two
constructors to the following: ::

  type FUN :: forall (m :: Matchability).
              forall (n :: Multiplicity) ->
              forall {q :: RuntimeRep} {r :: RuntimeRep}. TYPE q -> TYPE r -> Type

  type (->) :: forall (m :: Matchability).
               forall {q :: RuntimeRep} {r :: RuntimeRep}. TYPE q -> TYPE r -> Type
  type (->) = FUN 'Many

The synonym morally translates to::

  type (->) @m = FUN @m 'Many


Since the matchability argument is invisible, this synonym works just like one
would expect (in particular, there's no unexpected interaction from the fact
that ``(->)`` needs to bind the matchability argument to apply it out-of-order).

Costs and Drawbacks
-------------------

An implementation of this proposal would touch several parts of the
compiler and some new complexity *would* be introduced, most of it
concentrated in the implementation of the hybrid matchability
inference/defaulting scheme in the typechecker.

Another potential drawback is that users will now need to be aware of the arrow
dichotomy. However, this only concerns advanced users, and the feature aims to
be backwards-compatible. Notably, before this feature, the kind of a type family
only shows up when using ``StandaloneKindSignatures`` or in GHCi when using the
``:kind`` command. A new flag ``-fprint-explicit-matchabilities`` can be added,
similar to ``-fprint-explicit-runtime-reps``, that only shows the matchability
information to users who ask. This, together with the inference scheme proposed
above means no changes to most users.

Alternatives
------------

There are a number of alternative decisions regarding the specific
details of the proposal.

1.  Instead of matchability polymorphism,
    a subsumption relationship could be considered between the two arrows.
    This approach has been fully formalised by Richard Eisenberg in his
    `thesis <http://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf>`_,
    The main drawback of that approach is that inference would suffer compared
    to the scheme outlined above. Matchability polymorphism also fits more
    cleanly into the existing constraint solver mechanism. Alternatively, we
    could stick to the simple defaulting scheme mentioned in the section about
    annotation-free arrows. But...

2.  Type inference with the "simple" matchability defaulting scheme is
    incomplete. Take following program ::
      nested :: a b ~ c Id => b Bool
      nested = False

    initially, the matchabilities of ``a``, ``b`` and ``c`` are all
    instantiated with unification variables, and there are no further
    steps. So they are all defaulted to be matchable, at which point
    the equality can be decomposed, and we learn that
    ``(b :: * -> @M *) ~ (Id :: * -> @U *)``. This way, ``nested`` cannot be
    called, because no such ``b`` exists.

    Instead, we could do something more clever by defaulting
    matchabilities in dependency order, but it's not obvious if this
    additional complexity would be worth it.

3.  We could make different choices for the syntax, regarding how we annotate
    arrows with matchabilities or the particular names around the
    ``Matchability`` type.

4.  Data constructors could be considered to have matchable types. This would
    make promotion more unified, as promoted constructors have matchable kinds.
    This is quite appealing, but doing so would require additional engineering
    effort. Either we would need to introduce matchability-polymorphic
    term-level functions, or, perhaps more realistically, eta-expand all data
    constructor applications to demote them to unmatchable.

5.  The currently proposed change to CUSKs is not backwards compatible. For example,
    the following program compiles today as it has a CUSK ::

      type family Cusk :: Type -> k where
        Cusk = Maybe

    but is rejected under the current proposal due to its unannotated return
    kind. Since the matchability is not annotated, the type no longer has a
    CUSK, so the ``k`` would need to unify with the ``*`` of ``Maybe, so it is
    rejected.  An alternative consideration would be to introduce a notion of a
    "partial CUSK" that mentions all arguments except for the matchability ones.


Unresolved Questions
--------------------

1. Syntax
   We stick to just one operator, ``->``, but take the spot on the right of
   the arrow to specify matchability annotations, while the Linear Haskell work
   uses the spot on the left. Possibly two predefined operators that would stand
   for ``-> @U`` and ``-> @M``. Is there a better syntax to annotate arrows
   with matchabilities?

2. Precise inference/defaulting strategy.
   This is part of the work involved in implementing this proposal.

Implementation Plan
-------------------

I have implemented a
`prototype <https://gitlab.haskell.org/kcsongor/ghc/tree/master>`_
of this feature, as described in this proposal.
