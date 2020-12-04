Unsaturated Type Families
=========================

.. author:: Csongor Kiss
.. date-accepted:: 2020-12-04
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/242>`_.
.. contents::

This is a proposal to allow partial application of type families. The idea is
described in the paper
`Higher-Order Type-level Programming in Haskell <https://www.microsoft.com/en-us/research/publication/higher-order-type-level-programming-in-haskell>`_,
and is presented here with a few tweaks and refinements.

Committee Decision
------------------
The committee has accepted this proposal on an *experimental* basis. This
feature is wholly new to Haskell (and to programming, more generally), and
we will learn more from experience. But we cannot gain the experience without
incorporating and releasing the feature. We thus label it as subject to change.
Changes will be incorporated into this document before they are released.

In particular, the following aspects are subject to change:

* Syntax (especially in light of `Proposal #370 <https://github.com/ghc-proposals/ghc-proposals/pull/370>`_).

* The defaulting rules.

These changes might continue (without prior debate here) even after a release, but
all changes will be made in consultation with the GHC developer team, via debate
at `GitLab <https://gitlab.haskell.org/ghc/ghc/>`_.

Motivation
----------

GHC provides rich tools for type-level programming, but the type
language has several limitations compared to the term language, which makes
using these tools a challenging task. In particular, type functions (type families)
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

The aim of this proposal is to remove this limitation of the type language by
enabling partial application of type functions (both type families and type
synonyms), thereby making the type language higher-order. Thanks to this
feature, type-level programs are easier to understand and maintain, as common
abstractions like ``Map`` can be defined in a standard library.

Recap: saturation restriction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It might not be obvious, but there is in fact a very good reason why
type families must be saturated today. GHC's type inference engine
relies on syntactic equalities of type constructor applications.
For example, GHC can solve equalities of the shape ``f a ~ g b``
by decomposing them to ``f ~ g`` and ``a ~ b``.
This yields a unique solution only if ``f`` and ``g``
are type constructors (so ``f a`` and ``g b`` are syntactically equal).
To make sure that ``f`` and ``g`` are not type families, GHC disallows
unsaturated type families.

For more background and examples see
`Section 2 of the paper <https://www.microsoft.com/en-us/research/publication/higher-order-type-level-programming-in-haskell>`_.

Overview
--------

Here is an overview of the changes introduced by this proposal, together with
examples to illustrate the new behaviour.

Matchabilities
~~~~~~~~~~~~~~

The proposed change is to distinguish between type constructors and
type functions in the *kind system*. That is, a type family such as identity ::

  type family Id a where
    Id a = a

will have kind ``k -> @U k`` instead of ``k -> k`` -- the kind that GHC would
infer today. The ``U`` means "unmatchable". Similarly, type synonyms such as constant ::

  type Const a b = a

will have kind ``k -> @U j -> @U k``, and is also possible to partially apply.

Type constructors such as ``Maybe``
or ``[]`` would instead have kind ``Type -> @M Type``, meaning they are
matchable. Matchability is a property of the arrow that appears in the kind.
The saturation restriction for ``Map`` from earlier can now essentially be
summed up by stating that its first argument has kind ``a -> @M b``.

Then equalities of the shape ``f a ~ g b`` are only solved by
decomposition when ``f :: k -> @M j`` and ``g :: k -> @M j``.

With this distinction, it is now possible to define a version of ``Map`` that
abstracts over type families ::

   type family Map (f :: a -> @U b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

The kind of ``Map`` itself becomes ``(a -> @U b) -> @U [a] -> @U [b]``.

``M`` and ``U`` are both types of kind ``Matchability`` defined in ``GHC.Matchability``.


Matchability (due to `Richard Eisenberg <https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf>`_) is
defined as the conjunction of two properties, *generativity* and *injectivity*.

:Generativity: ``f`` and ``g`` are generative when ``f a ~ g b`` implies ``f ~ g``
:Injectivity: ``f`` is injective when ``f a ~ f b`` implies ``a ~ b``
:Matchability: ``f`` is when it is both generative and injective

Technically, generativity is a binary relation on type functions, but we
define matchability as a property of a single type function and say that
generativity holds for two type functions when they are both matchable.

For example, with ``f :: Type -> @M Type``, ``g :: Type -> @M Type``, and ``h :: Type -> @U Type``:

- ``f a ~ g b`` => ``f ~ g`` and ``a ~ b`` because both ``f`` and ``g`` are matchable
- ``f a ~ h b`` =/> ``f ~ h`` or ``a ~ b`` because ``h`` is unmatchable

Thus matchability characterises GHC's existing equality decomposition behaviour.
By adding this information to the kind system, we can keep all the type
inference behaviour for type constructors, while also allowing partial
application of unmatchable type functions.

All of the discussion in this proposal applies only at the `nominal role <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#roles>`_.
In particular, this means that the matchability information tells GHC whether a nominal
equality can be decomposed, but there is no way to say that a type function is
matchable at the representational role or phantom role.

Matchability polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~

The version of ``Map`` above can only be applied to type families
(which have kind ``-> @U``) but not type constructors (which have kind
``-> @M``). Since matchabilities are a first-class type, they can be
quantified over, thus enabling polymorphism in the matchability of arrows.

This way, ``Map`` can be defined to be *matchability polymorphic* (in
its first argument) ::

   type family Map (f :: a -> @m b) (xs :: [a]) :: [b] where
     Map _ '[]       = '[]
     Map f (x ': xs) = f x ': Map f xs

This new variant of ``Map`` support taking both ``Id`` (a type family) and
``Maybe`` (a type constructor) as the first argument. The complete kind of
``Map`` is ``forall (m :: Matchability) a b. (a -> @m b) -> @U [a] -> @U [b]``.

In fact, since matchabilities are ordinary types, they can be computed
by type families, e.g.::

  type family Alternate (m :: Matchability) :: Matchability where
    Alternate 'Matchable = 'Unmatchable
    Alternate 'Unmatchable = 'Matchable

  type ArrFlip (m :: Matchability) a b = a -> @(Alternate m) b

  -- F only accepts 'f's with a matchable arrow kind.
  type family F (f :: ArrFlip 'Unmatchable i j) (a :: i) :: j where
    F f a = f a

Other quantifiers
~~~~~~~~~~~~~~~~~

GHC has four quantifiers today: visible non-dependent (``ty ->``), invisible
non-dependent (``ty =>``), visible dependent (``forall ty ->``), and invisible
dependent (``forall ty.``).
`An earlier proposal <https://github.com/ghc-proposals/ghc-proposals/pull/102>`_
discussed the full range of quantifiers present in Dependent Haskell. This
current proposal addresses a subset of the ones included there: namely,
annotating three of the four existing quantifiers with matchability information
(``ty =>`` will always mean unmatchable).

The proposal up to this point has introduced the visible non-dependent case.
The visible dependent quantifier is analogous ::

  type FVis :: forall k -> @U k -> @U Type
  type family FVis k (a :: k) :: Type


  type DVis :: forall k -> @M k -> @M Type
  data DVis k (a :: k) :: Type

Now consider the invisible dependent version of the above two types ::

  type FInvis :: forall k. @U k -> @U Type
  type family FInvis (a :: k) :: Type

  type DInvis :: forall k. @M k -> @M Type
  data DInvis (a :: k) :: Type

Notice that the forall itself is annotated in both cases. The treatment of
invisible quantifiers is necessary to properly handle higher-rank programs. To
illustrate why, consider the following program ::

  type D :: forall (f :: forall k. @U k -> @U Type) -> @M Type
  data D f = D (f Bool) (f 0)

  type F :: forall k. @U k -> @U Type
  type family F a where
    F 0 = Int
    F Bool = Char

  p :: D F
  p = D 'c' 0

Here, ``D`` has a rank-2 kind and its argument is a function. To be able to
pass in ``F``, the forall must be unmatchable in ``D``'s argument.

A matchability variable introduced in a ``forall`` telescope is in scope in the
body of the ``forall``, but not in its matchability. That is, ``forall m x. @m x``
is invalid, but ``forall m. @U forall x. @m x`` is valid. The same applies to the
visible case: ```forall m -> @m Type``` is rejected.

.. _Inference:

Meaning of an annotation-free ``->``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Even though this proposal introduces a way to annotate arrows, in many cases the
annotations can be inferred. The primary aim of inference is to ease the
transition as most programs written today can be unambiguously inferred.

The meaning of ``(->)`` depends on the context in which it is written. Below is
a list of the different contexts with examples. Some of these are a result of a
*defaulting* mechanism, and some are *hard rules*. The first category can be
overridden with an appropriate annotation, but the second can not. The
defaulting mechanism first invents a unification variable for the matchability,
and only applies the defaulting rule here when the variable remains
unconstrained after constraint solving.

Data types
##########

The kind arrows of data types (and data families) are all matchable.

::

  -- inferred:  Type -> @M Type
  type Maybe :: Type -> Type
  data Maybe a = ...

here, users are not required to specify ``Type -> @M Type``, as this information
can be inferred from the data declaration itself. This is a *hard rule*, and
it is an error to write ::

  type Maybe :: Type -> @U Type
  data Maybe a = ...

Term-level functions
####################

The syntax of types and kinds is shared in GHC, so once we assign matchabilities to
kinds, we also have to assign matchabilities to types. Term-level functions are
*always* unmatchable. ::

  -- inferred: a -> @U a
  id :: a -> a
  id x = x

  -- inferred: (a -> @U b) -> @U [a] -> @U b
  map :: (a -> b) -> [a] -> [b]

That said, matchability information is not relevant in terms today, because its
sole purpose is in guarding application decomposition in equalities, and today
we only have type equalities but not term equalities. Defaulting everything to
unmatchable is thus a convenient step, but this situation will change with
Dependent Haskell.

Data constructors
#################

Data constructors are matchable. This means that using either syntax ::

  data Maybe a = Nothing | Just a

  data Maybe a where
    Nothing :: Maybe a
    Just :: a -> Maybe a

``Just :: a -> @M Maybe a``. Promoting ``Just`` thus results in a matchable
type constructor ``'Just :: a -> @M Maybe a``.

This is a *hard rule*.

The *Term-level functions* section specifies that term-level function are all
unmatchable.  GHC already eta-expands data constructors automatically, so in any
term context, `Just` is elaborated to ``(\x -> Just x)`` and thus gets an
unmatchable arrow type, just like any other term-level function. ::

  -- inferred: a -> @U Maybe a
  just :: a -> Maybe a
  just = Just -- eta-expanded


Type families
#############

Type family (and type synonym) *arguments* are unmatchable

::

  -- inferred: Type -> @U Type
  type Id :: Type -> Type
  type Id a = a

is unambiguous, and no annotation is required. However, the unambiguity here
arises not solely due to the fact that ``Id`` is a type function, but also that it
binds its argument on the left-hand side. This is a *hard rule*.

The arrows not corresponding to arguments
bound on the LHS are inferred to be matchable (by *default*) ::

  -- inferred: Type -> @M Type
  type MyMaybe :: Type -> Type
  type family MyMaybe where
    MyMaybe = Maybe

``MyMaybe`` is a nullary type family, and its return kind is thus matchable (see
the *Arity of type families* section for more details about type family arities).
Open type families are treated the same as closed type families.

The following is rejected ::

  -- inferred: Type -> @M Type
  type MyId :: Type -> Type
  type family MyId where
    MyId = Id -- rejected

because, as above, the kind of ``MyId`` is defaulted to ``Type -> @M Type`` as
the arrow occurs in the return kind. Then the equation does not match the kind
signature, and is thus rejected.
This is one of the rare occasions where users
explicitly need to assign an unmatchable arrow for the program to be accepted ::

  type MyIdGood :: Type -> @U Type
  type family MyIdGood where
    MyIdGood = Id

Importantly, when a signature is given, we do
not look at the equations to infer matchabilities, as in this case GHC only
checks kinds, but does no inference.

Higher-order arguments
######################

When a higher-order kind signature is given, the function arrow is assumed to
mean matchable *by default*
::

  -- inferred: (Type -> @M Type) -> @M Type
  type HK :: (Type -> Type) -> Type
  data HK f = ...

This is irrespective of the flavour of type function the signature belongs to.
That is, even for type families, higher-order arguments get assigned matchable
kinds unless specified otherwise
::

  -- inferred: forall a b. @U (a -> @M b) -> @U [a] -> @U [b]
  type Map :: (a -> b) -> [a] -> [b]
  type family Map f xs where ...

Note that the forall is unmatchable, as discussed previously. The function
argument is matchable, which is consistent with the behaviour today.

Also note that this higher-order defaulting mechanism only applies when a kind
signature is given. When no signature is given, the matchability gets
instantiated with a unification variable, and generalised at the end of type
checking ::

  -- inferred: Map :: forall a b m. @U (a -> @m b) -> @U [a] -> @U [b]
  type family Map f xs where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

  -- inferred: HK :: forall m. @M (Type -> @m Type) -> @M Type
  data HK f = MkHK (f Int)

**This is the only scenario where matchability generalisation occurs.** That is,
when no signature is given and the arrow is a higher-order argument to a type
function of any flavour.

Kind-arrows in type signatures
##############################

Whenever an arrow kind arises from the type signature of a term, they are
*defaulted* to matchable ::

  -- inferred: forall (m :: Type -> @M Type) a. @U m a
  foo :: forall (m :: Type -> Type) a. m a
  foo = undefined

Here ``m :: Type -> @M Type``. **The rule is that matchability variables are never
generalised in terms**: if it's a "term-level" arrow, it's assigned unmatchable,
if it's a "type-level" arrow, it's assigned matchable. This happens regardless
of whether the arrow is spelled out, viz: ::

  bar :: f a
  bar = undefined

This behaviour is the most conservative, as we don't trigger ambiguity errors,
and still allow decomposition of equality constraints. Users can override this behaviour
by specifying an explicit matchability annotation::

  bar :: forall (f :: Type -> @U Type) a. f a

Note that this type signature is now ambiguous (in the sense that it will be
rejected unless ``-XAllowAmbiguousTypes`` is enabled), because the type variable
``a`` cannot be determined as ``f`` can be any type family (and thus
non-injective).

It is also possible for the constraint solver to learn the precise matchability
before it resorts to defaulting ::

  baz :: f ~ Id => f a -> f a
  baz x = x

Here, ``f`` is inferred to have kind ``Type -> @U Type`` through the equality
constraints. Note here that the type of ``baz`` is unambiguous (because it
reduces to ``a -> a``).

Kind-arrows in classes
######################

When an arrow kind arises from a type class parameter, it's assumed to be
matchable *by default* ::

  -- inferred: Functor :: (Type -> @M Type) -> @M Constraint
  class Functor (f :: Type -> Type) where

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

Application in pattern position
###############################

When a kind arrow arises from an application in either a type family pattern or
a type class instance head, the arrows must be matchable as a *hard rule*  ::

  type family UnApp a where
    UnApp (f x) = x

  instance Show (g b)

``f`` and ``g`` are inferred to have a matchable kind. Indeed, they must have a matchable
kind, and declaring otherwise is an error.

RHS of type synonyms
####################

When writing::

  type Arrow = (->)

the arrow is *defaulted* to mean ``(->) @U``.

Note that making either choice here is a breaking change.
For example, today one can write ::

  data Maybe :: Arrow Type Type where ...

but this will no longer typecheck because the arrow means unmatchable. The
decision to default to matchable in this case is grounded in the observation
that most such synonyms today refer to term-level, thus unmatchable arrows.

A notable exception is the defunctionalisation arrow from the
`singletons <https://hackage.haskell.org/package/singletons-2.7/docs/Data-Singletons.html#t:-126--62->`_ library::

  type (~>) a b = TyFun a b -> Type

which really refers to a kind-level matchable arrow. However, we expect many
such use cases to be subsumed by first class higher-order functions introduced
by this proposal.

Apartness of type families
~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC has a notion of "apartness": when matching a type family equation, it needs
to make sure that no previous equations match or when matching a type class
instance, that no other instances match. Two types don't match when they are
apart. The apartness check has three possible outcomes: two types can "unify",
be "surely apart", or be "maybe apart".

We propose that type families unify with themselves, but they are "maybe apart"
from every other type. The consequence of this is that writing type families in
positions where apartness is considered is illegal. That is, both

::

  instance C Id where ...

   type instance F Id = ...

are illegal when ``Id`` is a type family.

Proposed Change Specification
-----------------------------

The following sections describe a new GHC extension, which can be
enabled with the pragma ``{-# LANGUAGE UnsaturatedTypeFamilies #-}``.
The pragma implies ``TypeFamilies``.

When the ``UnsaturatedTypeFamilies`` extension is enabled, partial application
of type families and type synonyms is allowed. When it is disabled, partial
application produces a type error suggesting the extension to be turned on.

.. _Syntax:

Syntax changes
~~~~~~~~~~~~~~

GHC's parser includes the following production rules for types::

  type ::= btype '->' ctype
       | ...


  ctype ::= 'forall' tv_bndrs '->' ctype
        |   'forall' tv_bndrs '.' ctype

This proposal adds the following rules::

  type ::= btype '->' ctype
       |   btype '->' PREFIX_AT atype ctype
       | ...


  ctype ::= 'forall' tv_bndrs '->' ctype
        |   'forall' tv_bndrs '->' PREFIX_AT atype ctype
        |   'forall' tv_bndrs '.' ctype
        |   'forall' tv_bndrs '.' PREFIX_AT atype ctype
        | ...

Where ``PREFIX_AT`` stands for the lexer token ``@`` that is to be parsed as a
prefix operator.

That is, it is now possible to annotate each existing form of quantifier with
matchability information.

Matchabilities
~~~~~~~~~~~~~~

Matchability is a first-class type, and is defined in ``GHC.Matchability`` as ::

  data Matchability = Matchable | Unmatchable

We use these long names to improve the discoverability of the feature,
but also provide shorter synonyms, which are used in the examples above::

  type M = 'Matchable
  type U = 'Unmatchable

The ``Matchability`` type and the ``M`` and ``U`` synonyms are exported from
the ``GHC.Matchability`` module.

FUN
~~~

The full kind of the ``(->)`` constructor becomes ::

  (->) :: forall (m :: Matchability)
                 {q :: RuntimeRep} {r :: RuntimeRep}. @M
          TYPE q -> @M TYPE r -> @M Type

The matchability part of the arrow can be instantiated using visible type
application in types, a recent addition to GHC.

The ``a -> @m b`` syntax is thus syntactic sugar for ``(->) @m a b``.

Since the ``LinearTypes`` extension has landed in GHC, the `(->)` constructor is defined
as a synonym for a more general constructor ``FUN`` that takes a multiplicity
argument. The full kind of ``FUN`` under the current proposal becomes ::

  type FUN :: forall (m :: Matchability). @M
              forall (n :: Multiplicity) -> @M
              forall {q :: RuntimeRep} {r :: RuntimeRep}. @M TYPE q -> @M TYPE r -> @M Type

which now accounts for both matchability and multiplicity annotations.
Then ``(->)`` is defined morally as::

  type (->) @m = FUN @m 'Many

Since the matchability argument is invisible, this synonym works just like one
would expect (in particular, there's no unexpected interaction from the fact
that ``(->)`` needs to bind the matchability argument to apply it out-of-order).

Note that the matchability argument is invisible, therefore manually specifying
it is optional.

Inference
~~~~~~~~~

The meaning of unannotated ``forall``\s and ``->``\s is inferred, using the
following rules (for more details see the *Overview* section):

1. Data types and data families have matchable kinds.
2. Type families and type synonyms have unmatchable kinds.
3. Higher-order kinds are

   a. defaulted to matchable when a signature is given
   b. generalised when no signature is given

4. Term-level functions have unmatchable arrows.
5. Kind arrows written in type signatures default to matchable if they cannot
   be inferred by the constraint solver.
6. Type class arguments have matchable kinds by default in both class
   declarations and instance declarations.
7. Instances for the ``(->)`` are assumed to be for the unmatchable arrow by
   default.
8. Arrows written in the RHS of type synonyms are assumed to be unmatchable.

Generalisation only occurs in kinds (and never types), and only when no
signature is given.

Showing matchabilities
~~~~~~~~~~~~~~~~~~~~~~

We propose a new flag, ``-fprint-explicit-matchabilities``, similar to
``-fprint-explicit-runtime-reps``, that only shows the matchability information
to users who ask. ``-XUnsaturatedTypeFamilies`` implies
``-fprint-explicit-matchabilities``.

Effects and interactions
------------------------

Promotion
~~~~~~~~~

The strategy to always assign an unmatchable arrow to term-level arrows
interacts with promotion::

  data T = MkT (Type -> Type)

  type S = 'MkT Maybe

This program is accepted today, but will be rejected under the current proposal.
The reason is that when defining ``T``, it is considered to be a term-level
entity, thus the field's type is assigned an unmatchable arrow type.

Then, ``Maybe`` cannot be used as an argument to it. A potential fix is
to turn the constructor matchability-polymorphic::

  data T = forall m. MkT (Type -> @m Type)

This is not done automatically in order to avoid confusion around
existential varibles.

Arity of type families
~~~~~~~~~~~~~~~~~~~~~~

GHC today determines the arity of a type family syntactically `arity
<https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#type-family-declarations>`_
by looking at the number of bound arguments in the type family header.

GHC then uses the arity to determine when can a type family be reduced (only
when it's fully saturated).

It is tempting to think that the matchability information alone is sufficient to
determine the arity of a type family, but in fact it is not.

Consider the following two type families ::

  type family Foo (a :: Type) :: Type
  type family Bar :: Type -> @U Type

Both have the same kind, namely ``Type -> @U Type``, but  the arity of ``Foo``
is 1, whereas ``Bar`` is nullary. Since partial application is now possible, the
arities no longer play such an important role. The main place where they still
show up is in the definitions of type families. Type family equations must bind
all of their arguments on the left-hand side ::

  type family Foo (a :: Type) :: Type where
    Foo Int  = Bool
    Foo Char = Int

but ``Bar``, a nullary type family, can only be defined without arguments and a
type family on its RHS ::

  type family Bar :: Type -> @U Type where
    Bar = Foo

Thus the following definition is invalid ::

  type family Bad :: Type -> @U Type where
    Bad x = Foo x

Thus, the relationship between the arity and the kind can be summarised as follows:
If a type family's arity is ``n``, then its kind will have *at least* its first
``n`` arrows unmatchable. Notably, the proposal doesn't change the existing
behaviour whereby every argument of a type family must be bound on the LHS.


Explicit specificity
~~~~~~~~~~~~~~~~~~~~

When supplying type arguments to matchability-polymorphic functions such as ::

   qux :: forall m (f :: Type -> @m Type) a. f a -> f a

the user needs to provide either a concrete matchability or a wildcard before
supplying the instantiation for ``f``, as in ``qux @_ @Id``. This is tiresome,
because ``m`` can *always* be inferred from the kind of ``f``, so it would be
preferable to write ``qux @Id`` instead.

The `explicit specificity <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_
feature greatly improves the usability of unsaturated type families, as now the signature
can be written as ::

   qux :: forall {m} (f :: Type -> @m Type) a. f a -> f a

Linear Haskell
~~~~~~~~~~~~~~

Under ``LinearTypes``, the arrow type is decorated with a different kind of
information: multiplicity. Other than syntactic considerations and somewhat
overlapping implementations, there is no interaction between matchability and
multiplicity.

Comparison with levity polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here we draw a comparsion between matchability polymorphism and `levity polymorphism <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/levity-pldi17.pdf>`_,
from the perspective of type inference. There is no notable interaction between these
two features, but there are noteworthy differences between the way matchability variables
are inferred compared to runtime representation variables.

In *types*, runtime representation variables are all defaulted to ``LiftedRep``, and
matchability variables are all defaulted depending on where the variables appear
(see the *Term-level functions* and *Kind-arrows in type signatures* sections
above).

In *kinds*, runtime representation variables are all defaulted to ``LiftedRep``,
but matchability variables are only defaulted when a signature is given, and
generalised otherwise.

As a simple example, consider the following two type families ::

  type Foo :: forall {r :: RuntimeRep} {m :: Matchability}. TYPE r -> @m TYPE r
  type Foo = ...

  -- inferred: Bar :: forall {m :: Matchability}. Type -> @m Type
  type Bar = Foo

``Foo`` is both levity-polymorphic and matchability-polymorphic. However, in
``Bar``'s kind, the ``RuntimeRep`` variable is defaulted, but the
``Matchability`` variable is generalised.

The rationale behind defaulting runtime rep variables in types is that inferring
polymorphism would trip up code generation. The rationale behind defaulting
matchabilities in types is that inferring polymorphism would lead to ambiguous
types. In kinds, however, we take a more nuanced approach, because
generalisation there is desirable.

See the *Higher-order arguments* section above and the *Alternatives* section below for
more details behind this approach.

Costs and Drawbacks
-------------------

The implementation of this proposal touches several parts of the
compiler and some new complexity is introduced, most of it
concentrated in the implementation of the hybrid matchability
inference/defaulting scheme in the typechecker.

Another potential drawback is that users will now need to be aware of the arrow
dichotomy. However, this only concerns advanced users, and the feature aims to
be backwards-compatible. Notably, before this feature, the kind of a type family
only shows up when using ``StandaloneKindSignatures`` or in GHCi when using the
``:kind`` command.

The proposed default of not showing matchabilities and the
``-fprint-explicit-matchabilities`` flag aim to reduce this overhead.

Alternatives
------------

There are a number of alternative decisions regarding the specific
details of the proposal.

1.  Instead of matchability polymorphism,
    a subsumption relationship could be considered between the two arrows.
    This approach has been fully formalised by Richard Eisenberg in his
    `thesis <http://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf>`_.
    The main drawback of that approach is that inference would suffer compared
    to the scheme outlined above. Matchability polymorphism also fits more
    cleanly into the existing constraint solver mechanism.

2.  Type inference with the "simple" matchability defaulting scheme is
    incomplete. Take following program ::

      nested :: a b ~ c Id => b Bool
      nested = False

    Initially, the matchabilities of ``a``, ``b`` and ``c`` are all
    instantiated with unification variables, and there are no further
    steps. So they are all defaulted to be matchable, at which point
    the equality can be decomposed, and we learn that
    ``(b :: Type -> @M Type) ~ (Id :: Type -> @U Type)``, which yields an error as the two kinds mismatch.
    Note that ``b`` has a matchable kind, because it was defaulted so, together
    with ``a`` and ``c``.

    Instead, we could do something more clever by defaulting matchabilities in
    dependency order (so only ``a`` and ``c`` are defaulted, as doing so might
    uncover more information about ``b``), but it's not obvious if this
    additional complexity would be worth it.

3.  When a kind signature is given, we make the choice of not generalising the
    matchabilities, which differs from the treatment of kind variables. Consider
    the following program ::

      type A :: Proxy a -> Type
      type family A

    The inferred kind is ``A :: forall {k} (a :: k). Proxy a -> @M Type``, so the
    kind of the type variable ``a`` did get generalised, but the matchability of
    the arrow didn't (note that ``A`` takes no visible arguments, the arrow is in its return kind).
    An alternative option would be
    to simply generalise these matchability variables too, and arrive at the more
    general ``A :: forall {k} {m} (a :: k). Proxy a -> @m Type`` kind.

    But we don't do this, because doing so would result in counterintuitive
    behaviour in many common cases, in particular, type variables introduced in
    this way could block type family reduction. Consider the following examples ::

      type B :: Type -> Type
      type family B where
        B = Maybe

      type C :: (Type -> Type) -> Type -> Type
      type family C f where
        C f = f

    If we infer ``B :: forall {m}. Type -> @m Type``, then ``:kind! B`` is stuck! This is
    because type variables have computational relevance in type family reduction. In other
    words, ``B`` becomes a matchability-indexed type family, which is likely not what the user
    intended. To reduce to ``Maybe``, the user would need to provide an explicit
    return kind ``:kind! B :: Type -> @M Type``.

    Similarly, the generalised kind of ``C`` would be
    ``C :: forall {m} {n}. (Type -> @m Type) -> (Type -> @n Type)``, then ``:kind! C Maybe`` is stuck,
    and so is ``:kind! C Id`` without explicit return kinds.

    It is important to note here that in *checking mode* (against a signature),
    GHC decides on a generalisation strategy *before* it looks at the equations
    of ``B`` and ``C``, making the decision purely based on the provided kind
    signature.  Thus, in the presence of a kind signature, the bodies are only
    kind checked, but no new information is learned from doing so. Thus, there
    is no hope of inferring the kind ``C :: forall {m}. (Type -> @m Type) -> @U
    Type -> @m Type`` (doing so would require looking at the equation), and the
    next best thing, short of an annotation, is to conservatively default to
    matchable.

    The treatment of matchability variables in generalisation is thus different
    from ordinary kind variables. In fact, the way kind variables are treated
    can also lead to unintuitive behaviour ::

      type ProxyType :: Proxy (a :: Type)
      type ProxyType = 'Proxy

      -- generalised to
      --   T :: forall {k} (a :: k). Proxy a
      type T :: Proxy a
      type family T where
        T = ProxyType

    Here, the ``a`` argument's kind in ``T``'s kind gets generalised, so ``T`` is
    indexed in the kind of ``a``. Then the given equation only matches when this
    kind is ``Type``, given by ``ProxyType``'s signature. Then ``T @Int`` reduces,
    but ``T @Maybe`` gets stuck.

    Thus it would be more consistent to also generalise matchabilities, but while
    this confusing behaviour is rare in the context of kind-variables, it is a
    much more common occurrence with matchability variables. For kind variables
    to trigger this behaviour, there needs to be a kind-polymorphic type (such as
    a type variable, or a type like ``Any``) applied to a kind-polymorphic type
    constructor (such as ``Proxy``). But since matchability variables arise from any
    higher-kinded argument, every higher-order type family like ``B`` and ``C``
    would be affected.

    To conclude the discussion, there are at least two alternatives to the
    proposed strategy:

    a. Generalise the matchability variables in the same way kind variables are
       generalised. The downsides of this approach are outlined above.
    b. Change the way type family reduction works, such that implicitly
       quantified type variables may never be computationally relevant, then
       generalise matchability variables. This would be a small win,
       because computations would not get stuck, and we could infer more
       polymorphism, such as ::

         type Map :: (a -> b) -> [a] -> [b]
         type family Map f xs where ...

       could be inferred to have a polymorphic argument. However, neither ``B``
       nor ``C`` above would typecheck, because in both cases the matchabilities are
       computationally relevant.

4.  When a kind signature is *not* given, we make the choice of generalising the
    matchabilities. An example from the *Higher-order arguments* section above ::

      -- inferred: Map :: forall a b m. @U (a -> @m b) -> @U [a] -> @U [b]
      type family Map f xs where
        Map f '[] = '[]
        Map f (x ': xs) = f x ': Map f xs

    Note that the ``f`` argument is inferred to be matchability polymorphic.
    So why generalise here, but not when a signature is given? As discussed above,
    in *checking mode*, GHC decides on generalisation before looking at any of
    the type family equations. However, in *inference mode*, the equations
    are consulted first, since that is where all the type/kind information comes from, and
    generalisation happens only when the variable in question is unconstrained.
    Thus, in the case of ``Map``, it is safe to generalise, since none of the equations
    match on the matchability, thus the variable is computationally irrelevant.

    ``B`` is accepted without a signature ::

      -- inferred: B :: Type -> @M Type
      type family B where
        B = Maybe

    But this time, not because of defaulting, but because the signature can be inferred.
    Similarly, ``C`` is also accepted without a signature ::

      -- inferred: C :: Type -> @U Type
      type family C where
        C = Id

    Note that ``B`` would also be accepted with the ``B :: Type -> Type``
    signature, but ``C`` would not (as the unannotated arrow in the return kind
    of a type family defaults to matchable).

    Finally, when the equations would require matchability indexing, the definition is
    rejected ::

      type BadIndex where
        BadIndex = Maybe
        BadIndex = Id

    because the two equations have different kinds. To have ``BadIndex`` accepted, the
    user needs to write a polymorphic signature ``BadIndex :: Type -> @m Type``.

    The alternative choice here would be to default matchabilities also when no
    signature is given, but that seems to offer no benefits, other than a minor
    simplification of the specification.

Future directions
-----------------

There are several avenues that would be interesting to explore that either build
on the current proposal, or have interesting interactions with it. These are
outside of the scope of this proposal, but mentioning them here is worthwhile to
keep track of them and also to evaluate the proposal with future extensions in
mind.

Injectivity
~~~~~~~~~~~

Matchable type functions are a subset of injective type functions, and it might
be worthwhile to investigate first-class injectivity annotations in the kind
system alongside matchabilities. Doing so would also allow higher-order
injectivity annotations, which are not possible with ``TypeFamiliyDependencies``
today (i.e. a type family might be injective if its argument is injective, but
not otherwise). One question that arises is how to fit injectivity into the
current matchable/unmatchable dichotomy. We've avoided subtyping so far, but
maybe it would be fine here?

Type class instances for type families
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The *Apartness of type families* section prescribes that type families can not
be used in type class instance heads. It would actually be possible to do so,
and I intend to pursue another proposal if this one gets accepted to allow for
writing class instances for type families. Happily, the choice to make type
families "maybe apart" is both backwards and forward compatible. Backward,
because no existing program can even ask the question of whether two partially
applied type families are equal, and forward, because "maybe apart" leaves the
possibility open to later arrive at a more specific result ("unify" or "surely
apart").

Dependent Haskell
~~~~~~~~~~~~~~~~~

A few words on future compatibility: the ``UnsaturatedTypeFamilies``
extension is compatible with Dependent Haskell, indeed tracking matchability
information is already part of design for Dependent Haskell (for more details see Section 4.2 of `Richard Eisenberg's thesis <https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf>`_).
Nevertheless, some of the choices in this proposal were made to ease the
transitionary period, with a preference for backwards-compatibility. Notably,
matchability inference and defaulting. As explained in the *Alternatives* section
above and in the *Unresolved Questions* section below, there is a tension here
between backward and future compatibility.

Higher-order arguments in kinds are defaulted to matchable, while higher-order
arguments in types are unmatchable. Dependent Haskell is most certainly going
to merge these two notions, which means that it is going to introduce a breaking
change around matchability inference.

Unresolved Questions
--------------------

1. Syntax

   We stick to just one operator, ``->``, but take the spot on the right of
   the arrow to specify matchability annotations, while the Linear Haskell work
   uses the spot on the left. Possibly two predefined operators that would stand
   for ``-> @U`` and ``-> @M``. Is there a better syntax to annotate arrows
   with matchabilities?

   A promising new direction is the `Syntax for Modifiers <https://github.com/ghc-proposals/ghc-proposals/pull/370>`_
   proposal, which aims to provide a general framework for modifiers such as
   multiplicity and matchability, and potential future extensions.

2.  Backwards compatibility is mentioned in several parts of this proposal, most
    notably the matchability defaulting scheme in kind signatures always
    defaults to matchable (see the *Higher-order arguments*  section in
    the *Overview*). This is so that declarations such as ::

      -- T :: (Type -> @M Type) -> @M Type
      data T (f :: Type -> Type) = MkT (f Int)

      -- F :: (Type -> @M Type) -> Type -> @M Type
      type family F (f :: Type -> Type) :: Type -> Type where ...

    retain their current meanings even when the extension is turned on.

    There is a tension between backwards compatibility and future compatibility
    here. Unsaturated type families are on the path towards dependent types, and
    as the language as a whole moves towards that goal, we can expect this tension
    to grow further. In concrete terms, a vast majority of functions passed into
    higher-order arguments are going to be unmatchable, so more often than not, users
    will want ::

      -- F' :: (Type -> @U Type) -> Type -> @U Type
      type family F' (f :: Type -> Type) :: Type -> Type

    This would mean that users would have to annotate only the arrows that they
    want to be matchable (opposite to the current proposal), which is arguably the
    more important to be explicit about. It would even mean being able to infer
    more polymorphism, since an arrow that users expect to be unmatchable is
    safe to generalise.

    Furthermore, making the decision to default to unmatchable arrows will allow
    for a much cleaner transition for promoting term-level higher-order
    functions to replace type families, since these functions already take
    unmatchable arguments.

    The exact defaulting strategy is a minor implementation detail of the current
    proposal that has a major impact on ergonomics, and the decision should be made based
    on whether we want to favour backwards or forward compatibility.
    The current proposal thus can be thought of as being parameterised in this
    decision. The `Support ergonomic dependent types
    <https://github.com/ghc-proposals/ghc-proposals/pull/378>`_ proposal
    discusses the general philosophy in more detail, and its result should
    directly influence the decisions made here.

Implementation Plan
-------------------

I have implemented a `prototype
<https://gitlab.haskell.org/kcsongor/ghc/tree/master>`_ of this feature, as
described in an earlier version of this proposal.
