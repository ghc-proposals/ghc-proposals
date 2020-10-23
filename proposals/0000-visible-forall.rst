Visible ``forall`` in types of terms
====================================

.. author:: Vladislav Zavialov
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/281>`_.
.. sectnum::
.. contents::

We propose to allow visible irrelevant dependent quantification, written as
``forall x ->``, in types of terms.

Background
----------

(Skip to "Motivation" if you are comfortable with the notions of dependence,
relevance, and visibility, when talking about quantifiers).

Function parameters can be bound using various quantifiers. Consider the
identity function::

  id :: forall a. a -> a
  id = \x -> x

There are two parameters to ``id``:

1. ``a :: Type``
2. ``x :: a``

Quantifiers are parts of the type that introduce the function parameter. In the
type of ``id``, there are two quantifiers:

1. ``forall a.`` introduces ``a :: Type``
2. ``a ->`` introduces ``x :: a``

We classify quantifiers along several axes:

* Dependent or non-dependent
* Relevant or irrelevant
* Visible or invisible

Dependence
~~~~~~~~~~
We call a quantifier dependent when the parameter can be used in the type of
the function result. ``forall a.``, which introduces ``a :: Type``, is a
dependent quantifier::

  id :: forall a. a -> a
                 ^^^^^^^^^^^^^^^^
                 'a' is used here

On the other hand, ``a ->``, which introduces ``x :: a``, is a non-dependent quantifier::

  id :: forall a. a -> a
                      ^^^^^^^^^^^^^^^^^^^^^^^
                      'x' cannot be used here

Relevance
~~~~~~~~~
We call a quantifier relevant when the parameter can be used in the function
body in a position other than a type annotation. Intuitively, it means that
relevant parameters are relevant to the evaluation of the function and must be
passed at runtime. ``forall a.``, which introduces ``a :: Type``, is not a
relevant quantifier::

  id :: forall a. a -> a
  id = \x -> x
      ^^^^^^^^^
      'a' cannot be used here (other than as a type annotation
                               with scoped type variables)

On the other hand, ``a ->``, which introduces ``x :: a``, is a relevant
quantifier::

  id :: forall a. a -> a
  id = \x -> x
            ^^^
            'x' is used here

Visibility
~~~~~~~~~~
We call a quantifier visible when the parameter must be specified at use sites,
and invisible when the compiler tries to infer it at use sites.

Consider an expression such as ``id True``. In this call, we have:

* ``x=True``, as specified
* ``a=Bool``, as inferred from ``(x :: a) = (True :: Bool)``

The reason we don't write ``id Bool True`` is that ``forall a.`` is an
invisible quantifier, while ``a ->`` is a visible quantifier.

With the ``TypeApplications`` extension, we can use a visibility override ``@``
to specify an invisible parameter as if it was visible::

  id @Bool True

Motivation
----------
In types of types (in kinds), we have the choice between invisible and visible
dependent quantification::

  type PInv :: forall k. k -> Type  -- invisible quantification of 'k'
  data PInv a = MkPInv

  type PVis :: forall k -> k -> Type  -- visible quantification of 'k'
  data PVis k a = MkPVis

Invisible parameters, introduced with ``forall x.``, are inferred by the
compiler at use sites. Visible parameters, introduced with ``forall x ->``,
must be specified by the user::

  type TInv = PInv     15   -- infer (k~Nat) from (a::k)~(15::Nat)
  type TVis = PVis Nat 15   -- no inference

This means our quantifier grid is complete with regards to dependence and
visibility::

  Quantifiers in
  types of types    Dependent     Non-dependent
                 +--------------+---------------+
        Visible  | forall a ->  |  a ->         |
                 +--------------+---------------+
      Invisible  | forall a.    |  c =>         |
                 +--------------+---------------+

On the other hand, in types of terms, our grid is incomplete::

  Quantifiers in
  types of terms    Dependent     Non-dependent
                 +--------------+---------------+
        Visible  |              |  a ->         |
                 +--------------+---------------+
      Invisible  | forall a.    |  c =>         |
                 +--------------+---------------+

Other than making terms and types more symmetrical, filling this empty cell
would let us design better APIs without the use of proxy types or ambiguous
types, and with better error messages.

For example, consider a function that gives the memory residence for a type::

  sizeOf :: forall a. Sized a => Proxy a -> Int

To find out the size of a boolean value, the user of this API would write
``sizeOf (Proxy :: Proxy Bool)`` or ``sizeOf (Proxy @Bool)``. This has two disadvantages:

* Constructing a ``Proxy`` value is unnecessarily verbose, making ``sizeOf``
  clunky to use.

* The ``Proxy`` value is passed at runtime. Even if the optimizer can eliminate
  it sometimes, there are cases when it cannot.

There is a workaround which involves ``AllowAmbiguousTypes`` and
``TypeApplications``. Here's an alternative API design::

  sizeOf :: forall a. Sized a => Int

The user is supposed to use a visibility override, ``sizeOf @Bool``. While it
does address the concerns about verbosity and the runtime cost, the error
messages degrade significantly. The invisible parameter ``a`` is now ambiguous,
so if the user forgets to specify it, the compiler tries to infer ``a`` and
inevitably fails::

  print_int :: Int -> IO ()

  -- Valid code:
  main = print_int (sizeOf @Bool)

  -- The parameter is not specified, extremely bad error message:
  --
  --    • Ambiguous type variable ‘a0’ arising from a use of ‘sizeOf’
  --      prevents the constraint ‘(Sized a0)’ from being solved.
  --      Probable fix: use a type annotation to specify what ‘a0’ should be.
  --      These potential instance exist:
  --        instance [safe] Sized Bool -- Defined at <interactive>:15:10
  --    • In the first argument of ‘print_int’, namely ‘sizeOf’
  --      In the expression: print_int sizeOf
  --      In an equation for ‘main’: main = print_int sizeOf
  --
  main = print_int sizeOf

It also means that eta-reduction is not possible::

  -- Valid code:
  mySizeOf :: forall a. Sized a => Int
  mySizeOf @a = sizeOf @a

  -- Eta-reduction attempt fails:
  --
  --  • Could not deduce (Sized a0) arising from a use of ‘sizeOf’
  --    from the context: Sized a
  --      bound by the type signature for:
  --                 mySizeOf :: forall a. Sized a => Int
  --    The type variable ‘a0’ is ambiguous
  --
  mySizeOf :: forall a. Sized a => Int
  mySizeOf = sizeOf


If we had visible ``forall``, for which there is already precedent in types of
types, we could design an API for ``sizeOf`` that has none of the issues listed
above::

  sizeOf :: forall a -> Sized a => Int

This type captures the intent behind this function, and, if we allow it, its
use would have the least noise and good error messages::

  print_int :: Int -> IO ()

  -- Valid code:
  main = print_int (sizeOf Bool)   -- NB: no visibility override '@'


  -- The parameter is not specified, good error message:
  --
  --    • Couldn't match expected type ‘Int’
  --                with actual type ‘forall a -> Sized a => Int’
  --    • Probable cause: ‘sizeOf’ is applied to too few arguments
  --      In the first argument of ‘print_int’, namely ‘sizeOf’
  --      In the expression: print_int sizeOf
  --      In an equation for ‘main’: main = print_int sizeOf
  --
  main = print_int sizeOf

Eta-reduction is now possible::

  -- Valid code:
  mySizeOf :: forall a -> Sized a => Int
  mySizeOf a = sizeOf a

  -- Eta-reduction attempt succeeds:
  mySizeOf :: forall a -> Sized a => Int
  mySizeOf = sizeOf

The proposed visible ``forall`` would be an irrelevant quantifier. However, if
we were to make it relevant, we would get full-blown dependent functions
(pi-types). Therefore, implementing this feature would pave the road for future
work on Dependent Haskell.

To summarize, there are three reasons to make this change:

* Language consistency (symmetry between terms and types)
* Ability to design better APIs (good error messages, no proxy types, no ambiguous types)
* Prepare the compiler internals for further work on dependent types

Definitions
-----------

The Resolved Syntax Tree
~~~~~~~~~~~~~~~~~~~~~~~~

Define **resolved syntax tree** as a representation of a Haskell program
that encodes its syntactic and binding structure, but does not yet include type
information. In particular, in the **resolved syntax tree**, the following
information has been fully determined:

* Variable and type variable occurrences have been linked to their bindings, in
  accordance with shadowing and punning rules.

  * Shadowing. Consider the following program::

      a = 42
      f a = \a -> a

    In the resolved syntax tree, the occurrence of ``a`` has been linked to
    its binding as follows::

      a₀ = 42
      f a₁ = \a₂ -> a₂

    Thus, we know it stands for ``a₂`` rather than ``a₁`` or ``a₀``.

  * Punning. Consider the following program:
    ::

      {-# LANGUAGE ScopedTypeVariables #-}
      id :: forall a. a -> a
      id a = (a :: a)

    In the resolved syntax tree, the occurrences of ``a`` have been linked to
    their bindings as follows:
    ::

      id :: forall aₜ. aₜ -> aₜ
      id aₑ = (aₑ :: aₜ)

* Data constructor and type constructor occurrences have been linked to their
  bindings, in accordance with the punning rules. Consider the following
  program:
  ::

      data Pair a b = Pair !a !b

      dup :: a -> Pair a a
      dup x = Pair x x

  In the resolved syntax tree, the occurrences of ``Pair`` have been linked to
  their bindings as follows::

      data Pairₜ a b = Pairₑ !a !b

      dup :: a -> Pairₜ a a
      dup x = Pairₑ x x

* The fixity and associativity of infix operators have been determined. Consider
  the following program:
  ::

    import Prelude ((+), (*))
    f x = x + x * x * x

  In the resolved syntax tree, the structure of the infix expression is
  established as follows:
  ::

    f x = x + ((x * x) * x)

* The meaning of built-in tuple syntax has been determined. Let us denote a
  pair as ``(a, b)ₑ`` and the type of a pair as ``(a, b)ₜ``. Now consider the
  following program:
  ::

    p :: (Integer, String)
    p = (42, "Hello")

  In the resolved syntax tree, the meaning of the built-in tuple syntax has
  been determined as follows:
  ::

    p :: (Integer, String)ₜ
    p = (42, "Hello")ₑ

  Likewise, for all tuple arities (including the unit type ``()`` as a 0-arity
  tuple).

* The meaning of built-in list syntax has been determined. Let us denote
  a singleton list as ``[a]ₑ`` and the list type as ``[a]ₜ``. Now consider the
  following program:
  ::

    f :: a -> [a]
    f x = [x]

  In the resolved syntax tree, the meaning of the built-in list syntax is
  determined as follows::


    f :: a -> [a]ₜ
    f x = [x]ₑ

  This also applies to the empty square brackets ``[]``, which can either stand
  for an empty list ``[]ₑ`` or the list type constructor ``[]ₜ``.

  With ``DataKinds``, the ``'[a]`` syntax in a type-level context is
  resolved as ``[a]ₑ``; in a term-level context, this syntax is not
  available.

* The meaning of ``*`` has been determined. It can stand for one of the following:

  1. ``Type`` from the ``Data.Kind`` module (under ``-XStarIsType``)
  2. An occurrence of a term-level ``(*)`` infix operator
  3. An occurrence of a type-level ``(*)`` infix operator (under ``-XTypeOperators``)

* The meaning of ``'`` has been determined. It can stand for one of the following:

  1. Namespace selection syntax (under ``-XDataKinds``)
  2. Name quotation syntax (under ``-XTemplateHaskell``)

Compilation Phases
~~~~~~~~~~~~~~~~~~

Define **typed syntax tree** as a representation of a Haskell program that
encodes its syntactic and binding structure, and assigns a type to every
expression, and a kind to every type.

In particular, we have two distinct relations:

* ``e ::ₑ t`` holds when the expression ``e`` has type ``t``
* ``t ::ₜ k`` holds when the type ``t`` has type (kind) ``k``

Compilation of a Haskell program includes, but is not limited to, the following
steps:

1. *Parsing and name resolution:* map the program text to a resolved syntax tree (defined above).
2. *Type checking:* map a resolved syntax tree to a typed syntax tree (defined above).

Syntactic Categories
~~~~~~~~~~~~~~~~~~~~

Haskell 2010 defines the following syntactic categories:

1. Expressions::

     e ::=
       | var                     -- variable occurrence
       | Con                     -- data constructor occurrence
       | e₀ e₁                   -- function application
       | if e₀ then e₁ else e₂   -- conditional expression
       | \p₀..pₙ -> e            -- lambda abstraction
       | ...

2. Patterns::

     p ::=
       | var          -- variable binding
       | Con p₀..pₙ   -- data constructor pattern
       | _            -- wildcard pattern
       | ...

3. Types::

     t ::=
       | tv                  -- type variable occurrence
       | TyCon               -- type constructor occurrence
       | t₀ t₁               -- type application
       | t₀ -> t₁            -- function type
       | ...

4. Kinds::

     k ::=
       | *            -- the kind of inhabited types
       | k₀ -> k₁     -- kind arrow


The GHC dialect of Haskell extends these syntactic categories with many new
constructs, for example overloaded label ``#lbl`` in expressions, explicit
``forall a. t`` in types, and so on. GHC also merges types and kinds into a
single syntactic category, so that we have::

  e ::= var | Con | e₀ e₁ | ...
  p ::= var | var | Con p₀..pₙ | _ | ...

  t, k ::=
       | *                   -- the kind of inhabited types
       | t₀ -> t₁            -- function type / kind arrow
       | tv                  -- type variable occurrence
       | TyCon               -- type constructor occurrence
       | t₀ t₁               -- type application
       | ...

This proposal is based on the current syntactic categories of GHC, where types
and kinds are merged, but types and expressions remain distinct.

Proposed Change Specification
-----------------------------

Primary Change
~~~~~~~~~~~~~~

1. Add a new language extension, ``RequiredTypeArguments``. When
   ``RequiredTypeArguments`` is in effect, lift the restriction that the
   ``forall a ->`` quantifier cannot be used in types of terms.

2. In types of terms, ``forall a ->`` is an irrelevant quantifier.
   This differs from its semantics in types of types, where it is relevant, but
   follows the precedent of ``forall a.``, which is also an irrelevant
   quantifiers in types of terms, but a relevant quantifier in types of types::

                            forall a.     forall a ->
                        +--------------+---------------+
        types of types  |   relevant   |   relevant    |
                        +--------------+---------------+
        types of terms  |  irrelevant  |  irrelevant   |
                        +--------------+---------------+

   Resolving this inconsistency between terms and types is left as future work.

3. Decouple the syntactic distinction and the semantic distinction between
   type and term variables.

   The syntactic distinction exists in the **parsing and name resolution**
   phase (defined above) and only affects the scope of a variable. We also
   refer to it as the **namespace** assigned to a variable.

   The semantic distinction exists in the **type checking** phase (defined
   above) and determines the **relevance** of the variable. Data variables are
   bound by relevant quantifiers, type variables are bound by irrelevant
   quantifiers.

   Before this proposal, the syntactic and semantic categories of variables
   used to coincide: all data variables (relevant) used names from the data
   namespace, and all type variables (irrelevant) used names from the type
   namespace. This is no longer the case.

4. Extend the syntactic categories of expressions and patterns with type
   variables and type constructors::

     e ::=
           | var                     -- variable occurrence
           | Con                     -- data constructor occurrence
     (NEW) | tv                      -- type variable occurrence
     (NEW) | TyCon                   -- type constructor occurrence
           | e₀ e₁                   -- function application
           | if e₀ then e₁ else e₂   -- conditional expression
           | \p₀..pₙ -> e            -- lambda abstraction
           | ...

     p ::=
           | var    -- variable binding
           | Con    -- data constructor occurrence
     (NEW) | TyCon  -- type constructor occurrence
           | _      -- wildcard pattern
           | ...

   A variable bound in a pattern is always assigned the data namespace, never
   the type namespace. This is a syntactic distinction (see point 3), and in
   the type checking environment it is possible that such a variable will map
   to a type variable (irrelevant).

   Extend the syntactic categories of types and kinds with term variables::

     t, k ::=
            | *                   -- the kind of inhabited types
            | t₀ -> t₁            -- function type / kind arrow
            | tv                  -- type variable occurrence
            | TyCon               -- type constructor occurrence
      (NEW) | var                 -- variable occurrence
            | Con                 -- data constructor occurrence   (pre-existing, see DataKinds)
            | t₀ t₁               -- type application
            | ...

   In the *parsing and name resolution* phase (defined above):

   * In expressions and patterns, prioritize the data namespace over the type
     namespace. That is, in ``f T = T``, if there are both a type constructor
     ``T`` and a data constructor ``T`` in scope, this is resolved in favor of
     the data constructor; however, if only the type constructor is in scope,
     do not error and resolve the reference as a type constructor occurrence.
     Likewise for variables and type variables.

   * In types and kinds, prioritize the type namespace over the data namespace.
     Guard the data variable occurrences behind the ``RequiredTypeArguments``,
     as it is a breaking change with regards to implicit quantification.
     Recall that we implicitly quantify over type variables that are free in
     the type, and consider this example::

       a = 42
       f :: Proxy a -> Proxy b

     Here, ``a`` in the type of ``f`` is no longer free, as it refers to the
     top-level term variable ``a = 42`` (``b`` is unaffected). Without
     ``RequiredTypeArguments``, the old behavior is retained.

   In the *type checking* phase (defined above), we continue to require that
   data variables/constructors (bound by relevant quantifiers) are used exclusively at the
   term level, and type variables (bound by irrelevant quantifiers) are used
   exclusively at the type level. Thus, these examples are ill-typed::

     f _ = Int                         -- invalid occurrence of "Int" in a term-level position
     a = 42; f :: Proxy a -> Proxy b   -- invalid occurrence of "a" in a type-level position

   While these examples are accepted::

     p = f Int           -- the input to "f" is a type, so the occurrence of "Int" is valid

     f :: forall a -> Typeable a => TypeRep a
     f x = typeRep @x    -- the input to "f" is a type, so the occurrence of "x" is valid

5. When in the checking mode of bidirectional type checking (e.g. in a function
   binding with an explicit type signature), allow a pattern to bind type
   variables in the data namespace, such as ``x`` here::

     f :: forall a -> ...
     f x = ...

   In the *parsing and name resolution* phase, ``a`` is a type variable,
   whereas ``x`` is a term variable. In the *type checking* phase, both ``x``
   and ``a`` refer to a single entity in the type checking environment: an
   irrelevant, type-level, ``forall``-bound type variable.

   A pattern that introduces such a variable must be either a variable name
   ``v`` or a wildcard ``_``, never a constructor match ``Con p₀..pₙ`` as it is
   impossible to pattern match on irrelevant variables.

   We never infer ``forall a -> ...``. For a pattern to introduce a type
   variable, we must be in checking mode during bidirectional type checking
   (see below).

6. In type checking, we alternate between two distinct modes: *checking* and
   *inference*. This idea, called bidirectional type checking, is presented in
   more detail in `"A quick look at impredicativity" <https://www.microsoft.com/en-us/research/uploads/prod/2020/01/quick-look-icfp20.pdf>`_.

   * In inference mode, we never infer ``forall x -> t``.

   * In checking mode, in a function application chain ``f e1 e2 e3``, we
     follow the rules shown in Figure 4 of "A quick look at impredicativity",
     extended as follows::

        rho = t2t(e)
        G |- sigma[rho/a]; pis  ~>  Theta; phis; rho_r
        ---------------------------------------------------------  ITVDQ
        G |- (forall a -> sigma); e,pis  ~>  Theta; phis; rho_r

     See "T2T-Mapping" below for an informal definition of ``t2t``.

     In other words, given ``f :: forall a -> t``, the ``x`` in ``f x`` is
     parsed and renamed as a term, but then mapped to a type.

T2T-Mapping
~~~~~~~~~~~

T2T (term-to-type) is a mapping from the syntactic category of expressions to
the syntactic category of types. It is performed during type checking and
operates on the resolved syntax tree (defined above).

* Variables and constructors (regardless of their namespace) are present in
  both syntactic categories and are mapped directly, without modification.

  * In the type checking environment, the variable must stand for a type variable,
    or else it is rejected.

  * In the type checking environment, the constructor must stand for a type
    constructor, or else require ``DataKinds``.

  * In the type checking environment, there should be no variable of the same
    name but from a different namespace, or else raise an ambiguity error (does
    not apply to constructors).

* Function application ``e₀ e₁`` is mapped to type-level function
  application ``t₀ t₁``, where ``t₀ = t2t(e₀)``, ``t₁ = t2t(e₁)``.

* With ``DataKinds``, a numeric literal ``42`` is mapped to a promoted numeric
  literal.

* With ``DataKinds``, a string literal ``"Hello"`` is mapped to a promoted
  string literal ``"Hello"``.

* A fractional numeric literal ``3.14`` cannot be mapped at the
  moment, as we do not have promoted fractional numeric literals.

* An unboxed numeric literal ``1337#`` cannot be mapped at the moment,
  as we do not have promoted unboxed types.

* A character literal ``'x'`` cannot be mapped at the moment, as we do
  not have promoted character literals.

* With ``DataKinds``, a tuple ``(e₀, e₁, ...)ₑ`` is mapped to a promoted tuple
  ``(t₀, t₁, ...)ₑ``, where ``t₀ = t2t(e₀)``, ``t₁ = t2t(e₁)``.

* An unboxed tuple ``(# a, b #)`` cannot be mapped at the moment, as we do not
  have promoted unboxed types.

* With ``DataKinds``, a list literal ``[e₀, e₁, ...]`` is mapped to a promoted
  list ``[t₀, t₁, ...]``, where ``t₀ = t2t(e₀)``, ``t₁ = t2t(e₁)``.

* With ``TypeApplications``, type application ``e₀ @t₁`` is mapped to
  type-level type application ``t₀ @t₁``, where ``t₀ = t2t(e₀)``.

* With ``TypeOperators``, infix application ``e₀ op e₁`` is mapped to
  type-level infix application ``e₀ tyop e₁``, where ``t₀ = t2t(e₀)``, ``t₁ =
  t2t(e₁)``, ``tyop = t2t(op)``.

* With ``KindSignatures``, a type signature ``e₀ :: t₁`` is mapped to a kind
  signature ``t₀ :: t₁``, where ``t₀ = t2t(e₀)``.

* Lambda functions ``\x -> b`` are not mapped and their use is an
  error, as we do not have type-level lambdas at the moment.

* Case-expressions ``case x of ...`` are not mapped and their use is
  an error, as we do not have type-level case-expressions.

* If-expressions ``if c then a else b`` are not mapped and their use
  is an error, as we do not have type-level if-expressions.

* In the same spirit, other syntactic constructs are mapped when
  there's a direct type-level equivalent, and their use is an error
  otherwise.

T2T preserves the binding structure and the meaning of the syntactic constructs
of the resolved syntax tree. For example, in ``f T`` it will never change
whether the ``T`` refers to a type constructor or a data constructor. Likewise,
it will not change ``[a]`` from a singleton list to the list type, or vice
versa. The mapping is as direct as possible and could be removed if we
had a single syntactic category for terms and types.

Secondary Change: ``type`` herald
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

7. Extend the syntactic categories of expressions and patterns as
   follows::

        e ::=
          | type t
          | ...

        p ::=
          | type t
          | ...

   Guard it behind the ``ExplicitNamespaces`` extension.

   Extend the T2T mapping as follows:

   * ``type t`` maps to ``t``.

   Any occurrence of ``type t`` that does not undergo the T2T mapping is
   rejected during type checking.

Secondary Change: Types-in-Terms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

8. Extend the term-level syntax with several constructs that previously could
   only occur at the type level:

   * Function arrows: ``a -> b``
   * Multiplicity-polymorphic function arrows: ``a %m -> b`` (under ``-XLinearTypes``)
   * Constraint arrows: ``a => b``
   * Universal quantification: ``forall a. b``
   * Visible universal quantification: ``forall a -> b``.

   We will call them "types-in-terms".

   Grammatically, their constituents are terms, not types::

                proposed grammar:                 as opposed to:
         +-------------------------------+----------------------------------+
         |                               |                                  |
         |  e ::=                        |    e ::=                         |
         |      | e₀ -> e₁               |        | t₀ -> t₁                |
         |      | e₀ => e₁               |        | t₀ => t₁                |
         |      | forall b₀..bₙ. e       |        | forall b₀..bₙ. t        |
         |      | forall b₀..bₙ -> e     |        | forall b₀..bₙ -> t      |
         |                               |                                  |
         +-------------------------------+----------------------------------+

   This is a necessity to avoid parsing conflicts, with the following
   consequences:

   1. The ``'`` symbol signifies Template Haskell name quotation rather than ``DataKinds`` promotion.
   2. The ``*`` symbol is treated as an infix operator regardless of ``-XStarIsType``.
   3. Built-in syntax for tuples and lists is interpreted as in terms.
      That is, ``[a]`` is a singleton list rather than the type of a list,
      and ``(a, b)`` is a pair rather than the type of a pair.

   For the above grammar to work, make ``forall`` a keyword at the term
   level. Not guarded by any extension (same motivation as `#193
   <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0193-forall-keyword.rst>`_).

   In name resolution, we follow the term-level rules that prioritize data
   constructors over type constructors, and variables over type variables.

   In types-in-terms, the ``forall``-bound type variables are put in the
   term-level namespace, so that in ``\x -> f (forall x. x)``, the ``x``
   occurrence refers to the forall-bound type variable rather than the
   lambda-bound variable.

   Extend the T2T mapping as follows:

   * The types-in-terms (such as ``a -> b``, ``a => b``, ``forall a. b``) are
     mapped to types directly, without modification aside from recursively
     processing subterms.

   Any occurrence of types-in-terms that does not undergo the T2T mapping is
   rejected during type checking.

9. When ``ViewPatterns`` are enabled, interpret ``f (a -> b) = ...``
   as a view pattern, otherwise as ``f ((->) a b) = ...``.

10. ``case ... of x -> y -> z`` is an error. We require parentheses to
    disambiguate:

    * ``case ... of (x -> y) -> z``
    * ``case ... of x -> (y -> z)``

Secondary Change: Lists and Tuples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

11. Introduce a new extension, ``ListTupleTypeSyntax``, on by default,
    which enables:

    * ``[]`` as the list type constructor
    * ``()`` as the unit type
    * ``[a]`` the syntax of a list type
    * ``(,)`` as the pair type constructor
    * ``(a, b)`` as the syntax of a pair type

    When the extension is on, these constructs retain their Haskell 2010
    meaning, which depends on whether we are in a type-level or term-level
    context.

    When the extension is off, all of the above are interpreted as in terms:

    * ``[]`` is always an empty list
    * ``()`` is always the unit data constructor
    * ``[a]`` is always a singleton list (not the list type)
    * ``(,)`` is always the pair data constructor (not the type constructor)
    * ``(a, b)`` is always a pair (not the type of a pair)

    Likewise for tuples of higher arities.

    Export the following synonyms from the ``GHC.Exts`` module::

      type List = []
      type Unit = ()
      type Tuple2 = (,)
      type Tuple3 = (,,)
      type Tuple4 = (,,,)
      ... -- up to the maximum tuple arity

Examples
--------

Compile-time color literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Definition site::

  type family ParseRGB (s :: Symbol) :: (Nat, Nat, Nat) where
    ...

  type KnownRGB :: (Nat, Nat, Nat) -> Constraint
  class KnownRGB c where
    _rgbVal :: (Word8, Word8, Word8)

  rgb :: forall s -> KnownRGB (ParseRGB s) => (Word8, Word8, Word8)
  rgb s = _rgbVal @(ParseRGB s)

Use site::

  ghci> rgb "red"
  (255, 0, 0)

  ghci> rgb "#112233"
  (17, 34, 51)

  ghci> rgb "asdfasdf"
  -- custom type error from ParseRGB

Corner Cases
~~~~~~~~~~~~

1. Scoped type variables:
   ::

     f :: forall a. [a] -> [a]
     f x = g a x

   Here the ``a`` in the first argument to ``g`` is not rejected; rather it is
   an occurrence of the lexically scoped type variable ``a`` bound by the
   ``forall`` in ``f``'s type signature. If ``g`` turns out to have a visible
   dependent type, the argument will be converted to a type; if not, it will be
   rejected.

2. Punning and a local variable:
   ::

     f :: forall a. [a] -> [a]
     f a = g a a

   Here both ``a`` arguments to ``g`` are bound to the inner term-level ``a`` binder (``f``'s
   argument), regardless of the type of ``g``.

3. Punning and a top-level variable:
   ::

     a :: Int
     a = 3

     f :: forall a. [a] -> [a]
     f x = g a a

   Both ``a`` arguments to ``g`` are bound to term-level bidning for ``a``.  In
   terms, a term-level binding "wins". If ``g`` turns out to have a visible
   dependent type, the program will be rejected because ``g``'s first argument
   is a type, not a term.

4. Punning and types-in-terms:
   ::

      f :: forall a. [a]->[a]
      f a = g (a -> (forall b. b -> a)) a

   Again, all those ``a``'s in ``g``'s arguments are bound to the term-level ``a``.
   The clues that we are in a type, from the ``(->)`` and ``forall``, are not
   used to change the namespace.

5. Punning and shadowing:
   ::

     h a = g (forall a. a->a) a

   The ``forall`` binds ``a`` and that binding is seen by the occurrences in ``a->a``.
   That is, in a term the forall-bound variables are in the data namespace.

6. Built-in syntax:
   ::

     x1 = g (Int,Bool)
     x2 = g [Int]

   Here, the built-in syntax occurs in a term-level context, so ``(Int,Bool)``
   is a promoted pair, and ``[Int]`` is a promoted singleton list.

   One way to change this is to use synonyms from ``GHC.Exts``::

     x1 = g (Tuple2 Int Bool)
     x2 = g (List Int)

   Another way is to use the ``type`` herald::

     x1 = g (type (Int,Bool))
     x2 = g (type [Int])

   This is purely a matter of style.

7. The ``@`` changes the meaning of built-in syntax:
   ::

      a = f @(Int,Bool)
      b = g  (Int,Bool)

   In ``a``, the argument is the pair type, in ``b`` it is a promoted pair.

   One way to resolve this is to use synonyms from ``GHC.Exts``::

      a = f @(Tuple2 Int Bool)
      b = g  (Tuple2 Int Bool)

   Another way is to use the ``type`` herald::

      a = f @(Int,Bool)
      b = g (type (Int,Bool))

8. The ``@`` changes the namespace:
   ::

     data StrictPair a b = StrictPair !a !b

     x = f  (StrictPair Int Bool)
     y = g @(StrictPair Int Bool)

   Resolved with the ``type`` herald or by renaming one of the ``StrictPair``
   constructors.

9. Type variables as function parameters:
   ::

     f :: forall a -> a -> a
     f x y = g True
       where
         g :: b -> x
         g _ = y

   Here, ``x`` is a name in the term namespace, but it is in fact a type
   variable, later used used in the type signature of ``g``.

   The ``b`` is bound implicitly in this example, assuming there's no top-level
   definition of ``b``. To make it clean, one can use an explicit ``forall``::

     ... where
             g :: forall b. b -> x
             g _ = y

   This is similar to the situation with ``ScopedTypeVariables``, where we also
   cannot assume that all lowercase variables in a signature are free.

Effect and Interactions
-----------------------

* Visible ``forall`` becomes available in types of terms, making them more
  similar to types of types. There remains a discrepancy that ``forall`` in
  types of types is actually a relevant quantifier, while the proposed ``forall
  x ->`` in types of terms is irrelevant. This is to be resolved in the future
  by making both of them irrelevant.

* The renaming of a visible dependent argument is different than that of a
  dependent argument with a visibility override. Consider this code::

    f :: forall a.   Tagged a ()
    g :: forall a -> Tagged a ()

    data T = T

    a = f @T
    b = g  T

  In ``f @T``, we refer to the type constructor, but in ``g T`` we refer to the
  data constructor.

  The implementation may offer warning flags to help the user identify such
  ambiguous occurrences.

* When punned names come from external libraries, there are several workarounds
  to resolve the ambiguity:

  1. Using ``-XExplicitNamespaces``::

      import Data.Proxy
      import qualified Data.Proxy (type Proxy) as T

      x = f   Proxy  -- refers to the data constructor
      y = f T.Proxy  -- refers to the type constructor

  2. Using a type synonym::

      import Data.Proxy

      type TProxy = Proxy

      x = f  Proxy  -- refers to the data constructor
      y = f TProxy  -- refers to the type constructor

  3. Using the ``type`` herald::

      import Data.Proxy

      x = f Proxy
      y = f (type Proxy)

* Identifiers bound to terms are
  not promoted. Consider this well-typed program::

    f :: forall a.   Tagged a ()
    g :: forall a -> Tagged a ()

    a = f @(Just True)  -- ok
    b = g  (Just True)  -- ok

  If we factor out ``Just True`` into a type synonym, it continues to work::

    type X :: Maybe Bool
    type X = Just True

    a = f @X  -- still OK
    b = g  X  -- still OK

  However, if we bind it to a term-level variable, the example becomes
  ill-typed::

    x :: Maybe Bool
    x = Just True

    a = f @x  -- not currently valid
    b = g  x  -- not valid under the proposal

  This is because we retain the distinction between terms and types. This
  proposal is a step towards dependent types, but it does not go all the way.
  Accepting the program above is left as future work.

Costs and Drawbacks
-------------------

This is one more feature to implement and maintain.

Alternatives
------------

1.  Include the proposed functionality in ``ExplicitForAll`` instead of
    introducing a new extension.

2.  The extension name could use a different name, such as ``-XVDQ`` or
    ``-XVisibleForAll``.

3.  We could guard type-level uses of visible ``forall`` behind the
    ``VisibleForAll`` extension flag. This would break existing code.

5.  Instead of the ``type`` herald, we could repurpose ``@`` as a syntactic
    marker that indicates types occurring within terms. That is, while ``forall
    x ->`` is a compulsory parameter and ``forall x.`` is not, the use sites
    would be ``f @Int`` in both cases.

    There are several issues with this alternative:

    * it creates more syntactic noise in the unambiguous cases (e.g. ``f Int``,
      assuming no data constructor named ``Int``)
    * it is inconsistent with what we have in types where ``@`` is used as a
      visibility override
    * it does not move us towards a single syntax for types and terms, which would
      be an advantage when we have dependent types
    * The dual purpose of ``@`` as both a visibility override and a namespace
      specifier would lead to unwanted interference between ``forall x.`` and
      ``forall x ->``. For example, given ``f :: forall k. forall (a::k) ->
      blah``, it wouldn't be possible to specify ``a=Int`` as ``f @Int``;
      one would have to write ``f @_ @Int`` or change the type of ``f`` to ``f
      :: forall {k}. forall (a::k) -> blah``.

    Richard Eisenberg characterizes this alternative as follows:

      It moves us away from uniformity. Let's even pretend for a moment that
      I'm not trying to actually merge the term-level and type-level.

      Right now, we can say this::

        type VDQ :: forall k1. forall k2 -> k1 -> k2 -> Type
        data VDQ k2 a b

        type VDQIntTrue = VDQ @Type Bool Int True
        type VDQCharFalse = VDQ Bool Char False

      If we were to require the ``@`` in terms, the term-level equivalent would be::

        vid :: forall a. forall b -> a -> b -> ()
        vid _ _ _ = ()

        ex1 = vid @Int @Bool 3 True
        ex2 = vid @_ @Bool 'x' False

      These look different! Why different syntaxes for the same idea?

      Worse, imagine a data constructor::

        data Silly a b where
          Mk :: forall a. forall b -> a -> b -> Silly a b

      Now we have this oddity::

        type Different1 = Mk @Nat Bool 3 True
        type Different2 = Mk Bool "hi" False
        different3 = Mk @Int @Bool 3 True
        different4 = Mk @_ @Bool "hi" False

      Here, the right-hand sides should be *the same*, but they have to be
      different.

      Today, we have non-uniformity by omission: we have no visible ``forall`` in
      types of terms. But with your proposed ``@`` on required dependent
      arguments, we would have active non-uniformity, which seems worse as it
      paints us into a corner that's difficult to escape from. At least
      non-uniformity by omission can, in theory, be fixed uniformly, later.

6. A previous iteration of this proposal dictated to switch to a type-level
   name resolution context when processing types-in-terms; we could also parse
   the right-hand side of ``forall a. t`` as a type; and we could map the
   ``forall`` in terms bind variables in the type namespace.

   The parsing and name resolution rules of these alternatives were deemed to
   subtle, so we opted for a design where types-in-terms are parsed and renamed
   as ordinary terms.

7. We could error on ambiguous variable occurrences earlier in the pipeline, in
   the renamer, but then enabling ``RequiredTypeArguments`` would result in
   rejecting currently valid code::

    id :: forall a. a -> a
    id a = (a :: a)

   Instead, we opted to raise the ambiguity error during T2T.

Unresolved Questions
--------------------

None at the moment.

Implementation Plan
-------------------

I (Vladislav Zavialov) or a close collaborator will implement this change.
There's currently a prototype by Daniel Rogozin in the works.
