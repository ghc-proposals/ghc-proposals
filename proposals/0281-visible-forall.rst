Visible `forall` in types of terms, and types in terms
======================================================

.. author:: Vladislav Zavialov
.. date-accepted:: 2021-11-01, amended 2024-01-15
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/281>`_ and `amended by #626 <https://github.com/ghc-proposals/ghc-proposals/pull/281>`_.
.. sectnum::
.. contents::

We propose to allow visible erased dependent quantification, written as
``forall x ->``, in types of terms.

Background
----------

This proposal closely follows the design sketch laid out in the accepted proposal
`#378 Design for Dependent Types <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_.
It may help to read that design sketch first.

In particular, we use the following terminology:

Type syntax
  Parts of the program that are parsed with the ``type`` family of
  non-terminals (i.e. ``type``, ``btype``, ``atype``, etc) or the ``kind``
  non-terminal are considered to be written in **type syntax**.

  This includes (but is not limited to) types in type or class declarations,
  types after the ``::`` in a type or kind signature, types after the ``@``
  sign in visible type applications, the LHS and the RHS of type family
  instances, and so on.

Term syntax
  Parts of the program that are parsed with the ``exp`` family of
  non-terminals (i.e. ``exp``, ``infixexp``, ``fexp``, ``aexp``, etc) or
  the ``pat`` family of non-terminals (i.e. ``pat``, ``apat``, etc) are
  considered to be written in **term syntax**.

  This includes (but is not limited to) the LHS and the RHS of function
  bindings ``f x y = e``, the LHS and the RHS of pattern bindings ``p = e``,
  the LHS and the RHS of ``p <- e`` bindings in ``do``-notation, the top-level
  Template Haskell splices (e.g. ``makeLenses ''T``), and so on.

Namespace
  Any identifier (including variables, type variables, data constructors, and
  type constructors) is said to belong to one of the two namespaces: the **term
  namespace** and the **type namespace**. The namespace of an identifier is
  determined at its binding site.

  Identifiers bound in type syntax populate the type namespace; identifiers
  bound in term syntax populate the term namespace.

Punning
  Punning occurs when identifiers of the same spelling are introduced in both
  namespaces, for example::

    data T = T

  Now any use of ``T`` must use certain rules to disambiguate which ``T`` is
  referred to.

Quantifier
  Parts of a type that introduce a function parameter are called quantifiers:

  * ``forall a. ty``
  * ``forall a -> ty``
  * ``foreach a. ty``
  * ``foreach a -> ty``
  * ``Eq a => ty``
  * ``t1 -> t2``

  (To see how ``=>`` is a quantifier, one must desugar it with dictionary-passing style).

  We classify quantifiers along several axes:

  * Dependent or non-dependent
  * Erased or retained
  * Visible or invisible

Dependence
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

Erasure
  We call a quantifier retained when the parameter can be pattern-matched on or
  returned as part of the result, and, as a consequence, must be passed during
  evaluation. For example,

  ``a ->`` is a retained quantifier::

    id :: forall a. a -> a
    id = \x -> x
              ^^^
              'x' is returned as the result

    not :: Bool -> Bool
    not b =
      case b of { ... }
          ^^^
          'b' is used in pattern-matching

  On the other hand, in types of terms, ``forall a.`` is an erased quantifier::

    bad :: forall a. a -> a
    bad x =
      case a of { ... }
          ^^^
          'a' can not be pattern-matched on!

  However, in types of types, ``forall a.`` is currently a retained quantifier,
  as it permits pattern-matching::

    type IsMaybe :: forall k. k -> Bool
    type family IsMaybe a where
      IsMaybe @(Type -> Type) Maybe = True     -- matching 'k' with (Type -> Type)
      IsMaybe @Type (Maybe _) = True           -- matching 'k' with Type
      IsMaybe _ = False

  This is considered an oversight in the design of kind polymorphism, and we
  generally speak of ``forall x.`` as an erased quantifier. (Making it truly so
  is left as future work, out of scope of this proposal).

Visibility
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
                 ┌──────────────┬───────────────┐
        Visible  │ forall a ->  │  a ->         │
                 ├──────────────┼───────────────┤
      Invisible  │ forall a.    │  c =>         │
                 └──────────────┴───────────────┘

On the other hand, in types of terms, our grid is incomplete::

  Quantifiers in
  types of terms    Dependent     Non-dependent
                 ┌──────────────┬───────────────┐
        Visible  │              │  a ->         │
                 ├──────────────┼───────────────┤
      Invisible  │ forall a.    │  c =>         │
                 └──────────────┴───────────────┘

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

The proposed visible ``forall`` would be an erased quantifier. However, if
we were to make it retained, we would get full-blown dependent functions
(pi-types). Therefore, implementing this feature would pave the road for future
work on Dependent Haskell.

To summarize, there are three reasons to make this change:

* Language consistency (symmetry between terms and types)
* Ability to design better APIs (good error messages, no proxy types, no ambiguous types)
* Prepare the compiler internals for further work on dependent types

Proposal Structure
------------------

We shall present this proposal in two parts:

* In Part I we introduce the ``forall a ->`` quantifier in types of terms but
  also require a syntactic marker at use sites. This is not as convenient to
  use (i.e. users would have to write ``sizeOf (type Bool)`` instead of
  ``sizeOf Bool``), but is much easier to specify and understand.

* In Part II we specify when it is permissible to omit the ``type`` herald.
  This greatly increases the convenience of using the proposed feature, but
  also makes the specification more intricate.


Part I: Proposed Change Specification
-------------------------------------

1. Add a new language extension, ``RequiredTypeArguments``. When
   ``RequiredTypeArguments`` is in effect, lift the restriction that the
   ``forall a ->`` quantifier cannot be used in types of terms.

2. **Syntax**. When ``ExplicitNamespaces`` is in effect, extend the
   grammar (as in the `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_) as follows::

        exp ::=
          | 'type' atype
          | ...

        pat ::=
          | 'type' atype
          | ...

   The ``type`` keyword at the top-level is interpreted as it always has been; it
   does not start an expression (as would be used in a Template Haskell declaration
   splice) or pattern (as would be used in a pattern binding).

3. **Name resolution**. A type embedded into a term with the ``type`` marker
   follows type-level name resolution rules (i.e. uses of punned identifiers
   resolve to the type namespace), both at binding sites and at use sites.

   The ``ScopedTypeVariables`` extension has no effect on variables introduced
   by ``forall a ->``.

4. **Type checking**. In type checking, we alternate between two
   distinct modes: *checking* and *inference*. This idea, called bidirectional
   type checking, is presented in more detail in
   `"A quick look at impredicativity" <https://www.microsoft.com/en-us/research/uploads/prod/2020/01/quick-look-icfp20.pdf>`_.

   * In inference mode, we never infer ``forall x -> t`` as the type of a lambda expression.
     Accordingly, writing ``\ (type a) -> ...`` in inference mode is always an error.

   * In checking mode, in a function application chain ``f e1 e2 e3``, we
     follow the rules shown in Figure 4 of "A quick look at impredicativity",
     extended as follows::

        G |- sigma_b[a := sigma_a];                     pis  ~>  Theta; phis; rho_r
        ---------------------------------------------------------------------------  ITVDQ
        G |- (forall a -> sigma_b);     (type sigma_a), pis  ~>  Theta; phis; rho_r

   * In checking mode, in a function binding ``f (type x) = ...`` or a lambda
     ``\(type x) -> ...``, the ``x`` is a fresh skolem.

5. **Validity**. Expressions and patterns of form ``type t`` but not covered by
   the type checking rules above are illegal.

   Specifically, any expression of form ``type t`` must be used as an argument
   to a function, or else it is rejected with a type error::

     x = f (type Int)   -- OK
     x = type Int       -- invalid use of a type in a term

   This is checked during type checking, so Template Haskell is unaffected:
   ``[e| type Int |]`` and ``[p| type Int |]`` are both allowed but different
   from ``[t| Int |]``.


6. **Erasure**. In types of terms, ``forall a ->`` is an erased quantifier.
   Making ``forall a ->`` erased in types of types is out of scope of this
   proposal.

7. **Data constructors**. When ``RequiredTypeArguments`` is in effect, allow
   ``forall a ->`` in data constructor declarations.

   * The flavour of the parent declaration (data, data instance, newtype,
     newtype instance) does not affect the validity of ``forall a ->``.

   * The GADT constructor syntax (GHC Proposal `#402 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst>`_)
     supports ``forall a ->`` quantification both for universals and existentials::

       data T a b where
         MkT1 :: forall a b.   forall x y.   (x, y, a, b) -> T a b  -- OK
         MkT2 :: forall a b.   forall x y -> (x, y, a, b) -> T a b  -- OK
         MkT3 :: forall a b -> forall x y.   (x, y, a, b) -> T a b  -- OK
         MkT4 :: forall a b -> forall x y -> (x, y, a, b) -> T a b  -- OK

     In Haskell98-style syntax, all data constructors have invisible universal
     quantifiers, and it is illegal to specify visible existential quantifiers::

       data T a b =
         forall x y.   MkT1 (x, y, a, b)    -- OK, `forall a b.` is added by GHC
         forall x y -> MkT2 (x, y, a, b)    -- Illegal syntax

     There is no inherent reason to rule out ``forall a ->`` in Haskell98-style
     existentials other than to save on implementation costs (see "Alternatives").

   * The use of ``forall a ->`` in the type of a data constructor is compatible
     with ``DataKinds`` promotion of the said constructor.

   * Pattern matching on a data constructor ``MkT :: forall a -> ...`` implies
     the use of type patterns in positions that correspond to required type
     parameters::

       data Ex where { MkEx :: forall a -> Show a => a -> Ex }

       f :: Ex -> String
       f (MkEx (type a) x) = show (x :: a)
       --      ^^^^^^^^
       --      type pattern corresponding to (forall a ->) in the type of MkEx

   * The rules for checking type patterns in constructors are derived from the
     rules for checking type abstractions ``@a``, mutatis mutandis; for the
     latter, see GHC Proposal `#448 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst>`_.

Part I: Examples and Explanation
--------------------------------

1. A variant of ``id`` that uses visible ``forall``:
   ::

     -- Definition:
     idv :: forall a -> a -> a
     idv (type a) x = x :: a

     -- Usage:
     n = idv (type Double) 42

   This is equivalent to ``n = (42 :: Double)``.

2. A wrapper around ``typeRep`` that uses visible ``forall``:
   ::

     -- Definition:
     typeRepVis :: forall a -> Typeable a => TypeRep a
     typeRepVis (type a) = typeRep @a

     -- Usage:
     t = typeRepVis (type (Maybe String))

3. A wrapper around ``sizeOf`` that uses visible ``forall`` instead of ``Proxy``:
   ::

     -- Definition:
     sizeOfVis :: forall a -> Storable a => Int
     sizeOfVis (type a) = sizeOf (Proxy :: Proxy a)

     -- Usage:
     n = sizeOfVis (type Int)

4. A wrapper around ``symbolVal`` that uses visible ``forall`` instead of ``Proxy``:
   ::

     -- Definition:
     symbolValVis :: forall s -> KnownSymbol s => String
     symbolValVis (type s) = symbolVal (Proxy :: Proxy s)

     -- Usage
     str = symbolValVis (type "Hello, World")

5. A type pattern in a constructor pattern:
   ::

     data T a b where
       MkT :: forall a. forall b -> (a,b) -> T a b
       --               ^^^^^^^^^^^^
       --               visible forall in a data constructor

     f :: T a b -> (b, a)
     f (MkT @a (type b) c) = (snd c :: b, fst c :: a)
     --        ^^^^^^^^
     --        the corresponding type pattern

6. A type pattern in a constructor pattern that requires unification:
   ::

       data U a where { MkU :: forall a -> U a }

       f :: U (Maybe Int) -> Int
       f (MkU (type (Maybe a))) = 42 :: a

   The type pattern ``type (Maybe a)`` is unified with ``Maybe Int`` in the
   signature. As a result, ``a`` stands for ``Int`` in ``42 :: a``.

Note that as long as we limit ourselves to part I of this proposal, we need the
``type`` marker in all of the above examples, even when the argument is a
syntactically valid term. If the programer were to write ``symbolValVis "Hello, World"``,
they would get an error message stating that a term argument was
received where a type argument was expected. That's because our typing rule
``ITVDQ`` explicitly requires the argument to be of form ``type sigma``.

Could we extend our system to permit arguments without the ``type`` prefix?
That is precisely the subject of part II.

Part II: Definitions
--------------------

The Resolved Syntax Tree
~~~~~~~~~~~~~~~~~~~~~~~~

Define **resolved syntax tree** as a representation of a Haskell program
that encodes its syntactic and binding structure, but does not yet include type
information. In particular, in the **resolved syntax tree**, the following
information has been fully determined:

* Variable and type variable occurrences have been linked to their bindings, in
  accordance with shadowing and punning rules.

  * Shadowing. Consider the following program:
    ::

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

* The meaning of ``->`` has been determined. It can stand for one of the following:

  1. Part of lambda, case-of, or multi-if syntax, as in ``\x -> ...``
  2. The function type constructor ``(->)``, as in ``Int -> Bool``
  3. A view pattern ``f (e -> p) = ...`` (under ``-XViewPatterns``)

Part II: Proposed Change Specification
--------------------------------------

Syntax
~~~~~~

1. Extend the term syntax (expressions and patterns) with several constructs
   that previously could only occur at the type level:

   * Function arrows: ``a -> b``
   * Multiplicity-polymorphic function arrows: ``a %m -> b`` (under ``-XLinearTypes``)
   * Constraint arrows: ``a => b``
   * Universal quantification: ``forall a. b``
   * Visible universal quantification: ``forall a -> b``.

   We will call them **types-in-terms**.

   To that end, we change the grammar of expressions and patterns as follows.
   Start with these nonterminals based on the `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_,
   modified to account for pattern signatures, view patterns, and the ``type`` herald::

     exp ::=
       | 'type' ktype                    -- only with ExplicitNamespaces (see Part I of #281)
       | infixexp :: [context =>] type
       | infixexp

     viewpat ::=
       | pat
       | exp '->' viewpat                -- only with ViewPatterns

     pat ::=
       | 'type' ktype                    -- only with ExplicitNamespaces (see Part I of #281)
       | infixpat :: [context =>] type   -- only with PatternSignatures  (see #448)
       | infixpat

     infixexp ::=
       | lexp qop infixexp
       | - infixexp
       | lexp

     infixpat ::=
       | lpat qconop infixpat
       | lpat

   Introduce the following changes:

   * add new non-terminals ``infixexp2`` and ``infixpat2`` that include terms-in-types
   * update ``exp`` and ``pat`` to use the new non-terminals in place
     of ``infixexp`` and ``infixpat`` respectively
   * rearrange ``viewpat`` to avoid parsing conflicts

   ::

     exp ::=
       | 'type' ktype                    -- only with ExplicitNamespaces (see Part I of #281)
       | infixexp2 :: [context =>] type
       | infixexp2

     viewpat ::= pat  -- the other production has been moved to infixpat2

     pat ::=
       | 'type' ktype                    -- only with ExplicitNamespaces (see Part I of #281)
       | infixpat2 :: [context =>] type  -- only with PatternSignatures  (see #448)
       | infixpat2

     infixexp2 ::=
            | infixexp
      (NEW) | infixexp      '->'  infixexp2      -- only with RequiredTypeArguments
      (NEW) | infixexp mult '->'  infixexp2      -- only with RequiredTypeArguments
      (NEW) | infixexp      '->.' infixexp2      -- only with RequiredTypeArguments
      (NEW) | infixexp      '=>'  infixexp2      -- only with RequiredTypeArguments
      (NEW) | 'forall' tv_bndrs '.'  infixexp2   -- only with RequiredTypeArguments
      (NEW) | 'forall' tv_bndrs '->' infixexp2   -- only with RequiredTypeArguments

     infixpat2 ::=
            | infixpat
      (NEW) | infixpat      '->'  infixpat2      -- only with RequiredTypeArguments and /without/ ViewPatterns (conflict described below)
      (MOV) | infixexp      '->'  infixpat2      -- only with ViewPatterns (takes precedence over RequiredTypeArguments, conflict described below)
      (NEW) | infixpat mult '->'  infixpat2      -- only with RequiredTypeArguments
      (NEW) | infixpat      '->.' infixpat2      -- only with RequiredTypeArguments
      (NEW) | infixpat      '=>'  infixpat2      -- only with RequiredTypeArguments
      (NEW) | 'forall' tv_bndrs '.'  infixpat2   -- only with RequiredTypeArguments
      (NEW) | 'forall' tv_bndrs '->' infixpat2   -- only with RequiredTypeArguments

     -- infixexp and infixpat are unchanged

   Note that the constituents of terms-in-types use the term syntax.
   For example::

                   proposed grammar:                    as opposed to:
         ┌──────────────────────────────────────┬───────────────────────────────────┐
         │  infixexp2 ::=                       │  infixexp2 ::=                    │
         │    | infixexp '->' infixexp2         │    | btype '->' ctype             │
         │    | 'forall' tv_bndrs '.' infixexp2 │    | 'forall' tv_bndrs '.' ctype  │
         │    | ...                             │    | ...                          │
         └──────────────────────────────────────┴───────────────────────────────────┘

   The use of term syntax on the LHS of ``e1 -> e2`` and ``e1 => e2`` is a
   necessity to avoid parsing conflicts. The use of term syntax on the RHS is
   done for consistency with the LHS.

3. Make ``forall`` a keyword at the term level (in expressions and patterns).
   Not guarded by any extension (same motivation as `#193 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0193-forall-keyword.rst>`_).
   This implies ``forall`` is no longer a valid identifier.

   For three releases before this change takes place, include a new warning
   ``-Wforall-identifier`` in ``-Wdefault``. This warning will be triggered
   at definition sites (but not use sites) of ``forall`` as an identifier.

   This change applies to ``∀`` (the ``UnicodeSyntax`` rendition of ``forall``)
   as well.

4. Adding ``p1 -> p2`` to the syntax of patterns (see the BNF of "types-in-terms"
   above) is incompatible with ``ViewPatterns``.

   * When both ``RequiredTypeArguments`` and ``ViewPatterns`` are enabled,
     the conflict is resolved in favor of ``ViewPatterns``. The programmer can
     write ``type (t1 -> t2)`` or ``(->) t1 t2`` instead.

   * When only ``ViewPatterns`` is enabled,
     the conflict is resolved in favor of ``ViewPatterns``.

   * When only ``RequiredTypeArguments`` is enabled, the conflict is not
     resolved either way. It is reported to the user in an error message.
     This can be reconsidered if an alternative syntax for view patterns is
     implemented in GHC.

   To make the grammar of Haskell more regular and to simplify the
   implementation of "types-in-terms", change the precedence of view patterns
   relative to pattern signatures as follows::

        f (e -> p :: t)   = ...     -- user-written function LHS
        f (e -> (p :: t)) = ...     -- old parse
        f ((e -> p) :: t) = ...     -- new parse

   The new parse is consistent with kind signatures, e.g. ``Int -> Bool :: Type``
   is parsed as ``(Int -> Bool) :: Type``, not ``Int -> (Bool :: Type)``.

   For three releases before this change takes place, include a new warning
   ``-Wview-pattern-signatures`` in ``-Wdefault`` to warn on affected code.
   The warning text will suggest to parenthesize the RHS of the view pattern.
   See "Effect and Interactions" for the impact analysis.

5. ``case ... of x -> y -> z`` is an error. We require parentheses to
   disambiguate:

   * ``case ... of (x -> y) -> z``
   * ``case ... of x -> (y -> z)``

Name resolution
~~~~~~~~~~~~~~~

6. During name resolution,

   * Identifiers bound in term syntax populate the term namespace;
     identifiers bound in type syntax populate the type namespace.

     This is already the case, but now we generalize this rule to cover
     types-in-terms, which are considered term syntax.

   * When looking up an identifier ``v`` or ``V`` in type syntax, look it up
     in the type namespace first; if it is not found there, look it up in the
     term namespace.

     This is already the case for uppercase identifiers if ``DataKinds`` is
     enabled, but now we extend this rule to lowercase identifiers if
     ``RequiredTypeArguments`` is enabled.

   * When looking up an identifier ``v`` or ``V`` in term syntax, look it up
     in the term namespace first; if it is not found there, look it up in the
     type namespace.

     This is a new rule, but notice how it mirrors the one for type syntax.

Implicit quantification
~~~~~~~~~~~~~~~~~~~~~~~

7. Implicit quantification is an existing feature that allows the programmer to
   omit a ``forall``::

     g ::           a -> a    -- implicit
     g :: forall a. a -> a    -- explicit

   This sort of quantification only happens if the variable is not already in
   scope::

     {-# LANGUAGE ScopedTypeVariables #-}

     f :: forall a. a -> a
     f = ...
       where
         g :: a -> a         -- No implicit quantification!

   In other words, we quantify only over *free* variables.

   With the proposed changes to name resolution, variables that were previously
   free are not free anymore::

       a = 42
       f :: a -> a           -- No implicit quantification!

   This is a breaking change, and that is why the fallback to the term
   namespace in type syntax is guarded behind ``RequiredTypeArguments``.

   Without ``RequiredTypeArguments``, implicit quantification is not affected.

   In order to facilitate writing code that is forward-compatible with
   ``RequiredTypeArguments``, introduce a new warning to ``-Wcompat``: ``-Wterm-variable-capture``.
   This warning will notify users when implicit quantification occurs that
   would stop working under ``RequiredTypeArguments``.

Type checking
~~~~~~~~~~~~~

8. Generalize the ``ITVDQ`` rule introduced earlier
   by using ``t2t``::

     sigma_a = t2t(e)
     G |- sigma_b[a := sigma_a];         pis  ~>  Theta; phis; rho_r
     --------------------------------------------------------------- ITVDQ-T2T
     G |- (forall a -> sigma_b);      e, pis  ~>  Theta; phis; rho_r

   ``t2t`` transforms term arguments into type arguments, see the "T2T-Mapping"
   section for an informal definition of ``t2t``.

   That is, given ``f :: forall a -> t``, the argument ``e`` in ``f e``
   is parsed and renamed as a term, but then mapped to a type.

   In the same way, generalize the type checking rule for patterns to invoke
   ``t2t`` to transform a pattern to a type when the function or constructor has
   a visible forall in its type.

9.  Any uses of terms in types are ill-typed:
    ::

      a = 42; f :: Proxy a -> Proxy b   -- invalid occurrence of "a" in a type-level position

    Any uses of types in terms that do not undergo the T2T transformation are also ill-typed::
    ::

      f _ = Int                         -- invalid occurrence of "Int" in a term-level position

10. When in the checking mode of bidirectional type checking (e.g. in a function
    binding with an explicit type signature), allow a pattern to bind type
    variables in the term namespace, such as ``x`` here::

      f :: forall a -> ...
      f x = ...

    The ``x`` identifier is bound in the term namespace, but stands for an
    erased, ``forall``-bound type variable.

    A similar principle applies to subpatterns in a constructor pattern that
    correspond to ``forall a ->`` in the type of the data constructor::

       data Ex where { MkEx :: forall a -> Show a => a -> Ex }

       f :: Ex -> String
       f (MkEx a x) = show (x :: a)
       --     ^^^
       --     the `a` corresponds to `forall a ->` in the type of `MkEx`

    In general, we permit term patterns in positions where type patterns are
    expected by applying the T2T transformation, see "T2T-Mapping" below.

T2T-Mapping
~~~~~~~~~~~

T2T (term-to-type) is a mapping from terms (expressions or patterns) to types
that operates on a resolved syntax tree and is invoked by the ``T2T`` typing
rule.

The T2T mapping is partial: it succeeds on expressions and patterns that are within the
Static Subset (introduced in `#378 Design for Dependent Types
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_),
and fails on expressions outside of this subset.

Common T2T Clauses (Expressions and Patterns)
  * Embedded types ``type t`` are mapped to ``t`` directly, without modification.

  * Variables and constructors (regardless of their namespace) are mapped
    directly, without modification.

    * The use of a data constructor requires ``DataKinds``.

    * The use of a variable (as opposed to binding) also imposes the following
      side conditions

      * In the type checking environment, there should be no variable of the same
        name but from a different namespace, or else raise an ambiguity error (does
        not apply to constructors).

      * In a temporary deviation from `#378 Design for Dependent Types <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst#term-variables-in-types>`_,
        require that the variable stands for a type; terms are not promoted.
        Lifting this restriction is left for a future proposal, as it does not
        seem useful without a retained quantifier like ``foreach`` (see the
        referenced section of ``#378`` for an explanation with an example).

  * The types-in-terms (such as ``a -> b``, ``a => b``, ``forall a. b``) are
    mapped to types directly, without modification aside from recursively
    processing subterms.

  * With ``DataKinds``, a numeric literal ``42`` is mapped to a promoted numeric
    literal.

  * With ``DataKinds``, a string literal ``"Hello"`` is mapped to a promoted
    string literal ``"Hello"``.

  * With ``DataKinds``, a character literal ``'x'`` is mapped to a promoted
    character literal ``'x'``.

  * A fractional numeric literal ``3.14`` cannot be mapped at the
    moment, as we do not have promoted fractional numeric literals.

  * An unboxed numeric literal ``1337#`` cannot be mapped at the moment,
    as we do not have promoted unboxed types.

  * With ``DataKinds``, a tuple ``(e₀, e₁, ...)ₑ`` is mapped to a promoted tuple
    ``(t₀, t₁, ...)ₑ``, where ``t₀ = t2t(e₀)``, ``t₁ = t2t(e₁)``.

  * An unboxed tuple ``(# a, b #)`` cannot be mapped at the moment, as we do not
    have promoted unboxed types.

  * With ``DataKinds``, a list literal ``[e₀, e₁, ...]`` is mapped to a promoted
    list ``[t₀, t₁, ...]``, where ``t₀ = t2t(e₀)``, ``t₁ = t2t(e₁)``.

  * With ``KindSignatures``, a type signature ``e₀ :: t₁`` is mapped to a kind
    signature ``t₀ :: t₁``, where ``t₀ = t2t(e₀)``.

T2T in Expressions
  * Function application ``e₀ e₁`` is mapped to type-level function
    application ``t₀ t₁``, where ``t₀ = t2t(e₀)``, ``t₁ = t2t(e₁)``.

  * With ``TypeApplications``, type application ``e₀ @t₁`` is mapped to
    type-level type application ``t₀ @t₁``, where ``t₀ = t2t(e₀)``.

  * With ``TypeOperators``, infix application ``e₀ op e₁`` is mapped to
    type-level infix application ``e₀ tyop e₁``, where ``t₀ = t2t(e₀)``,
    ``t₁ = t2t(e₁)``, ``tyop = t2t(op)``.

  * Lambda functions ``\x -> b`` are not mapped and their use is an
    error, as we do not have type-level lambdas at the moment.

  * Case-expressions ``case x of ...`` are not mapped and their use is
    an error, as we do not have type-level case-expressions.

  * If-expressions ``if c then a else b`` are not mapped and their use
    is an error, as we do not have type-level if-expressions.

  * In the same spirit, other syntactic constructs are mapped when
    there's a direct type-level equivalent, and their use is an error
    otherwise.

T2T in Patterns
  * Wildcard patterns ``_`` are mapped to wildcard type patterns.

  * Constructor patterns ``Con @t₀ @t₁ p₂ p₃`` are mapped to nested
    applications and type applications ``Con @t₀ @t₁ t₂ t₃``, where
    ``t₂ = t2t(p₂)``, ``t₃ = t2t(p₃)``.
    ``DataKinds`` is required if ``Con`` is a data constructor.

  * As-patterns ``x@p`` are not mapped and their use is
    an error, as we do not have type-level as-patterns.

  * Lazy patterns ``~p`` are not mapped and their use is
    an error, as we do not have type-level lazy patterns.

  * Bang patterns ``!p`` are not mapped and their use is
    an error, as we do not have type-level bang patterns.

  * View patterns ``e -> p`` are not mappend and their use is
    an error, as we do not have type-level view patterns.

  * In the same spirit, other syntactic constructs are mapped when
    there's a direct type-level equivalent, and their use is an error
    otherwise.

In accordance with the **Lexical Scoping Principle** of `#378 Design for Dependent Types
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst>`_,
T2T preserves the binding structure and the meaning of the syntactic constructs
in the resolved syntax tree.

For example, in ``f T``, the T2T transformation will never change whether the
``T`` refers to a type constructor or a data constructor. Likewise, it will not
change ``[a]`` from a singleton list to the list type, or vice versa. The
mapping is as direct as possible and could be removed if we had a single
syntactic category for terms and types.

Part II: Examples
-----------------

1. A variant of ``id`` that uses visible ``forall``:
   ::

     -- Definition:
     idv :: forall a -> a -> a
     idv a x = x :: a

     -- Usage:
     n = idv Double 42

   This is equivalent to ``n = (42 :: Double)``.

2. A wrapper around ``typeRep`` that uses visible ``forall``:
   ::

     -- Definition:
     typeRepVis :: forall a -> Typeable a => TypeRep a
     typeRepVis a = typeRep @a

     -- Usage:
     t = typeRepVis (Maybe String)

3. A wrapper around ``sizeOf`` that uses visible ``forall`` instead of ``Proxy``:
   ::

     -- Definition:
     sizeOfVis :: forall a -> Storable a => Int
     sizeOfVis a = sizeOf (Proxy :: Proxy a)

     -- Usage:
     n = sizeOfVis Int

4. A wrapper around ``symbolVal`` that uses visible ``forall`` instead of ``Proxy``:
   ::

     -- Definition:
     symbolValVis :: forall s -> KnownSymbol s => String
     symbolValVis s = symbolVal (Proxy :: Proxy s)

     -- Usage
     str = symbolValVis "Hello, World"

5. A type pattern in a constructor pattern:
   ::

     data T a b where
       MkT :: forall a. forall b -> (a,b) -> T a b
       --               ^^^^^^^^^^^^
       --               visible forall in a data constructor

     f :: T a b -> (b, a)
     f (MkT @a b c) = (snd c :: b, fst c :: a)
     --        ^
     --        the corresponding type pattern

6. A type pattern in a constructor pattern that requires unification:
   ::

       data U a where { MkU :: forall a -> U a }

       f :: U (Maybe Int) -> Int
       f (MkU (Maybe a)) = 42 :: a

   The type pattern ``Maybe a`` is unified with ``Maybe Int`` in the
   signature. As a result, ``a`` stands for ``Int`` in ``42 :: a``.

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

   Both ``a`` arguments to ``g`` are bound to term-level binding for ``a``.  In
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
   That is, in a term the forall-bound variables are in the term namespace.

6. Built-in syntax:
   ::

     x1 = g (Int,Bool)
     x2 = g [Int]

   Here, the built-in syntax occurs in a term-level context, so ``(Int,Bool)``
   is a promoted pair, and ``[Int]`` is a promoted singleton list.

   One way to change this is to use synonyms
   from ``GHC.Tuple``::

     x1 = g (TupleN Int Bool)
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

   One way to resolve this is to use synonyms
   from ``GHC.Tuple``::

      a = f @(TupleN Int Bool)
      b = g  (TupleN Int Bool)

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
  types of types is actually a retained quantifier, while the proposed ``forall
  x ->`` in types of terms is erased. This is to be resolved in the future
  by making both of them erased.

* Even though types-in-terms may look like types they are considered term
  syntax, and a variable bound by a forall-in-terms populates the term
  namespace. This means that in ``\x -> f (forall x. x)``, the occurrence
  of ``x`` refers to the forall-bound type variable rather than the
  lambda-bound variable.

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

* Before this proposal, all term variables (retained, values, runtime) used
  names from the term namespace, and all type variables (erased, types,
  compile-time) used names from the type namespace.

  With the changes proposed, the namespace of a variable is no longer a reliable
  indicator of the level (term level or type level) of the entity that the
  variable stands for. Consider::

    f :: forall a -> String
    f = ...

    g :: forall a -> Show a => a -> String
    g t x = show @t (x :: t) ++ f t

  Now let us compare ``x`` and ``t``

  * The name ``x`` is bound in the term namespace. It is a true term variable:
    it is used as a value argument to ``show``; it exists at the term level.
  * The name ``t`` is also bound in the term namespace. However, ``t`` denotes
    a type variable: it is used in the type application ``show @t``, in the type
    annotation ``x :: t``, and as a required type argument in ``f t``; it exists
    at the type level.

* Even in the absence of punning, term syntax and type syntax differ in a few
  subtle ways

  1. In term syntax with ``TemplateHaskell`` enabled, the ``'`` symbol signifies
     name quotation; in type syntax with ``DataKinds`` enabled, ``'`` selects
     the term namespace.

  2. In term syntax, ``*`` is always an infix operator; in type syntax with
     ``StarIsType`` enabled, ``*`` is built-in notation for ``Data.Kind.Type``.

     This discrepancy can be resolved by disabling ``StarIsType``, which is
     slated for deprecation by the accepted
     GHC Proposal `#143 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0143-remove-star-kind.rst>`_.

  3. Term syntax allows binding local operators, but type syntax with
     ``TypeOperators`` does not::

       -- Term syntax
       f (:#) = ...   -- usage of constructor (:#)
       f (#)  = ...   -- binding a local variable (#)

       -- Type syntax
       f (type (:#)) = ...   -- usage of type constructor (:#)
       f (type (#))  = ...   -- usage of type constructor (#)

     To be consistent with terms, ``type (#)`` would have to be a binding of a
     local type variable named ``(#)``.

  Accounting for these discrepancies is a non-goal for the T2T transformation

* The impact of the change associated with ``-Wview-pattern-signatures`` is
  minimal. We patched GHC 9.6 to parse ``e -> p :: t`` with the new precedence
  and compiled 3318 packages (`full list <https://gist.githubusercontent.com/int-index/d687ddc01cf589b73f032ce6950ab035/raw/4d86a929edf649d8086157ef86bd74fba767c2cf/view-pattern-signatures>`_) using the patched compiler.

  Only 2 out of 3318 packages (0.06%) were affected: ``shake-0.19.7`` (2 lines
  of code affected) and ``arithmoi-0.13.0.0`` (1 line of code affected), with
  the total of 3 lines of code affected in the analyzed data set.

  In every instance, the fix was to add parentheses.

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

   The parsing and name resolution rules of these alternatives were deemed too
   subtle, so we opted for a design where types-in-terms are parsed and renamed
   as ordinary terms.

7. We could error on ambiguous variable occurrences earlier in the pipeline, in
   the renamer, but then enabling ``RequiredTypeArguments`` would result in
   rejecting currently valid code::

    id :: forall a. a -> a
    id a = (a :: a)

   Instead, we opted to raise the ambiguity error during T2T.

8. We could permit visible ``forall`` in Haskell98-style data declarations,
   where invisible ``forall`` is already allowed to bind existential variables.

   However, GHC does not support syntactically nested foralls in such
   declarations. Compare:

   * ``data Flat = forall a b. MkFlat a b``
   * ``data Nested = forall a. forall b. MkNested a b``

   At the time of writing this proposal, ``Flat`` is a valid declaration
   whereas ``Nested`` results in a parse error.

   This means that the following declaration would be rejected even if we
   allowed visible forall::

      data MixedVis = forall a. forall b -> MkMixedVis a b

   A similar limitation in GADTs is addressed by GHC Proposal `#402 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst>`_
   (accepted, not implemented). In principle, nothing precludes us from
   extending the syntax of existential quantification in Haskell98-style data
   declarations in a similar manner, but nothing compels us to do so either.

Unresolved Questions
--------------------

None at the moment.

Implementation Plan
-------------------

I (Vladislav Zavialov) or a close collaborator will implement this change.
See GHC tickets `#22326 <https://gitlab.haskell.org/ghc/ghc/-/issues/22326>`_
and `#23717 <https://gitlab.haskell.org/ghc/ghc/-/issues/23717>`_ for an
implementation plan with a subtask breakdown.
