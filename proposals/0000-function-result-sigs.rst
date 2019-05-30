Function Result Type Signatures
===============================

.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/228>`_.
.. sectnum::
.. contents::

We propose to allow function result type signatures, shown as ``:: r`` in the
following example::

  f (x :: a) (y :: b) :: r = rhs

Note that this is an annotation on the LHS, not on the RHS::

  f (x :: a) (y :: b) = rhs :: r   -- not the same!

Motivation
----------

There are several reasons to add function result type signatures:

1. Code style
2. Fine-grained control over type checking
3. Consistency with types

Let us take a closer look at each of these points.

1.  **Code style.** There are two ways to think of a type signature: it is a
    standalone summary of a definition, or it is an integral part of it.

    The first way of thinking leads to idioms such as top-level type signatures,
    comma-separated names in signatures, and interface/implementation sections::

      -- Interface
      even, odd :: Integer -> Bool
      concat :: [[a]] -> [a]
      reverse :: [a] -> [a]

      -- Implementation
      even = ...
      odd = ...
      concat = ...
      reverse = ...

    The second way of thinking leads to inline type annotations on patterns and
    function bodies::

      even (a :: Integer) = ... :: Bool
      odd (a :: Integer) = ... :: Bool
      concat (xs :: [[a]]) = ... :: [a]
      reverse (xs :: [a]) = ... :: [a]

    Both styles have their merit. Haskell has good support for the former,
    while the latter would benefit from moving the annotation to the left-hand
    side. For one thing, it is what Scala, F#, OCaml, and some other languages
    do, so we would better accommodate users coming from these languages by
    allowing this style::

      def fib(n: Int): Int = ...        -- Scala
      let rec fib (n : int) : int = ... -- F# and OCaml
      fib (n :: Int) :: Int = ...       -- Haskell (proposed)

    For another thing, in the presence of guards a type annotation on the
    right-hand side must be duplicated, or else it would only annotate only one
    of the equations::

      wet (x :: a)
        | p x = g x      :: [a]   -- duplicated [a]
        | otherwise = [] :: [a]

      dry (x :: a) :: [a]         -- no duplication
        | p x = g x
        | otherwise = []

2.  **Fine-grained control over type checking.** Type annotations in
    patterns and in expressions behave differently when it comes to type
    variables.

    In patterns, the type variables are bound to the inferred types::

      f :: (Bool -> Bool) -> Bool
      f (g :: a -> a) = g True    -- OK, a ~ Bool

    In expressions, the type variables are implicitly quantified::

      f :: (Bool -> Bool) -> Bool
      f g = (g :: a -> a) True    -- Couldn't match type ‘a’ with ‘Bool’

    We do not unify ``a ~ Bool``, quantifying over ``a`` instead. Thus, there
    is an important design decision: should type variables in function result
    type signatures on the left-hand side exhibit pattern-like (unification) or
    expression-like (quantification) behavior?

    We propose that type signatures on the left-hand side are subject to
    unification, and type signatures on the right-hand side are subject to
    quantification. This is a simple, consistent principle, which gives more
    control to the user: now it is possible to control whether unification or
    quantification is used by putting the type annotation in the appropriate
    position.

    In particular, consider the following example::

      qf = <rhs> :: a -> a
      uf :: a -> a = <rhs>

    ``qf`` and ``uf`` are vastly different. ``qf`` requires ``rhs`` to be fully
    polymorphic, so there is only possible implementation::

      qf = id :: a -> a   -- OK
      qf = not :: a -> a  -- Couldn't match type ‘a’ with ‘Bool’

    ``uf`` may unify ``a`` with another type, so ``uf`` may be defined as any function where
    the domain is equal to the codomain::

      uf :: a -> a = id       -- OK
      uf :: a -> a = not      -- OK, a ~ Bool
      uf :: a -> a = ('x':)   -- OK, a ~ String

    We expect programmers to make use of this power.

3.  **Consistency with types**. In data declarations, we can add kind
    annotations to the bound type variables::

      data Vec (a :: Type) (b :: Nat) where <defn>

    We can also add a result kind annotation::

      data Vec (a :: Type) (b :: Nat) :: Type where <defn>

    Note that all of these annotations are in the declaration header, not in
    the definition.

    With the recently accepted proposal about top-level kind signatures, one
    may write a top-level kind annotation::

      type Vec :: Type -> Nat -> Type
      data Vec a b where <defn>

    Therefore, in types we have:

    * Kind annotations on binders: Yes.
    * Top-level kind signatures: Yes. (Implementation in progress)
    * Result kind annotations: Yes.

    In terms, the situation differs. We have top-level type signatures, and
    with ``-XScopedTypeVariables``, we may write type annotations for the bound
    variables::

      f :: Bool -> Integer
      f (a :: Bool) = <defn>

    However, the result type annotations are not allowed::

      f (a :: Bool) :: Integer = <defn>
                    ^^^^^^^^^^

    Therefore, in terms we have:

    * Type annotations on binders: Yes.
    * Top-level type signatures: Yes.
    * Result type annotations: No. (Proposed)

    This discrepancy is slightly annoying from the language design standpoint.


Proposed Change Specification
-----------------------------

Allow function result type signatures on the left-hand side.

**Syntax.** These signatures are already a part of the grammar, and a
validation step rejects them. This check will be removed, and ``FunBind``
extended with a result type.

**Semantics.** The result type signature is unified with the inferred type of
the function body. It does not enable polymorphic recursion.

Result type signatures behave just like pattern signatures, as in ``\ (x ::
a->a) -> ...``. That is: there is no implicit quantification; it is a binding
site for ``a`` (in this example); and the ``a`` might be bound to any type, e.g
``Int``. The details of pattern signatures are worked out in the paper
`Type Variables in Patterns <https://www.microsoft.com/en-us/research/publication/type-variables-patterns/>`_.

Effect and Interactions
-----------------------

At the moment, a binding with no parameters and a signature is parsed as a
pattern binding::

  x :: String -> String = reverse    -- accepted as PatBind

The consequence of this is that we reject scoped type variables::

  x :: [a] -> [a] = reverse    -- rejected with an error:
  -------------------------------------------------------
    • You cannot bind scoped type variable ‘a’
        in a pattern binding signature
    • In the pattern: x :: [a] -> [a]
      In a pattern binding: x :: [a] -> [a] = reverse

Under this proposal, we reclassify this construct as a function binding and
allow scoped type variables::

  x :: [a] -> [a] = reverse    -- accepted as FunBind


Costs and Drawbacks
-------------------

This is one more feature to implement and maintain.


Alternatives
------------

* We could treat ``f :: t = <rhs>`` equivalently to ``f = <rhs> :: t``, but
  this is neither consistent nor terribly useful.

* We could detect CUSKs as we do in types to enable polymorphic recursion, but
  this makes little sense as we are in the proccess of their deprecation.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

I (Vladislav Zavialov) will (attempt to) implement.
