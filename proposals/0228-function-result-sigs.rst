Function Result Type Signatures
===============================

.. author:: Vladislav Zavialov (@int-index)
.. co-author:: John Ericson (@Ericson2314)
.. date-accepted:: 2019-07-16
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/18203
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/228>`_, `amended at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/254>`_, and further `amended following a bit more discussion <https://github.com/ghc-proposals/ghc-proposals/pull/228#issuecomment-558274417>`_.
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

Syntax
~~~~~~

Take the Haskell 2010 function left-hand side grammar as the
starting point::

  funlhs -> var apat {apat}
          | pat varop pat
          | ( funlhs ) apat {apat}

The change is to add an optional type annotation::

  funlhs' -> var apat {apat}
           | pat varop pat
           | ( funlhs' ) apat {apat}

  funlhs -> funlhs' [:: type]

Semantics
~~~~~~~~~

The result type signature is unified with the inferred type of
the function body. It does not enable polymorphic recursion.

Result type signatures behave just like pattern signatures, as in ``\ (x ::
a->a) -> ...``. That is: there is no implicit quantification; it is a binding
site for ``a`` (in this example); and the ``a`` might be bound to any type, e.g
``Int``. The details of pattern signatures are worked out in the paper
`Type Variables in Patterns <https://www.microsoft.com/en-us/research/publication/type-variables-patterns/>`_.

Effect and Interactions
-----------------------

Purposefully kept to a minimum. See the alternatives for why an extension to
this proposal had negative interactions.

Note that the result type may *not* have constraints. If we had a definition with
multiple equations, it would be unclear how to unify all the constraints, and so
we just forbid them. Some discussion of this is in proposed typing
rules at `<https://gitlab.haskell.org/rae/haskell/tree/poly-result-sigs>`_; note that
this link points toward a dead branch of that project, as this alternative has
been abandoned.

Costs and Drawbacks
-------------------

This is one more feature to implement and maintain.


Alternatives
------------

* We could treat ``f x :: t = <rhs>`` equivalently to ``f x = <rhs> :: t``, but
  this is neither consistent nor terribly useful.

* We could detect CUSKs as we do in types to enable polymorphic recursion, but
  this makes little sense as we are in the proccess of their deprecation.
  
* We could allow constraints in a result signature if there is only one equation,
  but it seems unnecessary to add this twist to the story.

* An earlier version of the proposal changed the boundary between function binds
  and pattern binds via not requring an argument:

  .. code-block:: diff

    - funlhs' -> var apat {apat}
    + funlhs' -> var {apat}

  and resolving the resulting ambiguity with pattern bindings in favor of
  function bindings.

  The motivation issue is that at the moment, a binding with no parameters and
  a signature is parsed as a pattern binding::

    x :: String -> String = reverse -- accepted as PatBind

  The consequence of this is that we reject scoped type variables::

    x :: [a] -> [a] = reverse -- rejected with an error:
    -------------------------------------------------------
      • You cannot bind scoped type variable ‘a’
          in a pattern binding signature
      • In the pattern: x :: [a] -> [a]
        In a pattern binding: x :: [a] -> [a] = reverse

  But by reclassifying this construct as a function binding we could allow
  scoped type variables::

    x :: [a] -> [a] = reverse -- accepted as FunBind

    -- | Remember 'a' can be constrained.
    x' :: a' = reverse -- accepted as FunBind

  The problem is that there is another conflicting interpretation where ``x``
  stays a pattern bind::

    x :: [a] -> [a] = reverse @Int -- maybe someday accepted as PatBind

    y :: a
    y = 1 :: Int

  Here, ``a`` is a (unified) type variable with the same scope as ``x``. This
  matches ``f (x :: a) = ...`` and ``(x :: a) <- ...`` where the ``x`` and
  ``a`` also have the same scope. The overlap with the monadic bind is
  especially intersting, because we *have* to have these the scope of ``a`` not
  include the RHS of the bind in order to bind existentials. This also scales
  to deeper pattern signatures::

    Identity (x :: a) = Identity @Int 1 -- maybe someday accepted as PatBind

  To recover the function signature, one could use an explicit ``forall``::

    x :: forall a. [a] -> [a] = reverse -- maybe someday accepted as PatBind

  Which can be combined with the above if ``-XImpredicativeTypes`` is ever
  cleaned up. True, this doesn't bind ``a`` over the function body, but `type lambdas
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0050-type-lambda.rst>`_
  provide a solution.::

    x :: forall a. [a] -> [a] = \@b -> id @[b]
    Identity (x :: forall a. [a] -> [a]) = Identity \@b -> id @[b]

  All this is of course future work, but it seems premature for this proposal
  to cut off that future work. By conservatively keeping pattern binds the same
  as today, we keep all options open.

Unresolved Questions
--------------------

No unresolved questions. The previous version of the proposal in changing the
division between pattern and function binds raised some, but now we avoid that
by keeping the existing division.

Implementation Plan
-------------------

I (Vladislav Zavialov) will (attempt to) implement.

The function result signatures are already a part of the ``Parser.y`` grammar,
and a validation step rejects them. This check will be removed, and ``FunBind``
extended with a result type.

I, John Ericson (@Ericson2314), started GHC MR `!1474`_, which makes GHC's division of pattern and function bindings match the spec.
I will attempt to finish it, probably after the 8.10 fork.
While it may not be necessary, it is probably the best way to ensure that `f :: a = ...` works according to the proposal and not the alternative.

.. _`!1474`: https://gitlab.haskell.org/ghc/ghc/merge_requests/1474
