.. sectnum::
.. highlight:: haskell

Principles for GHC
==================

This document lays out high-level principles for the evolution of GHC
and the language it compiles. In much the way that we like to write
*properties* of our functions -- instead of just writing down individual
test cases -- this document strives to write *principles* that our proposals
ought to maintain (or build towards).

Articulating these principles helps to guide future proposals: proposals
that maintain the principles in this document are more likely to be accepted,
while proposals that work against these principles are more likely to be rejected.
Note that neither direction is (at all) a guarantee: there will be excpetions
in both directions. Yet by writing down the principles, we can have an informed
discussion of the tradeoffs of accepted or rejecting an individual proposal.

How to update these principles
------------------------------

When making a proposal, following the `usual guidelines <https://github.com/ghc-proposals/ghc-proposals/#how-to-start-a-new-proposal>`_,
feel free to include a diff against this listing of principles. We can then
discuss the validity of the new principle(s) alongside the concrete proposal for
a change to GHC.

We urge proposers to resist the temptation to propose principles without an
accompanying concrete change to GHC. Debating principles in the abstract does
not seem a productive use of time, though it is possible to imagine exceptions.
For example, the justification for the rejection of a proposal may be distilled into
a new principle here. Accordingly, this document is not (and presumably never will be)
comprehensive: it contains the articulated prinicples guiding GHC's development
but lacks the unarticulated principles, which will be added over time.

All principles in this document are linked back to the PR that introduce them,
so that readers can learn the context in which they were written.

We use a notational convention here where each principle is given an abbreviation,
but we use its full spelling wherever possible. The full spelling is useful for readers,
given the multitude of principles in play. Including the abbreviation is useful in case
the abbreviation is used in other fora (e.g. email, GitHub, etc.) to allow the curious
to find (e.g. with Ctrl+F) the abbreviation within this page.

Accepted principles
-------------------

.. _`#281`: proposals/0281-visible-forall.rst
.. _`#378`: proposals/0378-dependent-type-design.rst
.. _`#425`: proposals/0425-decl-invis-binders.rst
.. _`#448`: proposals/0448-type-variable-scoping.rst

Syntax
~~~~~~

Syntactic Unification Principle (SUP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Syntactic Unification Principle`:

**Principle:** In the absence of punning, there is
no difference between type-syntax and term-syntax.

The `Syntactic Unification Principle`_ is a *long term* goal; today, there are many, many violations of this principle. (For example,
``case`` can never appear in types, and ``Int -> Bool`` is meaningless in a term.) However, this principle
should direct future proposals not to make the situation worse, and proposals that bring us closer to
the `Syntactic Unification Principle`_ are to be valued.

*Motivation:* The `Syntactic Unification Principle`_ keeps us forward-compatible with a possible future where the
distinction between term-syntax and type-syntax is removed.

From `#378`_.

Explicit Variable Principle (EVP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Explicit Variable Principle`:

**Principle:**
It is possible to write out all (specified) type arguments in every polymorphic function application,
give the type for every bound variable,
and write a type signature for every expression.
This requires the ability to bring type variables into scope.
These variables can be brought into scope without relying on matching or unification.

Examples::

  const :: a -> b -> a
  const x y = ...    -- there must be some way to name the types of x and y here
  -- using `const (x :: a) (y :: b) = ...` is not powerful enough, because it relies
  -- on matching the pattern signature with the argument type from the type signature
  const @a @b x y = ...  -- this version makes it easier to bind `a` and `b`

  data Ex = forall a. Ex a
  f (Ex x) = ...     -- there must be some way to name the type of x here
  f (Ex @a x) = ...  -- this version does it

  hr :: (forall a. a -> a -> a) -> ...
  hr = ...
  g = hr (\ x y -> ...)     -- there must be some way to name the type of x or y here
  g = hf (\ @a x y -> ...)  -- this version bind the variable

The `Explicit Variable Principle`_ does not hold today:

* Even forgetting about the "no matching" restriction, if we have ``hr2 :: (forall a. F a -> Int) -> ...``,
  where ``F`` is a type family,
  there is no way for a function passed to ``hr2`` to locally bind ``a``.

* With the "no matching" restriction,
  if we have ``hr3 :: (forall a. Maybe (Either (Int, Bool, a) Double) -> ...) -> ...``,
  the only way a function passed to ``hr3`` can bind ``a`` is to repeat the type of the argument to that function.

Once we have the `Explicit Variable Principle`_, there will never be a need for ``Proxy``.

*Motivation:*
As GHC supports more and more type-level programming,
the ability to write out type signatures, arguments, and annotations has become increasingly important.
With ``-XScopedTypeVariables``, GHC allows us to bring type variables into scope,
but often requires us to do so by cumbersome matching.
If we have a type ``Maybe (Either (Int, Bool, a) Double)``,
that's a lot to type just to be able to, say, bind ``a``. The EVP says we do *not* have to resort to matching, ever.

From `#448`_.

Visibility Orthogonality Principle (VOP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Visibility Orthogonality Principle`:

**Principle**: Whether an argument is visible or
invisible should affect only its visibility, not other properties.

A consequence of the `Visibility Orthogonality Principle`_ is that these two programs should have the same meaning::

   f1 :: forall a -> ...
   f1 blah1 = ...

   g1 = ... f1 blah2 ...

   -------

   f2 :: forall a. ...
   f2 @(blah1) = ...

   g2 = ... f2 @(blah2) ...

The only difference between these is the visibility.

Put another way: two programs that are the same except for visibility markers (such as
the ``.`` vs ``->`` in a ``forall`` or the presence or absence of a ``@``) should desugar
to the same Core program.

Currently, the design for `#281`_ (along with the design for ``-XTypeApplications``)
violates the `Visibility Orthogonality Principle`_, because the visibility marker ``@`` also affects the difference between
term-syntax and type-syntax. However, given the `Syntactic Unification Principle`_, we strive to uphold the `Visibility Orthogonality Principle`_ when
there is an absence of punning.

*Motivation:* Visibility should be just that: a superficial property that describes
(only) whether an argument is visible in the user-written source code.

Name resolution and scoping
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Explicit Binding Principle (EBP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Explicit Binding Principle`:

**Principle**:
Through the right combination of extensions,
every implicit form of variable binding must have an explicit equivalent that,
regardless of the context,
is unambiguously a binding site.

Examples:

#. Problem::

     -- Assume no `a` in scope

     id :: a -> a  -- The variable `a` has no explicit binding site.

   Solution::

     -- Assume no `a` in scope

     id :: forall a. a -> a  -- The `a` in `forall a.` is an explicit binding site.

   This is provided by ``-XExplicitForAll``, which predates the GHC proposal process.

#. Problem::

     -- Assume no `a` in scope

     data Foo (a :: k)

   Solution::

     data Foo @k (a :: k)

   This is provided by ``-XTypeAbstractions`` from `#425`_.

#. Problem::

     -- Assume no `b` in scope

     f :: (Bool, Bool) -> Bool
     f (x :: (b, b)) = ...   -- The variable `b` has no implicit binding site.

   We could declare one or both of the ``b`` occurrences above a binding site,
   as was the historical interpretation of this, but that doesn't help as this
   syntax isn't unambiguously a binding site regardless of context (i.e.
   regardless of whether there is a ``b`` already in scope).

*Motivation:*
The `Explicit Binding Principle`_ allows programmers to control exactly how variables come into scope.
It ensures all short-hands can be explained in terms of an explicit, unambiguous equivalent that is easier to understand at the cost of being more verbose:

- Positive-position signatures' free vars cause  implicit ``forall ... .``

- Negative position free vars cause different sorts of binding:

  - Signatures on term patterns (pattern signatures) cause implicit ``let type ... = _ in``

  - Signatures on type variables (kind signatures) cause implicit ``@...``

From `#425`_, `#448`_.

Lexical Scoping Principle (LSP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Lexical Scoping Principle`:

**Principle**:

a. For every appearance of an identifier,
   it is possible to determine whether that appearance is a mere *occurrence*,
   and thus must be bound elsewhere for the program to be valid,
   or the variable is a *binding site* (or causes an implicit binding, which is close enough),
   without examining the context.

b. For every *occurrence* of an identifier,
   it is possible to uniquely identify its *binding site*, without involving the type system.

This builds upon the `Explicit Binding Principle`_:
whereas that former principle ensures that explicit alternatives to implicit binding constructs *exist at all*,
this latter principle makes those explicit alternatives *compulsory*, because we must not have implicit binding in order to uphold this principle.

The `Lexical Scoping Principle`_ is almost true today, with the following nuances:

1. Template Haskell splices may need to be run before completing name resolution (and running those splices requires type-checking them).

2. The `deprecated mechanism <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/duplicate_record_fields.html#selector-functions>`_ for disambiguating duplicate record fields violates the `Lexical Scoping Principle`_ by requiring the type system.

3. In a pattern signature,
   if we have ``f (x :: Maybe a)``,
   the ``a`` is an occurrence if ``a`` is already in scope,
   and is implicitly bound otherwise.

4. In a type signature,
   if we have ``f :: a -> a``,
   the ``a`` is an occurrence if ``a`` is already in scope,
   and is implicitly bound otherwise.

#. In a kind signature,
   if we have ``data Foo (a :: k)``,
   the ``k`` is an occurrence if ``k`` is already in scope,
   and is implicitly bound otherwise.

*Motivation:*
These principles mean that we can understand the binding structure of a program without relying on type inference,
important both for the implementation of GHC and the sanity of programmers.
Furthermore, it allows readers to identify which variables should be brought newly into scope without tracking the list of variables already in scope.
This last point becomes even more poignant if we consider the possibility of mixing the term-level and type-level namespaces (`#270`_) and need to think about clashes between type variables and imported term variables.

\(a) from `#448`_;
\(b) from `#378`_.

Contiguous Scoping Principle (CSP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Contiguous Scoping Principle`:

**Principle**: The region of a program for which an identifier
is in scope is contiguous.

The `Contiguous Scoping Principle`_ is *not* respected by Haskell 2010 nor some of GHC's extensions.
Here are some places where it is violated:

1. ``do``\ -notation.
   Example: ``do (x, (f x -> Pat)) <- action; blah``.
   ``x`` is in scope in its pattern, to the right of its binding site, but then not in ``action``.
   It is in scope again in ``blah``.
   Example of potential confusion: ``f x = do x <- x; g x``.

#. List comprehensions.
   Example: ``[ (x, y) | x <- thing1, y <- thing2, condition3 ]``.
   The variable ``y`` is in scope in ``condition3`` and the ``(x, y)`` at the beginning, but nowhere else.
   Example of potential confusion:
   ``f x y = [ (x, y) | x <- y, y <- x ]``.

#. Arrow notation.
   Example: ``proc x -> do y <- task1 -< input1; task2 -< input2``.
   The variable ``x`` is in scope in ``input1`` and ``input2`` but not in ``task1`` or ``task2``.
   Example of potential confusion: ``f x = proc x -> x -< x``.
   The two ``x``\ s at the end refer to *different* variables.

#. ``-XScopedTypeVariables``.
   Example: ``f :: forall a. a -> a; x :: Int; f y = (y :: a)``.
   The type variable ``a`` is in scope in the definition of ``f`` but not in the type signature for ``x``.

#. GADT header variables.
   Example of potential confusion:
   ``data G a where MkG :: a Int -> G Bool deriving C a``.
   The ``a`` in the type of ``MkG`` is completely unrelated to the ``a`` toward the beginning and in the deriving clause.

There may be others beyond this.
The goal here is *not* to establish the `Contiguous Scoping Principle`_, but to be mindful of new violations.

*Motivation:*
The `Contiguous Scoping Principle`_ makes programs easier to read,
in that a reader can add a variable to their internal tracking of in-scope variables then remove that variable from their in-scope set just once.

From `#448`_.

Semantics
~~~~~~~~~

Predictable Erasure Principle (PEP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Predictable Erasure Principle`:

**Principle**: The programmer knows, for sure, which bits of the program will be
retained at runtime, and which will be erased.

The `Predictable Erasure Principle`_ is true today: types are erased, while terms are retained.

From `#378`_.

User experience
~~~~~~~~~~~~~~~

Opt-In Principle (OIP)
^^^^^^^^^^^^^^^^^^^^^^

.. _`Opt-In Principle`:

**Principle**: Users who do not opt into an advanced feature will
not be affected by it.

This principle is violated in various ways today: it is easy for GHC to generate error messages that refer to
advanced features even when writing simple code. In addition, the existence of advanced features likely slow
down GHC even when those features are not active. Yet this principle is important to keep in mind going forward,
as we hope not to make the current situation worse.

From `#378`_, slightly generalized.
