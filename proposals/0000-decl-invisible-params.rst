Invisible parameters for declarations
=====================================

.. author:: John Ericson (@Ericson2314)
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/326>`_.
.. contents::

There are certain implicit bindings that cannot be written explicitly.
This adds the syntax to fix that not already proposed.

Motivation
----------

Scoping and parameter order in declaration heads
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When someone sees declarations like::

  data Foo_0 a :: Type -> forall (b :: k). Type

  data family Foo_1 a :: Type -> forall (b :: k). Type

  type family Foo_2 a :: Type -> forall (b :: k). Type

They might think the signature works just like a term signature, and they can explicitly bind ``k`` without changing the meaning like::

  data Bar_0 a :: forall k (b :: k). Type

  data family Bar_1 a :: forall k (b :: k). Type

  type family Bar_2 a :: forall k (b :: k). Type

But this is in fact not the same: the ``k`` parameter comes before the ``a`` in the first group, but after the ``a`` and before the ``b`` in the second group.
At least, we can use standalone kind signatures to illustrate the difference if not fix the problem::

  type Foo_0, Foo_1, Foo_2 ::
    forall k. Type -> Type -> forall (b :: k). Type

versus::

  type Bar_0, Bar_1, Bar_2 ::
    Type -> forall k (b :: k). Type

(pretending for concision's sake we had multiple name SAKS syntax).

Scoping in declaration bodies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These all work (with SAKSs for just clarity---they can be removed)::

  type Foo :: Type -> Type
  data Foo k = forall (b :: k). MkFoo

  type Foo :: forall k. k -> Type
  data Foo (a :: k) = forall (b :: k). MkFoo

  type Foo :: forall k. k -> Type
  type Foo (a :: k) = k

  type C :: Type -> Constraint
  class C k where
    type F :: k

  type C :: forall k. k -> Constraint
  class C (a :: k) where
    type F :: k

But these don't::

  type Foo :: forall k. Type
  data Foo = forall (b :: k). MkFoo -- k is unbound

  type Foo :: forall k. Type -> Type
  data Foo a = forall (b :: k). MkFoo -- k is unbound

  type Foo :: forall k. k -> Type
  type a = k

  type C :: forall k. k -> Constraint
  class C a where
    type F :: k

The problem is while free variables in the LHS / head are implicit bound, free variables in the RHS / body are not.
The ``forall k`` in the SAKSs corresponds the invisible parameter we'd like to use.
In the ambiguous cases like the first failing example, where no visible parameter references the invisible parameter, there is in fact no way to make the experiment type-check today.

Arity
~~~~~

For type synonyms and type families, the kind doesn't yet tell the whole story.
There is also a notion of arity: the number of arguments a type synonym family must be applied if the use of it is to be allowed.

We have the rough intuition that the number of parameters with patterns is the arity.
The first problem is that implicitly bound variables in the kind, in floating to the LHS, also count::

  -- arity [invisible]
  type Foo_0 :: Type -> forall (b :: k). Type
  -- arity [invisible, visible]
  type family Foo_1 a :: Type -> forall (b :: k). Type

  -- arity []
  type Foo_0 :: forall k (b :: k) -> Type
  -- arity [visible]
  type family Foo_1 a :: forall k (b :: k) -> Type

Now, since there is no body here, unlike the previous section we don't have a scoping problem or problem defining with ambiguous kinds.
We can always just write down the any quantifiers we want in the kind, and then control should they count towards the arity or not.
The problem is, because there is no pattern syntax for invisible binders, GHC uses the legacy explicit return kind to determine the *non*\ -arity part of the kind signature from the legacy inline kind signature, and *just* for invisible binders!.
This is convoluted to both learn and implement, and requires the user to write more than they should::

  type F :: forall k. Maybe k
  type family F
            -- arity = [invisible]

  type G :: Type -> forall k. Maybe k
  type family G
            -- arity = []

  type H :: Type -> forall k. Maybe k
  type family H a
            -- arity = [visible, invisible]

  type H :: Type -> forall k. Maybe k
  type family H a :: forall k. Maybe k
            -- arity = [visible]

The solution for all of these problems ``@``\ -prefixed patterns for invisible parameters, just as is already proposed for constructor patterns, in `Proposal 126`_, and lambdas, in proposal `Proposal 155`_.
We can use the same simple argument order, scoping, and arity rules for both visible and invisible parameters, and complete the obseletion of inline kind signatures and CUKSs.
Finally, this proposal completes what the other two proposals started, bringing a much-needed uniformity to the language, and matching users expectations after they first encounter ``@``\ -bindings in one context and try to use in another.

Proposed Change Specification
-----------------------------

Parsing
~~~~~~

``data``, ``newtype``, ``type``, ``class``, ``type family``, and ``data family`` declarations will no longer the prohibit the use of ``@``\ -prefixed applications in their heads.
\[This prohibition is currently a side-condition prohibition, as these declaration heads use the regular type grammar.\]

These declaration forms also now allow parameters of the form ``@{var}``, where ``var`` is, as usual, a lower-case identifier.

``@``\ -prefixed applications remain only expressible with ``-XTypeApplications``.

Renaming
~~~~~~~~

``@``\ -prefixed parameters bind their variables just like normal ones do, in the same namespace and with the same scopes.
\[The same rules on shadowing, duplication, and the mixing of implicitly and explicitly bound variables apply, as all follow from the choice of scope variables are bound in.\]

Type checking
~~~~~~~~~~~~~

An invisible parameter is given a invisible forall quantifier (``forall ... .`` kind).
Invisible parameters need not all be given, the rule will be dual to type applications, and analogous to type lambdas:

 - An invisible parameter must match a corresponding invisible quantifier in the same position before, after, or between visible quantifiers.

 - Invisible parameter in those positions must match a prefix of invisible quantifiers.

Explicit and implicit visible parameters are both type-checked in the same way.

An invisible parameter in braces can be used only when the type being declared also has a standalone kind signature (SAK).
The parameter name in braces must exactly match the name of a parameter bound with the ``forall {var}`` construct in the type's SAK.

Examples
--------

Data type
~~~~~~~~~
::

  type F :: forall k. k -> Type
  data F @k :: k -> Type -- OK

::

  type F :: forall k -> k -> Type
  data F @k :: k1 -> Type -- Rejected: doesn't match kind signature

::

  type F :: forall k1 k2. forall (a :: k1) -> k -> Type
  data F @kA @(a :: kB) :: k1 -> Type
    -- Rejected: implicit binding for kB comes after kA, so kind is not matched.

::

  type F4 :: forall {k1} {k2} a b. Proxy a -> Proxy b -> Type
  data F4 @{k1} @{k2} @a @b p1 p2   -- OK

::

  type F5 :: forall {k1} {k2} a b. Proxy a -> Proxy b -> Type
  data F5 @{k1} @{k2} @p @q x y     -- OK: specified type variables can be renamed

::

  type F6 :: forall {k1} {k2} a b. Proxy a -> Proxy b -> Type
  data F6 @{k2} @{k1} @a @b p1 p2  -- Rejected: variables in wrong order

::

  type F7 :: forall a b. Proxy a -> Proxy b -> Type
  data F7 @{k1} @{k2} @a @b p1 p2  -- Rejected: names do not match up (because no name
                                   -- supplied in SAK)

Type synonym
~~~~~~~~~~~~~

::

  type F :: forall k. k -> Type
  type F (a :: k) = k -- OK, already
  --           ^    ^
  --           induces implicit binding

::

  type F :: forall k. k -> Type
  type F @k (a :: k) = k -- OK
  --              ^    ^
  --              Use not binding

::

  type F :: forall k. k -> Type
  type F @k (a :: k1) = k -- k1 not bound

Class
~~~~~

::

  type F :: forall k. k -> Constraint
  class F (a :: k) -- OK, already

::

  type F :: forall k. k -> Constraint
  class F @k (a :: k) -- OK

::

  type F :: forall k. k -> Constraint
  class Foo k1 -> F @k (a :: k) -- k1 is not bound

::

  type F :: forall k. k -> Constraint
  class Foo k -> F @k (a :: k1) -- k1 is not bound

Family
~~~~~~

::

  type F :: forall k. k -> k -> Type
  type family F @k (a :: k) :: k -> Type -- OK

::

  type F :: forall k. k -> k
  type family F @k (a :: k) :: k -- OK

Effect and Interactions
-----------------------

Whether to mix explicit and implicit invisible parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The prohibition on mixing ``@`` patterns and implicit variable binding is modeled on the existing "forall-or-nothing" rule.
That says if one has an outermost ``forall`` in a signature, no free variables are implicitly bound.
The idea is if a user is fastidious enough to not *rely* on implicit binding, they probably don't want it.
\[Nested use ``forall`` is required to express things, and thus doesn't indicate fastidiousness.]

Likewise, the invisible parameters being proposed here also indicate fastidiousness.

What about instances?
~~~~~~~~~~~~~~~~~~~~~

It may seem like class and family instances bind variables.
In fact, those are deemed uses.
To wit, one can use an explicit ``forall`` with each:

::

  instance forall a. Foo a

::

  type instance forall a. Foo a = a

::

  data instance forall a. Foo a

Explicit inferred variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Users can write ``forall {k}`` to introduce an *inferred* variable in a kind signature. We would like to
be able to bind these explicitly in type definitions. However, we must be able to know the *order* of such
variables. For example, if we had ``type T :: forall a b. Proxy a -> Proxy b -> Type``, which comes first:
the kind of ``a`` or the kind of ``b``? GHC might change the order of these variables, even between
minor releases. We thus require that the name of such variables in the type definition matches that
in the SAK. In the case of the SAK given in this paragraph, there is no name in the SAK, and so the
``@{k}`` construct can never work. We thus add this naming restriction as a way of binding inferred
variables predictably.

Costs and Drawbacks
-------------------

None known at this time.

Alternatives
------------

Prohibit implicit bindings if explicit visible bindings are used.
This was deemed to draconian.
@Ericson2314 would argue that thanks to the simplifications to the arity rules, we arguable have extra "complexity budget" to "spend" on the interaction between implicit and explicit parameters.

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

I have begun this in `GHC MR 3145`_.
I have some bugs but it has not been hard so far.
@int-index's syntax work has provided a very good foundation.

Endorsements
-------------

.. _`Proposal 126`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0126-type-applications-in-patterns.rst

.. _`Proposal 155`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst

.. _`GHC MR 3145`: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3145
