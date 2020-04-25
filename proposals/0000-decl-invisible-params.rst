Invisible parameters for declarations
==============

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

Implicit binding is confusing to users.
Even advanced users.
It is far from obvious that:

::

  data F :: forall (a :: k) -> Type

  data family F :: forall (a :: k) -> Type

do not mean the same thing (respectively) as:

::

  data F :: forall k. forall (a :: k) -> Type

  data family F :: forall k. (a :: k) -> Type

For both the ``data`` and ``data family`` declaration, the order in which the ``k`` parameter is quantified changes, but perhaps that isn't so bad on its own.
Worse is that for the family definition, the implicitly bound ``k`` is something instances can scrutinize, while the other ``k`` is purely parametric.

Making matter worse still, the latter forms of each cannot be written with all variables explicitly bound.
How are we suppose to teach these subtle things made even by no explicit way to highlight their differences!

The solution is ``@``-prefixed "invisible parameter", as they are known, just as is already proposed for constructor patterns, in `Proposal 126`_, and lambdas, in proposal `Proposal 155`_.
The uniformity between all of these should round out the language according to expectations.

Proposed Change Specification
-----------------------------

Parsing
~~~~~~

``data``, ``newtype``, ``type``, ``class``, ``type family``, and ``data family`` declarations will no longer the prohibit the use of ``@``-prefixed applications in their heads.
\[This prohibition is currently a side-condition prohibition, as these declaration heads use the regular type grammar.\]

These declaration forms also now allow parameters of the form ``@{var}``, where ``var`` is, as usual, a lower-case identifier.

``@``-prefixed applications remain only expressible with ``-XTypeApplications``.

Renaming
~~~~~~~~

Any such declaration with an invisible parameter must only bind variables via explicit parameters in its head.
There is no implicit binding of free variables in this case.

Type checking
~~~~~~~~~~~~~

An invisible parameter is given a invisible forall quantifier (``forall ... .`` kind).

An invisible parameter in braces can be used only when the type being declared
also has a standalone kind signature (SAK). The parameter name in braces must
exactly match the name of a parameter bound with the ``forall {var}``
construct in the type's SAK.

Examples
--------

Data type
~~~~~~~~~
::

  type F :: forall k. k -> Type
  data F @k :: k -> Type -- OK

::

  type F :: forall k. k -> Type
  data F @k :: k1 -> Type -- k1 is not bound

::

  type F :: forall k -> k -> Type
  data F @k :: k1 -> Type -- dosen't match kind signature

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

Allow implicit variable binding always, or some in-between (such as allowing based off whether the first parameter is visible).

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

I have begun this in `GHC MR 3145`_.
I have some bugs but it has not been hard so far.
@int-index's syntax work as provided a very good foundation.

Endorsements
-------------

.. _`Proposal 126`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0126-type-applications-in-patterns.rst

.. _`Proposal 155`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst

.. _`GHC MR 3145`: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3145
