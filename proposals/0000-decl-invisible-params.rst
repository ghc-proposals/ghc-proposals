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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
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

``@``-prefixed applications remain only expressible with ``-XTypeApplications``.

Renaming
~~~~~~~~

Any such declaration with an invisible parameter must only bind variables via explicit parameters in its head.
There is no implicit binding of free variables in this case.

Type checking
~~~~~~~~~~~~~

An invisible parameter is given a invisible forall quantifier (``forall ... .`` kind).

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

    In 3.2 you refer to a "hypothetical not-yet-proposed" syntax for ``data type data T @k (a:k) = .....`` Is it really not yet proposed? Please write a proposal! We obviously want it!

– SPJ in `<https://github.com/ghc-proposals/ghc-proposals/pull/285#issuecomment-567267248>`_.

.. _`Proposal 126`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0126-type-applications-in-patterns.rst

.. _`Proposal 155`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst

.. _`GHC MR 3145`: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3145
