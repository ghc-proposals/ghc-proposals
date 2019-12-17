Treat kind variables and type variables identically in ``forall``
=================================================================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-05-22
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15264
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/103>`_.
.. contents::


GHC has supported kind polymorphism since version 7.4. Kind polymorphism allows (among
other features) users to specify *kind variables* in their declarations. When the
feature was first introduced, type variables and kind variables were wholly separate,
introduced in different syntactic contexts and subject to different rules around
type inference. (For example, all kind variables were always of kind ``BOX``; abstraction
over kind constructors like a promoted ``Maybe`` was not allowed.) Of particular interest,
GHC 7 offered no way to explicitly bring a kind variable into scope. Instead, users just
mentioned ``k`` in an appropriate context and the kind variable came into being.

Because of this legacy, current GHC still treats kind variables differently than type
variables, as expanded in the Motivation_ section, below.

Under this proposal, kind variables and type variables would be treated identically,
removing this legacy distinction. This is a *simplification* over the status quo.

Note that this proposal assumes that proposal `#83`_ has been accepted, meaning that
explicit quantification over kind variables is possible with only ``-XPolyKinds``,
not needing ``-XTypeInType``.

.. _`#83`: https://github.com/goldfirere/ghc-proposals/blob/no-type-in-type/proposals/0000-no-type-in-type.rst


Motivation
------------
There are two ways in which current treatment
type and kind variables differ. This section enumerates these, showing that they are awkward
to understand and maintain.

We will consider the following collection of type signatures::

  f :: a -> a
  g :: forall a. a -> a
  h :: forall a. a -> b -> a
  i :: Proxy (a :: k) -> ()
  j :: forall a. Proxy (a :: k) -> ()


1. **The "forall-or-nothing" rule**

   Currently, all of the signatures above are accepted except ``h``. GHC has a rule I call the
   "forall-or-nothing" rule: if you begin a type signature with an explicit ``forall``,
   you must specify *all* the type variables brought into scope. The idea behind
   forall-or-nothing is that if a user is writing an explicit ``forall``, then they
   should be protected if they make an accidental misspelling of a variable later on.

   However, because kind variables initially couldn't be introduced in a ``forall``,
   the forall-or-nothing rule doesn't apply to variables mentioned at least once after
   a ``::`` in a type. So, even though ``j`` doesn't explicitly introduce ``k``, the
   type is accepted as in compliance with the forall-or-nothing rule. A consequence
   of this is that there is no way for a type signature to indicate that a kind variable
   is meant to come from an outer scope.

2. **Scoped type variables**

   An explicit ``forall`` in a type signature does double-duty: not only does it bring
   a type variable into scope in the type, that type variable is also in scope in the
   definition of a function (with ``-XScopedTypeVariables``). Now we must ask: how
   do we bring a *kind* variable into scope? GHC brings kind variables into scope
   whenever there is a ``forall`` present in the type signature, regardless of whether
   or not the kind variable was explicitly included in the ``forall``. Referring
   back to the type signatures above, this means that ``k`` is brought into scope
   in the body of ``j`` but not ``i``. It would be more consistent to have to put
   ``k`` in the explicit list of variables brought into scope.

Proposed Change Specification
-----------------------------

Type variables and kind variables will be treated identically in types.
Specifically:

1. With ``-Wcompat``, warn if a kind variable is brought into scope implicitly in
   a type with an explicit ``forall``. This applies to type signatures and to other
   contexts that allow a ``forall`` with the forall-or-nothing rule in effect (for example,
   class instances).

2. Two releases after `#83`_ is implemented, make it an error to bring a kind variable
   into scope implicitly in a type with an explicit ``forall`` and where the forall-or-nothing
   rule is in effect.

Effect and Interactions
-----------------------
This will not be backward compatible, because more explicit listing of kind variables
will be necessary. (To wit, example ``j`` above would be newly rejected.) The fix will
be to explicitly list kind variables in ``forall`` clauses. However, this has been
possible only with ``-XTypeInType``, not simply ``-XPolyKinds``. Thus, it seems best
to wait until 2 releases after `#83`_ is implemented, so that adding the kind variable
to the ``forall`` will not require adding a new extension.

Costs and Drawbacks
-------------------
This is a simplification to the specification and implementation of GHC,
at least after the ``-Wcompat`` migration help
is done. I can't think of any drawbacks.


Alternatives
------------

* Keep status quo, but that's hard to justify.

* Don't wait for two releases after `#83`_, given that the fix is compatible
  with three releases if ``-XTypeInType`` is enabled.

* Treat the ``k`` in ``forall (a :: k). Proxy a -> ()`` specially, allowing
  this syntax to bring ``k`` into scope. Specifically, any unbound variable
  mentioned in a kind signature of a type variable binder could be brought
  into scope. This was suggested on the GitHub thread, but I find it to be
  an unnecessary special case, just to preserve a sliver of legacy behavior
  that we needn't preserve. I'm listing it here as a viable, consistent alternative,
  however.

Unresolved questions
--------------------
None that I know of.


Implementation Plan
-------------------
I or a close collaborator volunteers to implement. Offers of help are welcome.
