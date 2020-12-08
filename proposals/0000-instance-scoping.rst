Simplify type synonym and type instance declarations
=======================================================

.. author:: Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight::
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/386>`_.
.. sectnum::
.. contents::

Summary
---------

In a couple of places GHC is being too clever when dealing with type synonym
or type-family instance declarations.  This proposal suggests two
related simplifications, that make the language easier to read, less ad hoc,
and simpler to implement.

It depends on proposal #326 (Inivisible parameters for declarations).
This proposal has a draft implementation in MR !3145.

Proposed Change Specification
-----------------------------

* Change #1.  Introduce the following rule: in

  * a type synonym declaration, or
  * a type family instance declaration
  every type variable mentioned on the RHS must be bound on the LHS.

  To match this change, in the user manual, remove the text in
  `Implicit quantification in type synonyms and type family instances (6.4.11.12)
  <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html>`_,
  from the beginning of that section down to
  "Kind variables can also be quantified in visible positions...".

  (Implementation note: see ``Note [Implicit quantification in type synonyms]`` in ``GHC.Rename.HsType``.)

* Change #2.   Introduce the following rule: in a type family instance declaration,
  the instantiation of the family instance is fully determined by the left hand side, without
  looking at the right hand side.

Examples and motivation
-----------------------

Change #1
~~~~~~~~~

Consider::

  type T1 = 'Nothing :: Maybe a
  type T2 = 'Just ('Nothing :: Maybe a)
  
``T1`` is currently legal, yielding ``T :: forall a. Maybe a``. The general rule is that the
free variables of a *top-level* kind signature on the RHS are brought into scope
implicitly, and will be quantified in the final kind of the type constructor.

In constrast, ``T2`` is currently illegal, because the kind signature is not at the top level.

With this proposal, both declarations woudl be illegal.  Instead you must write:::

  type T1 @a = 'Nothing :: Maybe a
  type T2 @a = 'Just ('Nothing :: Maybe a)

so that all the variables occurring on the RHS are bound on the LHS.
This proposal exploits #326 to allow a nice, simple, uniform rule.
As the manual says, "The reason for this exception [the strange, ad-hoc,
top-level-kind-signature rule] is that there may be no other way to bind k".

The same rule applies to type family instances::

   type family F a :: k

   type instance F Int = Any :: j -> j

(where ``Any :: forall p. p``). This type instance is current legal, but under
this proposal ``j`` would not be in scope.  You would have to write::

   type instance F @(j->j) Int = Any :: j -> j

This form is already allowed today; it does not require #326.

Change #2
~~~~~~~~~

Consider::

  type family F a :: k

  type instance F Int = Char
  type instance F Int = Maybe

From the family declaration we see that ``F :: forall k. Type -> k``.
The two ``type instance`` declarations appear to have an identical head, but by
looking at the RHS we can infer that the invisible kind argument of ``F`` is
``Type`` in the first instance, and ``Type -> Type`` in the second.  It would
be much clearer to write::

  type instance F @Type         Int = Char
  type instance F @(Type->Type) Int = Maybe

and indeed this is already legal.

This proposal requires that the type instance be fully determined by the LHS,
so that the programmer sees two visibly distinct instance heads.  For the purpose
of determining the LHS, the RHS is ignored.  So under this proposal the
instance::

   type instance F Int = Char

would mean::

   type instance F @k Int = Char

(where the LHS instantation is at an unconstrained kind ``k``).
Now the kind of the RHS if fixed to be ``k``, and the kind of ``Char`` does
not match that, so the declaration is rejected.

The principle is that it should be possible to see what instance the
programmer intended by looking only at the instance head (the LHS).
This property already holds for *data* family instances.  Suppose
``D`` is a data family of kind::

   D :: forall k. (k->Type) -> k -> Type

Now consider ::

   data instance D p q where
      MkD :: forall r. r Int -> T r Int

So what kind do ``p`` and ``q`` have?  No clues from the header, but from
the data constructor we can clearly see that ``r :: Type->Type``.  Does
that mean that the the *entire data instance* is instantiated at ``Type``
like this::

   data instance D @Type (p :: Type->Type) (q :: Type) where
      ...

Or does it mean that the GADT data constructor specialises that kind argument,
thus::

   data instance D @k (p :: k->Type) (q :: k) where
     MkD :: forall (r :: Type -> Type).
            r Int -> T @Type r Int

(It might be specialised differently in some other data constructor ``MkD2``).
GHC avoids this question by determining the instance header solely from the
header.  This proposal simply extends the same principle to type family instances.

More examples
-------------

Here's an example from the wild (thanks Jakob Bruenker)::

  data Relation n m = MkR

  type Trans :: forall a b c . Relation a b -> Relation b c -> Relation a c
  type family Trans pa pb where
    Trans rel MkR = rel -- this type checks but is a partial type family

In current GHC this typechecks, but the type family is not total?  Why?
Because the fully-explicit version is::

  type family Trans pa pb where
    Trans @a @b @b rel MkR = rel

Notice the repeated ``b`` on the LHS. The author was entirely unaware
that the resulting type family was partial, because the equation he wrote
looked total.  With Change #2, the original program::

    type family Trans pa pb where
      Trans rel MkR = rel

would be rejected. WHy? Because the LHS imposes no kind constraints, so
we get::

    type family Trans pa pb where
      Trans @a @b @c (rel :: Relation a b) (MkR :: Relation b c) = ...

so the RHS must have kind ``Relation a c``.  But it doesn't; ``rel``
has kind ``Relation a b``.  So the declaration is rejected, which would
have saved Jakob some time.


Effect and Interactions
-----------------------

These changes will make fewer programs compile.

* For change #1 the approved new programming style requires proposal #326,
  and there is no backward compatible workaround.  So the phase-in will
  need to be planned.

* For change #2 there is a backward-compatible workaround, so we could
  perhaps bring it in immediately.  It would be somwhat tricky to implement
  a deprecation cycle, beucause we'd have to figure out whether the instantiaon
  was driven by the RHS


Costs and Drawbacks
-------------------


Alternatives
------------

Unresolved Questions
--------------------


Implementation Plan
-------------------

Easy to implement.

Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
