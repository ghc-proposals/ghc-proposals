Relaxing ``HasField`` constraints
=================================

.. author:: Ollie Charles, Adam Gundry
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/21324
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/515>`_.
.. sectnum::
.. contents::

In the `overloaded record fields proposal
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst>`_,
there is a `set of limitations
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#virtual-record-fields>`_
as to when a user can declare custom ``HasField`` instances. In this proposal
we relax these restrictions, allowing users to define ``HasField`` instances
when they were previously unable to.


Motivation
----------

The current conditions prohibit a user from defining ``HasField`` custom
instances under a few conditions. This proposal argues that these constraints
are excessively restrictive and that GHC would benefit from relaxing these
constraints.

Consider the following module::

  module Reactive.Banana ( Behavior ) where
  newtype Behavior a = Behavior { unB :: ... }

This type represents a time-varying value, and occurs in the
``reactive-banana`` project. A ``Behavior`` is a ``Functor``, and it would be
nice if we could "lift" any fields in ``a`` to ``Behavior a``::

  instance HasField x a b => HasField (x :: k) (Behavior a) (Behavior b) where
    getField b = getField @x <$> b

Unfortunately, because ``Behavior`` was defined as ``{ unB :: ... }``, GHC
considers it to have fields, and we are unable to write the ``HasField``
instance we want. Worse, this is an abstraction leak - users of
``reactive-banana`` can only see ``data Behavior a`` - its constructor and
fields are not exported. This leads to a confusing error message if a user
tries to define the above custom (orphan) instance, as they are told the type
has fields, though without reading the source they are otherwise unable to
observe that.

This was originally reported as `GHC issue #21324 <https://gitlab.haskell.org/ghc/ghc/-/issues/21324>`_.

A similar request is at `issue #21369
<https://gitlab.haskell.org/ghc/ghc/-/issues/21369>`_. In this issue,
``@parsonsmatt`` would like to write::

  instance
      ( SymbolToField sym ent typ
      , PersistEntity ent
      )
    =>
      HasField sym (Entity ent) typ
    where
      getField ent =
          view (persistFieldLens (symbolToField @sym @ent @typ)) ent

However, they are unable to as ``Entity`` is defined to have fields. The
workaround was to `redefine Entity without fields
<https://github.com/yesodweb/persistent/pull/1381/files>`_, but this is a
rather significant change, and if users wanted to pattern match on ``Entity``
as they would before, they now need a ``MonadFail`` constraint (due to matching
on a pattern synonym).

If the constraints blocking this instance were dropped, Matt could simply have
added the instance above.


Proposed Change Specification
-----------------------------

In the `overloaded record fields proposal
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst>`_,
there is a `set of limitations
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#virtual-record-fields>`_
as to when a user can declare custom ``HasField`` instances. These conditions
prevent defining a ``HasField`` instance when:

* ``HasField _ r _`` where ``r`` is a variable;

* ``HasField _ (T ...) _`` if ``T`` is a data family (because it might have
  fields introduced later, using data instance declarations);

* ``HasField x (T ...) _`` if ``x`` is a variable and ``T`` has any fields at
  all (but this instance is permitted if ``T`` has no fields);

* ``HasField "foo" (T ...) _`` if ``T`` has a field ``foo`` (but this instance
  is permitted if it does not).

These restrictions are in place to guarantee coherence, but come at the cost of
ruling out a variety of productive instances. This proposal allows users to opt
in to lifting all four of these restrictions.

More precisely, a new "severe" warning flag ``-Wincoherent-hasfield-instances``
is introduced.  "Severe" means that it is treated as an error by default (as if
``-Werror=incoherent-hasfield-instances`` were specified); see `proposal #571
<https://github.com/ghc-proposals/ghc-proposals/pull/571>`_ for a proposal to
introduce this category more generally.  When a user defines an instance that
would violate the restrictions above, a warning will be emitted controlled by
the ``-Wincoherent-hasfield-instances`` flag.

The effect of this is that by default, no more programs will be accepted than
the status quo, but users may opt in to allowing such programs by using
``-Wwarn=incoherent-hasfield-instances`` to downgrade the severity of the
warning, or ``-Wno-incoherent-hasfield-instances`` to silence it entirely.


Overlapping instances
~~~~~~~~~~~~~~~~~~~~~

``HasField`` constraints have `special treatment in the constraint solver
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst#solving-hasfield-constraints>`_.
Currently, the constraint solver is able to ignore the possibility that
user-defined ``HasField`` instances will overlap with those generated
internally, because of the restrictions on user-defined instances.

With the change proposed here, it will be legal to define instances of
``HasField`` just like any other class, ignoring its special-purpose constraint
solving behaviour.  Instead, during constraint solving, the compiler will check
for overlapping ``HasField`` instances at use sites and report errors, just as
if an "implicit instance" of ``HasField`` was generated by each in-scope field
in each datatype.  (As is already the case for ``HasField``, fields that are not
in scope do not need to be considered.)

The "implicit instance" for a type ``T`` with a field ``f`` of type ``A`` is: ::

  instance b ~ A => HasField "f" T b where
    getField = f

In particular, this instance will not have the ``OVERLAPPABLE``, ``OVERLAPPING``
or ``INCOHERENT`` flags set, regardless of language extensions in effect.

For example: ::

  data T = MkT { f :: Int }  -- implicitly defines instance b ~ Int => HasField "f" T b

  instance HasField fld T () where
    getField _ = ()

  eg1 = getField @"f" (MkT 0)       -- rejected: needs HasField "f" T alpha which matches both instances
  eg2 = getField @"f" (MkT 0) :: () -- rejected: needs HasField "f" T () which matches both instances
  eg3 = getField @"g" (MkT 0)       -- accepted: matches only user-defined instance


Functional dependencies
~~~~~~~~~~~~~~~~~~~~~~~

The "implicit instance" introduced above is not legal for users to define
because it violates the functional dependency on ``HasField``.  (Though it is
possible to work around this, see `proposal #374
<https://github.com/ghc-proposals/ghc-proposals/pull/374>`_.)

The compiler will not check consistency of functional dependencies between
"implicit instances" and user-defined instances.  Users who define instances
that may conflict with implicit instances (i.e. those that result in an
error/warning from ``-Wincoherent-hasfield-instances``) are responsible for
ensuring that type inference behaves reasonably.


Examples
--------

Example 1
~~~~~~~~~

For one example, see the example in the motivation. With the changes in this
proposal, the following would be accepted::

  -- Implementation omitted, this is just the public interface users see
  module Reactive.Banana where
    data Behavior a

  {-# OPTIONS_GHC -Wno-incoherent-hasfield-instances #-}
  module Reactive.Banana.Orphans where
    instance HasField x a b => HasField (x :: k) (Behavior a) (Behavior b) where
      getField b = getField @x <$> b

(This example is split over two modules to reflect the reality that
``Reactive.Banana`` will likely live in a different compilation unit than
``Reactive.Banana.Orphans``).

Since the public API does not expose the ``unB`` field of ``Behaviour``, the
fact that the user-defined ``HasField`` instance overlaps with it is not a
problem.


Example 2
~~~~~~~~~

Finally, this example revisits `GHC issue #21369
<https://gitlab.haskell.org/ghc/ghc/-/issues/21369>`_::

  {-# OPTIONS_GHC -Wno-incoherent-hasfield-instances #-}
  module Database.Persist.Class.PersistEntity where
    data Entity record =
      Entity { entityKey :: Key record
             , entityVal :: record }

    instance {-# OVERLAPPABLE #-}
        ( SymbolToField sym ent typ
        , PersistEntity ent
        )
      =>
        HasField sym (Entity ent) typ
      where
        getField ent =
            view (persistFieldLens (symbolToField @sym @ent @typ)) ent

Despite ``Entity`` being defined as a record with fields, the instance here
would be permitted. Should a user try and access the field ``entityKey``, the
more specific ``HasField "entityKey"`` instance (automatically generated by
GHC) would match. Should any field /other/ than ``entityKey`` or ``entityVal``
be accessed, the custom instance would be used instead.

Note that this example requires the use of the ``OVERLAPPABLE`` keyword. This
is because ``Entity`` as defined with fields, so GHC will create ``HasField``
instances for ``entityKey`` and ``entityVal``. The instance ``HasField sym
(Entity ent) typ`` overlaps with these two (more specific) instances, and if we
don't mark the new instance as ``OVERLAPPABLE`` we'll get an error about
overlapping instances if we try and access ``entityKey`` or ``entityVal``
fields.


Effect and Interactions
-----------------------

This change allows incoherent (non-canonical) solutions to ``HasField`` constraints: two
identical ``HasField`` constraints solved in different modules may be
instantiated with different dictionaries.  However, during optimization the
compiler may assume that two ``HasField`` dictionaries of the same type have the
same value.  In such situations, optimization may change program semantics.
(This is similar to situations that may arise with ``INCOHERENT`` pragmas.)
This possibility is why this proposal requires users to explicitly disable the
``-Wincoherent-hasfield-instances`` warning.

For example::

  module M where
    data T = MkT { foo :: Int }

    bar = getField @"foo" (MkT 42)

  {-# OPTIONS_GHC -Wno-incoherent-hasfield-instances #-}
  module N where
    import M (T(MkT))

    instance HasField "foo" T Int where
      getField (MkT x) = negate x

    baz = getField @"foo" (MkT 42)

Here ``bar`` evaluates to ``42`` but ``baz`` evaluates to ``-42``, despite
having apparently the same definition.

Incoherence occurs only in the presence of orphan instances, because for a
user-defined ``HasField`` instance to be non-orphan it must be defined in the
same module as the record datatype.

It's also worth noting that custom ``HasField`` instances defined as non-orphan
instances have an interaction with exported field selectors::

  {-# OPTIONS_GHC -Wno-incoherent-hasfield-instances #-}
  module M ( T(T) ) where
    data T = MkT { foo :: Int }

    instance HasField "foo" T Bool where
      getField (MkT x) = x > 0

  module N where
    import M (T)

    bar :: T -> Bool
    bar = getField @"foo"

In this example, module ``N`` is importing the ``T`` type but *not* the ``foo``
field. However, ``M`` defines a ``HasField "foo"`` instance which is
unconditionally available in ``N`` (as ``N`` imports ``M``).

The ``HasField "foo" T Bool`` constraint arising from the call to ``getField``
in ``bar`` is solved using this user-defined instance.  As the ``foo`` field
selector is not in scope, the built-in constraint solving behaviour does not
apply.

If the definition of ``bar`` was in module ``M`` instead, or if module ``N``
had an unqualified import of module ``M``, an overlapping instance error would
be reported because the ``foo`` field would be in scope and would conflict with
the user-defined ``HasField`` instance.


Costs and Drawbacks
-------------------

The implementation cost of this proposal is likely to be relatively small (removing the
code for the check is easy, but adjusting the constraint solver to check for overlapping
``HasField`` instances ma be more complex).  However, the implementation cost may later rise if we
want to provide more informative error messages. For now, we propose that the
compiler just emit the traditional overlapping instances error messages.

``HasField`` currently uses a functional dependency to determine the field type
from the field name and record type, but it has been suggested that a new
``FieldType`` type family should be used instead (e.g. in `proposal #286
<https://github.com/tysonzero/ghc-proposals/blob/patch-3/proposals/separate-get-set-field.md>`_).
If this suggestion is followed, and the restrictions on ``HasField`` instances
are relaxed as proposed here, we would need to be careful to forbid
``FieldType`` to be reduced inconsistently (as this could otherwise lead to type
unsoundness). This should be possible to check by extending the existing
consistency check for type family instances.


Backwards Compatibility
-----------------------

This proposal is fully backwards compatible.


Alternatives
------------

Rather than relaxing all four restrictions at once, we could instead remove
restrictions as requested. Such a strategy may lead to smaller changes, but on
the other hand may lead to more changes. Perhaps it is better to make sweeping
changes to a new feature while it's settling, rather than having developers try
and develop against a moving target.

We could imagine adding explicit annotations to particular types or fields to
prevent ``HasField`` constraints being solved automatically for those cases, and
relax the restrictions only when the annotations are present.  However this
requires new syntax (or a new
`modifier <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0370-modifiers.rst>`_)
and requires the author of the original datatype to add an annotation, which may
prevent downstream users from adding useful ``HasField`` instances.

The original version of this proposal suggested that the optimiser should
refrain from assuming canonicity for ``HasField`` dictionaries (similar to the
``IP`` class used for implicit parameters).  This would reduce optimization
opportunities for all uses of ``HasField``, and it is difficult to characterise
how bad this might impact runtime performance.  Instead, the current version
opts to make users responsible for ensuring that potentially-incoherent
``HasField`` instances are not problematic in their context.


Implementation Plan
-------------------

Ollie Charles has offered to help implement this proposal if it is accepted.
