HasField redesign
=================

.. author:: Adam Gundry
.. date-accepted:: 2023-10-12
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/583>`_.
.. sectnum::
.. contents::


This proposal recommends improvements to the design of the ``HasField``
typeclass (differing from those planned under `#158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_).  In particular,
the proposed design supports updates using a new ``SetField`` class, adds
support for unlifted datatypes and fields, and specifies laws for the classes.

In order to keep this proposal simple, it does not yet propose adding support
for type-changing update, which is left for a future proposal.


Motivation
----------

Following `proposal #6 <https://github.com/ghc-proposals/ghc-proposals/pull/6>`_,
GHC 8.2 introduced a special built-in typeclass ``HasField`` in the
``GHC.Records`` module, defined thus::

  class HasField (x :: k) r a | x r -> a where
    getField :: r -> a

When the constraint solver sees a constraint of the form ``HasField "foo" T a``,
where ``T`` is a concrete datatype and ``foo`` is a symbol corresponding to one
of its fields, and this field is in scope, the constraint will be solved
automatically with a dictionary derived from the record selector function for
the field.

This makes it possible to get a form of type-directed name resolution for field
selection: given the expression ``getField @"foo" t``, the inferred type of
``t`` can be used to determine which ``foo`` field is meant, even if there are
multiple ``foo`` fields in scope and hence the expression ``foo t`` would be
ambiguous.  (This arises in particular with the ``DuplicateRecordFields``
extension, which has a somewhat ad hoc mechanism for disambiguating such
expressions that has been removed in GHC 9.4, following `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/pull/366>`_.)

GHC 9.2 includes support for using "record dot syntax" for selection with the
``OverloadedRecordDot`` extension, e.g. ``t.foo`` can be used as syntactic sugar
for ``getField @"foo" t``.  This is described in the accepted `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ (as modified by
`proposal #405 <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_).
However, while the proposals describe both ``OverloadedRecordDot`` and another
extension ``OverloadedRecordUpdate`` which allows type-based disambiguation of
record update expressions, only the selection part is fully implemented so far.

The accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ plans to change the
definition of ``HasField`` to support updates, which is necessary for the full
implementation of the ``OverloadedRecordUpdate`` extension.
An implementation of proposal #158 is available as `GHC merge request !3257
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3257>`_, but has not yet
been merged, because the compile-time performance cost of the selected
implementation strategy is unacceptably high.  Such costs were not really
considered in previous discussions, but it is not appropriate to slow down
compilation of all programs with records for the benefit only of those using
``HasField``.

In the light of experience implementing these proposals, and discussion arising
from `proposal #405 <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_,
it seems worth systematically re-evaluating the design choices surrounding
``HasField`` and type-directed name resolution for field updates.


Recap: Planned changes to HasField
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ plans to change the
definitions in ``GHC.Records`` to the following::

  class HasField (x :: k) r a | x r -> a where
    hasField :: r -> (a -> r, a)

  getField :: forall x r a . HasField x r a => r -> a
  getField = snd . hasField @x

  setField :: forall x r a . HasField x r a => r -> a -> r
  setField = fst . hasField @x

This makes it possible to both get and set fields, based on a single class.  The
``OverloadedRecordDot`` extension would continue to desugar field selection
syntax to call ``getField``, while the ``OverloadedRecordUpdate`` extension
would desugar record update syntax to call ``setField``.

Since ``setField`` has not yet been added to a released compiler, the version of
``OverloadedRecordUpdate`` in GHC 9.2 requires ``RebindableSyntax`` to be
enabled and a user-defined ``setField`` function to be in scope.  It provides no
standard definition of this function.


Design highlights
~~~~~~~~~~~~~~~~~
The essence of the new design is captured in the following definitions, which
will replace the existing contents of ``GHC.Records``.  For a complete picture
of the new contents of this module, including auxiliary definitions, see the
`Proposed Change Specification`_.

::

  type HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  class HasField x r a | x r -> a where
    getField :: r -> a

  type SetField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  class SetField x r a | x r -> a where
    modifyField :: (a -> a) -> r -> r
    setField :: a -> r -> r
    {-# MINIMAL modifyField | setField #-}

These are the key points of the new design.  Detailed justification for each
point is deferred to subsequent sections.

* The existing ``HasField x r a`` class continues to have a single method for
  record field selection, ``getField :: r -> a``.

* There is a new class ``SetField x r a`` for updates, rather than combining
  both selection and update into the ``HasField`` class (as in `proposal #158
  <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_).

* ``SetField x r a`` has two methods  ``setField :: a -> r -> r`` and
  ``modifyField :: (a -> a) -> r -> r``.

* The order of arguments to ``setField :: a -> r -> r`` is reversed compared to
  the status quo: it takes the new field value first, followed by the record
  being updated.

* The classes are representation-polymorphic, allowing support for unlifted
  fields and datatypes.

* The classes are polymorphic in the kind ``k`` of field labels.

* Functional dependencies are used to allow type inference to determine the
  field type from the record type and field name.

As noted above, type-changing update is not being considered in this proposal,
but may be addressed in a follow-up proposal.


Motivation for changing the accepted design
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Why change the accepted design from `proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_?
Defining ``getField`` and ``modifyField`` in separate classes is a better design:

* It gives more flexibility to users, in particular to define read-only or
  write-only virtual fields (cf. `proposal #286
  <https://github.com/ghc-proposals/ghc-proposals/pull/286>`_), and it leaves
  open the possibility of devising modifiers to mark particular fields as
  read-only or write-only.

* Types can be used to indicate whether
  particular definitions need read-only, write-only or read-write access to
  named fields.  For example, a function of type ``(HasField "foo" r Int,
  SetField "bar" r Bool) => r -> r`` can only read the ``foo`` field and write
  the ``bar`` field.

* GHC can emit more precise warnings when partial fields are used, indicating
  whether they are being used for selection or update.

* Keeping ``HasField`` essentially unchanged is more backwards-compatible,
  rather than forcing ``HasField`` users to change their code unnecessarily.

* A separate ``SetField`` class should make it easier to add type-changing
  update in a future proposal (though this is a controversial point, and this
  proposal does not commit to doing so).


Proposed Change Specification
-----------------------------

This proposal involves both changes to existing definitions in ``base``, and
adding new definitions. As per the `plan agreed with CLC
<https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/accepted/051-ghc-base-libraries.rst>`_,
the latter should first be added to the forthcoming ``ghc-experimental``
package.  Thus it adds two new modules, ``GHC.Records`` and
``GHC.Records.Experimental``.

The ``GHC.Records`` module (in the ``base`` package) will be defined as follows::

  {-# LANGUAGE AllowAmbiguousTypes #-}     -- for type of getField
  {-# LANGUAGE FunctionalDependencies #-}  -- for HasField class

  module GHC.Records
    ( HasField(getField)
    ) where

  import GHC.Types (Constraint, TYPE)

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- selected from the record type @r@.
  --
  -- This will be solved automatically for built-in records where the field is
  -- in scope, but manual instances may be provided as well.
  --
  type HasField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  class HasField x r a | x r -> a where
    -- | Selector function to extract the field @x@ from the record @r@.
    getField :: r -> a


The ``GHC.Records.Experimental`` module (in the ``ghc-experimental`` package)
will be defined as follows::

  {-# LANGUAGE AllowAmbiguousTypes #-}     -- for type of setField
  {-# LANGUAGE DefaultSignatures #-}       -- for setField/modifyField
  {-# LANGUAGE FunctionalDependencies #-}  -- for SetField class

  module GHC.Records.Experimental
    ( HasField(getField)
    , SetField(setField, modifyField)
    , Field
    ) where

  import GHC.Records (HasField(getField))
  import GHC.Types (Constraint, TYPE)

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  -- updated in the record type @r@.
  --
  -- This will be solved automatically for built-in records where the field is
  -- in scope, but manual instances may be provided as well.
  --
  -- Instances of this class are subject to the following laws, for every record
  -- value @r@ and field @x@:
  --
  -- > modifyField @x id r === r or ⊥
  -- > (modifyField @x g . modifyField @x f) r === modifyField @x (g . f) r
  -- > setField @x v r == modifyField @x (\ _ -> v) r
  --
  -- Where a 'HasField' instance is available as well as an instance of this
  -- class, they must together satisfy the laws defined on 'Field'.
  --
  type SetField :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  class SetField x r a | x r -> a where
    -- | Change the value stored in the field @x@ of the record @r@.
    modifyField :: (a -> a) -> r -> r
    default modifyField :: (r_rep ~ LiftedRep, a_rep ~ LiftedRep, HasField x r a) => (a -> a) -> r -> r
    modifyField f r = setField @x (f (getField @x r)) r

    -- | Update function to set the field @x@ in the record @r@.
    setField :: a -> r -> r
    default setField :: a_rep ~ LiftedRep => a -> r -> r
    setField v = modifyField @x (\ _ -> v)

    {-# MINIMAL modifyField | setField #-}

  -- | Constraint representing the fact that a field @x@ of type @a@ can be
  --  selected from or updated in the record @r@.
  --
  -- Where both 'HasField' and 'SetField' instances are defined for the
  -- same type, they must satisfy the following laws:
  --
  -- For every @r@ which has the field @x@
  -- (that is, wherever 'getField @x r' is defined):
  --
  -- > getField @x (setField @x v r) === v
  -- > setField @x (getField @x r) r === r
  --
  -- For every @r@ which does not have the field @x@
  -- (that is, wherever 'getField @x r' is not defined):
  --
  -- > getField @x (setField @x v r) === ⊥
  -- > setField @x (getField @x r) r === r or ⊥

  type Field :: forall {k} {r_rep} {a_rep} . k -> TYPE r_rep -> TYPE a_rep -> Constraint
  type Field x r a = (HasField x r a, SetField x r a)

See the `Design highlights`_ for a brief summary of the changes in this design
relative to the previously-accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_.  There are many
possible alternative choices of detail here, which are explored in the
`Alternatives`_ section.


Automatic constraint solving
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constraint solving for ``HasField`` constraints is essentially unchanged from
the behaviour of existing GHC versions, as described in the `GHC user's guide
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/hasfield.html#solving-hasfield-constraints>`_.
The only change is the introduction of representation-polymorphism, so that
``getField`` may be used even if the types involved are unlifted.

A constraint ``SetField x r a`` will be solved automatically if and only if the
corresponding constraint ``HasField x r a`` would be solved
automatically. Specifically, this occurs when ``r`` is a concrete record type,
``x`` is a ``Symbol`` naming one of the fields of the record, the field is in
scope and is not existentially quantified or higher-rank.

When a constraint is solved automatically, GHC will generate a dictionary with
an implementation of ``modifyField``, as if an instance for ``SetField``
existed. It will not actually generate instances of ``SetField``, however,
because instances have global scope whereas ``SetField`` constraints are solved
automatically only if the field is in scope.  (This is identical to the
behaviour for ``HasField``.)

If ``R x y`` is a record type with a field ``f :: T x`` belonging to
constructors ``MkR1`` and ``MkR2`` but not ``MkR3``, the generated dictionary
for ``SetField "f" (R x) a`` will be equivalent to: ::

  instance a ~ T x => SetField "f" (R x y) a where
    modifyField :: (T x -> T x) -> R x y -> R x y
    modifyField g MkR1{f=x, ..} = MkR1{f=g x, ..}
    modifyField g MkR2{f=x, ..} = MkR2{f=g x, ..}
    modifyField g MkR3{..}      = throw (RecUpdError ...)

That is, where a record type has a partial field, the generated definition of
``modifyField @x f r`` will throw an exception if and only if ``getField @x r``
will throw an exception.


User-defined instances
~~~~~~~~~~~~~~~~~~~~~~

Current GHC versions impose restrictions on when users may define their own
instances of ``HasField``.  `Proposal #515
<https://github.com/ghc-proposals/ghc-proposals/pull/515>`_ seeks to lift these
restrictions, but at the time of writing has not yet been accepted.  For
consistency, ``SetField`` will be subject to the same restrictions, and they
will be lifted for ``SetField`` if they are lifted for ``HasField``.


Change to ``-Wincomplete-record-updates``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Accepted `proposal #516 <https://github.com/ghc-proposals/ghc-proposals/pull/516>`_
introduces a warning flag ``-Wincomplete-record-selectors`` that emits a warning
when a ``HasField`` constraint is solved for a partial field.

For consistency with this, when a ``SetField`` constraint is solved for a
partial field, a warning will emitted if the existing
``-Wincomplete-record-updates`` warning flag is enabled.  (This warning flag is
not enabled as part of the ``-Wall`` warning group.)

Notice that easily distinguishing between selection and update in these warnings
requires the separation of the ``HasField`` and ``SetField`` classes.  Were they
a single class, it would be difficult to determine at the time of solving the
constraint whether it was being used for selection, update or both.


Change to ``OverloadedRecordUpdate``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `Order of arguments to setField`_ has been changed so that the field value
comes first, followed by the record value.  Correspondingly, the
``OverloadedRecordUpdate`` extension will be changed so that it calls
``setField`` with the arguments in the same order:

======================= ================================== ==================================
Expression              Previous interpretation            New interpretation
======================= ================================== ==================================
``e{lbl = val}``        ``setField @"lbl" e val``          ``setField @"lbl" val e``
======================= ================================== ==================================

This includes the case where ``RebindableSyntax`` is enabled, so ``setField``
refers to whichever name is in scope, rather than to ``GHC.Records.Experimental.setField``.
While this is a breaking change, the support for ``OverloadedRecordUpdate`` in
GHC 9.2 was explicitly advertised as experimental, so this should not
inconvenience users unexpectedly.



Examples
--------

For the first field of each example datatype, we describe the behaviour of the
constraint solver by giving the corresponding instances (though GHC does not
actually generate these instances).


Simple record
~~~~~~~~~~~~~

::

  data Person = Person { name :: String, age :: Int }

  instance a ~ String => HasField "name" Person a where
    getField = name

  instance a ~ String => SetField "name" Person a where
    modifyField g (Person name age) = Person (g name) age



Partial field
~~~~~~~~~~~~~

::

  data T = MkT1 { f1 :: Int } | MkT2 { g2 :: Bool }

  instance a ~ Int => HasField "f1" T a where
    getField = f1

  instance a ~ Int => SetField "f1" T a where
    modifyField g (MkT1 f1) = MkT1 (g f1)
    modifyField g (MkT2 _)  = throw (RecUpdError ...)


Representation polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~

With an unlifted field: ::

  data U = MkU { f :: Int# }

  instance a ~ Int# => HasField "f" U a where
    getField = f

  instance a ~ Int# => SetField "f" U a where
    modifyField g (MkU f) = MkU (g f)
    setField v (MkU f) = MkU v


With ``UnliftedDatatypes``: ::

  type V :: UnliftedType -> UnliftedType
  data V x = MkV { f :: x }

  instance a ~ x => HasField "f" (V x) a where
    getField = f

  instance a ~ x => SetField "f" (V x) a where
    modifyField g (MkV f) = MkV (g f)
    setField v (MkV f) = MkV v



Effect and Interactions
-----------------------

Record dot syntax
~~~~~~~~~~~~~~~~~

This proposal does not significantly affect ``OverloadedRecordDot``, as the
``HasField`` class is essentially unchanged.  It will allow
``OverloadedRecordDot`` to be used for unlifted datatypes and fields.

This proposal will make it easier to fully implement ``OverloadedRecordUpdate``,
which depends on having ``setField`` implemented.  As noted above, there is a
`change to OverloadedRecordUpdate`_ which may be noticed by users who are using
it already via ``RebindableSyntax``.


Virtual fields
~~~~~~~~~~~~~~
A "virtual field" is an instance of the ``HasField`` or ``SetField`` classes
that is defined explicitly by the user, and which does not correspond to an
existing record datatype.  For example::

  data V = MkV Int

  instance HasField "foo" V Int where
    getField (MkV i) = i

  instance SetField "foo" V Int where
    modifyField f (MkV i) = MkV (f i)

Even though ``V`` is not defined as a record, the presence of these instances
means ``foo`` can be used as a field, e.g. ``let e = MkV i in e.foo`` is
accepted with ``OverloadedRecordDot``.

Splitting ``HasField`` into separate ``HasField`` and ``SetField`` classes means
it is possible to define get-only or set-only virtual fields (although set-only
fields must still have the ability to define ``modifyField``).

Unlike the automatic constraint solving, which takes account of whether the
field name is in scope, normal ``instance`` declarations are globally scoped and
cannot be hidden at module boundaries.  This means that once a virtual field is
defined, its existence cannot be hidden from client code, which may be
undesirable as it may expose internal implementation details.

Virtual fields are sometimes useful for backwards compatibility after a field
has been refactored, since pattern synonym fields do not lead to automatic
constraint solving for ``HasField``.

It is sometimes useful to define virtual ``HasField`` instances that are
polymorphic in the field name, to give a specific datatype a convenient syntax
using ``OverloadedRecordDot``. For example, this is used by
`esqueleto <https://hackage.haskell.org/package/esqueleto-3.5.10.0/docs/src/Database.Esqueleto.Internal.Internal.html#line-2276>`_.

Various more general virtual field ``HasField`` instances have been proposed,
some of which (to be non-orphan) would need to live in ``GHC.Records``, such as:

* `Instances for tuples with numeric field names
  <https://github.com/haskell/core-libraries-committee/issues/143>`_
  (currently available in the
  `tuple-fields package <https://hackage.haskell.org/package/tuple-fields>`_).

* An `instance for Maybe <https://github.com/haskell/core-libraries-committee/issues/191>`_
  or `for a general Functor <https://github.com/ghc-proposals/ghc-proposals/issues/600>`_.

* `Unit datatypes with virtual fields based on MonadReader or MonadWriter
  <https://github.com/ghc-proposals/ghc-proposals/pull/583#issuecomment-1646789620>`_.

While these are undoubtedly convenient in some cases, some of them may lead to
code that cannot be easily understood in terms of field selection and update,
and (having been designed for ``RecordDotSyntax``) they may or may not interact
well with uses of ``HasField``/``SetField`` in optics libraries. Thus we do not
propose to add such instances to ``GHC.Records`` for now, pending further
experimentation.  In some cases it may be more appropriate to define new
operators, rather than overloading ``.`` with yet more potential
interpretations.  The intent of ``HasField``/``SetField`` is to allow type
information to help resolve otherwise ambiguous field names from Haskell
records, not to be a general abstraction over all possible notions of record or
uses of dot syntax.


Costs and Drawbacks
-------------------
The costs of this proposal should be no greater than those of the previously
accepted `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_:

* This will require moderate development effort, but does not seem like it will
  introduce a substantial maintenance burden.

* Novice users may find ``HasField``, ``SetField`` and overloaded record
  dot/update syntax more complex to reason about than traditional Haskell record
  syntax.


Backward Compatibility
----------------------

This proposal is more limited in its backward compatibility impact than the
previously accepted design (which would break all user-defined ``HasField``
instances).

Users relying on ``OverloadedRecordUpdate`` plus ``RebindableSyntax`` will need
to follow the change to the order of arguments to ``setField``.  This is a
breaking change, but ``OverloadedRecordUpdate`` has been `explicitly advertised
as experimental and subject to change <https://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/exts/overloaded_record_update.html>`_.

Otherwise, this proposal does not break backward compatibility.  Existing code
importing ``GHC.Records`` is unaffected because the module does not expose the
new definitions. While ``HasField`` has been generalised to support
representation polymorphism, GHC's existing defaulting support for
``RuntimeRep`` should ensure that user code continues to compile unchanged.


Alternatives
------------
There are many alternative designs possible for ``HasField`` and related
classes, which is part of the reason progress in this area has been slow.  This
proposal attempts a detailed discussion of each individual design choice, but
there are many variations possible.

* `Proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_
  used a design with a single ``HasField`` class, no type-changing update,
  functional dependencies.  This is the current accepted design, although the
  implementation is not yet merged into GHC HEAD.

* `Proposal #286 <https://github.com/ghc-proposals/ghc-proposals/pull/286>`_
  suggests splitting ``HasField`` into two classes and switching to type
  families in place of functional dependencies.  It gives a rather larger
  definition for the ``SetField`` class, including ``GetField`` as a
  superclass.

* `Proposal #510 <https://github.com/ghc-proposals/ghc-proposals/pull/510>`_
  adds support for overloaded variants alongside the existing support for
  overloaded records.

Another possibility is to abandon the plan to generalise ``HasField`` to support
updates and deprecate the ``OverloadedRecordUpdate`` extension, perhaps in
favour of another approach.

* Optics libraries provide various options for working with record types, and
  they do not necessarily need ``HasField``, although some use cases could
  directly benefit from it.

* `Proposal #310 <https://github.com/ghc-proposals/ghc-proposals/pull/310>`_
  suggests adding a syntax for record update that would explicitly specify the
  type, thereby avoiding the need for type-directed field resolution.

* It would be possible to extend name resolution so that datatype names could be
  used like module qualifiers, somewhat along the lines of `proposal #283
  <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_ on local modules.
  (See `discussion #506
  <https://github.com/ghc-proposals/ghc-proposals/discussions/506#discussioncomment-2741293>`_
  for more background on this idea.)  This would not allow updates that are
  polymorphic in the record type, but it would make it easier to disambiguate
  selectors/updates to uniquely refer to a single type.

This proposal does not address support for anonymous records. There are many
design choices around different ways to integrate anonymous records with
Haskell, and the right way forward is not obvious. ``HasField`` is designed to
reflect the capabilities of existing Haskell records. It may be useful for some
libraries implementing anonymous records as they can provide ``HasField``
instances in order to support record dot syntax or optics. However, it does not
attempt to add support for row polymorphism, in contrast with e.g.
`proposal #180 <https://github.com/ghc-proposals/ghc-proposals/pull/180>`_.

Subsequent subsections discuss alternative choices for particular aspects of the
design recommended by this proposal.


Order of arguments to setField
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`Proposal #158 <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_
specifies that the type of ``setField`` is::

  setField :: HasField x r a => r -> a -> r

However, swapping the order of arguments so that the new field value is first
means that composing of multiple updates for a single record becomes simpler::

  setField :: HasField x r a => a -> r -> r

  example :: (HasField "age" r Int, HasField "colour" r String) => r -> r
  example = setField @"age" 42 . setField @"colour" "Blue"

While we do not typically expect users to call ``setField`` directly, in cases
where they prefer to do so, this seems like a good reason to prefer this
argument order.  Moreover, this order is consistent with the ``set`` function in
the ``lens`` and ``optics`` libraries.  It is not clear what the rationale was
for the alternative order in the previous proposal.

Since this proposal specifies that calls to ``setField`` take the field value
first, followed by the record, it is not backward compatible with code that
relied on the previous behaviour when using ``OverloadedRecordUpdate`` with
``RebindableSyntax``.  We could revert to the previous order of arguments to
avoid this backward incompatibility, if the committee prefers this approach.


Single class vs. multiple classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Proposal #286 <https://github.com/ghc-proposals/ghc-proposals/pull/286>`_
suggests splitting ``HasField`` into two classes, there named ``GetField`` and
``SetField``, permitting selection and update respectively.  It was primarily
motivated by the possibility of supporting read-only (virtual) fields.
The present proposal similarly splits ``HasField`` into two classes, for the
reasons set out in `Motivation for changing the accepted design`_.


Relationships between the classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
There are various options for the superclass relationships between the split
classes.  `Proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_ suggests having
``GetField`` be a superclass of ``SetField``. However, this would rule out the
possibility of write-only fields, and incur additional compile-time cost at each
overloaded update in order to generate an (often unnecessary) ``GetField``
dictionary.

Instead we propose that ``HasField`` and ``SetField`` should be independent
classes, with no superclasses, and that ``Field`` should be a constraint synonym
for both constraints.  This constraint synonym means that where both
``getField`` and ``setField`` are used, users can write simpler types, and GHC
can use it to represent inferred types more simply.



Naming the classes
^^^^^^^^^^^^^^^^^^
We propose to keep the name ``HasField`` for the existing class.  This is
backwards-compatible with existing code, avoiding unnecessary breaking changes.

However, this will lead to a long-lasting inconsistency in naming, because
``GHC.Records.Experimental`` will export ``HasField(getField)`` and ``SetField(modifyField)``.
An alternative would be to rename ``HasField`` (e.g. to ``GetField``), at the
cost of breaking any code with an explicit import like ``HasField(getField)``,
or that defines a virtual field instance of ``HasField``.

While we could use a type synonym ``type HasField = GetField`` for partial
backwards compatibility, this would not allow defining instances, and would mean
that a ``HasField(..)`` import could no longer import ``getField``.



Downsides of keeping the classes independent
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
A potential disadvantage of splitting ``HasField`` into two independent classes
is that where a user defines a "virtual field" that requires indexing into a
data structure (e.g. a map), it may be possible to implement an operation that
gets and modifies a field more efficiently than defining it from ``getField``
and ``modifyField``.  This is why `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ settled on
``hasField :: r -> (a -> r, a)``.  This represents a lens, i.e. the combination
of a getter and setter into a single value, although it uses a first-order
representation that is simpler and does not compose as well as the "van
Laarhoven" or profunctor representations of lenses.

However, practical cases where the choice of ``hasField`` vs. the combination of
``getField`` and ``modifyField`` matters are likely to be rare.  In particular,
normal record types with the built-in constraint-solving behaviour do not gain
anything from ``hasField`` being a single method. Where this matters, users are
likely to be better off using an optics library.  Thus we prefer the simplicity
of separate classes.

If users do wish to organise field-like lenses into a class, they can define an
auxiliary class such as the following::

  class Field x r a => FieldLens x r a where
    fieldLens :: Lens' r a
    fieldLens = lens getField setField

  -- | Instance will be selected by default, but can be overridden by defining an
  -- instance for a specific type with a non-default 'fieldLens' implementation
  instance {-# OVERLAPPABLE #-} Field x r a => FieldLens x r a

We do not propose to add such a class to ``GHC.Records.Experimental``, since it is better
defined by specific optics libraries.  (The ``optics`` library defines a class
``LabelOptic`` that plays essentially this role.)

Laws
~~~~

Where ``HasField`` and ``SetField`` instances are defined we expect the lens
laws to hold.  As noted in the Haddocks in the Proposed Change Specification,
the specific laws are:

- For each type with a ``SetField`` instance and every record value ``r``
  and field ``x``: ::

    modifyField @x id r === r or ⊥
    (modifyField @x g . modifyField @x f) r === modifyField @x (g . f) r

  This ensures that ``modifyField :: (a -> a) -> r -> r`` defines a functor.
  The "PutPut" lens law follows as a consequence.

- For each type with both ``HasField`` and ``SetField`` instances and every
  record value ``r`` which has a field ``x``: ::

     getField @x (setField @x v r) === v  -- PutGet
     setField @x (getField @x r) r === r  -- GetPut

  or if ``r`` does not have the field ``x`` (i.e. ``getField @x r === ⊥``): ::

     getField @x (setField @x v r) === ⊥
     setField @x (getField @x r) r === r or ⊥

Where the constraint solver automatically solves one of these constraints, the
laws will be satisfied.

Where a field is absent, that is where ``getField`` is undefined,
the laws permit ``modifyField`` to be defined (to be a no-op) or undefined.
However it may not change the constructor so that the field is present.

A disadvantage of independent classes is that it is slightly unsatisfactory to
have typeclass laws relating them (as the instances may be defined in separate
modules). This would is unlikely to cause practical problems, however.  It would
be more of an issue in a language where the laws were enforced as part of the
class.


Functional dependencies
~~~~~~~~~~~~~~~~~~~~~~~
The existing ``HasField`` class expresses the relationship between the record
type and the field type using a functional dependency::

  class HasField x r a | x r -> a

That is, the field label and record type should together determine the field
type.  This is necessary to allow good type inference.  In particular, it allows
the type of a composition of field selectors to be inferred::

  getField @"foo" . getField @"bar"
    :: (HasField "foo" b c, HasField "bar" a b) => a -> c

The middle type ``b`` appears only in the context, so it would be ambiguous in
the absence of the functional dependency.

Instead of using a functional dependency, it is also possible to express this
using a type family (associated or otherwise), like so::

  class HasField x r where
    type FieldType x r :: Type

    getField :: r -> FieldType x r

With this definition, we obtain::

  getField @"foo" . getField @"bar"
    :: (HasField "foo" (FieldType "bar" a), HasField "bar" a) =>
       a -> FieldType "foo" (FieldType "bar" a)

Introducing such a type family would give more options to optics library
implementers and other power users, and `proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_ suggests making this
change.

However, we propose to retain the use of functional dependencies in the class
definitions, for the following reasons:

* The functional dependency approach generally leads to simpler inferred types
  because unsolved constraints look like ``HasField x r a`` which has a natural
  reading "``r`` has a field ``x`` of type ``a``".  In contrast, the type family
  approach ends up with unsolved ``HasField x r`` constraints (meaning ``r`` has
  a field ``x`` of unspecified type) and equalities including ``FieldType``.
  (See `previous discussion on proposal #158
  <https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449419429>`_.)

* Supporting representation polymorphism with the type family approach would introduce
  extra complexity, because we would need another type family to determine the
  ``RuntimeRep`` of the field, and it would be difficult to hide this type
  family from users.  In contrast, supporting them is relatively straightforward
  with functional dependencies, and GHC will automatically hide unused representation
  polymorphism.

* If we wish to extend ``SetField`` to support type-changing update in the
  future, it is desirable that either the original or updated types may be used
  to infer the other.  This can be achieved using multiple functional
  dependencies, something like this::

    class SetField (x :: k) s t a b | x s -> a, x t -> b, x s b -> t, x t a -> s

  A similar effect is possible to achieve with type families
  (e.g. see `the SameModulo approach by @effectfully
  <https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right#the-samemodulo-approach-full-code>`_)
  but requires additional complexity.  While we do not propose type-changing
  update for now, we wish to leave the door open for adding it in a follow-up
  proposal.

* It is desirable to permit user-defined ``HasField`` instances that may not
  strictly be consistent with the automatic constraint-solving behaviour in some
  corner cases (see `proposal #515
  <https://github.com/ghc-proposals/ghc-proposals/pull/515>`_).  This is
  relatively harmless with functional dependencies, because the worst that can
  happen is the equivalent of incoherent instance resolution (risking the
  results of type inference being confusing, but not threatening type
  soundness).  In contrast, type family consistency checks are crucial to type
  soundness, so more care would be needed to ensure the ``FieldType`` type
  family could not reduce to inconsistent values as a result of user-defined
  instances interacting with the built-in constraint solver.

Functional dependencies do not carry evidence.  This means that from the given
constraints ``(HasField x r a, HasField x r b)`` it would not be possible to
conclude that ``a ~ b``.  However this does not seem like a significant
practical limitation in the ``HasField`` context.



Higher-rank fields
~~~~~~~~~~~~~~~~~~
Consider the following::

  data Rank1 = Rank1 { identity :: forall a . a -> a }

  data Rank2 = Rank2 { withIdentity :: (forall a . a -> a) -> Bool }

In the first definition, the field has a rank-1 type, but this means the
selector function has a type with a ``forall`` to the right of an arrow.
Similarly, in the second definition, a rank-2 field type leads to a higher-rank
selector function type::

  identity     :: Rank1 -> forall a . a -> a  -- NOT forall a . Rank1 -> a -> a (in recent GHCs)

  withIdentity :: Rank2 -> (forall a . a -> a) -> Bool

Should it be possible to solve ``HasField`` or ``SetField`` constraints
involving such fields?  Unfortunately it is not feasible to solve for
"impredicative" constraints such as
``HasField "identity" Rank1 (forall a . a -> a)``,
even with the recent introduction of Quick Look Impredicativity (following
`proposal #274 <https://github.com/ghc-proposals/ghc-proposals/pull/274>`_).
Bidirectional type inference, on which both ``RankNTypes`` and
``ImpredicativeTypes`` (now) rely, requires that instantiations of
``forall``-bound variables be determined while traversing the term, prior to the
constraint solver being invoked.

On the other hand, it would be possible in principle to solve constraints such
as ``HasField "identity" Rank1 (a -> a)`` for arbitrary ``a``, making it appear
as if the field has an infinite family of types.  However, this would not extend
to ``SetField``, because there we really need the value being set to be
polymorphic.  Moreover, it would violate the functional dependency ``x r -> a``
on the ``HasField`` class, leading to a violation of confluence: given wanteds
``HasField "identity" r (α -> α)`` and ``HasField "identity" r (β -> β)``,
applying the fundep forces ``α ~ β``; whereas if we were first to learn ``r ~
Rank1`` then we could solve both constraints without requiring ``α ~ β``.

Accordingly, we propose that ``HasField`` or ``SetField`` constraints
involving fields with higher-rank types should not be solved automatically.
(This is the existing behaviour for ``HasField`` in current GHC versions.)


Partial fields
~~~~~~~~~~~~~~
In ``Haskell2010`` it is permitted to define *partial fields*, i.e. fields that
do not belong to every constructor of the datatype.  This means that traditional
record selection and update may throw runtime exceptions, as in these examples::

  data T = MkT1 { partial :: Int } | MkT2

  t = MkT2
  oops1 = partial t
  oops2 = t { partial = 0 }

Many Haskell programmers prefer not to define partial fields, as part of a
general desire to avoid unnecessary partiality (see for example `proposal #351
<https://github.com/ghc-proposals/ghc-proposals/pull/351>`_).

Partial fields may be identified at definition sites via the existing
``-Wpartial-fields`` warning.  However, this is somewhat conservative: it is
perfectly safe to *define* partial fields provided they are *used* only via
record construction and pattern-matching, not via selection or update.

Users have `asked for the ability to prevent unsafe uses while permitting
datatype definitions
<https://www.reddit.com/r/haskell/comments/ln6eu1/implementation_of_nofieldselectors_is_merged/gnzviyt/>`_,
because giving field names can help with readability when a datatype has many
constructors and many fields. The accepted `proposal #516
<https://github.com/ghc-proposals/ghc-proposals/pull/516>`_ adds a
new warning ``-Wincomplete-record-selectors`` when ``HasField`` constraints are
solved with a partial selector function, and this proposal adds the
corresponding feature for ``SetField``.  This relies on the fact that
``HasField`` and ``SetField`` are distinct classes, so GHC can emit an
appropriate warning for selection and update.


Updates could ignore partial fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In principle, it is not necessary for ``setField`` or ``modifyField`` to emit a
runtime error if used with a field that is not present in the datatype; they
could silently return the value unchanged instead.  This behaviour may be more
convenient in some circumstances, but may also mask errors, and would not be
consistent with traditional record updates.

We could imagine giving the option to the user, e.g. via some modifier on the
datatype definition.  Somewhat related is `proposal #535
<https://github.com/ghc-proposals/ghc-proposals/pull/535>`_, which suggests an
extension ``MaybeFieldSelectors`` to control whether partial fields can lead to
runtime exceptions.


Refrain from solving partial fields?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Another option would be for GHC to refrain from solving ``HasField`` or
``SetField`` constraints automatically where the fields involved are partial.
This would allow users to define virtual fields with the behaviour they want,
without conflicting with the automatic solutions.  See `this comment from
@pnotequalnp <https://github.com/ghc-proposals/ghc-proposals/pull/583#issuecomment-1489278894>`_
for more motivation for this idea.

However, this would make ``getField`` and ``setField`` less consistent with
traditional record selectors and record updates.  Moreover it would lead to
backwards incompatibility for ``HasField``.


Affine traversals
^^^^^^^^^^^^^^^^^
Optics libraries in principle have a better story to tell here. Partial fields
give rise to *affine traversals*, where the accessor function returns a
``Maybe`` value and the setter leaves the value unchanged if it does not mention
the field (rather than throwing a runtime exception).

We could consider supporting this using built-in classes like the following::

  class GetPartialField x r a | x r -> a where
    getPartialField :: r -> Maybe a

  class SetPartialField x r a | x r -> a where
    modifyPartialField :: (a -> a) -> r -> r

  class FieldTotal x r a (is_total :: Bool) | x r -> a is_total

Note that ``modifyField`` will throw an exception on missing fields, whereas
``modifyPartialField`` would return the value unchanged.  The ``FieldTotal``
class would allow an optics library to determine whether a particular field was
total and hence whether it should produce a lens or an affine traversal.

For now we propose not to include support for partial fields through classes
like this, in the interests of minimizing complexity, and because it is not
clear how they could be used together with ``OverloadedRecordDot``.


Setting vs modification
~~~~~~~~~~~~~~~~~~~~~~~

A previous iteration of the design supported only ``setField :: a -> r -> r`` and not
``modifyField :: (a -> a) -> r -> r``.  The latter generalises ``setField`` to
allow modifying any ``a`` values in the datatype (of which there may be none).

It is easy to implement ``setField`` in terms of ``modifyField``, but not vice
versa, because we would need to define: ::

  modifyFieldAlt :: forall x r a . (HasField x r a, SetField x r a) => (a -> a) -> r -> r
  modifyFieldAlt f r = setField @x (f (getField @x r)) r

This imposes an additional ``HasField`` constraint, and will necessarily be
partial if ``getField`` is partial (whereas ``modifyField`` can in principle be
total, although this will not be the case for automatically solved constraints,
as discussed above).

Thus we propose to include both ``modifyField`` and ``setField`` as class
methods. Default implementations can be provided such that users implementing
virtual field instances typically need implement only one (except where
representation polymorphism is in use, or where there is no ``HasField``
instance).

A consequence of this is that it is not possible to use ``SetField`` for types
that are "write-only", e.g. where they do not contain a value for the field at
all, and hence ``modifyField`` cannot be defined.

Another possibility would be to define ``setField`` at the top level, rather
than being a class method.  This would make the ``SetField`` dictionary smaller
(and lead to it being represented as a newtype).  However, from the perspective
of a user defining instances of ``SetField`` it seems preferable to be able to
define either ``setField`` or ``modifyField`` (or both, if there is some runtime
performance advantage to doing so).  Moreover, the presence of representation
polymorphism would require this definition to be given a "compulsory unfolding",
meaning that ``setField x`` would be inlined at every call site (at which point
the representation of the argument is necessarily fixed).  See `previous
discussion on the ghc-devs mailing list
<https://mail.haskell.org/pipermail/ghc-devs/2021-October/020241.html>`_.


Kind of field labels
~~~~~~~~~~~~~~~~~~~~

When ``HasField`` was originally introduced in `proposals #6
<https://github.com/ghc-proposals/ghc-proposals/pull/6>`_, the kind of the
parameter ``x`` representing the field label was polymorphic::

  class HasField (x :: k) r a | x r -> a where ...

While the class allows ``k :: Type`` to vary freely, ``HasField`` constraints
will be solved only if it is instantiated to ``Symbol``.  Moreover,
``OverloadedRecordDot`` and ``OverloadedRecordUpdate`` will only ever generate
constraints using ``Symbol``.  Other possibilities were permitted in order to
support hypothetical anonymous records libraries, which might support different
kinds of fields, e.g. drawn from explicitly-defined enumerations.

In principle it would be possible to simplify the class by specialising it to
use ``Symbol`` rather than ``k``.  However we propose to retain the poly-kinded
definition in the interests of generality and compatibility.  For example,
the ``record-hasfield`` library makes use of the possibility to define label
kinds other than ``Symbol``, allowing tuples of labels to be used for
composition of fields.  In particular, it defines an instance like::

  instance (HasField x1 r1 r2, HasField x2 r2 a2)
      => HasField '(x1, x2) r1 a2

which means ``getField @("foo", "bar")`` will be treated like the composition
``getField @"bar" . getField @"foo"``.

Another potential application could be to use labels of kind ``Nat`` to index
into a tuple::

  instance HasField 1 (x,y) where
    getField = fst



Representation polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The existing definition of ``HasField`` does not support unlifted fields or
datatypes, such as in the following example::

  data T = MkT { foo :: Int# }

  type R :: forall (l :: Levity) . TYPE (BoxedRep l) -> TYPE (BoxedRep l)
  data R a where
    MkR :: { bar :: a } -> R a

The constraint ``HasField "foo" T Int#`` or ``HasField "bar" (R a) a`` are not
even well-kinded, because the field type and record type are required to be
lifted.

At the time ``HasField`` was introduced, it was not possible to define type
classes over potentially unlifted types.  However, thanks to representation polymorphism
in more recent GHC versions, this is now relatively straightforward.  In
particular, we can define::

  type HasField :: forall {k}{r1 :: RuntimeRep}{r2 :: RuntimeRep} .
                     k -> TYPE r1 -> TYPE r2 -> Constraint
  class HasField x r a | x r -> a where
    -- | Selector function to extract the field from the record.
    getField :: r -> a

This makes it possible to formulate and solve constraints such as ``HasField
"foo" T Int#``.
See `#22156 <https://gitlab.haskell.org/ghc/ghc/-/issues/22156>`_ for a request
for this feature.

Observe that the ``RuntimeRep`` parameters are inferred rather than specified
(hence the curly braces in the kind signature).  This means that when
``getField`` is used with explicit type application, the ``RuntimeRep``
parameters are skipped.

The default implementation of ``setField`` in terms of ``modifyField`` (and vice
versa) works only when the representation is constrained via a default signature
to be ``LiftedRep``.  This is currently necessary for the default definition to
typecheck, because there is no other way to express the requirement that at each
instance the representation should be concrete.  It may be possible to lift this
restriction in the future (see `#14917
<https://gitlab.haskell.org/ghc/ghc/-/issues/14917>`_), but for the moment,
users defining their own ``SetField`` instances for unlifted types will need to
define both ``setField`` and ``modifyField``.



Linear types
~~~~~~~~~~~~
Rather like representation polymorphism, it is possible to make the definition
of ``HasField`` multiplicity-polymorphic, so that it could be used with the
``LinearTypes`` extension, like this (kind and representation polymorphism
omitted for clarity)::

  type HasField :: Multiplicity -> Symbol -> Type -> Type -> Constraint
  class HasField m x r a | ... where
    getField :: r %m -> a

  type SetField :: Multiplicity -> Multiplicity -> Type -> Type -> Type -> Constraint
  class SetField m1 m2 x s t b | ... where
    setField :: b %m1 -> s %m2 -> t

The constraint solver would set the ``Multiplicity`` parameters appropriately
when solving a ``HasField`` or ``SetField`` constraint for a particular concrete
record type and field.

However, this introduces extra complexity, the current implementation of
``LinearTypes`` does not yet support linear record projection (`#18570
<https://gitlab.haskell.org/ghc/ghc/-/issues/18570>`_) or multiplicity annotations
on fields (`#18462 <https://gitlab.haskell.org/ghc/ghc/-/issues/18462>`_),
and it has various limitations on solving constraints involving ``Multiplicity``.
Thus we do not propose to support multiplicity-polymorphic ``HasField``
or ``SetField`` constraints for the time being.


Visible foralls
~~~~~~~~~~~~~~~
At the time of writing, GHC supports "visible foralls" (visible dependent
quantification) in kinds, but not in the types of terms.  The accepted `proposal #281
<https://github.com/ghc-proposals/ghc-proposals/pull/281>`_ allows
the types of terms to use visible foralls.  This is desirable for ``getField``
and similar functions, because it is always necessary to supply the field name
using a type application.

We currently have::

  getField :: forall {k} (x :: k) r a . HasField x r a => r -> a

which at use sites must use an explicit type application, e.g. ``getField
@"foo"``.  If the type application is omitted, an ambiguity error will result,
because there is no way to infer the field label from the record type or field
type.

If and when support for visible foralls is added, the type of ``getField`` could
change to::

  getField :: forall r a {k} . forall (x :: k) -> r -> a

meaning that we could instead use ``getField "foo"`` at use sites.  (Per the
visible forall proposal, here ``"foo"`` is a type-level ``Symbol`` even though
it syntactically resembles a ``String`` literal.)

This would be a breaking change, and visible dependent quantification is not yet
fully implemented, so changing ``getField`` and ``setField`` to use it is not
part of the present proposal.


Pattern synonyms
~~~~~~~~~~~~~~~~
An infelicity with the current constraint solving behaviour for ``HasField`` is
that it does not work for record pattern synonyms.  Thus where
``OverloadedRecordDot`` or similar is used, replacing a datatype with an
equivalent record pattern synonym may require declaring manual ``HasField`` and
``SetField`` instances.

It would be relatively easy to extend the automatic behaviour to support single
record pattern synonyms.  For example, given the declaration::

  pattern MyPair{car,cdr} = (car, cdr)

it would be possible to solve a constraint like::

  HasField "car" (x, y) x

and hence a declaration like this would be accepted::

  swap :: (x, y) -> (y, x)
  swap p = MyPair { car = p.cdr, cdr = p.car }

However, the fact that pattern synonyms can be added for arbitrary types (in
this example, for the built-in type of pairs) mean that such behaviour can give
rise to incoherent solutions to ``HasField`` constraints (cf. `proposal #515
<https://github.com/ghc-proposals/ghc-proposals/pull/515>`_). For example, if
another module defined::

  pattern MyPair2{car,cdr} = (cdr, car)

then the constraint ``HasField "car" (x, x) x`` would be solved differently
depending on whether ``car`` from ``MyPair`` or from ``MyPair2`` was in scope.

Moreover, it is unclear how to extend the automatic treatment of pattern
synonyms to handle multiple-constructor types.  For example, given the
declarations::

  pattern MyLeft{val}  = Left val
  pattern MyRight{val} = Right val

we would ideally generate a solution to ``HasField "val" (Either a a) a`` that
used both patterns, as in::

  get_val :: Either a a -> a
  get_val MyLeft{val} = val
  get_val MyRight{val} = val

However, it is not clear how to do this in general, since pattern synonyms are
not necessarily grouped and may overlap in arbitrarily complex ways.  (While
``COMPLETE`` pragmas do give a notion of grouping for pattern synonyms, their
purpose is currently limited to the pattern-match completeness checker, and is
not clear that they should have a semantic impact.)


Unresolved Questions
--------------------
Changing ``SetField`` to support type-changing update is deliberately left out
of this proposal, so that it can be considered in detail as a subsequent
proposal.


Implementation Plan
-------------------
Support with the implementation of this proposal would be welcome.  The
implementation of ``setField`` (in some form) is
currently blocking the full implementation of ``OverloadedRecordUpdate``
(`proposal #282 <https://github.com/ghc-proposals/ghc-proposals/pull/282>`_).


Compile-time performance
~~~~~~~~~~~~~~~~~~~~~~~~
It is important that the implementation of this proposal should not regress
compile-time (or runtime) performance.  This was a problem for the previous
implementation of proposal #158 (`GHC merge request !3257
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3257>`_).

The existing implementation of ``HasField`` benefits from being able to reuse
the record selector functions that GHC already generates ahead of time for every
field of every datatype.  Since ``HasField`` has a single method corresponding
to this function, the constraint solver is able to construct a dictionary merely
by casting the existing selector function.

The previous implementation attempt followed this, generating additional
functions ahead of time for every field of every datatype.  However, this can
add a significant cost when defining large record datatypes, especially if
``SetField`` is not subsequently used.  Thus a better implementation strategy
would probably be to generate the dictionaries on-the-fly in the constraint
solver (much as when GHC compiles a traditional record update it generates and
type-checks a suitable case expression).

If necessary, we could imagine adding flags to allow the user to control whether
to generate the needed functions at datatype definition sites (which may be more
efficient if ``SetField`` is used frequently) or at use sites (which may be more
efficient if records are large and ``SetField`` is used rarely).


Default methods
~~~~~~~~~~~~~~~

The default definitions of ``setField`` and ``modifyField`` as written above are
not currently accepted by GHC, which appears to be a bug (see `#23884
<https://gitlab.haskell.org/ghc/ghc/-/issues/23884>`_).  If it turns out to be
difficult to resolve this, we may wish to revisit the design.
