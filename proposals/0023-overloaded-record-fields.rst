Overloaded Record Fields
========================

.. author:: Adam Gundry
.. date-accepted:: 2017-02-04
.. implemented:: 8.2
.. contents::

This is a proposal to introduce a new built-in typeclass,
``HasField``, to allow type-based resolution of field names and
polymorphism over record selectors.  It also makes some changes to the
existing ``OverloadedLabels`` extension for consistency.  It *does
not* currently introduce a new extension ``OverloadedRecordFields``.


Motivation
----------

A serious limitation of the Haskell record system is the inability to
overload field names in record types: for example, if the data types

.. code-block:: haskell

  data Person  = Person  { personId :: Int, name :: String }
  data Address = Address { personId :: Int, address :: String }

are in scope in the same module, there is no way to determine which
type an occurrence of the ``personId`` record selector refers to.  A
common workaround is to use a unique affix for each record type, but
this leads to less clear code and obfuscates relationships between
fields of different records.  Qualified names can be used to
distinguish record selectors from different modules, but using one
module per record is often impractical.  Thus it is desirable to make
use of type information to disambiguate record selectors.

Moreover, modern Haskell code often makes use of lenses, which
(amongst other things) provide a compositional way to get and set
fields in nested record types.  However, defining lenses requires
either significant boilerplate or the use of Template Haskell, and
either the field name or lens must use an affix to avoid ambiguity.
This proposal lays the foundation for a compiler mechanism to generate
lenses automatically, requiring nothing more than a field definition.


Recap: DuplicateRecordFields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``DuplicateRecordFields`` extension, introduced in GHC 8.0.1,
makes it possible for the same field to be declared multiple times in
a single module.  (Without it, ``Person`` and ``Address`` cannot even
be defined simultaneously.)  However, its ability to disambiguate
record selectors is deliberately limited to information that is
immediately apparent during type *checking*, in the interests of
predictable type inference behaviour.

For example, the following are accepted when both occurrences of
``personId`` are in scope:

.. code-block:: haskell

  \ p -> personId (p :: Person)         -- explicit type signature on argument
  \ p -> (personId :: Person -> Int) p  -- type pushed in to selector

  f :: Person -> Int
  f = personId                          -- another way to push type in to selector

However, the following are rejected:

.. code-block:: haskell

  \ (p :: Person) -> personId p  -- type of argument not used, even if obvious
  personId (Person 1 "Me")       -- again, argument is not an explicit signature

See `#11343 <https://gitlab.haskell.org/ghc/ghc/issues/11343>`_ for
discussion of this aspect of ``DuplicateRecordFields``.  We may wish
to make changes to the rules for ``DuplicateRecordFields`` in the
future, but doing so is outside the scope of this proposal.

The point of this proposal is that we would like some way to write
polymorphic record projections, such that the ambiguous selector is
resolved using constraint-based type *inference*.


Recap: OverloadedLabels
~~~~~~~~~~~~~~~~~~~~~~~

The ``OverloadedLabels`` extension, also introduced in GHC 8.0.1,
provides a way to indicate that an identifier should be resolved using
type inference.  This extension provides a new syntax (using a ``#``
prefix) for "labels", identifiers whose meaning depends on their type
as well as their name.  A label ``#foo`` has type ``IsLabel "foo" t =>
t`` where ``IsLabel`` is an ordinary type class (rather like
``IsString`` but with an additional type parameter for the text of the
label).

This extension has already been used by
various anonymous records libraries, such as
`bookkeeper <https://hackage.haskell.org/package/bookkeeper>`_,
`rawr <http://hackage.haskell.org/package/rawr>`_,
`ruin <http://hackage.haskell.org/package/ruin>`_ and no doubt others.
However, it is not so easy to use with normal Haskell record
datatypes, because there is no built-in support for polymorphism over
record selectors.

We want to give an instance

.. code-block:: haskell

    instance ... => IsLabel x (r -> a)

to make it possible to interpret a label (e.g. ``#personId``) as a
record selector function, determining the record and field types as
part of the type inference process.  For example, the expression

.. code-block:: haskell

  \ (p :: Person) -> #personId p

will be accepted with inferred type ``Person -> Int``.  In addition,
the proposed change enables definitions that are polymorphic over
record fields.

But what should go in the instance context? In the following section,
we introduce a new built-in class ``HasField`` for this purpose.


Proposed Change
---------------

HasField class
~~~~~~~~~~~~~~

The new module ``GHC.Records`` defines the following:

.. code-block:: haskell

  class HasField (x :: k) r a | x r -> a where
    getField :: r -> a

A ``HasField x r a`` constraint represents the fact that ``x`` is a
field of type ``a`` belonging to a record type ``r``.  The
``getField`` method gives the record selector function.

This is a magic built-in typeclass (similar to ``Coercible`` or
``KnownSymbol``, for example).  It is given special treatment by the
constraint solver, although users may also define their own instances,
as described below.


Solving HasField constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the constraint solver encounters a constraint ``HasField x r a``
where ``r`` is a concrete datatype with a field ``x`` in scope, it
will automatically solve the constraint using the field selector as
the dictionary, unifying ``a`` with the type of the field if
necessary.  This happens irrespective of which extensions are enabled,
as with other built-in classes with special constraint solving
behaviour (e.g. ``Coercible``).

For the ``personId`` example above, the end result is rather like
having an instance

.. code-block:: haskell

  instance HasField "personId" Person Int where
    getField = personId

except that this instance is not actually generated anywhere, rather
the constraint is solved directly by the constraint solver.

A field must be in scope for the corresponding ``HasField`` constraint
to be solved.  This retains the existing representation hiding
mechanism, whereby a module may choose not to export a field,
preventing client modules from accessing or updating it directly.
Thus we cannot actually generate and export ``HasField`` instances
from defining modules: since there is no mechanism for limiting the
scope of instances, doing so would expose representation details that
should be hidden.


Limitations on solving HasField constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If a record field does not have a selector function because its type would allow
an existential variable to escape, the corresponding ``HasField`` constraint
will not be solved.  For example,

.. code-block:: haskell

  {-# LANGUAGE ExistentialQuantification #-}
  data Exists t = forall x . MkExists { unExists :: t x }

does not give rise to a selector ``unExists :: Exists t -> t x`` and we will not
solve ``HasField "unExists" (Exists t) a`` automatically.

If a record field has a polymorphic type (and hence the selector function is
higher-rank), the corresponding ``HasField`` constraint will not be solved,
because doing so would violate the functional dependency on ``HasField`` and/or
require impredicativity.  For example,

.. code-block:: haskell

  {-# LANGUAGE RankNTypes #-}
  data Higher = MkHigher { unHigher :: forall t . t -> t }

gives rise to a selector ``unHigher :: Higher -> (forall t . t -> t)`` but does
not lead to solution of the constraint ``HasField "unHigher" Higher a``.


Interaction with GADTs
^^^^^^^^^^^^^^^^^^^^^^

A record GADT may have a restricted type for a selector function, which may lead
to additional unification when solving ``HasField`` constraints.  For example,

.. code-block:: haskell

  {-# LANGUAGE GADTs #-}
  data Gadt t where
    MkGadt :: { unGadt :: Maybe v } -> Gadt [v]

gives rise to a selector ``unGadt :: Gadt [v] -> Maybe v``, so the solver will reduce
the constraint ``HasField "unGadt" (Gadt t) b`` by unifying ``t ~ [v]`` and
``b ~ Maybe v`` for some fresh metavariable ``v``, rather as if we had an instance

.. code-block:: haskell

  instance (t ~ [v], b ~ Maybe v) => HasField "unGadt" (Gadt t) b


Interaction with DatatypeContexts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If a record type has an old-fashioned datatype context, the ``HasField``
constraint will be reduced to solving the constraints from the context, rather
like superclasses.  For example,

.. code-block:: haskell

  {-# LANGUAGE DatatypeContexts #-}
  data Eq a => Silly a = MkSilly { unSilly :: a }

gives rise to a selector ``unSilly :: Eq a => Silly a -> a``, so
the solver will reduce the constraint ``HasField "unSilly" (Silly a) b`` to
``Eq a`` (and unify ``a`` with ``b``), rather as if we had an instance

.. code-block:: haskell

  instance (Eq a, a ~ b) => HasField "unSilly" (Silly a) b


Changes to OverloadedLabels extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``IsLabel`` class defined in ``GHC.OverloadedLabels`` is changed
from:

.. code-block:: haskell

  class IsLabel (x :: Symbol) t where
    fromLabel :: Proxy# x -> t

to:

.. code-block:: haskell

  class IsLabel (x :: Symbol) t where
    fromLabel :: t

  instance HasField x r a => IsLabel x (r -> a) where
    fromLabel = getField

When the ``OverloadedLabels`` extension is enabled, a
label ``#foo`` is translated to
``fromLabel @"foo" :: IsLabel "foo" t => t``,
instead of ``fromLabel (proxy# :: Proxy# "foo")``.

The ``Proxy#`` argument has been removed, since this is redundant in
the presence of ``TypeApplications``.  In addition, an ``IsLabel``
instance has been supplied for ``(->)`` that delegates to the
``HasField`` class.  This will require changes to code using the GHC
8.0.1 version of ``OverloadedLabels``.

The instance for ``IsLabel x (r -> a)`` makes its possible to use an
overloaded label as a field selector.  For example, ``\ x -> #foo x``
has type ``HasField "foo" r a => r -> a``.

However, other useful instances for ``IsLabel`` are available.  In
particular, if we had a partner class to ``HasField`` that allowed
polymorphism over record updates, overloaded labels could be used to
create lenses.  (In the case of the van Laarhoven representation used
by the ``lens`` package, this requires a newtype to avoid overlap with
the existing ``IsLabel`` instance for functions.)  Built-in support
for such a class may be introduced in the future, or users can use
Template Haskell or Generics to define it in the meantime.


Interaction with RebindableSyntax
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When ``RebindableSyntax`` is enabled in addition to ``OverloadedLabels``,
a label ``#foo``
is translated to ``fromLabel @"foo"`` using whatever ``fromLabel`` is
in scope (see `#12243
<https://gitlab.haskell.org/ghc/ghc/issues/12243>`_ for a request for
this feature).

This allows alternative interpretations of labels that cannot be
expressed using the ``IsLabel`` class.  For example, labels could be
translated directly to van Laarhoven lenses without the need for a
newtype wrapper.  This allows maximum flexibility for the user to
specify how labels get interpreted.

However, all the labels in the module are subject to the same
translation, so it may not be very convenient to use two different
libraries that rely on this option.  This is why ``OverloadedLabels``
and the ``IsLabel`` class are retained.


Virtual record fields
~~~~~~~~~~~~~~~~~~~~~

Users may define their own instances of ``HasField``, provided they do
not conflict with the built-in constraint solving behaviour.  This
allows "virtual" record fields to be defined for datatypes that do not
otherwise have them.

For example, this instance would make the ``name`` field of ``Person``
accessible using ``#fullname`` as well:

.. code-block:: haskell

  instance HasField "fullname" Person String where
    getField = name

More substantially, an anonymous records library could provide
``HasField`` instances for its anonymous records, and thus be
compatible with the polymorphic record selectors introduced by this
proposal.  For example, something like this makes it possible to use
overloaded labels to access ``Record`` values with the appropriate
string in the type-level list of fields:

.. code-block:: haskell

  data Record (xs :: [(k, Type)]) where
    Nil  :: Record '[]
    Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

  instance HasField x (Record ('(x, a) ': xs)) a where
    getField (Cons _ v _) = v
  instance HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
    getField (Cons _ _ r) = getField @x r

  r :: Record '[ '("personId", Int), '("name", String) ]
  r = Cons Proxy 42 (Cons Proxy "R" Nil)

  i = #personId r

Since representations such as this can support field labels with kinds
other than ``Symbol``, the ``HasField`` class is poly-kinded (even
though ``IsLabel`` and the built-in constraint solving works only at
kind ``Symbol``).  In particular, this allows users to declare scoped
field labels such as in the following example:

.. code-block:: haskell

  data PersonFields = PersonId | Name

  s :: Record '[ '(PersonId, Int), '(Name, String) ]
  s = Cons Proxy 43 (Cons Proxy "S" Nil)

  j = getField @PersonId s

In order to avoid conflicting with the built-in constraint solving,
the following user-defined ``HasField`` instances are prohibited (in
addition to the usual rules, such as the prohibition on type
families):

 * ``HasField _ r _`` where ``r`` is a variable;

 * ``HasField _ (T ...) _`` if ``T`` is a data family (because it
   might have fields introduced later, using data instance declarations);

 * ``HasField x (T ...) _`` if ``x`` is a variable and ``T`` has any
   fields at all (but this instance is permitted if ``T`` has no fields);

 * ``HasField "foo" (T ...) _`` if ``T`` has a field ``foo`` (but this
   instance is permitted if it does not).

If a field has a higher-rank or existential type, the corresponding ``HasField``
constraint will not be solved automatically (as described above), but in the
interests of simplicity we do not permit users to define their own instances
either.


Drawbacks
---------

Existing code using ``OverloadedLabels`` from GHC 8.0.1 will need to
be adapted to work with the changes proposed here.  Removing the
``Proxy#`` argument should be straightforward, but the new ``IsLabel``
instance may conflict with existing instances elsewhere.  If
necessary, ``RebindableSyntax`` can be used to adapt existing code
that relies on giving alternative instances to ``IsLabel``.

Using ``OverloadedLabels`` instead of ``DuplicateRecordFields`` to
disambiguate record selectors may lead to worse compiler performance,
as it requires more use of the constraint solver.  Moreover, excessive
use of polymorphism over record fields may reduce runtime performance
if there is not enough specialisation, as with other uses of typeclass
polymorphism.  Both of these issues are limited to code that uses the
new extension.


Alternatives
------------

OverloadedRecordFields extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The current version of this proposal does not introduce
``OverloadedRecordFields`` as a new extension.  It could be introduced
as the combination of ``OverloadedLabels`` and
``DuplicateRecordFields``, even though these extensions are somewhat
orthogonal.  However, we might want to define it differently as
discussed in the next subsection.


Multiple interpretations of labels
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under a previous revision of this proposal, ``OverloadedRecordFields``
was a separate extension to ``OverloadedLabels``, and the translation
of a label depended on the extensions that are enabled.  The following
table shows how the desugaring of overloaded labels would depend on
which of ``OverloadedRecordFields``, ``OverloadedLabels`` and
``RebindableSyntax`` were enabled:

======  ======  ======  =================================================================
ORF     OL      RS      Desugaring of ``#foo``
======  ======  ======  =================================================================
Off     Off     On/Off  Invalid syntax
On      Off     Off     ``GHC.Records.getField @"foo" :: HasField "foo" r a => r -> a`` (the "``HasField`` interpretation")
On/Off  On      Off     ``GHC.OverloadedLabels.fromLabel @"foo" :: IsLabel "foo" t => t`` (the "``IsLabel`` interpretation")
On/Off  On/Off  On      ``fromLabel @"foo"`` using in-scope ``fromLabel``
======  ======  ======  =================================================================

Note that the ``HasField`` interpretation is a special case of the
``IsLabel`` interpretation, where all occurrences of ``IsLabel``
constraints are forced to use the ``(->))`` instance (compare the
`Forced Class Instantiation proposal
<https://github.com/ghc-proposals/ghc-proposals/pull/23>`_).

Where overloaded field selectors are required but general overloaded
labels are not, the ``HasField`` interpretation is simpler and likely
to give better error messages than the ``IsLabel`` interpretation,
because the user will not be presented with the ``IsLabel`` class at
all.  Some users would like to write polymorphic field selectors
without the additional polymorphism provided by ``IsLabel``.
Moreover, type inference is less likely to lead to ambiguity errors (see example below).

The downside of this alternative is that the different possible
interpretations may be confusing.  For this reason, the current
proposal is to retain the more general ``IsLabel`` interpretation
only.  We can always introduce ``OverloadedRecordFields`` as a
separate extension in the future, or users can make use of
``RebindableSyntax`` to get the ``HasField`` interpretation.

Example of ambiguity under IsLabel interpretation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider the following module:

.. code-block:: haskell

  {-# LANGUAGE OverloadedRecordFields, NoMonomorphismRestriction #-}
  import Control.Category
  import Prelude hiding ((.))
  fooBar = #foo . #bar

If the labels are directly interpreted as polymorphic selector
functions using ``HasField``, type inference succeeds for ``fooBar``,
giving it the inferred type

.. code-block:: haskell

  (HasField "foo" s t, HasField "bar" r s) => r -> t

where the functional dependency on ``HasField`` means that ``s`` is
not an ambiguous type variable.

However, if labels are interpreted using ``IsLabel``, the inferred
type of ``fooBar`` is

.. code-block:: haskell

  (Category cat, IsLabel "foo" (cat s t), IsLabel "bar" (cat r s)) => cat r t

where ``s`` is ambiguous.  (Of course, the ambiguity can be resolved by giving
``fooBar`` a more specific type signature.)



Alternatives to a built-in HasField class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to define a ``HasField``-like class in user code,
without requiring built-in constraint solving behaviour.  Two
approaches have been demonstrated:

 * using generic programming (`generic-records
   <https://hackage.haskell.org/package/generic-records>`_);

 * using Template Haskell (`overloaded-records
   <http://hackage.haskell.org/package/overloaded-records>`_).

It could be argued that the ability to do this in user code means it
is not necessary to make it part of the compiler.

However, these approaches must be explicitly enabled for each datatype
(either by deriving ``Generic`` or by calling a Template Haskell
function).  They cannot take advantage of the efficient record
selector functions that GHC already generates for every record
datatype.  More seriously, they do not support representation hiding,
because they cannot take account of whether or not fields are
exported.


Other minor design alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rather than dropping the ``Proxy#`` argument to ``fromLabel``, we
could retain it.  This would be backwards compatible with GHC 8.0.1,
and would allow ``fromLabel`` to be called directly without use of the
``TypeApplications`` extension.  However, the argument is unnecessary
and would cause a (small) performance overhead.  Moreover, users are
not usually expected to call ``fromLabel`` directly, rather they will
typically use the overloaded label syntax.

We could use a type family rather than a functional dependency in the
definition of ``HasField``.  That is, we could define

.. code-block:: haskell

  class HasField (x :: k) r where
    type FieldType x r :: *
    getField :: r -> FieldType x r

with the constraint solver automatically reducing ``FieldType x r``
whenever ``r`` is a concrete record type with a field ``x``.  This is
slightly more expressive, as it is possible to talk about the type of
a field independently of a particular ``HasField`` constraint.
However, it is more complex and significant care would be required to
check user-defined ``FieldType`` instances (as a conflict with the
built-in behviour would threaten type soundness, not merely
coherence).


Unresolved Questions
--------------------

In the interests of simplicity, this proposal does not include a class
to provide polymorphism over record updates (needed to interpret
overloaded labels as lenses), nor does it discuss anonymous records.
The exact design of such features still needs final specification.
They should be compatible with the changes proposed here, however.

Unlike record datatypes, record pattern synonyms do not currently lead to the
automatic solution of ``HasField`` constraints.  In principle this is possible,
but it needs careful specification, and is left for future work.
