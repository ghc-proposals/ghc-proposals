.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Overloaded Record Fields
========================

This is a proposal to introduce a new extension,
``OverloadedRecordFields``, to make it easier to work with record
datatypes that reuse the same field names.  It also makes some changes
to the existing ``OverloadedLabels`` extension for consistency.


Motivation
----------

A serious limitation of the Haskell record system is the inability to
overload field names in record types: for example, if the data types

.. code-block:: haskell

  data Person  = Person  { personId :: Int, name :: String }
  data Address = Address { personId :: Int, address :: String }

are in scope in the same module, there is no way to determine which
type an occurrence of the ``personId`` record selector refers to.  A
common workaround is to use a unique prefix for each record type, but
this leads to less clear code and obfuscates relationships between
fields of different records.  Qualified names can be used to
distinguish record selectors from different modules, but using one
module per record is often impractical.

DuplicateRecordFields
~~~~~~~~~~~~~~~~~~~~~

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

See `Trac #11343 <https://ghc.haskell.org/trac/ghc/ticket/11343>`_ for
discussion of this aspect of ``DuplicateRecordFields``.

We need some way to write polymorphic record projections, so that
the ambiguous selector is resolved using type *inference*.

From OverloadedLabels to OverloadedRecordFields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``OverloadedLabels`` extension, also introduced in GHC 8.0.1,
provides a way to indicate that an identifier should be resolved using
type inference.  This extension provides a new syntax (using a ``#``
prefix) for "labels", identifiers whose meaning depends on their type
as well as their name.

The current proposal makes it possible to interpret a label
(e.g. ``#personId``) as a record selector, determining the record and
field types as part of the type inference process.  For example, the
expression

.. code-block:: haskell

  \ (p :: Person) -> #personId p

will be accepted with inferred type ``Person -> Int``.
This works by giving the occurrence of ``#personId`` the type
``r -> a`` and introducing a constraint ``HasField "personId" r a``,
meaning that ``r`` must be some type with a field ``personId :: a``.
Unification will determine that ``r ~ Person``, and then ``HasField
"personId" Person a`` can be solved automatically, setting ``a ~ Int``
in the process.

In addition to resolving ambiguous field selectors to a single record
type using type inference, the proposed change enables definitions
that are polymorphic over record fields, so for example

.. code-block:: haskell

  \ p -> #personId p + (1 :: Int)

will be given the inferred type

.. code-block:: haskell

  HasField "personId" r Int => r -> Int

and can be applied to any record type with a field ``personId ::
Int``.


Proposed Change
---------------

HasField class and OverloadedRecordFields extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The module ``GHC.Records`` defines the following class:

.. code-block:: haskell

  class HasField (x :: Symbol) r a | x r -> a where
    fromLabel :: r -> a

When the new extension ``OverloadedRecordFields`` is enabled:

* overloaded label syntax is available, and
* a label ``#foo`` is translated to
  ``GHC.Records.fromLabel @"foo" :: HasField "foo" r a => r -> a``.

The second point assumes ``OverloadedLabels`` and ``RebindableSyntax``
are *disabled* (see `Multiple versions of fromLabel`_ below).

Note that this is orthogonal to ``DuplicateRecordFields``.  While they
will often be used together, it is entirely possible to use
``DuplicateRecordFields`` alone (avoiding ambiguous selectors via type
annotations if necessary) or ``OverloadedRecordFields`` alone (using
overloaded label syntax even where a plain selector would be
unambiguous).


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
    fromLabel = personId

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

Users may define their own instances of ``HasField``, subject to the
usual rules about overlapping and incoherent instances.  This allows
"virtual" record fields to be defined for datatypes that do not
otherwise have them.  For example, an anonymous records library could
provide ``HasField`` instances and thus be compatible with the
polymorphic record selectors introduced by ``OverloadedRecordFields``.



OverloadedLabels extension
~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    fromLabel = GHC.Records.fromLabel

The ``Proxy#`` argument has been removed, since this is redundant in
the presence of ``TypeApplications``.  In addition, an ``IsLabel``
instance has been supplied for ``(->)`` that delegates to the
``HasField`` class.  This will require changes to code using the GHC
8.0.1 version of ``OverloadedLabels``.

When the ``OverloadedLabels`` extension is enabled (regardless of
whether ``OverloadedRecordFields`` is enabled), a label ``#foo`` is
translated to
``GHC.OverloadedLabels.fromLabel @"foo" :: IsLabel "foo" t => t``.

The instance for ``IsLabel x (r -> a)`` makes its possible to use an
overloaded label as a field selector, consistently with the
``OverloadedRecordFields`` behaviour.  For example, ``\ x -> #foo x``
has type ``HasField "foo" r a => r -> a``, just as it does with
``OverloadedRecordFields`` alone.

However, other useful instances for ``IsLabel`` are available.  In
particular, if we had a partner class to ``HasField`` that allowed
polymorphism over record updates, overloaded labels could be used to
create lenses.  (In the case of the van Laarhoven representation used
by the ``lens`` package, this requires a newtype to avoid overlap with
the existing ``IsLabel`` instance for functions.)  Built-in support
for such a class may be introduced in the future, or users can use
Template Haskell or Generics to define it in the meantime.


Multiple versions of fromLabel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under this proposal, the translation of a label depends on the
extensions that are enabled.  Where overloaded field selectors are
required but general overloaded labels are not, using
``OverloadedRecordFields`` alone is simpler and likely to give better
error messages than ``OverloadedLabels``.  Moreover, type inference is
less likely to lead to ambiguity errors, as the following example
demonstrates.

Consider the following module:

.. code-block:: haskell

  {-# LANGUAGE OverloadedRecordFields, NoMonomorphismRestriction #-}
  import Control.Category
  import Prelude hiding ((.))
  fooBar = #foo . #bar

When ``OverloadedRecordFields`` is enabled but ``OverloadedLabels`` is
not, the label is automatically interpreted as a function.  This means
that type inference succeeds for ``fooBar``, giving it the inferred
type

.. code-block:: haskell

  (HasField "foo" s t, HasField "bar" r s) => r -> t

However, when ``OverloadedLabels`` is enabled as well, the inferred
type of ``fooBar`` is

.. code-block:: haskell

  (Category cat, IsLabel "foo" (cat s t), IsLabel "bar" (cat r s)) => cat r t

In the first case, the functional dependency on ``HasField`` means
that ``s`` is not an ambiguous type variable, but in the second, ``s``
is ambiguous.


Interaction with RebindableSyntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When ``RebindableSyntax`` is enabled (plus at least one of
``OverloadedRecordFields`` or ``OverloadedLabels``), a label ``#foo``
is translated to ``fromLabel @"foo"`` using whatever ``fromLabel`` is
in scope (see `Trac #12243
<https://ghc.haskell.org/trac/ghc/ticket/12243>`_ for a request for
this feature).  The existence of this third option explains why we use
``fromLabel`` as the name of the ``HasField`` class method, rather
than introducing a different name.

This allows alternative interpretations of labels that cannot be
expressed using the ``IsLabel`` class.  For example, labels could be
translated directly to van Laarhoven lenses without the need for a
newtype wrapper.  This allows maximum flexibility for the user to
specify how labels get interpreted.

However, all the labels in the module are subject to the same
translation, so it may not be very convenient to use two different
libraries that rely on this option.  This is why ``OverloadedLabels``
and the ``IsLabel`` class are retained.


Drawbacks
---------

Existing code using ``OverloadedLabels`` from GHC 8.0.1 will need to
be adapted to work with the changes proposed here.  Removing the
``Proxy#`` argument should be straightforward, but the new ``IsLabel``
instance may conflict with existing instances elsewhere.  If
necessary, ``RebindableSyntax`` can be used to adapt existing code
that relies on giving alternative instances to ``IsLabel``.

Using ``OverloadedRecordFields`` instead of ``DuplicateRecordFields``
to disambiguate record selectors may lead to worse compiler
performance, as it requires more use of the constraint solver.
Moreover, excessive use of polymorphism over record fields may reduce
runtime performance, as with other uses of typeclass polymorphism.
Both of these issues are limited to code that uses the new extension.


Alternatives
------------

The different possible translations of overloaded labels may be
confusing.  Instead of adjusting the translation based on which of
``OverloadedRecordFields`` or ``OverloadedLabels`` are enabled, we
could pick one and require ``RebindableSyntax`` to access the other.
However, each translation has advantages and disadvantages, so it
seems worth making them both available.

Rather than dropping the ``Proxy#`` argument to ``fromLabel``, we
could retain it (for both ``GHC.Records.fromLabel`` and
``GHC.OverloadedLabels.fromLabel``).  This would be backwards
compatible with GHC 8.0.1, and would allow ``fromLabel`` to be called
directly without use of the ``TypeApplications`` extension.  However,
the argument is unnecessary and would cause a (small) performance
overhead.  Moreover, users are not usually expected to call
``fromLabel`` directly, rather they will typically use the overloaded
label syntax.

We could use a type family rather than a functional dependency in the
definition of ``HasField``.  That is, we could define

.. code-block:: haskell

  class HasField (x :: Symbol) r where
    type FieldType x r :: *
    fromLabel :: r -> FieldType x r

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
