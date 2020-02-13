Add ``setField`` to ``HasField``
================================

.. author:: Neil Mitchell
.. date-accepted:: 2019-01-22
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/16232
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_.
.. contents::

This is a proposal to adjust the built-in typeclass ``HasField``, removing ``getField``
and adding ``hasField`` (which is powerful enough to get, set and modify a field).
The result would allow type-based resolution of field names in functions that update
records. It *does not* introduce any new extensions.

Motivation
----------

A serious limitation of the Haskell record system is the inability to
overload field names in record types: for example, if the data types

.. code-block:: haskell

  data Person  = Person  { personId :: Int, name :: String }
  data Address = Address { personId :: Int, address :: String }

are in scope in the same module, there is no way to determine which
type an occurrence of the ``personId`` record selector refers to.
The ``HasField`` extension defined in the already-implemented
`Overloaded Record Fields proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst>`_
introduced ``HasField``, to allow type-based resolution of field names and
polymorphism over record selectors. The class ``HasField`` is currently defined as:

.. code-block:: haskell

  -- | Constraint representing the fact that the field @x@ belongs to
  -- the record type @r@ and has field type @a@.
  class HasField (x :: k) r a | x r -> a where
    getField :: r -> a

While this class provides a way to get a field, it provides no way to set a field.
To quote the previous proposal:

  In the interests of simplicity, this proposal does not include a class
  to provide polymorphism over record updates

Such a proposal to deal with record updates would clearly be desirable.

Proposed Change Specification
-----------------------------

We propose to adjust ``HasField`` in ``GHC.Records`` to become:

.. code-block:: haskell

  -- | Constraint representing the fact that the field @x@ can be get and set on
  --   the record type @r@ and has field type @a@.  This constraint will be solved
  --   automatically, but manual instances may be provided as well.
  --
  --   The function should satisfy the invariant:
  --
  -- > uncurry ($) (hasField @x r) == r
  class HasField x r a | x r -> a where
    -- | Function to get and set a field in a record.
    hasField :: r -> (a -> r, a)

We propose to have GHC automatically solve new ``HasField`` constraints the same
way it does for the existing ``HasField`` constraints.

To enhance reverse compatibility and make it easier to use the ``hasField`` function,
we propose also adding to ``GHC.Records``:

.. code-block:: haskell

  getField :: forall x r a . HasField x r a => r -> a
  getField = snd . hasField @x

  setField :: forall x r a . HasField x r a => r -> a -> r
  setField = fst . hasField @x

This proposal *does not* change how record updates are desugared.

Effect and Interactions
-----------------------

Using ``hasField`` it is possible to write a function:

.. code-block:: haskell

  mkLens :: forall x r a . HasField x r a => Lens' r a
  mkLens f r = wrap <$> f v
      where (wrap, v) = hasField @x r

And thus allow generating lenses from the field classes. The function
``setField`` is also useful in its own right, complementing the existing ``getField``
method and providing the ability to modify records by field name.

Costs and Drawbacks
-------------------

More code in the compiler.

Alternatives
------------


Separate ``getField`` and ``setField`` methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An alternative is to provide two separate methods, rather than the combined ``hasField``.
The separate methods are both simpler, but to implement any fields that perform computation
(e.g. delving into a ``Map``) would require performing that computation twice in a field
modification. By combining the two functions that extra cost can be eliminated.

Separate methods would also avoid breaking compatibility for people who have already defined
``HasField``. However, a search of Hackage has not identified anyone defining ``HasField``,
so the breakage is minor.

Polymorphic updates
~~~~~~~~~~~~~~~~~~~

A *type-changing update* is one where the type ``r`` is higher-kinded and the field
``x`` is the only member of that type. As an example, given a value of type ``(Int, Bool)``,
the selector pointing to the first component, and a new value of type ``Double`` we can
produce ``(Double, Bool)``. The design space for type-changing updates is large, and almost
certainly requires additional complexity. In contrast, the design space for type-preserving
updates is small and it can be easily incorporated into the existing design. The addition
of type-preserving updates in no way constrains the design space for future type-changing
updates, but is useful in its own right.

Read-only fields
~~~~~~~~~~~~~~~~

By splitting the type class we could support read-only fields. However, read-only fields
are essentially just functions, and we already have good support for functions throughout
Haskell. In addition, it would likely be necessary to have a decision procedure for whether
a field was read-only, which would quickly become unweildy.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

Adam Gundry has offered to implement this feature.
