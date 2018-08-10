Add ``setField`` to ``HasField``
================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/158>`_.
.. sectnum::
.. contents::

This is a proposal to add the function `setField` to the built-in typeclass
``HasField``, allowing type-based resolution of field names in record update functions.
It *does not* introduce any new extensions.

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
  -- the record type @r@ and has field type @a@.  This will be solved
  -- automatically, but manual instances may be provided as well.
  class HasField (x :: k) r a | x r -> a where
    -- | Selector function to extract the field from the record.
    getField :: r -> a

While this class provides a way to get a field, it provides no way to set a field.
To quote the previous proposal:

  In the interests of simplicity, this proposal does not include a class
  to provide polymorphism over record updates

Such a proposal to deal with record updates would clearly be desirable.

Proposed Change Specification
-----------------------------

We propose to rename the ``HasField`` class to ``GetField``.

We propose to add a ``SetField`` class to ``GHC.Records`` with the contents:

.. code-block:: haskell

  -- | Constraint representing the fact that the field @x@ can be set on
  --   the record type @r@ and has field type @a@.  This will be solved
  --   automatically, but manual instances may be provided as well.
  --
  --   Where both 'GetField' and 'SetField' are provided they should satisfy
  --   the invariant:
  --
  -- > getField @lbl (setField @lbl a s) = a
  class SetField (x :: k) r a | x r -> a where
    -- | Update function to set a field in the record.
    setField :: a -> r -> r

We propose to have GHC automatically solve ``SetField`` constraints exactly the same
way it does for the existing ``HasField`` constraints.

This proposal *does not* change how record updates are desugared.

Effect and Interactions
-----------------------

Using this additional function it is possible to write a function:

.. code-block:: haskell

  mkLens :: forall lbl r a . (GetField lbl r a, SetField lbl r a) => Lens' r a
  mkLens = lens (getField @lbl) (flip (setField @lbl))

And thus allow generating lenses from the field classes. The function
``setField`` is also useful in its own right, complementing the ``getField``
method and providing the ability to modify records by field name.

Costs and Drawbacks
-------------------

More code in the compiler.

Alternatives
------------

Not renaming ``HasField``
~~~~~~~~~~~~~~~~~~~~~~~~~

We could leave the ``HasField`` name unchanged, thus avoiding breaking compatibility.
We consider changing the name feasible because ``HasField`` is not widely used, and naming
it ``GetField`` makes the distinction between the pieces clearer.

Relationship between ``GetField`` and ``SetField``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to merge ``GetField`` and ``SetField`` into a single typeclass, or make one
a supertype of the other. We prefer to disentangle the concerns, providing the minimal
primitive building blocks that have to go into the compiler.
Libraries that build on top of these classes may well define a single class, e.g.:

.. code-block:: haskell

  class (GetField x r a, SetField x r a) => HasField x r a where
    -- simple van Laarhoven lens, e.g. view field @"foo" bar ...
    field :: Lens' r a
    default field :: (GetField lbl r a, SetField lbl r a) => Lens' r a

We leave this design space to libraries, allowing choice of which lens flavour to use,
how they compose/generalise etc.

Polymorphic ``setField``
~~~~~~~~~~~~~~~~~~~~~~~~

A *type-changing update* is one where the type ``r`` is higher-kinded and the field
``x`` is the only member of that type. As an example, given a value of type ``(Int, Bool)``,
the selector pointing to the first component, and a new value of type ``Double`` we can
produce ``(Double, Bool)``. The design space for type-changing updates is large, and almost
certainly requires an additional type class. In contrast, the design space for type-preserving
updates is small and it can be easily incorporated into the existing design. The addition
of type-preserving updates in no way constrains the design space for future type-changing
updates, but is useful in its own right.

Adding ``updateField``
~~~~~~~~~~~~~~~~~~~~~~

An alternative to ``setField`` is:

.. code-block:: haskell

  -- | Update function to set a field in the record.
  updateField :: (a -> a) -> r -> r

The function ``updateField`` can be recovered using ``setField`` and ``getField``, but
``setField`` is simpler, so we prefer it.

Order of arguments to ``setField``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can pick either of:

.. code-block:: haskell

  setField :: a -> r -> r
  setField :: r -> a -> r

We consider the former to be cleaner, and allows for better composition when updating many fields,
e.g. you can see the equivalence between:

.. code-block:: haskell

  foo{x = 1, y = 2}
  foo & (setField @"x" 1 . setField @"y" 2)

This order is different to the ``lens`` function in ``Control.Lens``, whose order was chosen to
aid implementation, at the slight cost of direct usability, as
`mentioned here <https://www.reddit.com/r/haskell/comments/91wtze/signature_of_lens_combinator/e31d8gy/>`_.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

Adam Gundry has offered to implement this feature.
