NoFieldSelectors
==============

.. date-accepted:: 2019-08-23
.. author:: reactormonk
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/5972
.. implemented:: 9.2
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/160>`_.
.. contents::

Enabling this Language Extension removes the default of Haskell data
declarations to generate toplevel field accessor functions for records, such
that the user can define their own via Generic or positional extraction.

Motivation
------------

There have been proposals to extend the usage of records in Haskell
(DuplicateRecordFields, OverloadedRecordFields, DeriveGeneric to name a few),
but none of them address the original issue where record fields steal the
toplevel function name for each field for selector purposes. This proposal
intends to open the design space to this issue by removing the generation of
these toplevel selector functions.

Possible Alternative Use Cases for Record Names
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After removing the toplevel selector names, the field names could be used as
bindings for other values.

- A `generic-lens` equivalent for toplevel lenses
- namespaced accessors
- namespaced lenses
- overloaded-labels based accessors / lenses
- row types
- `<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_
- ...

Proposed Change Specification
-----------------------------

This proposal introduces a new extension ``FieldSelectors`` that controls the
generation of toplevel record field selector functions. The extension is enabled
by default to match the current behavior, but may be disabled per-module with
the ``NoFieldSelectors`` language pragma.

When ``NoFieldSelectors`` is enabled, Record construction/update syntax and
pattern matching will work as before, as will the disambiguation handled by
``DuplicateRecordFields``.

Record fields will still be part of a ``Record(..)`` export, or can also be
named individually. They always have to be associated with the data type though,
because there is no more toplevel selector (see `Example`_).

Template Haskell should not rely on the selectors being present, and should use
named pattern matching instead.

Because field labels and toplevel selectors are now different entities,
import/export lists now behave differently. Names listed with the constructor
(e.g. ``Record(field)``) refer to the field, whereas direct mentions ``field``
refer to a function named ``field``. Without setting ``NoFieldSelectors``, these
two would be equivalent.

Examples
--------

Given a data structure

    data Foo = Foo { bar :: Int, baz :: String }

The following will be available:

1. the type constructor ``Foo``
2. the data constructor ``Foo``
3. the fields ``bar`` and ``baz`` for record construction, update, and patterns
4. the two functions ``bar`` and ``baz``, which are ``Foo -> Int`` and ``Foo -> String``

If the language extension ``NoFieldSelectors`` is enabled, items (1), (2), and (3)
will still be generated, but (4) will not.

Wildcard exports will work as before, except for the two functions. Even if
these functions are otherwise defined, the wildcard will not export them.
Exporting the names for record construction now has to be specific to the
record. Without ambiguitiy, previously this was equivalent

.. code-block:: haskell

    module A (Foo(Foo, bar, baz)) where
    data Foo = Foo { bar :: Int, baz :: Int }

.. code-block:: haskell

    module B (Foo(Foo, bar), baz) where
    data Foo = Foo { bar :: Int, baz :: Int }

Under ``NoFieldSelectors``, these two export statements are now different. The
first one will export the field ``baz``, but not the function ``baz``, while the
second one will export the function ``baz`` (assuming one is defined), but not
the field ``baz``. Because of this change, writing out all selector functions by
hand is still different, because they all have to be exported separately.

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module Exports (Foo(Foo, bar, baz)) where
    data Foo = Foo { bar :: Int, baz :: Int }

    bar (Foo x _) = x
    baz (Foo _ x) = x

is different from

.. code-block:: haskell

    module Exports (Foo(Foo, bar, baz)) where
    data Foo = Foo { bar :: Int, baz :: Int }

Because the functions in the first example don't get exported.

Let's take a module ``A`` with a function with the same name as a field, with
the extension enabled:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module A (Foo(Foo, bar, baz)) where
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Which would be equivalent to:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module A (Foo(..)) where
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

A second module, ``B``, which does not export the selector ``baz`` of
constructor ``Foo``, but instead exports the toplevel binder ``baz``. The fields
can still be used when exported (as in module ``A``).

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module B (Foo(Foo, bar), baz) where
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Using ``baz`` as a field when importing ``B`` will fail, because the field
``baz`` is not in scope anymore, because it is not exported by ``B``.

.. code-block:: haskell

    import B
    foo = Foo 23 42
    foo { baz = 1 }

However, it is possible to use the imported variable ``baz``, because ``B`` exports it.

.. code-block:: haskell

    import B
    main = print baz

If you wanted to use both, you'd have to export both explicitly:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module C (Foo(Foo, bar, baz), baz) where
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Now ``baz`` here assigns the value ``42`` to the field ``baz``.

.. code-block:: haskell

   import C
    foo = Foo 23 1
    foo { baz = baz }


Effect and Interactions
-----------------------

`HasField` will work as before, if the corresponding field has been exported. It
doesn't need to be exported as function.

Breakage estimation
^^^^^^^^^^^^^^^^^^^

Enabling this extension will beak Template Haskell which assumes the presence of
 a field selector. Use named pattern matching instead.

Anything that generates code with the help of Generic should be fine. The same
functionality that generates the anonymous functions for Generic could be used
to provide TH functionality to replace the existing toplevel functions.

The record extensions NamedFieldPuns, RecordWildCards, DisambiguateRecordFields,
and DuplicateRecordFields are unaffected by this change.


Costs and Drawbacks
-------------------

This might cause some confusion that record fields can't be accessed by toplevel
selectors anymore - however, that shouldn't be too big of an issue, because some
library authors already stopped exporting these selectors so they don't have to
break downstream software on record changes.


Alternatives
------------

None.


Implementation Plan
-------------------

I'm currently on the way of implementing this extension. It's roughly as
follows:

- Add new `NameSpace` to `OccName`: `RecordSelector String`
- Remove `flSelector` from `FieldLabel`, add an flag which denotes if it should
  be found as `VarName`
- Remove `FlParent`
- Change any field lookup code to look for new `OccName`
- Implement `FieldSelector` flag to look for selectors if you're looking
  for `VarName`
- Adjust `Generic` instances
- Add new `TH` function to access record selectors

Future Plans
------------

Make the behavior outlined in the discussion work:

.. code-block:: haskell

    data Foo = Foo { foo :: Int } deriving selectors
