NoFieldSelectors
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/160>`_.
.. sectnum::
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
generation of record field selector functions. The extension is enabled by
default to match the current behavior, but may be disabled per-module with the
``NoFieldSelectors`` language pragma.

Record construction/update syntax and pattern matching will work as before, the
disambiguation handled by ``DuplicateRecordFields``. The necessary selectors
will be part of a ``Record(..)`` export, or can also be named individually. They
always have to be associated with the data type though, because there is no more
toplevel selector (see `Example`_).

A function for Template Haskell to go from a record field name to a selector for
that field will be provided, because it's not possible anymore to go from field
name directly to selector.

A new ``TH`` function is added which takes a ``Name`` (for the constructor) and
a ``String`` (for the field) and returns a ``Q Exp``.

.. code-block:: haskell

    mkFieldSelector :: Name -> String -> Q Exp

Example
^^^^^^^

Given a data structure

    data Foo = Foo { bar :: Int, baz :: String }

The following will be available:

- the type ``Foo``
- the constructor ``Foo``
- the two functions ``bar`` and ``baz``
- the names ``bar`` and ``baz`` for record construction (``Foo { bar = 3, baz = "foo" }``)
- the names ``bar`` and ``baz`` for ``RecordWildCards``

If the language extension ``NoFieldSelectors`` is enabled for the module
or ``Foo`` specifically, all of the above will be generated, except for the two
functions ``bar`` and ``baz``.

Wildcard exports will work as before, except for the two functions. Even if
these functions are otherwise defined, the wildcard will not export them.
Exporting the names for record construction now has to be specific to the
record. Without ambiguitiy, previously this was equivalent

.. code-block:: haskell

    module A where (Foo(Foo, bar, baz))
    data Foo = Foo { bar :: Int, baz :: Int }

.. code-block:: haskell

    module B where (Foo(Foo, bar), baz)
    data Foo = Foo { bar :: Int, baz :: Int }

Because of the new semantics, these two export statements are now different. The
first one will export the field ``baz``, but not the function ``baz``, while the
second one will export the function ``baz``, but not the field ``baz``. Because
of this change, writing out all selector functions by hand is still different,
because they all have to be exported manually.

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module A where (Foo(Foo, bar, baz))
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

Which would be equivalent to:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module A where (Foo(..))
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

A second module, ``B``, which does not export the selector ``baz`` of
constructor ``Foo``, but instead exports the toplevel bind ``baz``.

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}
    module B where (Foo(Foo, bar), baz)
    data Foo = Foo { bar :: Int, baz :: Int }
    baz = 42

The updaters can still be used when exported (as in module ``A``).

.. code-block:: haskell

    import A
    foo = Foo 23 42
    foo { baz = 1 }

This will now fail, because the record updater ``baz`` is not in scope anymore,
because the selector is not exported by ``B``.

.. code-block:: haskell

    import B
    foo = Foo 23 42
    foo { baz = 1 }

The value ``baz`` is only exported from module ``B``, not ``A``. This would fail:

.. code-block:: haskell

    import A
    main = print baz

Template Haskell
^^^^^^^^^^^^^^^^

A new function will be added to Template Haskell, where the ``Name`` is a
reference to a constructor. This function should be used in new TH even if this
extension isn't enabled. It will fail if it's not possible to create a valid
selector / selector doesn't exist.

.. code-block:: haskell

    mkFieldSelector :: Name -> String -> Q Exp

Additionally, ``NameSpace`` will be extended with a new constructor ``FieldName``.

Effect and Interactions
-----------------------

`HasField` will work as before, if the corresponding field has been exported. It
doesn't need to be exported as function.

Interaction with DuplicateRecordFields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Because of Record updates still being valid, this code will still fail to
compile without DuplicateRecordFields:

.. code-block:: haskell

    {-# LANGUAGE NoFieldSelectors #-}

    data Foo = Foo { foo :: Int }
    data Bar = Bar { foo :: Int }

Breakage estimation
^^^^^^^^^^^^^^^^^^^

Enabling this extension will break a lot of Template Haskell. Going from record
field name to selector won't work anymore. A new way to go from record field
name to selector has to be found.

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


Unresolved questions
--------------------

- Which order of arguments in the new TH function?
- Should this extension imply DuplicateRecordFields?


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
