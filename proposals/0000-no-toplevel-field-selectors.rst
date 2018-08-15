NoToplevelFieldSelectors
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
that the user can define their own, either via OverloadedRecordFields, or via
Generic.

Motivation
------------

There have been proposals to extend the usage of records in Haskell
(DuplicateRecordFields, OverloadedRecordFields, DeriveGeneric to name a few),
but none of them address the original issue where record fields steal the
toplevel function name for each field for selector purposes. This proposal
intends to open the design space to this issue by removing the generation of
these toplevel selector functions.

Possible Alternative Designs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- A `generic-lens` equivalent for toplevel lenses
- namespaced accessors
- namespaced lenses
- overloaded-labels based accessors / lenses
- row types
- ...

Proposed Change Specification
-----------------------------

Record definition no longer defines toplevel selector functions when the
``NoToplevelFieldSelectors`` extension is enabled. A
``NoToplevelFieldSelectors`` pragma for records will also be available, such
that this extension can be enabled on a per-record basis.

Record construction/update syntax and pattern matching will work as before, the
disambiguation handled by ``DuplicateRecordFields``. The necessary selectors
will be part of a ``Record(..)`` export, or can also be named one by one. They
always have to be associated with the data type though, because there is no more
toplevel selector.

A function for Template Haskell to go from a record field name to a selector for
that field will be provided, because it's not possible anymore to go from field
name directly to selector.

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

If the language extension ``NoToplevelFieldSelectors`` is enabled for the module
or ``Foo`` specifically, all of the above will be generated, except for the two
functions ``bar`` and ``baz``.

Wildcard exports will work as before, except for the two functions. Even if
these functions are otherwise defined, the wildcard will not export them.
Exporting the names for record construction now has to be specific to the
record. Without ambiguitiy, previously this was equivalent

    module A where (Foo(Foo, bar, baz))

    module A where (Foo(Foo, bar), baz)

Because of the new semantics, these two export statements are now different.
Maybe a wildcard export will export functions of the same name as well, this
question isn't resolved yet.

Effect and Interactions
-----------------------

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

Should exporting ``Foo(..)`` also export functions based on the name of the
field accessors?


Implementation Plan
-------------------

For deriving the Generic instances, either ``Record.fieldName``,
``OverloadedRecordFields`` or an anonymous function which generates the
corresponding selector in core will be used. (to be clarified)
