Allow record wildcards for empty records
==============

.. author:: John Ericson (@Ericson2314)
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/496>`_.
.. contents::

Allow record wild cards for empty records.
This removes and artificial restriction, making the language simpler and more predictable.

Motivation
----------

Currently this is allowed::

  data Foo = Foo { a :: Int }

  f (Foo {..}) = 1

  x = Foo {..}

But this is not::

  data Foo = Foo { }

  f (Foo {..}) = 1

  x = Foo {..}

Prohibiting this doesn't seem good:

#. It feels arbitrary and artificial.
   Does anyone expect this behavior?

#. It is a pitfall for generated code, which cannot uniformly use record wildcard syntax no matter how many fields there are.

Furthermore this is allowed ::

  data Foo = Foo { a :: Int }

  f (Foo { a = 1, ..}) = 1

  x = Foo { a = 1, ..}

So we see ``..`` binding or using no fields isn't even consistently prohibited!

Proposed Change Specification
-----------------------------

Allow record wildcard syntax for empty variants.
There are no (named) fields, so no variables are bound in patterns, and no variables are used in expressions.

Examples
--------

The second example in the motivation is allowed.

This is also allowed::

  data Foo = Foo -- no {}

  f (Foo {..}) = 1

  x = Foo {..}

for it doesn't matter today whether empty variants are declared with ``{}``, and this should remain true.

Effect and Interactions
-----------------------

``Foo {..}`` has the same meaning as ``Foo {}`` for an empty record.


Costs and Drawbacks
-------------------

Cannot think of any.


Alternatives
------------

Do nothing.
Can't think of anything else.

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

This should be very easy.
Perhaps we should use it as a mentoring exercise for new contributors.

Endorsements
-------------

There was positive feedback in https://github.com/ghc-proposals/ghc-proposals/issues/484 where this was previously brought up.
