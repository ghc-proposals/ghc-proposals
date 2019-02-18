MINIMAL should warn about extra definitions
==============

.. proposal-number:: 

.. trac-ticket:: 

.. implemented:: 
                 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**

.. sectnum::

.. contents::

Currently, GHC issues no warnings if a class has a `MINIMAL` pragma requiring `foo`, and yet you also
do give a default definition for `foo`. This proposal suggests the addition of this warning.


Motivation
------------
I was recently quite confused about an error message from GHC, stemming from the following:

::
  class X a where
  foo :: a

  {-# MINIMAL foo #-}
  foo = undefined

  instance X Int

For this, GHC says:

::
  GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
  [1 of 1] Compiling Main             ( a.hs, interpreted )

  a.hs:7:10: warning: [-Wmissing-methods]
      • No explicit implementation for
          ‘foo’
      • In the instance declaration for ‘X Int’
    |
  7 | instance X Int
    |       

This is arguably correct, since I made foo part of `MINIMAL`; but it's very confusing: because I know
I added a default definition. Also note that the warning comes at the instantiation site instead of
the class definition, which can be miles apart! (Imagine class coming from a library, and the instantiation
from a user code: Even worse!)

This proposal suggests that GHC should warn about this discrepancy right at the point where the `class X`
is defined, with a message of the form:

::
  You made `foo` MINIMAL, but also gave an explicit definition for it.

(Exact wording TBD.)

I have filed this as a feature request: https://ghc.haskell.org/trac/ghc/ticket/16314. Simon PJ asked me to
create a proposal so it can gather feedback.

Proposed Change Specification
-----------------------------

The change is quite mimimal. First, we need a minor change to the user manual, where the grammar
of ``MINIMAL`` descriptions are given as:

::
  mindef ::= name
          |  '(' mindef ')'
          |  mindef '|' mindef
          |  mindef ',' mindef

This is already not quite correct, because GHC actually allows empty minimal declarations, as
in ``{-# MINIMAL #-}``, which isn't part of this grammar. We should change it to:

::

  mindef  :: <empty>
           | mindef1

  mindef1 ::= name
           |  '(' mindef1 ')'
           |  mindef1 '|' mindef1
           |  mindef1 ',' mindef1


Abusing the notation in the obvious way, define the following function from a ``MINIMAL``
expression to a set of names:

::
  required <empty>          = Set.empty
  required name             = Set.singleton name
  required ('(' expr ')')   = required expr
  required (left '|' right) = required left `Set.intersection` required right
  required (left ',' right) = required left `Set.union`        required right

For each class declaration with a ``MINIMAL`` pragma, compute:

::
  D = set of all methods with default definitions
  R = the required set, as defined above
  E = D `Set.difference` R

If ``E`` is not empty, then GHC should emit a warning saying the methods in ``E`` are required by
the ``MINIMAL`` pragma but also are given a default definition. If ``E`` is empty, no warning is generated.

Effect and Interactions
-----------------------

If a method as a definition via the ``default signatures`` extension, then that definition should
not be added to the set ``D`` as defined above. While adding it would be a strict check, I think it
is likely to increase the false-positives. Though feedback is welcome on the impact of this.

Costs and Drawbacks
-------------------
Cost: The compiler probably already has all the necessary bits and pieces to do this in short order.
For someone familiar with that part of the code, I doubt it's more than an afternoon worth of work;
including test cases and integration.

Drawbacks: I don't think there is any!

Alternatives
------------
Do nothing. But in a large refactoring case (which prompted this proposal in the first case) it is
much nicer to get warnings close to where the problem is, as opposed to later on. In the particular
case of the class being defined in a library and the instance being in user code, this issue gets
amplified as there is really nothing the user of the library can do.

Unresolved Questions
--------------------
None.

Implementation Plan
-------------------
TBD.
