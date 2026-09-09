Qualified Numerics
==================

.. author:: Brandon Chinn
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/725>`_.
.. sectnum::
.. contents::

This proposal proposes replicating ``-XQualifiedDo`` for literal numbers, to enable more ergonomic and typesafe API's. Another way to view this proposal would be replicating ``-XRebindableSyntax`` for literal numbers, but only within a local scope.

See also:

* `QualifiedStrings <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_
* `QualifiedLists <https://github.com/ghc-proposals/ghc-proposals/pull/724>`_

Motivation
----------

No granular typeclass for numeric literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``fromInteger`` and ``fromRational`` are part of ``Num`` and ``Fractional``, so there's no way to use numeric literal syntax for custom types that shouldn't implement operators like ``+``.

Related: https://github.com/ghc-proposals/ghc-proposals/issues/438

Proposed Change Specification
-----------------------------

Introduce ``-XQualifiedNumerics`` that desugars literal numbers to function calls in a similar way to ``-XQualifiedDo`` (`docs <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html>`_, `proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst>`_).

As long as the desugared expressions/patterns type check, users are free to define these functions however they want. No whitespace is allowed between the ``.`` and the module name / literal.

Currently, numeric literals have the following desugaring:

.. list-table::
    :align: left

    * - **Expression**
      - **Enabled extensions**
      - **Desugared expression syntax**
    * - ``1``
      -
      - ``Prelude.fromInteger 1``
    * - ``-1``
      -
      - ``Prelude.negate (Prelude.fromInteger 1)``
    * - ``-1``
      - ``-XNegativeLiterals``
      - ``Prelude.fromInteger (-1)``
    * - ``1.5``
      -
      - ``Prelude.fromRational 1.5``

With ``-XQualifiedNumerics``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``M.1``
      - ``M.fromInteger 1``
    * - ``M.(1)``
      - ``M.fromInteger 1``
    * - ``M.(-1)``
      - ``M.fromInteger (-1)``
    * - ``M.(1.2)``
      - ``M.fromRational 1.2``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``M.1``
      - ``((== M.fromInteger 1) -> True)``
    * - ``M.(1)``
      - ``((== M.fromInteger 1) -> True)``
    * - ``M.(-1)``
      - ``((== M.fromInteger (-1)) -> True)``
    * - ``M.(1.2)``
      - ``((== M.fromRational 1.2) -> True)``

See *Section 8.1 Alternative QualifiedNumerics API* for a discussion on the chosen API here.

Parentheses are required for negative integers and rationals, to avoid ambiguity, both in the lexer and for human readers. Parentheses are optional for positive integers.

``M.10e6`` will desugar to ``M.fromInteger 10e6`` if ``NumDecimals`` is enabled, or ``M.fromRational 10e6`` otherwise.

Parser
~~~~~~

Update `Section 10.5 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ of the Haskell 2010 report as follows.

.. code-block:: abnf

  aexp → qvar
       | ...
       | modid . integer
       | modid . ( {-} integer )
       | modid . ( float )

  apat → var [ @ apat ]
       | ...
       | modid . integer
       | modid . ( {-} integer )
       | modid . ( float )

Module name resolution
~~~~~~~~~~~~~~~~~~~~~~

Module names are resolved immediately, when parsing a quote. This matches the behavior of resolving modules in normal qualified values in quotes.

::

  module A where

  import OneImpl qualified as M

  -- Immediately resolves to OneImpl.123
  -- Errors if M is not in scope
  foo = [| M.123 |]

Proposed Library Change Specification
-------------------------------------

Template Haskell
~~~~~~~~~~~~~~~~

We'll add the following constructors instead of modifying existing constructors (e.g. ``IntegerL``), to maintain backwards compatibility:

::

  data Lit
    = ...
    | QualIntegerL ModName Integer
    | QualRationalL ModName Rational

Examples
--------

Scientific
~~~~~~~~~~

`Scientific <https://hackage.haskell.org/package/scientific-0.3.8.0/docs/Data-Scientific.html#t:Scientific>`_ represents an arbitrary precision number. It has a ``Num`` instance, but ``+`` and ``-`` are unsafe and can cause OOM. Safety-minded developers might desire to wrap with a newtype that provides ``unsafeAdd`` but not ``+``, to prevent call-sites from accidentally blowing up memory.

::

  newtype BigDecimal = BigDecimal Scientific

  unsafeAdd :: BigDecimal -> BigDecimal -> BigDecimal
  unsafeAdd = coerce (+)

If you want to write ``BigDecimal`` literals (e.g. for tests), you have to use either the ``BigDecimal`` constructor or write a ``big = BigDecimal`` helper, but that's unsafe if accidentally called on a non-literal, as ``Scientific`` throws a runtime error if converting from a repeating decimal.

With ``QualifiedNumerics``, you could write ``Big.123``, which guarantees that ``Big.fromNumeric`` is only called on literals (e.g. you could configure hlint to ban calling ``BigDecimal.fromNumeric`` directly and only be used via ``QualifiedNumerics``).

::

  -- only called on literals
  fromInteger = BigDecimal . fromInteger
  fromRational = BigDecimal . fromRational

Effect and Interactions
-----------------------

Interactions with other extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Related to `QualifiedStrings <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_ and `QualifiedLists <https://github.com/ghc-proposals/ghc-proposals/pull/724>`_, but all three proposals are orthogonal to each other.

* `Allow arbitrary identifiers as fields in OverloadedRecordDot <https://github.com/ghc-proposals/ghc-proposals/pull/668>`_ has similar syntax to the proposed qualified string literal, but as ``M.bar`` is parsed as a qualified identifier even with OverloadedRecordDot, it makes sense that ``M."bar"`` is also parsed as a qualified literal.

Costs and Drawbacks
-------------------

Development and maintenance should be low effort, as the core implementation is in the renamer step, and typechecking would proceed as normal.

The syntax is approachable for novice users and shouldn't be an extra barrier to understand.

Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.

Furthermore, turning on the extension will generally not break existing code. Any existing code written as ``M.123`` would be parsed as function composition between a data constructor and a literal, which would only typecheck if someone adds an ``Num`` instance for a function type.

Alternatives
------------

* Use PatternSynonyms for literal numbers in patterns

  * The View pattern more closely matches `Section 3.17.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-60015x7>`_ in the 2010 Report

* Allow some fallback logic, e.g. for ``M.1``, use ``M.fromInteger`` if it exists or ``M.fromRational`` otherwise.

  * Haskell generally prefers explicit "this is the function I'm calling" rather than any implicit logic that switches the function being called.
  * Adding ``M.fromInteger`` would be a breaking change; currently with PVP, adding a function is not generally considered a breaking change.

Alternative QualifiedNumerics API
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There were different APIs we could have implemented for ``-XQualifiedNumerics``:

#. Mirror Prelude with ``-XNegativeLiterals`` and translate to ``M.fromInteger 1``, ``M.fromInteger (-1)``, ``M.fromRational 1.5``, ``M.fromRational (-1.5)``

   * Pro: 1:1 correspondence with standard Haskell98 semantics + ``-XNegativeLiterals``
   * Con: If you want non-negative guarantees, you could type ``M.fromInteger`` with ``Natural``, but you'd be relying on GHC's hardcoded ``-Woverflowed-literals`` check.

#. Mirror Prelude without ``-XNegativeLiterals`` and do ``M.fromInteger 1``, ``M.negate (M.fromInteger 1)``, ``M.fromRational 1.5``, ``M.negate (M.fromRational 1.5)``

   * Pro: 1:1 correspondence with standard Haskell98 semantics
   * Pro: Non-negative guarantees by just not defining ``negate``
   * Con: 3 functions to define in the common case of supporting all numbers, 2 functions in the common case of supporting all integers

#. Add a bit more expressiveness by breaking out Natural: ``M.fromNatural 1``, ``M.fromNegativeInt (-1)``, ``M.fromRational 1.5``, ``M.fromRational (-1.5)``

   * Pro: Explicit non-negative guarantee
   * Con: 3 functions to define in the common case of supporting all numbers, 2 functions in the common case of supporting all integers
   * Con: Asymmetry in separation of positive/negative integers but 1 function for positive/negative rationals

#. Use a single possibly-polymorphic ``M.fromNumeric`` definition that should work for any of: ``Natural``, ``Integer``, ``Rational``.

   * The vast majority of cases would/should implement ``fromNumeric`` with ``Natural``, ``Integral a => a``, or ``Real a => a``.
   * If distinguishing between the three cases is absolutely necessary, the user may still do so with normal typeclass techniques.
   * Pro: Optional non-negative guarantee
   * Pro: Majority of use cases would only define one ``fromNumeric`` definition using existing typeclasses
   * Con: Rather divorced from standard Haskell98 semantics

Support distinguishing natural numbers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Haskell98, ``13`` desugars to ``fromInteger 13`` and ``2.7`` desugars to ``fromRational 2.7``. If a type ``T`` does not wish to support rationals, one could simply fail to provide an instance for ``Fractional T``, then ``fromRational 2.7 :: T`` will be statically rejected. But if ``T`` does not want to support negative integers, there is no way to reject it statically.

We'll leave desugaring natural numbers to a future proposal, which can undergo deeper discussion about whether we're okay with a discrepancy between the qualified and unqualified desugaring, or if we want to backport a Natural literal to the unqualified desugaring somehow.

Future work
~~~~~~~~~~~

* Some literals are not supported yet (Chars, unboxed literals) due to lack of use-cases, but could be extended in the future.

* Future work could be done to allow compile time logic, e.g. ``$M.1`` => ``$(M.fromNumeric [|1|])``, but that is out of scope of this proposal.

Unresolved Questions
--------------------

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.

Endorsements
------------
