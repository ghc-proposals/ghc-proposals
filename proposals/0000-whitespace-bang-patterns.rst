Simplify parsing of (~) and (!)
===============================

.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/229>`_.
.. sectnum::
.. contents::

We propose to simplify GHC internals and the lexical syntax of Haskell by
replacing ad-hoc parsing rules with whitespace-based disambiguation::

  a ! b = <rhs>  -- Infix (!)
  f !a = <rhs>   -- Bang pattern


Motivation
------------

At the moment, Haskell has inconsistent and complicated rules to decide whether
a ``~`` or a ``!`` is used as an infix operator or as a prefix annotation. It
poses a significant burden on the parser implementation without any benefits
to the users.

We have six cases to consider:

* ``!`` as a bang pattern
* ``~`` as a lazy (irrefutable) pattern
* ``!`` as a strictness annotation in type declarations
* ``~`` as a strictness annotation in type declarations
* ``!`` as an infix operator
* ``~`` as an infix operator

GHC puts a great deal of effort into distinguishing these cases, and still does
a poor job at it (for example, see `#1087
<https://gitlab.haskell.org/ghc/ghc/issues/1087>`_). Here are the rules we have as of GHC 8.8:

* In term-level patterns, ``!`` is considered either an infix operator or a
  bang pattern, depending on the module-wide ``-XBangPatterns`` flag.
* In term-level patterns, ``~`` is always considered a lazy (irrefutable) pattern.
* In term-level expressions, ``!`` is always considered an infix operator.
* In term-level expressions, ``~`` is never valid.
* In types, ``!`` and ``~`` are considered type operators or if they have no LHS, or strictness
  annotations otherwise.

Here are the consequences of these rules:

* We cannot write both of these definitions in the same module::

    a ! b = <rhs>  -- Infix (!)
    f !a = <rhs>   -- Bang pattern

* One of these definitions is valid, the other is not::

    ~a + ~b = <rhs>   -- Valid
    !a + !b = <rhs>   -- Invalid (#1087)

* We cannot use ``~`` as a term-level operator.

The implementation of the current rules is not pretty. In terms, we always
parse ``!`` as an infix operator and rejig later, and it doesn't cover all
possible cases. In types, we have a hand-written state machine to detect the
"no LHS" -- it is more robust but difficult to maintain.

In order to unify term-level and type-level parsers, a milestone towards
Dependent Haskell, we will need to parse the ``(~)`` operator in terms, but it
will further complicate parser implementation under the current arrangement.

Users typically write bang patterns and lazy (irrefutable) patterns without
whitespace after it, and we can make use of this information during parsing. A
similar rule is used to distinguish between ``@`` in type applications and
as-patterns, and it works remarkably well in practice.

Proposed Change Specification
-----------------------------

* When ``!`` or ``~`` is preceded by whitespace and not followed by
  whitespace, consider it a prefix occurrence, otherwise an infix occurrence.
* A prefix occurrence is treated as bang/lazy pattern in term-level patterns,
  or as a strictness annotation in types.
* An infix occurrence is treated as an infix operator in terms, or an infix
  type operator in types.
* For the purposes of these rules, comments are considered whitespace.

Effect and Interactions
-----------------------

The users regain the ability to define infix ``(!)`` even when
``-XBangPatterns`` are enabled::

  {-# LANGUAGE BangPatterns #-}
  a ! b = <rhs>   -- works as expected now

Costs and Drawbacks
-------------------

It is a slight deviation from the standard which dictates the following to be
accepted::

  f ~ a ~ b = <rhs>     -- standard interpretation: lazy (irrefutable) patterns
  x !y = x == y         -- standard interpretation: infix operator (!)
  data T = MkT ! Int    -- standard interpretation: strict field !Int

The migration strategy is to adjust whitespace::

  f ~a ~b = <rhs>
  x ! y = x == y
  data T = MkT !Int

This already matches the style of most Haskell users and will simplify the
implementation.


Alternatives
------------

If this proposal is rejected, the implementation will need another hand-written
state machine, which is hard to extend and maintain. This state machine will
not be able to handle some corner cases which whitespace-based disambiguation
handles easily.


Implementation Plan
-------------------

I (Vladislav Zavialov) will implement this change. The idea is to add tokens
``BANG`` and ``TILDE`` in addition to ``'!'``, ``'~'``, akin to ``TYPEAPP`` vs
``'@'``.
