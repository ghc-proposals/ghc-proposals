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

* An opening character is an ``$idchar`` (a digit, a letter, or an underscore
  ``_``), an opening bracket ``(``, ``[``, ``{``, or a quotation mark ``"``,
  ``'``, excluding the occurrences of ``{`` that start a comment or a pragma.
* An closing character is an ``$idchar`` (a digit, a letter, or an underscore
  ``_``), a closing bracket ``)``, ``]``, ``}``, or a quotation mark ``"``,
  ``'``, excluding the occurrences of ``}`` that end a comment or a pragma.
* In the lexer, when ``!`` or ``~`` is not preceded by a closing character, and
  is followed by an opening character, consider it a prefix occurrence,
  otherwise an infix occurrence.
* A prefix occurrence is treated as bang/lazy pattern in term-level patterns,
  or as a strictness annotation in types.
* An infix occurrence is treated as an infix operator in terms, or an infix
  type operator in types.
* In the grammar, a bang/lazy pattern must be followed by ``aexp1``, a
  strictness annotation must be followed by ``atype``.

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

This may break existing programs. The migration strategy is to adjust
whitespace::

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

Unresolved Questions
--------------------

Under the proposed rules, we parse both ``f !C{x=a} = <rhs>`` and ``f !C {x=a}
= <rhs>`` as a bang pattern on a record pattern match. While the former is
desirable, the latter is questionable. It is not clear how to allow one but
disallow the other.

Implementation Plan
-------------------

I (Vladislav Zavialov) will implement this change. The idea is to add tokens
``BANG`` and ``TILDE`` in addition to ``'!'``, ``'~'``, akin to ``TYPEAPP`` vs
``'@'``.

Appendix A: Extended Proposed Change Specification
--------------------------------------------------

As a basis for a future proposal, here's an outline of a more general
whitespace-based parsing framework for other operators. Accepting the main
proposal does not entail accepting this appendix.

* Operator occurrences are classified into four groups, based on preceding and
  following characters::

    a . b   -- a loose infix occurrence
    a.b     -- a tight infix occurrence
    a .b    -- a prefix occurrence
    a. b    -- a suffix occurrence

  ``a`` and ``b`` stand for closing and opening characters respectively,
  whitespace stands for all other characters.

* A loose infix occurrence should always be considered an operator. Other types
  of occurrences may be assigned a special per-operator *meaning override*:

  +-------------------+---------------------+--------------------------------------------+
  | Operator          | Occurrence          | Meaning override                           |
  +===================+=====================+============================================+
  | ``!``, ``~``      | prefix              | strictness annotation in types,            |
  |                   |                     | bang/lazy pattern in term-level patterns   |
  +-------------------+---------------------+--------------------------------------------+
  | ``$``, ``$$``     | prefix              | untyped/typed Template Haskell splice      |
  +-------------------+---------------------+--------------------------------------------+
  | ``@``             | prefix              | type application                           |
  +-------------------+---------------------+--------------------------------------------+
  | ``@``             | tight infix, suffix | as-pattern                                 |
  +-------------------+---------------------+--------------------------------------------+
  | ``-``             | prefix              | negation                                   |
  +-------------------+---------------------+--------------------------------------------+

  This wouldn't be a backward compatible change in every corner case, but the
  migration path does not require ``-XCPP``.

* As a consequence of these rules, ``@`` (loose infix) and ``~`` (suffix, loose
  infix, tight infix) are now proper infix operators.

* As a consequence of these rules, ``(- x)`` is now an operator section,
  ``(-x)`` is infix negation. This change is to be guarded behind a new
  language extension ``-XPrefixNegation``.

* Add a new warning, ``-Woperator-whitespace``, disabled by default, that warns
  on prefix, suffix, and tight infix uses of operators that do not have a
  meaning override at the moment. Users who desire forward compatibility may
  enable this warning in case we create new operator meaning overrides in the
  future. Enabled by ``-Weverything`` but not ``-Wall`` or ``-Wcompat``.

* The operator meaning override system has lower precedence than other lexical
  rules that steal operator syntax:

  * ``#`` under ``-XMagicHash`` or ``-XOverloadedLabels``
  * ``?`` under ``-XImplicitParams``
  * ``.`` as module qualification

  We choose not to handle these cases under the new framework because their
  rules do not apply to arbitrary subexpressions:

  * ``(f x)#`` is not a proper use of magic hash
  * ``(f x).id`` is not proper module qualification
  * ``?(f x)`` is not an implicit parameter
  * ``#(f x)`` is not an overloaded label
