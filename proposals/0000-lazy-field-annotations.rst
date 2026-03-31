Lazy Field Annotations
======================

.. author:: Simon Jakobi
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/752>`_.
.. sectnum::
.. contents::

This proposal introduces ``-XLazyFieldAnnotations``. The extension enables the
existing prefix ``~`` field annotation syntax in data and GADT constructor
fields, independently of ``-XStrictData``. It allows users and source
generators to write laziness explicitly without also changing the default
strictness of unannotated fields.

See discussion in `GHC issue #24455 <https://gitlab.haskell.org/ghc/ghc/-/issues/24455>`_.


Motivation
----------

GHC already supports two ways to talk about constructor-field strictness in
surface syntax:

* Prefix ``!`` explicitly marks a field strict.
* ``-XStrictData`` changes the default so that unannotated fields are strict,
  and prefix ``~`` can be used to opt a field back to lazy.

This coupling is awkward. A programmer or code generator may want to express
that a field is lazy *explicitly*, without changing the meaning of every other
unannotated field in the module.

For example, this declaration is accepted in any module::

  data A = A !Int

But the analogous lazy annotation is rejected unless ``StrictData`` (or
``Strict``, which implies it) is enabled::

  data B = B ~Int

Today GHC reports that "Lazy field annotations (~) are disabled" and suggests
enabling ``StrictData``. But enabling ``StrictData`` is not just a syntactic
change: it also changes the default semantics of every unannotated field in the
module.

This particularly affects generated declarations. Template Haskell already has
``SourceLazy`` in ``Language.Haskell.TH.Syntax``, but using it without
``StrictData`` is rejected for the same reason. As a result, generators must
inspect the ambient extension set and emit either an explicit ``~`` annotation
or a plain field type depending on whether ``StrictData`` is enabled. Similar
issues arise for other source generators such as ``happy``.

This proposal unbundles "allow the explicit ``~`` syntax" from "change the
module-wide default for unannotated fields". It addresses a small but real gap
in the language design of ``StrictData``: syntax and semantics are currently
bundled together.

This proposal is deliberately narrow. It is not the proposal in `GHC issue
#16836 <https://gitlab.haskell.org/ghc/ghc/-/issues/16836>`_ to require every
field to be annotated with either ``!`` or ``~``. It only adds the missing
opt-in syntax for explicit laziness.


Proposed Change Specification
-----------------------------

A new language extension ``LazyFieldAnnotations`` is added. The extension is
disabled by default.

When ``LazyFieldAnnotations`` is enabled, prefix ``~`` is accepted as a lazy
field annotation in every constructor-field position where prefix ``!`` is
accepted today.

More precisely, this proposal reuses the existing syntax and semantics of lazy
field annotations under ``StrictData``. The only language change is to the
extension guard: a prefix ``~`` in a constructor field is accepted when either
``StrictData`` or ``LazyFieldAnnotations`` is enabled.

Syntax
~~~~~~

This applies to:

* Haskell-98 prefix constructor fields
* Haskell-98 record fields
* GADT constructor argument types
* GADT record fields

For example, all of the following declarations are accepted when
``LazyFieldAnnotations`` is enabled::

  data A = A ~Int Bool
  data B = B { b1 :: ~Int, b2 :: !Bool }

  data C where
    C1 :: ~Int -> C
    C2 :: { c1 :: ~Int, c2 :: !Bool } -> C

In the terminology introduced by `proposal #402
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst>`_,
the grammar already admits a ``strictness_sigil`` of either ``!`` or ``~``.
Today that proposal specifies the side condition:

  In ``strictness_sigil``, the ``~`` is guarded behind ``-XStrictData``.

This proposal changes that side condition to:

  In ``strictness_sigil``, the ``~`` is guarded behind ``-XStrictData`` or
  ``-XLazyFieldAnnotations``.

The corresponding Haskell-98 constructor-field syntax is changed in the same
way: wherever a field strictness annotation is currently permitted, a prefix
``~`` is accepted when either of those two extensions is enabled.

Semantics
~~~~~~~~~

``LazyFieldAnnotations`` does not change the default strictness of any
unannotated field.

* With ``LazyFieldAnnotations`` enabled and ``StrictData`` disabled, an
  unannotated field remains lazy. A field written ``~ty`` has the same
  semantics as an unannotated field ``ty``.
* With ``StrictData`` enabled, behaviour is unchanged: unannotated fields are
  strict, and ``~ty`` marks that field lazy.
* With both extensions enabled, behaviour is the same as with ``StrictData``
  alone.
* ``Strict`` continues to imply ``StrictData``, and therefore also continues to
  permit ``~`` field annotations.

``LazyFieldAnnotations`` has no effect outside constructor-field types. In
particular, it does not change the syntax or meaning of term-level irrefutable
patterns.

This proposal does not relax the existing restriction on ``newtype``
constructors: ``newtype`` fields still must not have a strictness annotation.
For example, this declaration remains invalid::

  newtype N = N ~Int

Existing rules for ``{-# UNPACK #-}`` and ``{-# NOUNPACK #-}`` are unchanged.
Any current warnings or restrictions involving those pragmas continue to apply.


Proposed Library Change Specification
-------------------------------------

None.

Template Haskell already exposes the existing ``SourceLazy`` annotation. This
proposal does not add new library API; it only changes when that existing
annotation is accepted in generated declarations.


Examples
--------

With ``LazyFieldAnnotations`` alone, the default remains lazy::

  {-# LANGUAGE LazyFieldAnnotations #-}

  data A = A ~Int Bool

Both fields of ``A`` are lazy. The ``~`` on the first field is explicit but
semantically redundant.

With both ``LazyFieldAnnotations`` and ``StrictData``, the default remains the
one from ``StrictData``::

  {-# LANGUAGE LazyFieldAnnotations, StrictData #-}

  data B = B ~Int Bool

The first field of ``B`` is lazy and the second is strict.

The extension also applies to record and GADT syntax::

  {-# LANGUAGE GADTs, LazyFieldAnnotations #-}

  data T where
    MkT :: { lazyField :: ~Int, strictField :: !Bool } -> T

Generated code can now say what it means directly instead of branching on
``StrictData``. For example, a Template Haskell declaration using
``SourceLazy`` can be accepted in a module that enables
``LazyFieldAnnotations`` even if it does not enable ``StrictData``.


Effect and Interactions
-----------------------

This proposal addresses the motivating use cases directly:

* Template Haskell code can generate a lazy field explicitly without also
  requiring ``StrictData``.
* Other code generators, such as ``happy``, can emit explicit lazy field
  annotations uniformly instead of adapting their output to the ambient default.
* Hand-written code gains a way to document that a field is intentionally lazy,
  symmetric with today's explicit ``!`` syntax.

Interaction with ``StrictData`` is intentionally conservative. The behaviour of
modules that already use ``StrictData`` or ``Strict`` is unchanged.

The proposal does not solve every strictness-annotation issue. In particular,
``newtype`` constructors still reject strictness annotations, so generators that
produce both ``data`` and ``newtype`` declarations still need to account for
that distinction.

No new warning classes are introduced. Existing warnings, such as the warning
that ``{-# UNPACK #-}`` on a lazy field lacks a ``!``, are unchanged.

At the same time, making both ``!`` and ``~`` available independently of the
module default may make a future warning such as
``-Wimplicit-field-strictness`` more practical, by letting users write either
choice explicitly without also opting into ``StrictData``. Such a warning is
outside the scope of this proposal.


Costs and Drawbacks
-------------------

The implementation cost is small, but not zero:

* GHC gains one more language extension to document, test, and maintain.
* Users need to learn another extension name.
* Outside ``StrictData``, the annotation ``~`` is semantically redundant, so
  some users may regard it as visual noise.

The proposal also does not address the broader desire for mandatory
strictness/laziness annotations on all fields; that remains a separate design
question.


Backward Compatibility
----------------------

This proposal has expected impact level 0 under the scale in the proposal
template: no breakage.

No existing program changes meaning unless it opts into
``LazyFieldAnnotations``. With the extension enabled, strictly more programs
are accepted. The meaning of existing ``StrictData`` code is unchanged.

There is no migration burden. Users and code generators may opt into the new
extension when they want to write lazy field annotations explicitly.


Alternatives
------------

Keep the status quo
~~~~~~~~~~~~~~~~~~~

Code generators can continue to inspect whether ``StrictData`` is enabled and
either emit ``~ty`` or erase the annotation. This is workable, but it pushes
extension-dependent bookkeeping into every generator and does not help
hand-written code.

Accept ``~`` everywhere, perhaps with a warning
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One alternative is to make lazy field annotations accepted unconditionally, and
perhaps warn when ``~`` is redundant in a lazy-by-default module. This would
avoid a new extension name, but it is a broader language change. The proposal
here keeps the change explicitly opt-in and therefore aligns better with GHC's
stability goals.

Silently erase ``SourceLazy`` outside ``StrictData``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Template Haskell helper functions could try to compensate for the missing
surface syntax by dropping ``SourceLazy`` when ``StrictData`` is disabled. This
is insufficient:

* it only helps some Template Haskell APIs;
* it does not help hand-written source code;
* it does not help non-TH generators such as ``happy``;
* it makes generated code depend on ambient extension settings in a less direct
  and less visible way.

Require every field to be annotated
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`GHC issue #16836 <https://gitlab.haskell.org/ghc/ghc/-/issues/16836>`_
suggests a broader extension that would require each field to carry either
``!`` or ``~``. That is a different proposal. The present proposal only adds
the missing syntax needed to express explicit laziness independently of
``StrictData``.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

No implementation commitment is required for the proposal to stand. The
expected implementation work is modest:

* adjust the existing extension guard for lazy field annotations;
* add parser and renamer/typechecker tests for H98, record, and GADT syntax;
* update the Users' Guide documentation for constructor-field strictness.


Acknowledgments
---------------

Thanks to Oleg Grenrus for raising `GHC issue #24455
<https://gitlab.haskell.org/ghc/ghc/-/issues/24455>`_, and to the commenters on
that issue and `GHC issue #16836
<https://gitlab.haskell.org/ghc/ghc/-/issues/16836>`_ for discussion.
