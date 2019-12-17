Lower precedence for {-# UNPACK #-}
===================================

.. author:: Vladislav Zavialov
.. date-accepted:: 2018-10-20
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/14761
.. implemented:: 8.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/174>`_.
.. contents::

The ``{-# UNPACK #-}`` pragma is not valid inside a type but binds more
tightly than function application. This is suboptimal because it contradicts
user expectations (`#14761
<https://gitlab.haskell.org/ghc/ghc/issues/14761>`_) and forces unnecessary
parenthesization of types. We propose to lower the precedence of ``{-# UNPACK
#-}``.

Motivation
------------

Compare the following declarations::

  data A = A { a :: {-# UNPACK #-} Maybe Int }
  data B = B { b :: {-# UNPACK #-} (Maybe Int) }

While ``B`` is accepted (without warning when ``-XStrictData`` is on), ``A``
fails with the following error::

  <interactive>:1:21: error:
      • Unexpected strictness annotation: {-# UNPACK #-}Mayb
  e
      • In the type ‘{-# UNPACK #-}Maybe Int’
        In the definition of data constructor ‘A’
        In the data declaration for ‘A’

This happens because it is parsed as ``({-# UNPACK #-} Maybe) Int``, but ``{-#
UNPACK #-}`` is only valid at the outer level of a type in a data constructor
field.

In GADT declarations, a similar issue occurs::

  data A where A :: {-# UNPACK #-} Maybe Int -> A
  data B where B :: {-# UNPACK #-} (Maybe Int) -> B

Proposed Change Specification
-----------------------------

Lower the precedence of ``{-# UNPACK #-}`` and ``{-# NOUNPACK #-}`` so that
they bind as loosely as possible, but tighter than ``(->)``.

Effect and Interactions
-----------------------

The change is conservative: strictly more programs will be accepted.

Costs and Drawbacks
-------------------

None.

Alternatives
------------

A similar argument can be made for ``!`` and ``~`` when they are used as
strictress/laziness annotations, but Simon Peyton Jones `argues
<https://gitlab.haskell.org/ghc/ghc/issues/14761#note_148688>`_ that it would "look
wrong":

    Both ``{-# UNPACK #-}`` and ``!`` only make sense at the outer level of a type
    in a data constructor field. But in GADT-style declarations, they can
    appear to be "inside" a type::

      data T where K :: !(Maybe Int) -> {-# UNPACK #-} !Int -> T

    Somehow ``!`` looks as if it should bind tightly.  e.g. ``!Maybe Int ->
    T`` looks wrong. But I agree that ``{-# UNPACK #-}`` would be better with
    a lower precedence.

Unresolved Questions
--------------------

Should we also lower the precedence of ``!`` and ``~``?

Implementation Plan
-------------------

I (Vladislav Zavialov) will implement this change. Most of the work is already
done in `Phab:D5180 <https://phabricator.haskell.org/D5180>`_.
