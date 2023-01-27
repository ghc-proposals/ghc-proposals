Deprecated instances
====================

.. author:: Vladislav Zavialov
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/17485
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/575>`_.
.. sectnum::
.. contents::

GHC allows to deprecate modules, functions, data constructors, type
constructors, but not instances. The lack of support for deprecation of
instances makes it impossible to remove them gracefully, with an advance
warning to the users. We propose to allow ``{-# WARNING ... #-}`` and
``{-# DEPRECATED ... #-}`` pragmas on instances to correct this.

Background
----------

GHC already allows library authors to deprecate modules, functions, data
constructors, and type constructors (including data types, newtypes, type
synonyms, type families, data families, and classes).

Here is an example of a deprecated module::

        module M {-# DEPRECATED "Do not use M" #-} where

Importing ``M`` produces the following warning::

        Example.hs:3:1: warning: [-Wdeprecations]
            Module ‘M’ is deprecated: "Do not use M"

And here is an example of a deprecated function::

        {-# DEPRECATED f "Do not use f" #-}
        f :: Int -> Int
        f = id

Using ``f`` produces the following warning::

        Example.hs:5:5: warning: [-Wdeprecations]
            In the use of ‘f’ (imported from M): Deprecated: "Do not use f"

Library authors use deprecation warnings to great effect.
Searching Hackage for ``{-# DEPRECATED`` results in
`3024 matches across 656 packages <https://hackage-search.serokell.io/?q=%5C%7B-%23+DEPRECATED>`_.

Motivation
----------

A discussion at `deepseq #16 "remove instance NFData (a -> b)" <https://github.com/haskell/deepseq/issues/16>`_
reveals that there is also demand for deprecation warnings on instances,
which are currently not supported.
The goal is to inform the users that the instance should be avoided
before removing it outright or rendering it unusable via a ``TypeError`` constraint.

Functions, data constructors and type constructors are referenced by name in deprecation pragmas.
This is not an option for instances, as instances are anonymous.

Modules use inline deprecation pragmas right before the ``where`` keyword.
An attempt to add an inline pragma to an instance leads to a parse error::

        -- Code:
        data T
        instance Eq T {-# DEPRECATED "Do not use Eq T" #-} where

        -- Error message:
        Example.hs:7:15: error: parse error on input ‘{-# DEPRECATED’

We propose to allow inline deprecation pragmas on instances.
The ``{-# WARNING ... #-}`` pragma is a close sibling of ``{-# DEPRECATED ... -}``, so we propose to allow it too.

Proposed Change Specification
-----------------------------

Syntax
~~~~~~

The existing non-terminals in ``Parser.y`` are defined thus::

        maybemodwarning
            : '{-# DEPRECATED' strings '#-}'
            | '{-# WARNING' strings '#-}'
            |  {- empty -}

        inst_decl
            : 'instance' overlap_pragma inst_type where_inst
            | ...

The ``maybemodwarning`` is used in module headers. Rename it to
``maybewarning`` and employ it in ``inst_decl`` as follows::

        inst_decl
            : 'instance' overlap_pragma inst_type maybewarning where_inst
            | ...

Semantics
~~~~~~~~~

The **use of an instance** is a point in the program where GHC would have produced
an error message if the instance context of the said instance contained
the ``Unsatisfiable`` constraint (specified in `#433 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0433-unsatisfiable.rst>`_).

Any use of an instance marked with a ``{-# DEPRECATED ... #-}`` or a ``{-# WARNING ... #-}`` pragma
shall trigger the attached warning.


Examples
--------

The notorious ``NFData`` instance can be modified as follows::

        instance NFData (a -> b) {-# DEPRECATED "Do not use NFData (a -> b). See deepseq issue #16" #-} where
          rnf = rwhnf


Effect and Interactions
-----------------------

* We have tested and confirmed that the syntax changes do not lead to any
  shift/reduce or reduce/reduce conflicts. The proposed syntax is easy to parse.

* The proposal is restricted to class instances and does not cover type family
  or data family instances. While it is trivial to extend the syntax,
  the semantics are less clear and we do not have concrete motivating examples.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature
to be minimal.

Alternatives
------------

An alternative, constraint-based approach, is presented in `#454 <https://github.com/ghc-proposals/ghc-proposals/pull/454>`_.
The pragma-based approach proposed here is more conservative and easier to implement.

Implementation Plan
-------------------

Vladislav Zavialov, Sylvain Henry, and Moritz Angermann are likely to collaborate to implement this.
