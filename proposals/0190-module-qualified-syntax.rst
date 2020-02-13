Allow ``qualified`` to follow module name
=========================================

.. author:: Shayne Fletcher
.. date-accepted:: 2019-05-02
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/merge_requests/853
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/190>`_.
.. contents::

We propose to allow the syntax ``import M qualified`` to solve hanging indents in module import lists.

Motivation
----------
To import a qualified module you must specify ``qualified`` in prepositive position : ``import qualified M``. This often leads to a "hanging indent" (which is automatically inserted by some `autoformatters <https://github.com/commercialhaskell/hindent/blob/master/src/HIndent.hs>`_ and `common <https://github.com/owickstrom/gi-gtk-declarative/blob/master/gi-gtk-declarative/src/GI/Gtk/Declarative/Container/Class.hs>`_ `in <https://github.com/commercialhaskell/intero/blob/master/src/GhciFind.hs>`_ `many <https://github.com/aristidb/aws/blob/master/Aws/Iam/Core.hs>`_  `code <https://github.com/input-output-hk/cardano-sl/blob/develop/explorer/src/Pos/Explorer/DB.hs>`_ `bases <https://github.com/PostgREST/postgrest/blob/master/src/PostgREST/Error.hs>`_). For example:

::

 import qualified A
 import           B
 import           C

We propose to also allow ``qualified`` to appear in postpositive position : ``import M qualified``. In that case, one could write:

::

   import A qualified
   import B
   import C

While a small change, this annoyance is a significant issue in many large codebases, where programmers are forced to "pick a style" - chosing to add redundant whitespace to have modules align, or to make scanning import lists harder. Additionally, the location of `qualified` makes sorting imports harder.

Proposed Change Specification
-----------------------------
A new language extension ``ImportQualifiedPost`` is introduced. When enabled, ``qualified`` will be accepted in postpositive position, by updating the ``importdecl`` production like so:

::

   importdecl :: { LImportDecl GhcPs }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid optqualified maybeas maybeimpspec

A new warning ``-Wprepositive-qualified-module`` (off by default) will be introduced that warns on prepositive syntax:

::

  Preposition.hs:5:8: warning: [-Wprepositive-qualified-module]
      Found ‘qualified’ in prepositive position
      Suggested fix: place  ‘qualified’ after the module name instead.
    |
  5 | import qualified Data.List
    |        ^^^^^^^^^

Effect and Interactions
-----------------------
The proposed change adds the ability to specify a qualified import by placing ``qualified`` either before or after the module name. Under ``ImportQualifiedPost``, ``qualified`` will be permitted in either pre- or post-positive positions (but not both). If in the future we decide to deprecate the pre-positive syntax, we would make an extension flag for that and allow it to be turned off.

The position of the ``qualified`` does not change the semantics, and is independent from any other import features (e.g. package imports or safe annotations).  As an example, both ``import qualified D as E`` and ``import D qualified as E`` are permitted and equivalent.

There should be no other interactions with any existing language or compiler features.

Costs and Drawbacks
-------------------
The implementation of the change is but a few lines (``Parser.y`` for the grammar and ``RdrHsSyn.hs`` for warnings/errors). The increased flexibility comes with no discernible drawbacks.

Alternatives
------------
The alternatives appear to be:
(1) Keep the status-quo and do not allow the alternate syntax;
(2) Mandate the alternative syntax and formulate a migration strategy.

The second alternative solves the motivating hanging indent issue but in our opinion both alternatives seem needlessly strict when both conventions can be had cheaply with only upside.

Unresolved Questions
--------------------
There are no remaining unresolved questions.

Implementation Plan
-------------------
If accepted, the proposal authors will implement the change.
