Allow ``qualified`` to follow module name
=========================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/190>`_.
.. sectnum::
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

Proposed Change Specification
-----------------------------

The ``importdecl`` production is updated like so:

::

   importdecl :: { LImportDecl GhcPs }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid optqualified maybeas maybeimpspec

A new language extension ``QualifiedImportsPostpositive`` is introduced. When enabled, ``qualified`` will be accepted in postpositive positition and a new warning ``-Wprepositive-qualified-module`` introduced that when set, warns on prepositive syntax:

::

  /Users/shaynefletcher/Preposition.hs:5:8: warning: [-Wprepositive-qualified-module]
      Found ‘qualified’ in prepositive position
      Suggested fix: place  ‘qualified’ after the module name instead.
    |
  5 | import qualified Data.List
    |        ^^^^^^^^^

Effect and Interactions
-----------------------
The proposed change adds the ability to specify a qualified import by placing ``qualified`` either before or after the module name (or both). A warning may be optionally enabled that alerts usages of ``qualified`` in prepositive position. There should be no other interactions with any existing language or compiler features.

Costs and Drawbacks
-------------------
The implementation of the change is but a few lines (``Parser.y`` for the grammar and ``RdrHsSyn.hs`` for the warning). The increased flexibility comes with no discernible drawbacks.

Alternatives
------------
The alternatives appear to be:
(1) Keep the status-quo and do not allow the alternate syntax;
(2) Mandate the alternative syntax and formulate a migration strategy.

The second alternative solves the motivating hanging indent issue but in our opinion both alternatives seem needlessly strict when both conventions can be had cheaply with only upside.

Unresolved Questions
--------------------
There are no unresolved questions at this time.

Implementation Plan
-------------------
If accepted, the proposal authors will implement the change.
