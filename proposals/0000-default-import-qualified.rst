Default Import Qualified
========================

.. author:: Akhra Mellivora Gannon
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/760>`_.
.. sectnum::
.. contents::

An extension to change default import behavior from unqualified to qualified, for better ergonomics and habit reinforcement around commonly preferred import practices.


Motivation
----------
Open imports have decades of historical inertia in Haskell, and not without reason. However, the past decade has seen significant adoption of Haskell in team settings and for non-academic applications, where qualified imports have significant benefits and are usually preferred. (Many popular packages outright demand them!) It is not uncommon for ``qualified`` to be repeated a dozen times or more at the top of a module, representing significant noise.

Further, in environments where qualified imports are preferred, the compiler offers little assistance: forgetting the keyword only produces an error in the case of an identifier collision. While this is irrelevant to program behavior, it can make habit-building more difficult.


Proposed Change Specification
-----------------------------
Two language extensions are proposed: ``ImportUnqualified``, and ``DefaultImportQualified``.

``ImportUnqualified``
^^^^^^^^^^^^^^^^^^^^^
This extension adds an ``unqualified`` *varid* keyword to import declarations. Wherever ``qualified`` may appear, ``unqualified`` may appear instead. (They may not both appear in the same declaration.)

Concretely, the `Haskell 2010 Report section 5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_ has:

  | *impdecl* 	→ 	``import`` [``qualified``] *modid* [``as`` *modid*] [*impspec*]
  | ...

This becomes:
  | *impdecl* 	→ 	``import`` [*impqual*] *modid* [``as`` *modid*] [*impspec*]
  | ...
  | *impqual* 	→ 	``qualified`` | ``unqualified``

Changes to where ``qualified`` can appear generalize to ``impqual``.

In isolation ``unqualified`` does nothing, but separating it from ``DefaultImportQualified`` greatly aids migration to and from the latter. Options to build further on this are offered in Alternatives.

``DefaultImportQualified``
^^^^^^^^^^^^^^^^^^^^^^^^^^
``DefaultImportQualified`` implies ``ImportUnqualified``. Under this extension, all import declarations which do not use the ``unqualified`` keyword behave as if they had used the ``qualified`` keyword, per `Section 5.3.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3.2>`_.

Implicit (undeclared) imports are unaffected.


Examples
--------
Haskell 2010:
::
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import qualified Data.Vector as Vec
  import Data.Vector ((!), (!?), (//))

Proposed:
::
  {-# LANGUAGE DefaultImportQualified #-}
  import Data.Map as Map
  import Data.Set as Set
  import Data.Vector as Vec
  import Data.Vector unqualified ((!), (!?), (//))


Effect and Interactions
-----------------------
With default-qualified imports, it takes intent for a binding's origin to lack a direct indicator at its use point; it can't happen by accident. Cases where lower inline noise is worth the trade-off (e.g. frequently-used operators, or a DSL used extensively in a module) are supported via ``unqualified``, which helps readers narrow down where to look for a binding's source when it is not otherwise explicit.

``ImportQualifiedPost``
^^^^^^^^^^^^^^^^^^^^^^^
This interaction is covered by the core specification, along with any similar extensions/revisions which may arise in the future.

``ImplicitPrelude``
^^^^^^^^^^^^^^^^^^^
The specification excludes this (and any other) undeclared import.

``ImportAnd``
^^^^^^^^^^^^^
`Proposal #763 <https://github.com/ghc-proposals/ghc-proposals/pull/758>`_ grew out of work on this proposal. Each can (and should) stand on its own, but their combined expressive power exceeds either alone. For instance, the following examples would be exactly equivalent.

Haskell 2010:
::
  {-# LANGUAGE NoImplicitPrelude #-}
  import MyCustomPrelude
  import qualified Data.Vector as Vec
  import Data.Vector ((!), (!?), (//))

Proposed:
::
  {-# LANGUAGE DefaultImportQualified, ImportAnd, NoImplicitPrelude #-}
  import MyCustomPrelude unqualified
  import Data.Vector as Vec and ((!), (!?), (//))


Costs and Drawbacks
-------------------
Regarding development and maintenance: uncertain, feedback requested!

``DefaultImportQualified`` represents a fork, but in a way that feels coherent with the base language: the extension's name and behavior are hopefully self-explanatory and completely unsurprising.

Contextually, ``ImportUnqualified`` is a strict improvement for syntatic parity; compare the currently proposed ``LazyFieldAnnotations`` (`#752 <https://github.com/ghc-proposals/ghc-proposals/pull/752>`_).


Backward Compatibility
----------------------
``DefaultImportQualified`` has level 5 impact: approximately *all* existing programs will break when it is enabled. However, this is the point! It is intended as a local opt-in. More importantly, ``ImportUnqualified`` has zero impact and serves as the migration solution: the keyword can be enabled and all imports annotated before any behavior is changed.


Alternatives
------------
Migration Assistance Warnings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Two warnings could potentially be enabled alongside ``ImportUnqualified`` to improve migration ergonomics.

``-Wunspecified-import-qualification`` would issue a warning when an import declaration excludes *impqual*. This enables GHC to directly assist migration by guaranteeing all import declarations are annotated.

Conversely, ``-Wspurious-import-qualification`` would warn when ``qualified`` is encountered in the presence of ``DefaultImportQualified``, and when ``unqualified`` is encountered in its absence. This helps post-migration cleanup of redundant annotations.

These have been relegated to Alternatives for two reasons:

1. Both can be accomplished by a linter. (That said, the option of ``-Werror`` is likely desirable for rigor during migration.)
2. Mutually exclusive warnings might be unprecedented? If so, adding both would carry extra implementation cost.

Bikeshedding
^^^^^^^^^^^^
``unqualified`` is a very long keyword. Other languages (and other proposals!) use ``open`` in a similar context, and it could be adopted here.

``toplevel`` also seems viable, but gains less for the break from current language. (Plus it's one letter shorter than ``qualified``, so they wouldn't even line up nicely.)

``unqualified`` Without Extension
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Prior drafts of this proposal allowed ``unqualified`` without enabling an extension. A recent `comment <https://github.com/ghc-proposals/ghc-proposals/pull/748#issuecomment-4634935718>`_ by Adam Gundry on another proposal pointed out that this creates a break in what e.g. ``-XHaskell2010`` means: programs written with the new syntax could be incompatible with prior GHC versions despite no apparent difference in required language support.

Negative Extension Naming
^^^^^^^^^^^^^^^^^^^^^^^^^
If support is strong enough that it's likely to become a future language default, ``DefaultImportQualified`` could instead be ``NoDefaultImportUnqualified``. When this proposal was first conceived (quite some time ago), this seemed reasonable to consider. More recently though, several proposals around qualified exports and localized imports have raised the possibility of such fundamental changes in import practices that cementing a new default seems unwise.

Further, negative naming for ``ImportUnqualified`` doesn't really work; this alternative would likely require enabling it without an extension.


Unresolved Questions
--------------------


Implementation Plan
-------------------
Tentatively and with a likely need for guidance, the author can attempt implementation if nobody else finds it an exciting project.


Endorsements
-------------


Acknowledgments
---------------
