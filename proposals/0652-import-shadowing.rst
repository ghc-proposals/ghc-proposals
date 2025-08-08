Import shadowing
================

.. author:: Gergo Erdi
.. date-accepted:: 2024-08-15
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/25187
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/652>`_.
.. sectnum::
.. contents::

Local binders are allowed to shadow names defined in outer
scopes. Here, we propose that top-level binders should also be allowed
to shadow imported names, basically treating imported modules as
living in a scope that is "outside" the top-level one.


Motivation
----------
Currently, there is no way in Haskell to shadow an imported name in
the top-level scope. The only workaround is to avoid importing said
name at all. For example, if we want to use some ``Control.Exception``
functions while also defining our own ``catch``, we have to import the
former with an explicit hiding of ``catch``:

::

 module Example1 where

 import Control.Exception hiding (catch)

 catch :: Fish -> IO Food
 catch fish = ...

 goFishing :: IO Food
 goFishing = bracket goToCabin goHome $ catch salmon

Without hiding ``catch``, this results in a name resolution conflict
in the definition of ``goFishing``:

::

 Ambiguous occurrence `catch'
 It could refer to
    either `Control.Exception.catch',
           imported from `Control.Exception' at Example1.hs:1:1-24
           (and originally defined in `GHC.IO')
        or `Example1.catch', defined at Example1.hs:7:1

Our proposal is an alternative name resolution policy exposed as a
language extension tentatively named ``ImportShadowing``. It allows
definitions from the current module's top-level scope to shadow names
from imported modules. So in this example, the occurrence of ``catch``
in the definition of ``goFishing`` resolves to the definition in
``Example1`` even when ``catch`` is not explicitly hidden from the
import list of ``Control.Exception``.

We believe this is an improvement to the status quo because it is a
natural extension of the idea behind the shadowing policy of local
binders: that shadowing expresses the intention by the user to only
care about the names that are defined "more nearby". A cursory search
on `Stack Overflow <https://stackoverflow.com/>`_ finds lots of
Haskell users who implicitly expected imports to be shadowed by
top-level definitions:

* https://stackoverflow.com/q/73788349/477476
* https://stackoverflow.com/q/7761238/477476
* https://stackoverflow.com/q/56047335/477476
* https://stackoverflow.com/q/40964909/477476
* https://stackoverflow.com/q/40314142/477476
* https://stackoverflow.com/q/67246392/477476

A second-order effect of the proposed extension is that it can lead to
preemptive forward compatibility. Adding a new export to ``Prelude``
can lead to breakage just by virtue of existing code defining and
using top-level definitions with the same name. With
``ImportShadowing``, the existing intra-module references keep their
meaning and there is no migration needed to accomodate the new
``Prelude`` names.

Proposed Change Specification
-----------------------------

A new language extension ``ImportShadowing`` is added.

When ``ImportShadowing`` is enabled, the following changes take place:

Resolution of references in module body
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider an occurrence of an unqualified name ``x``, not bound locally
(by ``let``, lambda, a ``case`` alternative, etc). There are two
possible sources of resolving it:

(A) If there is a top-level binding of ``x`` then the occurrence is
    resolved to that binding.

(B) If the import declarations bring into scope a unique entity with
    unqualified name ``x``, the occurrence is resolved to that entity.

Consider an occurrence of a qualified name ``M.x``:

(A) If the module is called ``M`` and there is a top-level binding of
    ``x``, the occurrence is resolved to that binding

(B) If the import declarations bring into scope a unique entity with
    qualified name ``M.x``, the occurrence is resolved to that
    entity.

In both cases, Haskell 2010 regards cases (A) and (B) on equal footing
`as per Section 5.5.2
<https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1090005.5.2>`_:
if exactly one of the two cases can be used to resolve the name, that
case is used; if both cases can be used, then the occurrence is
ambiguous and reported as such.

Instead, we propose that when ``ImportShadowing`` is enabled,
(A) and (B) are tried in order, i.e. if the (A) case resolves the
occurrence, then that is used, and the (B) case is only checked
otherwise.

Alternative perspective: desugaring into ``let`` bindings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In Haskell 2010, all imported names and all top-level definitions in
the current module together make up a single unified top-level
scope. With this proposed alternative policy, there are two top-level
scopes instead: one consisting of all imported names, and a second
one, *under* this first one, that consists of all top-level definitions
from the current module.

To model these two name resolution approaches, we can desugar the
Haskell 2010 name resolution policy for a given module to a single
nested ``let``-block, e.g. for the following program:
 
::

 module Mod (fun1, fun2) where

 import M1 hiding (overridden)
 import qualified M2

 overridden = ... importedFromM1 ...
 fun1 = ... overridden ...
 fun2 = ... M2.importedFromM2 ... fun1 ...

we can write out its explicit scoping as:

::

 let
   -- imports from M1
   importedFromM1 = ...

   -- imports from M2
   B.importedFromM2 = ...

   -- defined in Mod
   overridden = ... importedFromM1 ...
   fun1 = ... overridden ...
   fun2 = ... M2.importedFromM2 ... fun1 ...
 in
   -- exports of Mod
   (fun1, fun2)

With our proposed scheme, the same program with ``ImportShadowing``
turned on can be modeled as a two nested ``let`` blocks:

::

 let
   -- imported from M1
   importedFromM1 = ...

   -- imports from M2
   B.importedFromM2 = ...

 in
   -- defined in Mod
   let
     overridden = ... importedFromM1 ...
     fun1 = ... overridden ...
     fun2 = ... M2.importedFromM2 ... fun1 ...
   in
     -- exports of Mod
     (fun1, fun2)

Of course, in this example, there is no observable difference between
the two desugarings, since our module ``Mod`` was already well-scoped
with the Haskell 2010 shadowing rules. However, if we change the
program slightly by importing all of ``M1`` wholesale:

::

 module Mod (fun1, fun2) where

 import M1
 import qualified M2

 overridden = ... importedFromM1 ...
 fun1 = ... overridden ...
 fun2 = ... M2.importedFromM2 ... fun1 ...

then the desugaring using Haskell 2010 semantics leads to the
following invalid program (note the two bindings of ``overridden`` in
the same ``let``):

::

 let
   -- imports from M1
   importedFromM1 = ...
   overriden = ...

   -- imports from M2
   M2.importedFromM2 = ...

   -- defined in Mod
   overridden = ... importedFromA ...
   fun1 = ... overridden ...
   fun2 = ... M2.importedFromM2 ... fun1 ...
 in
   -- exports of Mod
   (fun1, fun2)

Whereas the ``ImportShadowing`` version is valid:

::

 let
   -- imported from M1
   importedFromM1 = ...
   overridden = ...

   -- imports from M2
   M2.importedFromM2 = ...

 in
   -- defined in Mod
   let
     overridden = ... importedFromM1 ... -- This shadows the imported "overridden"!
     fun1 = ... overridden ...
     fun2 = ... M2.importedFromM2 ... fun1 ...
   in
     -- exports of Mod
     (fun1, fun2)

Export lists
~~~~~~~~~~~~

References in a module's export specification are resolved in the same
scope as that used for references in the module body, as per
`Resolution of references in module body`_. For example if we have
something like

::

 module A (foo) where

 import M (foo)

 foo = ...

then the ``foo`` exported by ``A`` should be the one defined in
``A``'s top-level.

.. _module-reexports:

Module reexports
^^^^^^^^^^^^^^^^

In an export list, `Paragraph 5 of Section 5.2 of the Haskell 2010 report
<https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_
specifies that the form ``module M`` names the set of all entities
that are in scope with both an unqualified name ``e`` and a qualified
name ``M.e``.  So in this example, *without* ``ImportShadowing``:

::

 module A (module M) where

 import M (foo, wombat)
 import N (foo)

 foo = True

the ``module M`` exports ``M.foo`` because that entity is in scope
both as ``M.foo`` and with unqualified name ``foo``.  The fact that
``N.foo`` and ``A.foo`` are *also* in scope with unqualified name
``foo`` does not matter.

With ``ImportShadowing``, however, the local definition of ``A.foo``
*completely hides* the unqualified imports of ``M.foo`` and ``N.foo``;
so now ``M.foo`` is no longer in scope with unqualified name ``foo``;
so the ``module M`` export list exports only ``M.wombat``.

Conflicting exports
^^^^^^^^^^^^^^^^^^^

`Section 5.2 of the Haskell 2010 report
<https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_
says "The unqualified names of the entities exported by a module must
all be distinct".  That condition remains unchanged with this
proposal. For example consider

::
 
 module A( module M, module N, foo ) where

 import M( foo, wombat )
 import N( wombat )

 foo = True

Without ``ImportShadowing`` the export list would attempt to export
``M.foo`` and ``A.foo``; and would report a conflict. It also attempts
to export ``M.wombat`` and ``N.wombat`` and would again report a
conflict (assuming they are distinct entities).

With ``ImportShadowing`` the ``wombat`` behaviour is unchanged
(i.e. an export conflict is reported); but the exports ``module M``
and ``foo`` do not conflict since module ``M`` no longer exports
``M.foo`` (see :ref:`module-reexports` above).

Warnings
~~~~~~~~

Top-level bindings that shadow imported names should be regarding as
shadowing bindings for the purposes of ``-Wname-shadowing``.

     
Examples
--------
This extension shines especially when shadowing names defined in the
``Prelude``, since hiding ``Prelude`` imports otherwise requires
changing to an explicit import for ``Prelude``: we can go from

::

 module Mod where

 import Prelude hiding (zip)

 zip = ...

to just

::

 module Mod where

 zip = ...

The above example is taken directly from `the "Import" page of the
Haskell Wiki <https://wiki.haskell.org/Import>`_.

Costs and Drawbacks
-------------------
The usual drawback of language extensions leading to some language
fragmentation.

Users new to Haskell seem to find this idea intuitive. We have
gathered decade+-long experience with a Haskell compiler that uses
import shadowing (and doesn't even let users turn it off), with a
Haskell code base of several million lines of code that sees work from
both experienced Haskell developers as well as people with a
non-software-engineering background whose introduction to Haskell was
via this compiler. There's no record of either novices (learning only
the import-shadowing behaviour) or experienced Haskellers (who are
used to imports being in the same scope as top-level definitions) ever
getting into trouble due to this difference to Haskell 2010.


Backward Compatibility
----------------------
Haskell 2010 doesn't have a mechanism for shadowing imported names,
and valid Haskell 2010 programs retain their exact meanings with
``ImportShadowing`` turned on. The proposed extension only makes
previously unaccepted programs accepted by the scope checker.

So this is a "-1"-impact change: it doesn't break existing code, and
"un-breaks" existing broken code.

Alternatives
------------

Status quo
~~~~~~~~~~
Before this proposal, there are two alternative ways of referring to
names defined at the current module's top level:

* The imported names we want to shadow can be hidden from the import
  itself, using the ``import SomeModule hiding (someName)`` syntax

* The current module's name can be used to qualify names,
  i.e. ``CurrentModule.someName`` instead of just ``someName``.

Ordered imports
~~~~~~~~~~~~~~~
Other languages like OCaml or Agda have a linear top-level scope. The
Haskell equivalent of this would be that later ``import`` statements
and top-level bindings shadow earlier ones. By way of example,
supposing ``foo`` is exported by all of ``A``, ``B``, and ``C``:

::
   
 module Mod where

 import A
 import B

 -- Here, "foo" resolves to "B.foo"

 foo = ...

 -- Here, "foo" resolves to "Mod.foo"
 
 import C

 -- Here, "foo" resolves to "C.foo"

This would be a complete departure from Haskell's usual permutation
invariance of definitions. It is this proposal author's opinion that
this would be too large a change to be up to the addition of a mere
``LANGUAGE`` pragma.

A full proposal for this would also need to answer hairy questions
like:

* If ``Mod`` exports ``foo``, which ``foo`` does that resolve to?

* Can I import ``A`` again to make its ``foo`` shadow ``C.foo``?

* Is it allowed to re-bind ``foo`` in ``Mod`` if there are
  ``import`` statements between it and the previous binding of ``foo``?  
 
Unresolved Questions
--------------------

_None came up in the proposal discussion_


Implementation Plan
-------------------
For GHC specifically, it already has a similar name resolution policy,
only used by the GHCi REPL. Implementing ``ImportShadowing`` is as
easy as switching to the GHCi shadowing mechanism, plus some extra
fiddling around disambiguating exported names.

For other Haskell compilers, the implementation plan depends on their
current name resolution infrastructure.

Endorsements
------------
As mentioned in the Drawbacks section, we have positive
experience in a setting where ``ImportShadowing`` is always on in a
large Haskell code base with lots of developers over a long time.
