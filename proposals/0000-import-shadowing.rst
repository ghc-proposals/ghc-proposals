Import shadowing
================

.. author:: Gergo Erdi
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/652>`_.
.. sectnum::
.. contents::

Local binders are allowed to shadow names defined in outer
scopes. Here, we propose that top-level binders should also be allowed
to shadow imported names, basically treating imported modules as
living in a scope that is "outside" the top-level one.


Motivation
----------
Currently, there is no way in Haskell to shadow an imported name. The
only workaround is to avoid importing said name at all. For example,
if we want to use some ``Control.Exception`` functions while also
defining our own ``catch``, we have to import the former with an explicit
hiding of ``catch``:

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
care about the names that are defined "more nearby".

Proposed Change Specification
-----------------------------
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

 import A hiding (overridden)
 import qualified B

 overridden = ... importedFromA ...
 fun1 = ... overridden ...
 fun2 = ... B.importedFromB ... fun1 ...

we can write out its explicit scoping as:

::

 let
   -- imports from A
   importedFromA = ...

   -- imports from B
   B.importedFromB = ...

   -- defined in Mod
   overridden = ... importedFromA ...
   fun1 = ... overridden ...
   fun2 = ... B.importedFromB ... fun1 ...
 in
   -- exports of Mod
   (fun1, fun2)

With our proposed scheme, the same program with ``ImportShadowing``
turned on can be modeled as a two nested ``let`` blocks:

::

 let
   -- imported from A
   importedFromA = ...

   -- imports from B
   B.importedFromB = ...

 in
   -- defined in Mod
   let
     overridden = ... importedFromA ...
     fun1 = ... overridden ...
     fun2 = ... B.importedFromB ... fun1 ...
   in
     -- exports of Mod
     (fun1, fun2)

Of course, in this example, there is no observable difference between
the two desugarings, since our module ``Mod`` was already well-scoped
with the Haskell 2010 shadowing rules. However, if we change the
program slightly by importing all of ``A`` wholesale:

::

 module Mod (fun1, fun2) where

 import A
 import qualified B

 overridden = ... importedFromA ...
 fun1 = ... overridden ...
 fun2 = ... B.importedFromB ... fun1 ...

then the desugaring using Haskell 2010 semantics leads to the
following invalid program (note the two bindings of ``overridden`` in
the same ``let``):

::

 let
   -- imports from A
   importedFromA = ...
   overriden = ...

   -- imports from B
   B.importedFromB = ...

   -- defined in Mod
   overridden = ... importedFromA ...
   fun1 = ... overridden ...
   fun2 = ... B.importedFromB ... fun1 ...
 in
   -- exports of Mod
   (fun1, fun2)

Whereas the ``ImportShadowing`` version is valid:

::

 let
   -- imported from A
   importedFromA = ...
   overridden = ...

   -- imports from B
   B.importedFromB = ...

 in
   -- defined in Mod
   let
     overridden = ... importedFromA ... -- This shadows the imported "overridden"!
     fun1 = ... overridden ...
     fun2 = ... B.importedFromB ... fun1 ...
   in
     -- exports of Mod
     (fun1, fun2)

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
   
Effect and Interactions
-----------------------
Beside intra-module references, the other place where top-level
bindings can be used is export specifications. It feels natural to
resolve exports in the same scope used for the module. For example, if
we have something like

::

 module A (foo) where

 import B -- This exports "foo"

 foo = ...

then the ``foo`` exported by ``A`` should be the one defined in
``A``'s top-level.


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

A second-order effect of this is that using this extension can lead to
preemptive forward compatibility. Adding a new export to ``Prelude``
can lead to breakage just by virtue of existing code defining and
using top-level definitions with the same name. With
``ImportShadowing``, the existing intra-module references keep their
meaning and there is no migration needed to accomodate the new
``Prelude`` names.

Alternatives
------------
There are two alternative ways of referring to names defined at the
current module's top level:

* The imported names we want to shadow can be hidden from the import
  itself, using the ``import SomeModule hiding (someName)`` syntax

* The current module's name can be used to qualify names,
  i.e. ``CurrentModule.someName`` instead of just ``someName``.

Unresolved Questions
--------------------
**TODO: add later, from Proposal comments**

Implementation Plan
-------------------
For GHC specifically, it already has a similar name resolution policy,
only used by the GHCi REPL. Implementing ``ImportShadowing`` is as
easy as switching to the GHCi shadowing mechanism, plus some extra
fiddling around disambiguating exported names.

For other Haskell compilers, the implementation plan depends on their
current name resolution infrastructure.

Endorsements
-------------
A cursory search on `Stack Overflow <https://stackoverflow.com/>`_
finds lots of Haskell users who implicitly expected imports to be
shadowed by top-level definitions:

* https://stackoverflow.com/q/73788349/477476
* https://stackoverflow.com/q/7761238/477476
* https://stackoverflow.com/q/56047335/477476
* https://stackoverflow.com/q/40964909/477476
* https://stackoverflow.com/q/40314142/477476
* https://stackoverflow.com/q/67246392/477476

As mentioned in the Drawbacks section, we also have positive
experience in a setting where ``ImportShadowing`` is always on in a
large Haskell code base with lots of developers over a long time.
