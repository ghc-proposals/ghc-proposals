.. author:: Michael Peyton Jones
.. date-accepted:: ""
.. proposal-number:: ""
.. ticket-url:: ""
.. implemented:: ""
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/295>`_.
.. sectnum::
.. contents::

First class modules
===================

The Haskell module system helps us manage the names of the *entities* of a
Haskell program. That includes values, types, typeclasses, but notably
*not* modules. We can justifiably say that modules are second-class
citizens of their own system.

This proposal makes modules just like everything else in a Haskell program,
so they can be named and managed in the same way that we already do successfully
for other entities. In doing so, we also solve some existing ergonomic
issues with the module system.

Motivation
----------

Modules are not first-class in the Haskell module system. They cannot be
named in the way that other entities can, and this makes them difficult
to manipulate. From the point of view of [hs2010]_, modules barely
even exist - the only real thing is qualified names! This is frustrating
for users, since modules *seem* to have names, and they *seem* to have
a logical role as entities which are “containers” of bindings. [#]_

.. [#] This gives them a distinct similarity to objects, with Scala
   taking this to the logical extreme of identifying modules and objects.

This proposal suggests that modules should be entities in the module
system. This requires a conceptual reimagining of the current system, but it
allows us to make a number of further changes which unlock a lot
of power.

There are many examples of problems with the current module system in
[ghc283]_ and [ghc205]_  which I
will not repeat here: I believe this proposal solves them all.

Proposed change specification
-----------------------------

Outline
~~~~~~~

This proposal consists of three sets of proposed changes. The first one,
`Modules as entities`_, is a conceptual reinterpretation of the Haskell module
system in which modules are entities.

The second section, `Backwards compatibility extensions`_, lists a series
of backwards compatibility changes which would need to accompany `Modules as entities`_.

The third section, `Additional features`_, lists a number of additional features
which could be added. These are more-or-less independent, but are important for
the motivation of this proposal, and I would not recommend that it be
accepted without at least some of them.

This proposal is conceptually based on [hsmods]_. We assume that
this provides an accurate representation of the current state of the
Haskell module system and we describe our changes with respect to it. [#]_

.. [#] I find the relational setting very amenable for this work, since it makes it
   easy to describe the rules while gracefully handling un- or multiply-defined
   bindings.

The text here is mostly expository. There is a `Datalog implementation`_ of some
of the rules.

Terminology
~~~~~~~~~~~

-  An *entity* is something that can be referred to with a name.
   Currently this means types, values, field accessors, etc.
-  A *simple name* is a name that is given to something when it is
   defined.
-  A *qualified name* is a more complex form of name which can *refer*
   to an entity. We will say more about qualified names later. 
-  A *dotted name* is a simple name that contains ``.``\ s, e.g
   ``Data.Set``. We will want to distinguish these from qualified names. [#]_
-  A *binding* is a mapping from a name to an entity.
-  An *environment* is a set of bindings, which can be used to resolve
   variable references. We do not require that there be only one binding for
   any given name.
-  A *scope* is a region of the program which has an associated environment.
-  A *compilation unit* is the unit of Haskell compilation, currently a
   single ``.hs`` file.
-  A *module* is a named scope, of which file modules are currently the
   only examples.
-  A *file module* is a module which is defined at the top level of a
   ``.hs`` file. Currently this corresponds to a compilation unit.

.. [#] See `Approaches to dotted names`_ for discussion.

Finally, we can *merge* two environments by taking the union of their bindings. We will
also say we can "merge" modules by merging their exported environments.

Modules as entities
~~~~~~~~~~~~~~~~~~~

This change makes modules into entities, and allows them to be referred
to. We do *not* propose to gate this behind a language extension - rather we
provide a number of `backwards compatibility extensions <Backwards compatibility extensions>`_, enabled by default,
that ensure that we retain today's behaviour. [#]_

.. [#] See `All or nothing`_ for discussion of this approach and an alternative.

Modules become entities
^^^^^^^^^^^^^^^^^^^^^^^

We create an entity for each module in the program. All scopes now have a
*module namespace*, which contains bindings for modules. [#]_

.. [#] I will ignore namespaces for the rest of this proposal. They don't
       interact with this proposal much, since it is easy to distinguish which
       bindings belong to which namespaces by classifying the *entities* according
       to which namespace they belong to. Then we can have a "unified" namespace and
       simply filter out bindings that refer to the wrong kind of entities. 

Qualified names
^^^^^^^^^^^^^^^

Currently, qualified names consist of a simple name, possibly qualified
by a module name. That is, they could be represented by a type like:

::

   data QualName = Qualified ModuleName Name | Simple Name

We propose to replace this with a type like:

::

   data QualName = NonEmpty Name

That is, a qualified name a non-empty list of (non-dotted) simple name segments.
These correspond to the dot-separated segments of the textual name.

However, note that we propose to allow dotted names *as a kind of simple name*. [#]_
This introduces an ambiguity: a given ``QualName`` could be either:

1. A qualified name where the last segment is the simple name and the rest is the qualifier.
2. A dotted simple name.

.. [#] See `Approaches to dotted names`_ for discussion.

Bindings are for simple names only
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, the ``inScope`` relation relates *qualified* names to
entities, with the following signature: [#]_

.. [#] This signature is taken from [hsmods]_, which like this proposal is mostly concerned
       with modules. In reality the relation should relate *scopes* to bindings, but we are
       only care about module scopes at the moment.

::

   inScope :: Rel Module QualName Entity

We propose that instead it relates *simple* names to entities. We continue to use the ``exports``
relation as it is.

We add a new relation ``refersTo`` which relates qualified names to
entities in a scope. [#]_

.. [#] By analogy to programming languages: the bindings in scope are like an environment,
   simple names are like variables, qualified names are like expressions, and ``refersTo``
   is ``evalExpr``.

::

   inScope :: Rel Module Name Entity
   exports :: Rel Module Name Entity
   refersTo :: Rel Module QualName Entity

``refersTo`` works as follows:

- If the qualified name is a (possibly dotted) simple name, then 
  it refers to whatever that simple name is bound to in the current scope.
- If the qualified name has a qualifier, then we work out what
  the qualifier refers to in the current scope. If it refers to a module,
  then we work out what the simple name refers to in the exported environment of
  that module.

That is, we interpret ``A.b`` by first working out what ``A`` refers to, as a
module, and then looking inside it to find ``b``. ``A`` may itself be
qualified (since modules may export module bindings, just like any other
bindings), hence the need for the recursive lookup.

Imports
^^^^^^^

Normal imports can only appear at the top level of a file. The target of a 
normal import is a *dotted* name, which is resolved using the
current rules for locating external Haskell modules.

Importing a module brings all the exported bindings of the target module
into scope, as described in [hsmods]_. It also brings
the target module into scope bound to its declared simple name (which may be
a dotted name if it is a file module).

If a module is imported ``qualified`` then the exported bindings of the target
module are not brought into scope.

If a module is imported ``as`` a simple name, then the target module is brought
into scope bound to that simple name instead.

Module export specifiers
~~~~~~~~~~~~~~~~~~~~~~~~

The behaviour of the ``module`` export specifiers are changed as follows.

- ``module M`` *changes* to exports the name ``M`` as a module binding.
- ``module M(a, b, .. c)`` exports ``M`` and the listed in-scope bindings.
- ``module M(..)`` exports ``M`` and all of its exported bindings. [#]_

.. [#] See `Do we need additional syntax for re-exporting all bindings from a module?`_ for more discussion.

Backwards compatibility extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Flat module exports
^^^^^^^^^^^^^^^^^^^

The extension ``FlatModuleExports`` is added and enabled by default. It has the effect
that the ``module M`` export specifier has the behaviour given in [hs2010]_. [#]_

.. [#] See `Flat module exports - reasoning`_ for reasoning.

Import module merging
^^^^^^^^^^^^^^^^^^^^^

The extension ``ImportModuleMerging`` is added and enabled by default. It has the effect that
when multiple modules are imported ``as`` the same name, we merge the modules. [#]_

.. [#] See `Import module merging - reasoning`_ for reasoning. 
   multiple modules are defined with the same name?`_ for discussion
   about what the primary behaviour should be.

Privileged local module access
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The extension ``PrivilegedLocalModuleAccess`` is added and enabled by default. It has the effect
that for every file module ``M``, we add a new module declaration with the same name inside ``M``,
which exports all and only the local declarations of ``M``. [#]_

.. [#] See `Privileged local module access - reasoning`_ for reasoning.

Additional features
~~~~~~~~~~~~~~~~~~~

The following extensions add features beyond existing Haskell, building upon the previous changes.

Module imports
^^^^^^^^^^^^^^

We add the language extension ``ImportModule``, which introduces a a new
syntax ``import module`` [#]_ for importing modules in other contexts.
The target of an ``import module`` statement is a *qualified* name, resolved
as normal for qualified names, and will *not* resolve to an external module as
is possible for normal ``import``s. [#]_ . ``import module``
otherwise behaves as ``import``.

.. [#] See `What should the syntax be for importing qualified names?`_ for discussion of the syntax.

.. [#] This ensures that we can always compute the dependency graph of
   the compilation units of a Haskell program by looking at the top-level normal imports alone.

Local modules
^^^^^^^^^^^^^

We add the language extension ``LocalModules``, which is not enabled by default. This allows
module declarations within modules. The syntax for a local module is the same as for a file
module, only indented appropriately for the current scope.

Local modules:

- Can have ``import module``s, but not normal ``import``s.
- Can have nested module declarations.
- Can have export specifiers.

Local module declarations mostly behave the same as file modules:

- A local module declaration adds a binding for its declared name to
  its enclosing scope, as usual for declarations.
- Bindings from the enclosing scope are in scope in the local module,
  but are *shadowed* if there is a local declaration of the same name. [#]_

.. [#] See `Recursion`_ for some subtleties.

Additionally, local modules cannot define typeclass instances or type family
instances. [#]_

.. [#] See `No local typeclass or type family instances - reasoning`_ for reasoning.

Module aliases
^^^^^^^^^^^^^^

We add the language extension ``ModuleAliases``. This allows a
new kind of top-level declaration: ``module <name> = <qualname>``.

A module alias adds a binding in the current scope from the new name
to the module referred to by the right hand side.

Examples
--------

Exporting small utility modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   module A (f, module Unsafe) where

   f = Unsafe.g …

   module Unsafe where
       g = … 

Exporting qualified names
~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal allows exporting modules, but doesn’t say they can be
exported under *qualified* names, which is something that people want to
do. Consider the following `proposed
example <https://github.com/ghc-proposals/ghc-proposals/pull/283#issuecomment-541525245>`_:

::

   module MyPrelude ( qualified module BL
                    , qualified module BS
                    , Set
                    , qualified module Set ) where


   import qualified Data.ByteString.Lazy as BL
   import qualified Data.ByteString as BS
   import Data.Set ( Set )
   import qualified Data.Set as Set

The goal of this is to re-export everything in `Data.ByteString.Lazy` qualified
under `BL`, so for example `BL.ByteString` would be `Bytestring` from `Data.Bytestring.Lazy`.
And similarly for the other modules.

This could be achieved as follows:

::

   module MyPrelude (module BL, module BS, Set, module Set) where

   import qualified Data.ByteString.Lazy as BL
   import qualified Data.ByteString as BS
   import Data.Set ( Set )
   import qualified Data.Set as Set

This example is simple, because we only want a single level of qualification.
For example, suppose that we instead want to re-export `Data.Bytestring.Lazy`
as `Data.Bytestring`. Then we need to write:

::

   module MyPrelude (module Data) where

   import qualified Data.ByteString.Lazy as BL

   module Data (module ByteString) where
       module ByteString = BL

Here, we have to construct a `Data` module with the appropriate inner structure.
This is how we can export "deeply-qualified" names: instead of exporting a "qualified name"
per se, we export a module whose structure allows us to access the names we want to access
with the qualifiers that we want.

Note that this is not *quite* as transparent as we might hope: because we have different syntax for
importing from modules, users must use ``import module`` instead of ``import``.

Effect and interactions
-----------------------

I believe this proposal would solve all the problems described in [ghc283]_ and [ghc205]_.
Moreover, because it makes modules just like other entities, improvements to the module
system will equally benefit management of modules.

Backwards compatibility
~~~~~~~~~~~~~~~~~~~~~~~

No local typeclass or type family instances - reasoning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Typeclass and type family instances currently participate in type inference and instance
selection on a per-compilation-unit basis. This proposal does not seek to change that: the
local modules we propose only provide locality in the renamer.

We could allow instance declarations in local modules, but stipulate that they behave "as if" they were
defined in the top-level file module, even though they are not in scope there. However, not only is this is
confusingly inconsistent, there are competing proposals for "local" instances (see [ghc273]_). We therefore
choose not to allow local instances in this proposal, and instead adopt whatever solution the community
comes to (if it does so).

Flat module exports - reasoning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This straightforwardly restores the current behaviour for module export specifiers.

Privileged local module access - reasoning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The effect of this is to preserve section 5.5.1 of [hs2010]_, which states that the
qualified name of an entity is in scope in its defining module. 

This is *nearly* ensured by the `Modules as entities`_: the current module's name is in scope
inside the module, so we should be able to access the entity qualified. However, this is only
true if that entity is exported, so we have e.g.:

::

   module A () where
   x = 1
   y = A.x -- A is in scope but x is not exported, so this is an error

With ``PrivilegedLocalModuleAccess`` the compiler instead sees the following:

::

  module A () where
    -- This gives a new local declaration of A, so it shadows the outer declaration of A.
    -- The export specifier is well-scoped, since x and y are in scope in the
    -- enclosing scope.
    module A (x, y) where

    x = 1
    y = A.x -- A here refers to the new module, which does indeed export x

Import module merging - reasoning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The effect of this is to preserve the current behaviour, which does not care that
the *qualifiers* of names may clash. 

This proposal takes the position that they *should* clash, and so the merging behaviour is
an "backwards compatibility extension".

This is not the only position: [ghc283]_ (effectively) proposes that multiple modules with
the same name *should* be merged.

I think the argument for merging is:

- We have some existing cases where this happens (``import as``), which we need to support.
- It might be convenient to “mix in” additional names into an existing module.

The argument for ambiguity is:

- Ambiguity is consistent with the way names work for everything else, so makes the system work.
- It is not very inconvenient in practice

  - It is easily to “manually” merge modules if you have local module declarations.

Interactions
~~~~~~~~~~~~

`Modules as entities`_ should have no interaction with any other
features. We do not even need to change interface files, since module
names cannot be exported.

`Module export specifiers`_ has slightly more effects:

- Interface files must be able to handle the possibility that an exported name refers to a
  module. This may have some interaction with Backpack.
- Code which *imports* such modules can always treat it as if it
  exports qualified names. In particular, they do not need to be able to
  *import* exported module names to use them.

`Local modules`_ and `Module aliases`_ should again have few interactions
and has effects only in the module where it is used. Consumers again can
again interact with nested modules only using qualified names.

Costs and drawbacks
~~~~~~~~~~~~~~~~~~~

This proposal requires changing the way name resolution works in
Haskell. This is a critical part of the compiler, and the current
implementation is very battle-tested. Replacing it would be hard work.

However, it *should* be relatively easy to test the new implementation:
all existing modules should have no change to their interfaces.

This proposal would close off some other approaches to improving the
module system.

Alternatives
------------

All or nothing
~~~~~~~~~~~~~~

This proposal suggests that we unconditionally change the way the module system works in a way that *would* change
the current behaviour, and then add various `epicycles <Backwards compatibility extensions>`_ to
retain the current behaviour.

An alternative would be to just have a single extension that turned on the new system, behaviour changes and everything.

The downside of the "all-or-nothing" approach is that we would then have two largely independent implementations
of the module system, one of which would be rarely used (assuming that the extension takes a while to become popular, if ever),
and thus less robust. On the other hand, with this proposal as it stands, the new code path will be the only one
(albeit with some epicycles), so should be thoroughly exercised.

On the other hand, the approach in this proposal suffers from the need to replicate precisely the current behaviour
(with acceptable error messages!), no matter how "weird" it is. A clean break would perhaps give us more latitude to make
changes.

It is unclear to me which approach is the better one, and I would be happy with either. 

Approaches to dotted names
~~~~~~~~~~~~~~~~~~~~~~~~~~

The most awkward part of this proposal is how it treats dotted names. These are not actually *structured* in current Haskell, as
[hs2010]_ says:

    Module names can be thought of as being arranged in a hierarchy in which appending a new component creates a
    child of the original module name. For example, the module ``Control.Monad.ST`` is a child of the ``Control.Monad`` sub-hierarchy.
    This is purely a convention, however, and not part of the language definition; in this report a modid is
    treated as a single identifier occupying a flat namespace.

But they are extremely suggestive of structure. And indeed the final dot in the name *does* have structural behaviour: this dot
indicates a selection from a module. 

So we have two broad alternatives: either try and reinterpret dotted names so as to make them structured, or
keep them as non-structured names that just happen to have dots in them.

Structured dotted names
^^^^^^^^^^^^^^^^^^^^^^^

The obvious thing to do to make dotted names really structured is to *make* modules for all the components of a dotted
name. If we define the module ``Data.Set``, then the module ``Data`` is nowhere defined - but we could define it, and
bind the ``Data.Set`` module to ``Set`` inside ``Data``.

A thoroughgoing version of this approach is possible. For example, Scala where modules *are*
objects, the "packages" that qualify classes are themselves objects as well. 

However, this does not fit well with how dotted names work today. Firstly: if we were really generating new module
definitions for the qualifiers, then they should *clash* with other definitions. Does the ``Control`` from ``Control.Monad``
clash with the ``Control`` from ``Control.Applicative``? What if they were defined in different packages? Today the
answer is no: instead they just merge.

This is a surmountable problem. We could say that such "qualifying modules" do not clash with other bindings, but
instead merge with them. But what if one of the clashing bindings is not for a qualifying module? Suppose I define
an *actual* module called ``Control``? Do we still merge them? This would allow you to "inject" module bindings into
other people's modules, which seems like an odd behaviour to allow. [#]_

.. [#] In Scala it is an error to have a package object with the same name as a class/module. However, this wouldn't fly Haskell world: we have ``Debug.Trace``
   in ``base``, and e.g. the ``ghc`` package has a module called ``Debug``, so GHC would become uncompilable. I think the
   main difference is that packages in the JVM world are usually lowercased, while classes and objects are uppercased; whereas
   in Haskell they are typically all uppercased, which increases the likelihood of clashes.

Another problem is that dotted names can appear in places other than module declarations. Consider ``import A.B`` or ``import Prelude as A.B``.
In this case we might also need to create an ``A`` module and worry about whether it should merge with e.g. a locally
defined ``A``.

Overall, this approach seems initially appealing, but I cannot see a way to fill it out satisfyingly. I would welcome
any ideas for making this work.

Non-structured dotted names
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Keeping dotted names unstructured sidesteps most of the problems in the previous section. However, it
incurs its cost in ambiguity: we can no longer tell syntactically whether a name is a qualified name, or a simple
dotted name. [#]_

.. [#] One solution would be to use a syntax other than the dot for module selection, thus making dotted names
   syntactically distinct (this is the approach taken by Semmle QL, for example, see the Name Resolution section of [qllang]_).
   However, this ship sailed long ago, since dot is already used for module selection in Haskell today.

This costs us: the implementation is less elegant [#]_; it is harder for readers to tell what is going on; and it is generally inconsistent.
However, it does work, and so this is the approach taken by the current proposal.

.. [#] This is why in `Qualified names`_ we cannot represent qualified names with a datatype with two alternatives, since
   we do not know which case we are in until we try to resolve the name.

Local modules (GHC283)
~~~~~~~~~~~~~~~~~~~~~~

The primary alternative route to the new features in this proposal is the
“Local Modules” proposal [ghc283]_. This
proposal aims to support most of the same things, but I will point out a
few differences:

- Export specifiers

  - The export specifiers in [ghc283]_ are quite complex, and there are questions about how they should
    behave for nested modules etc. The export specifiers in this
    proposal are simple, and consistent with the export specifiers for other types of entity: they just allow exporting a
    module name. All additional structure must be added by structuring
    the exported module.

- Module aliases

  - Module aliases barely make sense in the world of [ghc283]_
    since there are no such things as modules.

Conceptually, [ghc283]_ is a bit 
like an “unrolled” version of this proposal, where we try and always
work with the fully explicated list of qualified names. The problems
above arise because we sometimes *want* to talk about modules
themselves, but there are also problems with working with the full set
of qualified names:

- Recursive modules could cause non-termination.

  - If ``A`` exports itself, then arguably it should export an
    infinite set of qualified names: ``A``, ``A.A``, ``A.A.A``\ …
  - [ghc283]_ avoids this by simply banning recursive local modules, but we don't
    have to do this.
  - See `Termination`_ below for why this isn’t a problem for this
    proposal.

Ultimately, I think this proposal is *simpler*, in that it makes
modules more like everything else in the module system, which then
allows us to solve our problems with the namespacing tools we already
have for other bindings.

There is also an earlier proposal “Structured module exports/imports” [ghc205]_
I won't respond to it in detail, but it takes the same approach of continuing to
focus on qualified names rather than giving modules an identity.

Unresolved questions
--------------------

What should the syntax be for importing qualified names?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``import module`` syntax is taken from [ghc283]_. I would prefer something shorter. Some
ideas:

- ``open``, following ML
- ``with``, following Nix

Indeed, possibly we should try harder to just allow ``import``. Having different syntax may just
be an annoyance from a user perspective, as it is in `Exporting qualified names`_.

Do we need additional syntax for re-exporting all bindings from a module?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal suggests using the export specifier ``module M(..)`` to export all the exported 
names in ``M``. By analogy with the export specifier for classes, it should also export ``M``
itself , but it is unclear if that is what we want.

Datalog implementation
----------------------

The text of this proposal is mostly expository. I have written a model
of the rules for the old and the new system in Souffle
[souffle]_, a Datalog variant, which is a
nice format for writing executable logical rules. This can be found at
[hsmods-logic]_.

So far, I have only implemented the simplest versions of the systems, in
particular ignoring qualified imports, import/export specifiers, and dotted names. I
will add qualified imports given more time, but I don’t plan to add
import/export specifiers, since they aren’t very interesting and just
act as filters on the imported/exported bindings.

Since it is a logic programming language, the Datalog rules could easily
be turned into inference rules to give a formal specification of the
scoping relations of Haskell.

The Datalog implementation also provides *an* implementation strategy: we
can directly copy the evaluation model of Datalog. However, this is
unlikely to be what we would do in a real compiler, and a procedural
version is certainly possible.

Recursion
~~~~~~~~~

In both systems the defining relations are naturally recursive. As it happens,
this naturally handles the possibility of recursive modules. It is known that
the Haskell module system could be made recursive via the usual fixpoint computation
(see the end of [hsmods]_), but writing the rules
in Datalog gives us this “for free”.

While supporting recursive modules across compilation units would be
challenging, supporting recursion between *local* modules (see `Local modules`_)
would be much less challenging and might be worth considering
if they are implemented.

There is one subtlety: in order for the fixpoint of the recursive computation to be
well-defined, the function we are iterating must be monotone. In Datalog, this incurs the
restriction that recursion must not go through a negation (as this is an anti-monotonic
operation on relations).

This actually has some design implications! Name shadowing is implemented by saying
that a binding from is visible if:

- It is defined locally, or
- It is *not* defined locally, and it is visible in an enclosing scope.

We therefore could not allow *imported* bindings to shadow bindings from enclosing scopes,
as that would make the recursion non-monotonic. Fortunately, we probably don’t want to do
that.

Termination
~~~~~~~~~~~

Datalog is a terminating language (Souffle allows for some features that
can cause non-termination, but I have not used them). So we are
guaranteed that the systems both terminate, despite the recursion.

This may seem surprising: given recursive modules, could there not be an
infinite number of ``refersTo`` facts? E.g. if ``A`` exports itself then
all of ``A``, ``A.A``, ``A.A.A`` etc. refer to ``A``.

However, what grounds the recursion is that we must provide the
relations that define all the qualified names we are interested in up
front. This is something we can do in reality too: we are only
interested in the qualified names that appear in the program. Or if we
implement this in a procedural way, we can implement ``refersTo`` as a
function that computes references on demand.

References
----------
.. [hsmods] `A formal specification of the Haskell 98 module system <https://web.cecs.pdx.edu/~mpj/pubs/hsmods.pdf>`_
.. [ghc283] `Local Modules <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_
.. [souffle] `Souffle <https://souffle-lang.github.io/>`_
.. [hsmods-logic] `<https://github.com/michaelpj/hsmods-logic>`_
.. [ghc205] `Structured module exports/imports <https://github.com/ghc-proposals/ghc-proposals/pull/205>`_
.. [ghc273] `Add support for local types <https://github.com/ghc-proposals/ghc-proposals/pull/273>`_
.. [hs2010] `Haskell 2010 - Language Report <https://www.haskell.org/onlinereport/haskell2010/haskell.html>`_
.. [qllang] `QL Language Handbook <https://help.semmle.com/QL/ql-handbook>`_
