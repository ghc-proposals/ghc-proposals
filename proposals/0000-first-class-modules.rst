.. author:: Michael Peyton Jones
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
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
to manipulate. From the point of view of the Haskell report, modules barely
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

This proposal consists of a series of changes, many of which could be
adopted separately, but which build upon a central change which is
described in the main section. However, the additions are important for
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

The proposed changes are:

1. `Modules as entities`_
2. `Module export specifiers`_
3. `Local modules`_
4. `Module aliases`_

Terminology
~~~~~~~~~~~

-  A *compilation unit* is the unit of Haskell compilation, currently a
   single ``.hs`` file.
-  A *file module* is a module which is defined at the top level of a
   ``.hs`` file. Currently this corresponds to a compilation unit.
-  A *module* is a named scope, of which file modules are currently the
   only examples.
-  A *simple name* is a name that is given to something when it is
   defined.
-  A *qualified name* is a more complex form of name which can *refer*
   to an entity. We will say more about qualified names later.
-  A *dotted name* is a (simple) module name that contains ``.``\ s, e.g
   ``Data.Set``. We will want to distinguish these from qualified names.
-  An *entity* is something that can be referred to with a name.
   Currently this means types, values, field accessors, etc.
-  A *binding* is a mapping from a name to an entity.

Modules as entities
~~~~~~~~~~~~~~~~~~~

This change makes modules into entities, and allows them to be referred
to. However, the only place that they can be referred to is in the
qualifiers of qualified names, so this changes little.

I believe this would be entirely internal, and would have no effect on
what names resolve to in existing Haskell programs. As such, it could be
implemented without a language extension flag, and is completely
compatible with existing code.

Modules become entities
^^^^^^^^^^^^^^^^^^^^^^^

Modules become entities. All scopes now have a *module namespace*, which
contains the name bindings for modules. [#]_

.. [#] I will ignore namespaces for the rest of this proposal. They don't
       interact with this proposal much, since it is easy to distinguish which
       bindings belong to which namespaces by classifying the *entities* according
       to which namespace they belong to. Then we can have a "unified" namespace and
       simply filter out bindings that refer to the wrong kind of entities. 

Qualified names become more structured
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, qualified names consist of a simple name, possibly qualified
by a module name. That is, they could be represented by a type like:

::

   data QualName = Qualified ModuleName Name | Simple Name

We propose to replace this with a type like:

::

   data QualName = Qualified QualName Name | Simple Name

That is, a qualified name is a simple name with a (possibly empty) list
of qualifying names. These correspond to the dot-separated segments of
the textual name.

Scopes contain simple names only
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, the ``inScope`` relation relates *qualified* names to
entities, with the following signature:

::

   inScope :: Rel Module QualName Entity

We propose that instead it relates *simple* names to entities, and we
add a new relation ``refersTo`` which relates qualified names to
entities in a scope. [#]_

.. [#] By analogy to programming languages: the bindings in scope are like an environment,
   simple names are like variables, qualified names are like expressions, and ``refersTo``
   is ``evalExpr``.

::

   inScope :: Rel Module Name Entity
   refersTo :: Rel Module QualName Entity

``refersTo`` works as follows:

- If the qualified name has no
  qualifiers, then it refers to whatever that simple name maps to in the
  scope.
- If the qualified name has a qualifier, then we work out what
  the qualifying name refers to. If it refers to a module, then we look at
  the bindings exported by the module to find one matching the name.

That is, we interpret ``A.b`` by first working out what ``A`` refers to, as a
module, and then looking inside it to find ``b``. ``A`` may itself be
qualified (since modules may export module bindings, just like any other
bindings), hence the need for the recursive lookup.

Exporting module names
^^^^^^^^^^^^^^^^^^^^^^

Module names *cannot* be exported, since there are no export specifiers that can
refer to them. [#]_ The existing ``module M`` export specifier
continues to work as it is defined in the Haskell report.

.. [#] See `Module export specifiers`_ for the changes to allow this.

Imports
^^^^^^^

Imports at the top level of a file module are treated specially. Their
target is interpreted as a *dotted* name, which is resolved using the
current rules for locating external Haskell modules. [#]_

.. [#] For imports in other settings, see `Local modules`_.

Importing a module brings all the exported bindings of the target module
into scope, as described in [hsmods]_. It also brings
the target module into scope bound to its declared simple name (which may be
a dotted name if it is a file module).

If a module is imported ``qualified`` then the target module is no
brought into scope.

If a module is imported ``as`` a simple name, then the target module is brought
into scope with that simple name instead.

If multiple modules are imported ``as`` the same name, then we retain the current
behaviour, which is to “merge” modules. [#]_ The semantics is roughly that:

.. [#] See `What should happen if multiple modules are defined
       with the same name?`_ for discussion.

::

   import A as M
   import B as M

means the same as

::

   module M where
       import A
       import B

This retains the current behaviour with respect to clashing names, since
Haskell does not issue ambiguity warnings on import, only on usage. Thus
the hypothetical definition of ``M`` is error-free even if ``A`` and
``B`` export ``n``, but an attempt to use ``M.n`` will result in an
ambiguity error. [#]_

.. [#] This explication of the semantics uses a local module definition,
   but we could wire the behaviour in as a special case if we decide
   not to support local modules.

Module export specifiers
~~~~~~~~~~~~~~~~~~~~~~~~

The previous section allows us to refer to modules by name, but not to
export those names.

We propose adding a language extension ``ExportModuleNames`` which
*changes* the behaviour of the ``module M`` export specifier. It now
exports the name ``M`` as a module binding.

This ``ExportModuleNames`` behaviour is the “right” behaviour as far as
this proposal is concerned, but we gate it behind a language extension
for backwards compatibility.

Local modules
~~~~~~~~~~~~~

We propose adding a language extension ``LocalModules``. This allows
module definitions within modules, with the same syntax as in [ghc283]_. Local
modules are very like file modules, in particular:

- They can have imports.

  - The imports can only target modules which can be referred to
    with a qualified name in the current scope. Their target module name is *never*
    interpreted as a dotted name.

- They can have nested module definitions.
- They can have export specifiers.
- They can *not* have dotted names.

The rules for local modules are mostly the same as for file modules,
but:

- A local module definition adds a binding for its declared name to
  its enclosing scope, as usual for definitions.
- Bindings from the
  enclosing scope are in scope in the local module, but are shadowed if
  there is a local definition of the same name. [#]_

.. [#] See the `Recursion`_ section for some subtleties.

Local modules *cannot* import file modules. This ensures that we can
always compute the dependency graph of the compilation units of a
Haskell program by looking at the top-level imports alone. [#]_

.. [#] See `Do we need different syntax for "local" imports?`_ for discussions
   of the consequences.

Module aliases
~~~~~~~~~~~~~~

We propose adding a language extension ``ModuleAliases``. This allows a
new kind of top-level definition: ``module <name> = <qualname>``

A module alias adds a binding in the enclosing scope from the new name
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

This could be achieved as follows:

::

   module MyPrelude (module Data, Set) where

   import qualified Data.ByteString.Lazy as BL
   import qualified Data.ByteString as BS
   import Data.Set ( Set )
   import qualified Data.Set as Set


   module Data where
       module ByteString where
           import BS
           module Lazy where
               import BSL
       module Set where
           import Set

Because qualified names correspond to module-qualification, we can
always create that structure ourselves. This is somewhat laborious for
the author, but does allow the desired experience for the consumer.

This example is somewhat unsatisfactory, since one would really like to
re-export the “module” ``Data.ByteString``, but this is not a module as
far as this proposal is concerned. See `Can we make dotted names less
special?`_ for more discussion.

Effect and interactions
-----------------------

I believe this proposal would solve all the problems described in [ghc283]_ and [ghc205]_.
Moreover, because it makes modules just like other entities, improvements to the module
system will equally benefit management of modules.

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

The primary alternative is the “Local Modules” proposal [ghc283]_. This
proposal aims to support most of the same things, but I will point out a
few differences:

- Export specifiers

  - The export specifiers in [ghc283]_ are quite complex, and there are questions about how they should
    behave for nested modules etc. The export specifiers in this
    proposal are *extremely* simple: they just allow exporting a
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

Ultimately, I think this proposal is just *simpler*, in that it makes
modules more like everything else in the module system, which then
allows us to solve our problems with the namespacing tools we already
have for other bindings.

There is also an earlier proposal “Structured module exports/imports” [ghc205]_
I won't respond to it in detail, but it takes the same approach of continuing to
focus on qualified names rather than giving modules an identity.

Unresolved questions
--------------------

What should happen if multiple modules are defined with the same name?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[ghc283]_ effectively proposes that multiple modules with the same name should be
*merged*. I would like to say that multiple modules with the same name
should be *ambiguous*, just like all other names.

I think the argument for merging is:

- We have some existing cases where this happens (``import as``), which we need to support.
- It might be convenient to “mix in” additional names into an existing module.

The argument for ambiguity is:

- Ambiguity is consistent with the way names work for everything else, so makes the system work.
- It is not very inconvenient in practice

  - Anecdotal evidence: I wrote the Semmle QL module system this way, and I never had anybody ask
    for module merging.
  - It is easily to “manually” merge modules if you have local module definitions.

If we thought ambiguity was more consistent with "modules as entities", we could add a language
extension `AmbiguousAs` which makes such `import as` statements produce ambiguity errors instead.

Do we need any tricky rules for typeclasses/type family instances?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider:

::

   module M where
       instance Ord T where …
       f :: T -> T -> Bool
       f = compare

   g = M.f

That is, we can use ``f`` without importing its defining module, and
hence the typeclass instance is not in scope. Possibly this is fine, but
I’d like someone who knows more about how typeclasses work to think
about this with me!

An alternative would be for typeclass instances to be associated with a
file module rather than any module. So if you import a module which
contains an instance inside a nested module, then that instance is used
during resolution even if it is not strictly in scope. But this would be
somewhat unhygienic.

Can we make dotted names less special?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

File modules declared with dots in them are awkward. The dots *look*
like the qualifier we use in qualified names, but they behaves
differently: there is a module called ``Data.Set``, but there is no
module ``Data`` which contains a module ``Set``.

We could change the system so there *was* such a ``Data`` module. [#]_
However, there is also a scoping
problem. If we want the dotted-name import ``import Data.Set`` to work
as a qualified-name import, then there must be a module ``Data`` *in scope*
at that point. But we probably don’t want ``Data`` to be unconditionally
in scope in the whole module! (Or do we?)

.. [#] This is the case in Scala, for example.

One possibility would be to have a special scope in the file module
header where the “external modules” are in scope. I don’t know whether
this would be nicer overall.

Do we need different syntax for "local" imports?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider:

::

   module A where
       import qualified B
       module C where
          import B.C

From a user perspective, the operations carried out in the two import
statements look very similar. However, there are differences because of the
way we're handling dotted names and file module imports. We *could* signal this
by having a different syntax for the new kind of imports, e.g. ``open`` (to mimic ML).
I don't *think* this is necessary, but I might be wrong, and it might be clearer
anyway.

Datalog implementation
----------------------

The text of this proposal is mostly expository. I have written a model
of the rules for the old and the new system in Souffle
[souffle]_, a Datalog variant, which is a
nice format for writing executable logical rules. This can be found at
[hsmods-logic]_.

So far, I have only implemented the simplest versions of the systems, in
particular ignoring qualified imports and import/export specifiers. I
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

In both systems the defining relations are naturally recursive. This
represents the possibility of recursive modules. Writing them in Datalog
gives us this “for free”, since it has good support for recursion via
fixpoint iteration. [#]_

.. [#] The fact that the Haskell module system could be
   made recursive via a fixpoint computation is observed in
   [hsmods]_, but the Datalog version is much simpler.

While supporting recursive modules across compilation units would be
challenging, supporting recursion between *local* modules (see `Local modules`_)
would be much less challenging and might be worth considering
if they are implemented.

In order for the recursion to be well-defined, we must not recurse
through a negation. This actually has some design implications! *Name
shadowing* is implemented by saying that a binding from an external
scope is visible if it is *not* defined locally. We therefore could not
allow *imported* bindings to shadow enclosing ones, as that would make
the recursion non-monotonic. Fortunately, we probably don’t want to do
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
