Splitting out stable interfaces from ``template-haskell``
========================================================

.. author:: Teo Camarasu
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/696>`_.
.. sectnum::
.. contents::


The ``template-haskell`` library exposes the user facing interfaces to Template Haskell (TH), GHC's metaprogramming facility.

Since template-haskell re-exports a diverse set of symbols from GHC's internals,
in practice every GHC release comes with a new major version of the template-haskell package.
Since template-haskell is used very widely in the ecosystem,
a large number of packages have to raise their upper bounds on template-haskell in response to a GHC release.
This is a large amount of work that is needlessly coupled to the release of GHC.

This proposal aims to avoid this cycle of breaking changes to the ecosystem.
We present both an open-ended strategy, and a concrete step in line with that strategy.

Our strategy is to split out smaller, coherent packages with more stable interfaces from ``template-haskell``.
Each of these packages can be versioned independently and can more easily be made compatible with a wider range of GHC versions.
We do not propose to remove from ``template-haskell`` these parts of the interface, rather a version of ``template-haskell`` will re-export the interfaces from the more refined packages.

Concretely, we propose introducing the following two libraries:

* ``template-haskell-lift``, exposes the ``Lift`` typeclass and compatibility functions. Users who make use of the ``DerivingLift`` language extension only need to depend on this package in order to derive instances of ``Lift`` or manually give instances using ``TemplateHaskellQuotes``.
* ``template-haskell-quasiquoter`` exposes the ``Quasiquoter`` datatype, which allows libraries to expose their own custom quasiquoters.

Motivation
----------
This proposal aims to reduce the maintenance burden for packages that depend on ``template-haskell``.
First, we sketch the relationship between GHC's internal AST and ``template-haskell``, and how this leads to frequent breaking changes.
Second, we show how our general strategy improves this situation.
Third, we show the benefits of publishing packages for the specific parts of the interface we describe.

.. _why TH unstable:
Why ``template-haskell`` is unstable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Template Haskell exposes an interface for manipulating Haskell syntax trees.
At the core of this interface is a set of types for representing these syntax trees: ``Type``, ``Expr``, ``Pat``, etc.
These are defined in the ``ghc-internal`` package, but exposed to end-users via the ``template-haskell`` library.
In order to implement Template Haskell, GHC includes functions for converting between its internal AST and these types.
This introduces a form of coupling between GHC internals and the interface of the ``template-haskell`` library.

When using Template Haskell quotes, we must convert an arbitrary GHC AST into a Template Haskell syntax tree.
And when using Template Haskell splices, we must convert an arbitrary Template Haskell syntax tree into a GHC AST.
This puts pressure on the Template Haskell syntax trees to be able to express the full breadth and depth of Haskell syntax.

Whenever a new syntactic construct is added to GHC, we also want to introduce a corresponding change to the Template Haskell syntax tree types.
As we expect GHC's internal AST to regularly evolve with each major version of GHC, it is likely that each new major release of GHC will force a new major release of the ``template-haskell`` library.

In ``template-haskell-2.18``, a new field was added to the ``ConP`` constructor of ``Pat`` to express the possibility of a list of type applications as part of a constructor pattern.
End-users then had to update their code to account for this change. ``yesod`` uses ``ConP`` in some code for generating typeclass instances.
The code had to be changed to pass an extra ``[]`` argument. See: `the PR to yesod <https://github.com/yesodweb/yesod/pull/1754/files#diff-b0e5dbc5d4ca2998772f987cc5f27c5fc761b34549bdecc93892bbe142d89d26R30>`_.

When upgrading GHC, users are often also forced to upgrade to the new GHC bundled ``template-haskell`` library.

.. _why strategy:
Justifying our strategy
^^^^^^^^^^^^^^^^^^^^^^^
Our strategy is informed by the classes of usages of ``template-haskell`` found in the ecosystem. We can divide users as follows:

* (\A) Quote-and-splice clients: These users use only splices, quotes, ``DeriveLift`` or quasiquotes. These users might not even need to import the ``template-haskell`` library.
* (\B) Syntax-construction clients. These users construct Template Haskell syntax trees either directly through its constructors, or indirectly through the smart-constructors exported by ``Language.Haskell.TH.Lib``.
* (\C) Reification clients. These users, notably various forms of deriving, use reification to interrogate the program. Reification currently returns Template Haskell ASTs.
* (\D) Syntax-analysis clients. Some clients pattern match on Template Haskell syntax tree datatypes.

These diverse usages of the library lead to diverse levels of breakage when a new major version of ``template-haskell`` comes out. We can rank them from (A) with the least breakage to (D) with the most.
For instance, the ``uuid`` library, which just depends on ``template-haskell`` in order to provide a derived ``Lift`` instance (a type (A) client), in all likelyhood would only need to bump its upper-bound on the library.
On the other hand ``th-desugar``, which pattern matches on the entire syntax tree (a type (D) client), would have to make code changes on most releases of the library.

Type (A) users are already using interfaces which are quite stable. Yet, they have to update their upper bounds whenever they want to be compatible with a new major version of GHC.
The first concrete step in our strategy is to publish package that provide these stable APIs. We will return to the benefits of this in the next section.

(B-D) do not currently use stable subsets of the ``template-haskell`` interface.
In the future, we aim to continue this strategy, by identifying stable interfaces for these classes of users which aren't tightly coupled to the Template Haskell AST.
The smart-constructors from the ``Language.Haskell.TH.Lib`` module are a good starting point for type (B) clients. Another idea is to use smart-constructors based on the Haskell2010 AST (See: `GHC#20828 <https://gitlab.haskell.org/ghc/ghc/-/issues/20828>`_).
For type (C) clients, we can build on the existing `th-abstraction` library, and perhaps expose a refined AST that doesn't need to be as expressive as the surface language.
Type (D) clients on the other hand are likely to be difficult to accommodate, since they are inherently tightly coupled to the Template Haskell syntax trees.

Our strategy of splitting out stable subsets of the API has the advantage that it allows users to opt-in to more stability.
``template-haskell`` is used very widely in the ecosystem. This makes it important that any attempt to improve its stability doesn't force a change to all users.
Users who wish to continue to use ``template-haskell`` may continue to do so, and the interfaces will continue to be exposed in both the new packages and the old.

By focusing on smaller subsets of the API we also make it much easier to be compatible with multiple versions of GHC.
This is an important property for any stable package as it allows a user to upgrade their dependencies independently of GHC.
We plan to implement this by create compatibility shims using ``CPP`` or ``PatternSynonyms``.

.. _advantages:
Benefits of splitting out ``template-haskell-lift`` and ``template-haskell-quasiquoter``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Publishing ``template-haskell-lift`` and ``template-haskell-quasiquoter`` will be beneficial both for GHC and the ecosystem.

The biggest benefit is that library authors who are just deriving or using ``Lift`` instances or just exposing ``Quasiquoter``\s no longer need to depend on the entirety of ``template-haskell``.
This can help avoid the sorts of dependency bounds propagation problems identified in the `GHC.X.Hackage proposal <https://github.com/bgamari/tech-proposals/blob/ghc-x-hackage/proposals/001-ghc-x-hackage.md>`_.

.. _independence:

Upgrading libraries independently of GHC
''''''''''''''''''''''''''''''''''''''''
When a new major version of GHC is released, the Haskell ecosystem has to respond to a variety of breaking changes.
This potentially includes changes to the compiler itself, but also changes to the libraries that are bundled with GHC.
A new major version of the compiler often ships new major versions of bundled libraries.

In turn, when maintainers release new versions of their packages to deal with the changes from the new version of GHC, they may choose to cut a new major version.
Their dependencies then have to respond to these changes.
This leads to a situation where the ecosystem accommodates the new changes in waves. It can take a long time for changes to fully apply to the entire ecosystem.

It is helpful for maintainers of packages in the ecosystem to be able to deal with new major versions of boot libraries independently of GHC upgrades.
Ideally the ecosystem would already be compatible with a new version of a boot library before it is bundled with a new version of GHC.
It also make upgrades safer for maintainers, since if a bug is introduced, then they can pinpoint it either to a change in the compiler or in a library.

Currently each version of ``template-haskell`` is tightly coupled to a specific version of GHC.
For instance, GHC-9.12.1 ships with ``template-haskell-2.23``. It is not possible to compile ``template-haskell-2.23`` with an earlier version of a compiler.
So, a maintainer cannot upgrade to ``template-haskell-2.23`` without upgrading to GHC-9.12. And vice versa.

Historically, there was a strong technical reason for this. ``template-haskell`` used to include wired-in identifiers referred to by GHC.
As of GHC-9.12, these have been `moved <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12479>`_ to ``ghc-internal``.

It should be possible to use, for instance ``CPP``, to make ``template-haskell`` compatible with multiple versions of GHC. But the large interface exposed by this package makes it difficult.

On the other hand, the small interfaces exposed by ``template-haskell-lift`` and ``template-haskell-quasiquoter`` are easier to make compatible with multiple versions of GHC.
They rarely change and if they don't change between two versions of GHC, then we can accommodate both versions for free.
If they do change, then it's likely that we can use ``CPP`` to expose a shim over GHC internals giving a consistent interface.

For instance, `Overloaded Quotations proposal <./0246-overloaded-bracket.rst>`_ changed the type of the ``lift`` method of ``Lift`` from ``lift :: a -> Q a`` to ``lift :: Qoute m => a -> m a``.
``Quote`` is a new typeclass, which only exposes a ``newName :: Quote m => String -> m a`` from ``Q``.

Suppose ``template-haskell-lift`` existed at the time and ``template-haskell-lift-0.1`` corresponded to the old interface and ``template-haskell-lift-0.2`` corresponded to the new interface.
Further suppose that GHC-9.0 ships with ``template-haskell-lift-0.1`` and GHC-9.2 ships with and implements the interface of ``template-haskell-lift-0.2``.

Our argument in this section is that it is convenient to make the following possible:

* ``template-haskell-lift-0.1`` can be compiled with GHC-9.2
* ``template-haskell-lift-0.2`` can be compiled with GHC-9.0

This allows an end-user to upgrade from GHC-9.0 to GHC-9.2 without having to change their version of ``template-haskell-lift``, and allows a package to support both versions of the compiler without introducing ``CPP``.
And it allows a user to upgrade from ``template-haskell-lift-0.1`` to ``template-haskell-lift-0.2`` without upgrading their compiler.

In this case, it would not have been possible to support both of these directions.
While we could have compiled ``template-haskell-lift-0.2`` with older versions of the compiler by exporting ``type Quote m = (m ~ Q)``, we could not do a similar step in the other direction.
A user could have used another method of ``Q`` such as ``runIO`` when giving an instance of the older version of ``Lift``, which is not available for a ``Quote`` monad, and thus unacceptable in the newer version of ``Lift``.

Yet, only having one of these directions is already helpful. ``template-haskell-lift-0.2`` support both GHC-9.0 and GHC-9.2 allows a user of the library to support both versions of the compiler without relying on ``CPP`` to paper over changes in the ``Lift`` interface.

In this section, we have argued that it is helpful to be able to upgrade boot libraries such as ``template-haskell`` independently of GHC.
This allows reducing the time taken for a boot library major version bump to spread through the ecosystem, and it allows end-users to support a broad range of GHC versions without having to rely on ``CPP``.
This is much more achievable for the smaller ``template-haskell-lift`` and ``template-haskell-quasiquoter`` libraries than it would be for ``template-haskell``.
But as we have seen, total independence isn't always possible even for these quite small interfaces. Sometimes changes to tightly coupled definitions are difficult to make both backwards and forwards compatible, but we still benefit from just one direction.


Depending on boot libraries from ``template-haskell``
''''''''''''''''''''''''''''''''''''''''
There is a more subtle benefit for the ``template-haskell`` package. Currently the wide usage of ``Lift`` instances greatly limits the possible dependencies of ``template-haskell``.
For instance, ``template-haskell`` cannot depend on ``containers`` or ``filepath``, since these libraries depend on ``template-haskell``.
But if these packages switch to depending on our new packages, then ``template-haskell`` could depend on them.
Currently ``template-haskell`` must vendor a small portion of ``filepath`` and ``containers``, and that would no longer be necessary.

Many boot packages depend on ``template-haskell``, but all of them only depend on it for the parts of the interface exposed by ``template-haskell-lift`` and ``template-haskell-quasiquoter``.
If we can convince their maintainers to depend on these packages instead, then GHC would no longer (transitively) depend on ``template-haskell``.
This makes it possible for packages to depend on the ``ghc`` library at the same time as a version of ``template-haskell`` different to the one bundled with that GHC.


Proposed Change Specification
-----------------------------
No changes to the language or the compiler are required for this proposal.

Proposed Library Change Specification
-------------------------------------

We propose to publish two new libraries: ``template-haskell-lift`` and ``template-haskell-quasiquoter``.
These will be shipped with GHC. So, they would be boot libraries, but wouldn't include any wired-in identifiers.
In other words, they would behave as ``bytestring`` or ``containers``, not like ``ghc-internal``.

They will also be published to and buildable from Hackage.
They can be built with the version of GHC they are bundled with, but should additionally be buildable with the previous and next version of GHC.
Concretely if ``template-haskell-0.1`` is shipped with GHC-9.14, then there should be minor releases (``template-haskell-0.1.*``) that can be built with GHC-9.12 and GHC-9.16.
We wish to have as broad a support range as feasible, eg, the current version of the libraries are compatible with GHC-8.10 up to GHC-9.12 (the present release).
We acknowledge that this might not always be possible, and that these interfaces might need to change in the future in ways that cannot be shimmed over.

These packages only depend on ``ghc-internal`` and ``base``. Crucially they do not depend on ``template-haskell``.

Their initial interfaces will be as follows:

``template-haskell-lift``::

   module Language.Haskell.TH.Lift
    ( Q
    , Code
    , Quote
    , Exp
    , Lift(..)
    , defaultLiftTyped -- a utility for writing `liftTyped` methods when an instance currently defines lift only
    , liftAddrCompat -- a utility for creating an `Addr#` value, eg, for defining `Lift ByteString`
    , liftIntCompat -- a utility for lifting an `Int` without causing issues when used with `OverloadedSyntax`
    )

``template-haskell-quasiquoter``::

   module Language.Haskell.TH.QuasiQuoter
    ( Q
    , Exp
    , Pat
    , Type
    , Dec
    , QuasiQuoter (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec)
    )

Effect and Interactions
-----------------------
This works towards removing the special case for ``template-haskell`` in `(GR1) <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#33stability-gr1>`_ from `Principles for GHC <../principles.rst>`_, but on its own it doesn't achieve it.
There should be no interactions with other proposals.


Costs and Drawbacks
-------------------
This proposal requires the GHC team to maintain two packages for the conceivable future.
This should be a relatively small cost as we expect these packages to be relatively stable.
Teo Camarasu is happy to take on any maintenance work necessary for these packages for the foreseeable future,
but someone else would have to take over if they are no longer able to.


Backward Compatibility
----------------------
As this proposal deals exclusively with creating new packages, there are no backwards compatibility worries.


Alternatives
------------

Remove the Template Haskell ASTs from ``template-haskell``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The majority of the breaking changes to ``template-haskell`` comes from changes to the TH AST.
An alternative approach would be to simplify move the TH AST into a new package, and keep ``template-haskell``
as the remaining interface.

The main issue with this alternative is that it would force a change on basically all users but type (A).
This would be a large and wide ranging breaking change.

Another issue is that the ``Lift`` interface has changed much more frequently in the past than the ``Quasiquoter`` interface.
If either of these changed in the future, then every user would have to update their upper bounds.
Whereas with the split packages, you only need to update your bounds if the interface you actually depend on has changed.

Just one stable package
^^^^^^^^^^^^^^^^^^^^^^^
This proposal splits out two interfaces from ``template-haskell`` into two packages.
An alternative would be to split them into one new package.

Fundamentally, these two interfaces are conceptually and practically independent.
They have evolved independently of each other in the past, and they are likely to continue to do so.

The majority of the users of ``Lift`` do not depend on ``QuasiQuoter``, and they would suffer from unnecessary version bumps if the two interfaces were packaged together.
The `PVP <https://pvp.haskell.org/>`_ dictates that if any interface in a package changes in a breaking way, then the entire package needs to bump its major version.

By keeping them apart, users can benefit from their independently versioned.
A user could pick and choose which versions they depend on from each package.
This allows us to minimise the breakage from backwards- and/or forwards-compatible changes.

We also be sceptical of a ``template-haskell-stable`` package because stability is not an essential property of an interface.
We can look back on the *past* stability of these interfaces, but we cannot know their *future* stability.
Part of the motivation of this proposal is to make it easier to accommodate future changes to these interfaces.
Our aim here isn't to split these interfaces out in order to fix them in stone, but to make it easier for end-users to cope with future changes,
and to eliminate unnecessary work when the subset of the interface they depend on hasn't actually changed.

Rather we should be looking at how interfaces are related. We should look at relations of tight coupling.
If two interfaces are tightly coupled, it makes sense to group them into one package as changes to one will force changes to the other.

These two interfaces are related in being parts of the overall Template Haskell feature set, but are otherwise conceptually independent.
They could evolve independently. We could imagine the interface of ``Lift`` changing without impacting ``QuasiQuoter`` and vice versa.

In light of the complexities surrounding ``Lift`` in the `Explicit Level Imports <./0682-explicit-level-imports.rts>` proposal,
having a distinct ``template-haskell-lift`` package also helps document that a package is depending on this interface.

Including ``DeriveLift`` as part of the interface of ``template-haskell-lift``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``Lift`` interface is often used in conjunction with the ``DeriveLift`` language extension.
This allows users to automatically derive ``Lift`` instances for many but not all datatypes, eg, ``GADT``\s are currently not supported.

A library maintainer using this interface and extension understandably wants to give their users strong guarantees that a new release of GHC won't break their library.
Currently they can enforce this by placing strict upper bounds on the versions of the ``base`` and ``template-haskell`` packages. These act as proxies for supported GHC versions.
Each new version of GHC in practice comes with a new major version of both of these packages.
Therefore, a strict upper-bound guarantees that end-users don't accidentally build the library against a version of GHC untested by the maintainer.
When the maintainer is satisfied that the new version of GHC doesn't break the library, they can release a Hackage revision with updated bounds.

The direction of travel in this proposal and efforts to make both ``base`` and ``template-haskell`` decoupled from GHC versions, make this strategy more difficult to implement.
If a maintainer replaces a dependency on ``template-haskell`` by one on ``template-haskell-lift`` and ``base`` becomes reinstallable,
then they can no longer depend on a new version of GHC forcing an upgrade of one of these packages.
So, their library might be built by an end-user with a version of GHC the maintainer hasn't yet tested.

We could argue that the maintainer doesn't have a reason to be worried since GHC comes with strong `stability principles <../principles.rst>`_. Yet, these also have exceptions.

Consider the following scenario:

1. GHC-9.14 adds support to ``DeriveLift`` for ``GADT``\s.
2. The maintainer adds ``DeriveLift`` instances for their ``GADT``\s.
3. The GHC maintainers realize that there is a fundamental issue with this new feature and that it must be reverted in GHC-9.16.
4. Now the maintainers code is broken by the release of GHC-9.16

A way to defend against this would be to count changes to ``DeriveLift`` as part of the interface ``template-haskell-lift``.
Then whenever the class of datatypes supported by ``DeriveLift`` changed, we would make a new major version of ``template-haskell-lift``.

While we hold these to be valid concerns, we do not think expanding the interface of ``template-haskell-lift`` is the way to resolve them.
This suggestion goes against our aim of decoupling ``template-haskell-lift`` from GHC.
It is possible to shim over changes to the ``Lift`` interface in ``ghc-internal`` as we've outlined in `section 1.3.1 <#131upgrading-libraries-independently-of-ghc>`_.
But it is not possible to shim over features implemented in the compiler.
If the implementation of ``DeriveLift`` changed then we would be forced to have a sequence of versions of ``template-haskell-lift`` without either backwards- or forwards-compatibility.
This would greatly limit the benefits of this proposal.

We feel that it is reasonable that a library maintainer should be able to explicitly define which versions of GHC they support, but that this can be done orthogonally to this proposal.
In fact, tackling this independently might lead to a more general solution.
For instance, we could use ``ghc-experimental`` or create a new ``ghc-version`` package, which exports nothing but carries the version of GHC. We would have to allow it to be exempted from unused package warnings, but this seems doable.
We could also explore a versioning scheme for language extensions and allow users to express bounds on them in ``.cabal`` files.

Unresolved Questions
--------------------

Implementation Plan
-------------------
Teo Camarasu has implemented an `MR <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13569>`_
and is happy to take on the work of finishing it and submitting patches to boot libraries.

Endorsements
-------------
