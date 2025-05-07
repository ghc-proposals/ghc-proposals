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
It is tightly coupled to GHC's internals.
Each release of GHC ships with a new major version of ``template-haskell``, which is the only version that is supported by that compiler.

``template-haskell`` is used very widely in the ecosystem, including by many dependencies of the compiler itself.
When a new version of GHC is released, a large amount of packages have to raise their upper bounds on ``template-haskell``.
This is a large amount of work that is needlessly coupled to the release of GHC.

This proposal aims to avoid this cycle of breaking changes to the ecosystem.
We present both an open-ended strategy, and a concrete step in line with that strategy.

Our strategy is to split out smaller, coherent packages with more stable interfaces from ``template-haskell``.
Each of these packages can be versioned independently and can more easily be made compatible with a wider range of GHC versions.
We do not propose to remove from ``template-haskell`` these parts of the interface, rather a version of ``template-haskell`` will re-export the interfaces from the more refined packages.

Concretely, we propose introducing the following two libraries:

* ``template-haskell-lift``, exposes the ``Lift`` typeclass and compatibility functions. Users who make use of the ``DerivingLift`` language extension only need to depend on this package in order to derive instances of ``Lift`` or manually give instances using ``TemplateHaskellQuotes``.
* ``template-haskell-quasiquote`` exposes the ``Quasiquoter`` datatype, which allows libraries to expose their own custom quasiquoters.

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

.. Note::
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
Benefits of splitting out ``template-haskell-lift`` and ``template-haskell-quasiquote``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Publishing ``template-haskell-lift`` and ``template-haskell-quasiquote`` will be beneficial both for GHC and the ecosystem.

The biggest benefit is that library authors who are just deriving or using ``Lift`` instances or just exposing ``Quasiquoter``\s no longer need to depend on the entirety of ``template-haskell``.
This can help avoid the sorts of dependency bounds propagation problems identified in the `GHC.X.Hackage proposal <https://github.com/bgamari/tech-proposals/blob/ghc-x-hackage/proposals/001-ghc-x-hackage.md>`_.

There is a more subtle benefit for the ``template-haskell`` package. Currently the wide usage of ``Lift`` instances greatly limits the possible dependencies of ``template-haskell``.
For instance, ``template-haskell`` cannot depend on ``containers`` or ``filepath``, since these libraries depend on ``template-haskell``.
But if these packages switch to depending on our new packages, then ``template-haskell`` could depend on them.
Currently ``template-haskell`` must vendor a small portion of ``filepath`` and ``containers``, and that would no longer be necessary.

Many boot packages depend on ``template-haskell``, but all of them only depend on it for the parts of the interface exposed by ``template-haskell-lift`` and ``template-haskell-quasiquote``.
If we can convince their maintainers to depend on these packages instead, then GHC would no longer (transitively) depend on ``template-haskell``.
This makes it possible for packages to depend on the ``ghc`` library at the same time as a version of ``template-haskell`` different to the one bundled with that GHC.


Proposed Change Specification
-----------------------------
No changes to the language or the compiler are required for this proposal.

Proposed Library Change Specification
-------------------------------------

We propose to publish two new libraries: ``template-haskell-lift`` and ``template-haskell-quasiquote``.
These will be shipped with GHC.
They will also be buildable from Hackage.
They will be buildable with at a *minimum* the last 3 versions of GHC.
The current version of the libraries are compatible with GHC 8.10 and later.

Their interfaces will be as follows:

``template-haskell-lift``::

   module TemplateHaskell.Lift
    ( Q
    , Code
    , Quote
    , Exp
    , Lift(..)
    , defaultLiftTyped -- a utility for writing `liftTyped` methods when an instance currently defines lift only
    , liftAddrCompat -- a utility for creating an `Addr#` value, eg, for defining `Lift ByteString`
    , liftIntCompat -- a utility for lifting an `Int` without causing issues when used with `OverloadedSyntax`
    )

``template-haskell-quasiquote``::

   module TemplateHaskell.Quasiquoter
    ( Q
    , Exp
    , Pat
    , Type
    , Dec
    , QuasiQuoter (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec)
    )

Note that these modules are in the ``TemplateHaskell.`` namespace rather than the ``Language.Haskell.TH.`` namespace.
The idea to use this less verbose namespace for the new stable interfaces is thanks to Adam Gundry.

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
The majority of the breaking changes to ``template-haskell`` comes from changes to the TH AST.
An alternative approach would be to simplify move the TH AST into a new package, and keep ``template-haskell``
as the remaining interface.

The main issue with this alternative is that it would force a change on basically all users but type (A).
This would be a large and wide ranging breaking change.

Another issue is that the ``Lift`` interface has changed much more frequently in the past than the ``Quasiquoter`` interface.
If either of these changed in the future, then every user would have to update their upper bounds. ]
Whereas with the split packages, you only need to update your bounds if the interface you actually depend on has changed.


Unresolved Questions
--------------------

- Should the modules live in the ```TemplateHaskell.`` or the ``Language.Haskell.TH.`` namespace?
- Should these packages live in the GHC repository, in another repository on the GHC Gitlab, or on GitHub?

Implementation Plan
-------------------
Teo Camarasu has implemented an `MR <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13569>`_
and is happy to take on the work of finishing it and submitting patches to boot libraries.

Endorsements
-------------
