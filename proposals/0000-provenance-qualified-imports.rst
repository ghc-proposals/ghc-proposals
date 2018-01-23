.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Provenance-Qualified Package Imports
==============

This proposal extends the existing ``-XPackageImports`` extension to also specify the *provenance* of packages. It involves:

A) Allowing the ghc packagedb to store *provenance information*
B) Extending the command-line options of ghc to allow *provenance aliases*
C) Extending the syntax of package imports to allow *provenance specifications*

Other build-tooling, such as cabal-install and stack, would in turn need to follow suit to take advantage of these features, but the details of how they might do so are beyond the scope of this proposal. (Though it is in scope to ensure that it provides enough hooks that they can take such advantage).

Motivation
------------
Currently we assume that even though module names may clash between packages, package names themselves are unique. But this presumes a single global package namespace. In fact, it is already somewhat common to have industrial situations with local package stores that are independent from the central upstream store. Furthermore, we have overlays such as http://hackage.mobilehaskell.org/ and https://github.com/hvr/head.hackage. Thus for full disambiguation we need not to allow specification not only of a package, also its provenance.

This is a logical extension of existing principles, and will allow different repositories to exist for many purposes without need for any centralized naming authority.

Proposed Change Specification
-----------------------------

A) *Provenance Information*

A new optional field, `source-origin` is added to the `InstalledPackageInfo <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installed-pkg-info>`_ file and in turn to ghc package databases. Tools which register packages to databases may also specify this field. The intended use of this field is to contain a url  that specifies the root of the package repository from which a package was sourced.

B) *Provenance Aliases*

Since urls can be unwieldy, and it is expected that few will be used, it makes sense to allow specification of shorthand aliases to ghc with a new flag. Hence we add a new flag `--source-origin-alias [ALIAS]=[URL]``  that may be specified multiple times. In the event the same alias is specified twice, an error is thrown.

C) *Provenance Specification*

With ``-XPackageImports`` we can specify e.g.

``import "somepackage" Foo.Bar``

Under this proposal we can also specify

``import "[REPO]:somepackage" Foo.Bar``

Where repo is either the url of a repository root or an alias which maps to one.

Effect and Interactions
-----------------------
I can think of no difficult interactions with existing features. There may need to be a fair amount of mechanical work to remove assumptions about uniqueness of package names throughout different portions of the codebase.

Costs and Drawbacks
-------------------
Aliases may be considered confusing and/or unnecessary, although I think they will be a boon to usability.

Alternatives
------------
The only current choice when package names overlap is to fork and rename one of the overlapping packages, which is unfortunate.

Unresolved questions
--------------------
This proposal extends an existing flag rather than specifying a new ``-XProvenanceImports`` flag. In my opinion since this does not conflict with existing syntax or features, there is no need for a new flag. Rather, it just makes package imports better and more powerful. Others may disagree.

Implementation Plan
-------------------
As discussed, this will require downstream tooling support to succeed. Other than that, it seems relatively straightforward to implement.
