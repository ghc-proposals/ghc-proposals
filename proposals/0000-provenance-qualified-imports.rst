.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/115>`_.

.. contents::

Provenance-Qualified Package Imports
==============

This proposal extends the existing ``-XPackageImports`` extension to also specify the *provenance* of packages. It involves:

A) Allowing the ghc packagedb to store *provenance information*
B) Extending the command-line options of ghc to allow *provenance aliases*
C) Extending the syntax of package imports to allow *provenance specifications*
D) Extending the syntax of cabal files to allow both aliases and specifications

Other build-tooling, such as cabal-install and stack, would in turn need to follow suit to take advantage of these features (and, in particular, to make use of the extended syntax of cabal files), but the details of how they might do so are beyond the scope of this proposal. (Though it is in scope to ensure that it provides enough hooks that they can take such advantage).

Motivation
------------
Currently we assume that even though module names may clash between packages, package names themselves are unique. But this presumes a single global package namespace. In fact, it is already somewhat common to have industrial situations with local package stores that are independent from the central upstream store. Furthermore, we have overlays such as http://hackage.mobilehaskell.org/ and https://github.com/hvr/head.hackage. Thus for full disambiguation we need not to allow specification not only of a package, also its provenance.

This is a logical extension of existing principles, and will allow different repositories to exist for many purposes without need for any centralized naming authority.

This proposal incidentally provides a uniform mechanism to specify dependencies on packages that are not hosted in package repositories or overlays but instead, e.g. on github, or elsewhere.

To expand further: currently when one uses an overlay, it means that the packages from the overlay *in addition to* the packages from the main repository are used. So if one specifies an package, and at least one overlay as well as a root repo, and versions of that package exist in the overlay(s) and the root repo both, then a choice of which package to install from which repo is made through some "search-path"-like functionality, though the exact semantics have not been fully pinned down and documented yet for cabal-install. But one use for an overlay may be an internal company repo.

To take a concrete example, suppose`foocorp` has an internal repo, provided within their intranet as an overlay. In that overlay is a package: ``common-network-utils``. Then, sometime later, somebody uploads a totally different ``common-network-utils`` to hackage. Now, it may be that their build resolves to the wrong package! By providing provenance-qualification on package imports, this situation is prevented. One might say, "wouldn't specifying carefully the precedence of overlays and root repos fix this? Well, it would, if it was never the case that both packages (which share nothing but a name!) are never depended on at once. But it may be that some upstream dependency incurs a dependency on ``hackage+common-network-utils``. By extending povenance qualification throughout namespacing, this proposal makes it possible to depend on both the ``foocorp-internal+common-network-utils`` package *and* the ``hackage+common-network-utils`` package, without any worry of overlap. 

Additionally, this proposal allows depdencies on packages hosted outside of a repo or overlay -- such as github, just on the web more generally, or within a filesystem (such as on a shared drive). A use case here would be if I had a package undergoing a major rewrite, and which I did not yet want to release a new version of, but which I wanted to encourage others to take for a test-drive. Rather than asking users to install it directly, or clone and vendor it into their tree, I could now tell them how to use provenance qualification to depend on a particular snapshot of the source repo through altering a few lines in their cabal file.

Proposed Change Specification
-----------------------------

A) *Provenance Information*

A new optional field, `source-origin` is added to the `InstalledPackageInfo <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installed-pkg-info>`_ file and in turn to ghc package databases. Tools which register packages to databases may also specify this field. The intended use of this field is to contain a url  that specifies the origin of the package, typically the root of the package repository from which a package was sourced.

B) *Provenance Aliases*

Since urls can be unwieldy, and it is expected that few will be used, it makes sense to allow specification of shorthand aliases to ghc with a new flag. Hence we add a new flag `--source-origin-alias [ALIAS]=[URL]``  that may be specified multiple times. In the event the same alias is specified twice, an error is thrown.

C) *Provenance Specification*

With ``-XPackageImports`` we can specify e.g.

``import "somepackage" Foo.Bar``

Under this proposal we can also specify

``import "[REPO]+somepackage" Foo.Bar``

Where repo is either the url of a repository root or an alias which maps to one.

D) Cabal syntax

Cabal files are extended in two ways.

``source-origin-alises:`` is a top-level property consisting of a list of aliases in the usual indented assignment style.  These aliases will be passed to all invocations of `ghc` in the course of a cabal build. Example syntax: ::

    source-origin-aliases:
       hackage: https://…
       foobar: http://…

``build-depends:`` fields now can take package names in the form of ``[REPO]+somepackage`` rather than just ``somepackage``.

Further, there is an implicitly defined ``baserepo`` alias that always refers to the base repo set by the user in their ``~/.cabal/config`` (or in the command-line, if the config file is overridden).

Effect and Interactions
-----------------------
I can think of no difficult interactions with existing features. There may need to be a fair amount of mechanical work to remove assumptions about uniqueness of package names throughout different portions of the codebase. I do not believe that there is any interaction with backpack, but it would be important to confirm this.

Costs and Drawbacks
-------------------
Aliases may be considered confusing and/or unnecessary, although I think they will be a boon to usability.

More importantly, there is a concern that widespread use of provenance information could lead to fragmentation of the ecosystem, brittle dependencies, etc. I think this is largely resolved by two choices that this proposal recommends making.

1) ``cabal sdist`` should emit a warning on use of the source-origin field.
2) the central hackage server should prevent upload of packages making use of ``source-origin`` in their ``.cabal`` file.

Alternatives
------------
The only current choice when package names overlap is to fork and rename one of the overlapping packages, which is unfortunate.

Possible areas of disagreement/future work
--------------------
1) The syntax chosen to separate provenance information from a package name is the ``+`` character. This was chosen because ``/`` and ``:`` are present in too many urls (and the latter may be used for distinguishing subpackages as well, in the future). There may be other views as to the best syntax here. One other suggestion would be a literal space (`` ``) as this is not legal in either urls or package names.

2) The question arises: should there be some global location for alias maps? At this point, I think we should not have this. In particular, the aim of the proposal is to allow genuine federation, without the need of any central authority. By introducing some global location by which names are assigned (outside of the mechanisms for assigning names that are common to the internet as a whole), we find ourselves with another version of the same problem. In the future, if we find it is too awkward for packages to each assign aliases, this can be revisited. (Also note -- it is expected that the use of these aliases is not going to be common, to begin with, and will generally not be for _central_ packages, but rather, for the most part, for proprietary code, and individual applications).

3) The potential of different choices for what is placed into the `source-origin` field and their meaning to tools like ``cabal-install`` is left open at this point. This will need to be worked out in the future to allow _fetching_ from such specifications (i.e. when present in `cabal` files). A suggested heuristic would be as follows (in order of precedence): If the url is of a directory that contains a ``root.json`` it is assumed to be a package repository as defined in ``hackage-security``. If the url is of a ``.tar.gz`` file, it is a direct specification of a package tarball. If it terminates in ``.git``, it is assumed to be a git repo of a single package. (In this latter case, this leaves unresolved the question of a syntax for branches or tags, which would need to be determined). Because this proposal involves modifying the ``Cabal`` library, but not yet the ``cabal-install`` tool, we can afford to leave this not fully worked out for the time being.

Implementation Plan
-------------------
As discussed, this will require downstream tooling support to succeed. Other than that, it seems relatively straightforward to implement.
