.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Componentized Cabal
===================

This is a proposal for adding per-component configure to the Cabal Setup
script API.  A prototype implementation is available at
`#3644 <https://github.com/haskell/cabal/pull/3644>`_.

Motivation
----------

A package (e.g., ``bytestring``) consists of multiple components (e.g., a
library and its test suite). There are several motivations behind having
multiple components in a package:

* It is convenient to not have to write a second package just to hold
  the test suite for a library. Part of this is historically motivated,
  as in the old days it was far more difficult to do development on a
  collection of packages than it was on a single package. But there is
  large amount of metadata, e.g., authorship, versioning, the package
  description, which would be needlessly duplicated if you needed to
  publish a separate package.

* The package serves as an abstraction barrier: components internal to a
  package have access to APIs that package authors don't want to expose
  to the wider world. One common "trick" which is used to achieve this
  (e.g. as seen in cabal-install), is that a test library will literally
  recompile the library modules it depends on to get access to private
  APIs that are not otherwise available. Similarly, test suites and
  benchmarks are not externally useful, except as support code for a
  library; it makes little sense to put them into separate packages.

* A test suite will usually have a different set of dependencies than
  the library proper. Assuming that you want both the library and the
  test suite in the same package, you need a way to specify distinct
  sets of dependencies for each of them, and the component is the
  logical distinction between the two.

Unfortunately, components were not designed into Cabal at the beginning,
and thus Cabal's support for components was not so much designed as
evolved. But there is a basic guiding principle that components are an
**internal implementation detail** of a package: the ``Setup`` interface
configures and builds a package as a whole, and internally this may mean
that several components get built. Over the years we have accumulated
options which violate this principle (``--enable-tests``, explicit
arguments to ``build``, ``ComponentDeps`` in cabal-install) but the core
architectural principle still holds.

In this document, I propose a **component** oriented design for Cabal,
where every essential operation is done on a per-component basis.
I am not the first to have done so: @snoyberg has previously
`asked for this very feature <https://github.com/haskell/cabal/issues/2802>`_.
The purpose of this proposal is to fully flesh out
the implications of the change.

This design has multiple benefits:

1. It permits building only specific components for packages, without
   needing all of the dependencies of the package as a whole to be
   built.  (`#1725 <https://github.com/haskell/cabal/issues/1725>`_)

2. It makes it possible for package managers like cabal-install and
   Stack to organize builds on a per-component basis, improving
   parallelism (e.g., when a package contains many independent
   executables) and making it easier to avoid unnecessary rebuilds when
   a new component is built. (`#2775 <https://github.com/haskell/cabal/issues/2775>`_)

3. It solves the cyclic dependency problem where a test suite depends on
   a testing library that depends on the the library being tested. Now
   the test suite can be built independently from the library, after the
   testing library has been built.

4. It allows for an architecturally sound implementation of Backpack, as
   the process of instantiating requirements of a Backpack only makes
   sense for libraries, rather than an entire package (an executable and
   test suite cannot be instantiated in any meaningful sense).

I have divided this proposal into two parts: first the changes necessary
for the Cabal library (which is shared between cabal-install and Stack),
and then a description of how cabal-install can take advantage of this
new feature.  There is not a Stack portion to this proposal, but
commentary from the Stack team would be appreciated.

Proposed Change: Cabal
----------------------

**The status quo.** Currently, the Cabal library defines an command line
API via the ``./Setup configure`` command. A conventional invocation of
this command from, e.g., ``cabal-install``, might look like this::

    ./Setup configure --ghc --prefix=$HOME/.cabal --user --ipid=package-name-1.0-abcdefg --dependency=array=array-0.5.1.0-abcdef1234567 --dependency=base=base-4.8.2.0-abcdef123456 --dependency=tasty=tasty-1.0-abcdef123456 --enable-tests --exact-configuration --disable-benchmarks

There are a few important flags: ``--ipid`` specifies the *installed
package identity* that the package will be built under: this is
identifier is a unique identifier chosen by the package manager (for
example, ``cabal new-build`` computes this identifier by hashing all of
the possible inputs into the build, with the hope that the hash uniquely
determines all of the build products of the package), and
``--dependency=pkgname=ipid-1.0-abcdef`` specifies a specific dependency
to be used during the build of all the components.
``--exact-configuration`` requires ALL dependencies of all enabled
components to be specified explicitly.  Notice that this particular
command has ``--enable-tests`` passed (thus enabling the building of the
test suite) and specifies which version of ``tasty`` (a test library)
should be used. This command configures both the library and the test
suite of this package *simultaneously*.

Currently, the ``configure`` command takes no positional arguments.

**The proposal.** We extend ``configure`` to take a single argument
specifying which component is to be configured::

    ./Setup configure test-suite

It is invalid to pass multiple arguments. Under this mode, the configure
command behaves differently in the following ways:

1. Subsequent commands, e.g., build, *no longer* use an inplace package
   database (conventionally in ``dist/package.conf.inplace``). It is
   expected that all dependencies live in the database stack specified
   by the user.

2. The set of required ``--dependency`` flags for an
   ``--exact-configuration`` is changed to precisely the ``build-depends``
   of the component being enabled. Dependencies from non-enabled
   components do not have to be specified (similar to how
   ``--disable-tests`` works today). However, an *internal* dependency,
   e.g., from a test suite to the library, must now be specified as
   ``--dependency=packagename=pkgipid-1.0-abcdefg123``. In the presence of
   convenience libraries, there may be multiple such extra "internal"
   dependencies. These are disambiguated from ordinary dependencies by
   inspection of the package description.

3. We replace the ``--ipid`` flag with a ``--cid`` flag, which specifies the
   *component identifier* of the component being built. The user must
   pick a distinct ``--cid`` for each component in the package, as
   distinct from an installed package identifier which was global for a
   package.  This flag is only valid when a component is explicitly
   specified to be built. For backwards compatibility, the ``--ipid`` flag
   retains its original meaning as an identifier for the package as a
   whole: thus when we build a component with it, the IPID is qualified
   with the name of the component to form a component id.


4. If a library contains a ``build-tools`` dependency on an executable, if
   the library is being configured by itself, it is expected that the
   caller arrange for the executable to be present on the ``PATH``, so
   that the build tool configuration process succeeds. (For example,
   if I have an internal exe ``foo`` and a component ``bar`` which
   has a ``build-tools`` dependency on it, you are guaranteed that ``foo``
   is part of the build graph for ``bar``.)

5. While a user can still explicitly specify installation paths for
   various files the component may install, we will apply different
   defaults to these paths::

        Variable    Current         New
        -----------------------------------------
        bindir      $prefix/bin     (same)
        libsubdir   $abi/$libname   $abi/$cid
        datasubdir  $abi/$pkgid     $abi/$cid
        docdir      $datadir/doc/$abi/$pkgid     $datadir/doc/$abi/$cid

6. As you might expect, subsequent ``build``, ``register``, etc commands
   operate only on the enabled component.

There are a few expectations as to how this per-component configure
interface is intended to be used by a client:

1. If the client reuses the same source directory to build multiple
   components, it must assign a distinct build directory ``--distdir`` to
   each component build.

2. The installation directories configured for each component are
   expected to be disjoint for each component. This is encouraged by the
   different default install directories. 

3. A component must be copied and registered to a package database
   within the package database, before any of its (same-package)
   dependents can be built.

4. The ordering components are built should respect internal
   ``build-tools`` dependencies.

**Interaction with hooks.** This proposal leaves the hooks API
unchanged. Thus, just as before, hooks are responsible for determining
what components are enabled before performing the operations they need.
If they operate on the available ``ComponentLocalBuildInfo`` hooks should
automatically work properly with these changes (even better than
manually specifying which components to build at ``./Setup build`` time)

**Interaction with Haddock.** Initially, ``haddock`` will be kept as it is
today: a command which builds Haddock documentation for every enabled
component (one in this case). However, eventually, it would be good to
treat building Haddock documentation as a "component" in and of itself,
so it can be generated and installed independently of building any
component (indeed, Haddock is completely independent of a build).
Thus, every component would also have a corresponding Haddock component
for the documentation.

The primary complication is that cross-linking for Haddock documentation
is implemented by storing paths for haddock interfaces and HTMLs in
the package database
(``hadock-interfaces`` and ``haddock-html``).  Clearly, since we want
to build Haddocks independently of libraries, this should no longer
be done.  I suggest we workaround this problem by making "fake"
installed package database entries to represent Haddock documentation:
Haddock components form an independent, parallel dependency tree
to the library tree.

**Interaction with package common files.** Cabal packages can be bundled
with package-common files (specified by the ``data-files`` field), which
are installed to a location common over all components inside a package.
Initially, our plan is that ``./Setup copy`` for a single enabled
component will unconditionally copy the data files to a
component-specific directory. A refinement would be to treat data file
installation as a "component" of its own, which the libraries and
executables of a package can depend on.

**Interaction with Backpack.** A component id encodes all of the
non-Backpack dependency configuration about a component; in the absence
of Backpack, it uniquely identifies the unit of compilation that is,
e.g., installed to a (now in-aptly named) installed package database.
With Backpack, we further qualify these identifiers with a *module
substitution*, which specifies how we instantiate the various
requirements (unfilled module implementations) of a component (using the
``--instantiate-with`` flag).

**Advisory note on convenience libraries.** Internal dependencies
permit a package to shadow packages that exist on Hackage: for
example, if I define an internal library named ``bytestring``,
and references to ``bytestring`` in ``build-depends`` refer to
the internal library, not the Hackage library.

When these internal libraries are installed to the package database,
it is expedient to distinguish an internal library from the
actual Hackage package.  The way this is done is that the registration
information for an internal library has its package name mangled
into the form ``z-pkgname-z-componentname``.

Proposed Change: cabal-install
------------------------------

We now describe how cabal-install can be engineered to make use of this
feature. The primary complication is the relationship between dependency
solving, which is necessarily a per-package affair, and building, which
is a per-component affair.

**Dependency solving status quo.** The dependency solving process picks
which versions of packages to use, and produces a *solver install plan*,
which is a graph of *resolver packages*. Resolver packages are either
pre-existing packages from the package database or planned packages that
need to be built: planned packages are identified only by a package
identifier, and also store other solver information such as the flag
assignment that was picked. These solver plan is the converted into an
install plan in a one-to-one correspondence (mapping package identifiers
into installed package identifiers).

When setup dependencies were added to cabal-install, cabal-install's
dependency solver was also refined to keep track of dependencies on a
per-component basis (``ComponentDeps``).  Thus, although the graph of
resolver packages is determined by the full, combined dependencies of
all components in a package, it's also possible to determine the
dependencies for a specific component. At the moment, this is only used
to build Setup scripts (which constitute a component of its own) with
only the necessary packages.

**The proposal.** Dependency solving naturally is a package-level
affair, since versions are ascribed to packages, not components.
However, we would like build components individually: thus, we need to
expand a package-based solver plan into a component-based install plan.
This is how it works:

1. For each package, we resolve the conditionals (using the solver
   provided flag assignments) in order to produce a
   ``PackageDescription``, which is essentially a Cabal file with all
   conditionals stripped off. Prior to this step, we don't even know
   what the components of the package necessarily will be (a component
   can be marked as un-buildable through the dependency solving
   process).

2. We can now convert a graph of packages into a graph of components. In
   doing so, the ``ComponentDeps`` tree of dependencies gets exploded into
   an individual set of package level dependencies. Each component
   identity is defined by looking at the input dependencies *of the
   component*, as well other options which would affect the build. Note
   that in a solver plan, the dependencies refer to *packages*, not
   components: these package references are resolved to the *public
   library component* of a package in question. After componentization,
   ``ComponentDeps`` is eliminated from the install plan.

3. A component is built by configuring its member package for solely the
   component in question and then building it in a distinct build
   directory. Execution of the install plan can now be parallelized over
   components, rather than packages.

**Determining if non-libraries are already installed.** In general, we
can determine if a library is already installed by consulting the
installed package database; however, no such database exists for
executables. However, in ``new-build``, executables are installed to a
deterministic directory in the Nix store; thus, we can simply check if
the directory already exists in order to determine if an executable has
already been built.

**Interaction with packages with legacy Custom scripts.** We can only
build a package on a per-component basis if the Setup script is
sufficiently new and supports this interface.  If it does not, we cannot
do so.  We determine the version of the Setup script in (1) and if
it is not supported, we treat the package as a legacy node in the
component graph.  Its component ID is simply the component ID of
its public library (if it has one; if it does not, no matter, as it's
not possible to refer to this package as a dependency in any case.)

**Interaction with Backpack.** Backpack needs to perform mix-in linking
on components, and then a further expansion step to instantiate
components.  Mix-in linking occurs as we are expanding the package
graph into a component graph, while instantiation happens as a separate
step after expansion.

Drawbacks
---------

* This feature will not be compatible with Custom setup scripts that
  are linked against an old version of Cabal.

* Package-global ``data-files`` will be duplicated until we consider
  these files a "component" in-and-of-themselves.

* File paths for installed things will change from where they are
  currently being stored.

* We need to ``configure`` the package for each component, rather
  than once for all components.

* This may require BC-breaking changes to the Cabal API (though I
  will work hard to minimize these.)

Alternatives
------------

An alternate design I considered was to not extend ``./Setup configure``
with a per-component mode.  Instead, a package would be configured once,
and then the package manager would use a newly added ``--assume-deps-up-to-date``
flag to build components individually (or in parallel.)  However, I
decided that this approach would not be hermetic enough.  It also
turned out to be difficult to work into the existing ``cabal-install``
code, although that is arguably a bug.

Unresolved Questions
--------------------

* Currently, ``./Setup`` scripts accept the arguments
  ``--enable-library-vanilla``, ``--enable-library-shared``,
  ``--enable-executable-dynamic``, ``--enable-library-for-ghci``,
  ``--enable-executable-profiling``, ``--enable-profiling``,
  ``--enable-library-profiling`` apply to a package as a whole.  With
  per-component configure, these options can be applied to a
  component *specifically*; thus, ``--enable-executable-profiling``
  doesn't make much sense if you're just configuring a library.
  Should we introduce new variants of these flags which are not
  "component" qualified? How in ``cabal-install`` can you ask for
  only a specific executable to be profiled?

