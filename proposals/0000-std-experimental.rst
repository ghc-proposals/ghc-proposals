Add -experimental flag
======================

.. author:: Moritz Angermann, Julian Ospald
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/617>`_.
.. sectnum::
.. contents::

Introduce a new compiler flag ``-experimental`` which guards experimental
features. We distinguish between experimental and non-experimental features by
their respective breaking change processes. Experimental features may introduce
breaking changes without prior warning at any time. Non-experimental (stable)
features on the other hand would need to follow a change evolution/deprecation
process and could only under exceptional circumstances introduce breaking
changes.

Motivation
----------
As GHC continues to evolve, there's a need to introduce new features that may
not be fully stabilized or might undergo significant changes in future releases.
These experimental features can potentially introduce breaking changes without
warning between different compiler versions. By introducing the
``-experimental`` flag, developers can opt-in to use these features, fully
aware of the potential risks. This ensures that features available without this
flag are stable and will only have breaking changes with a proper deprecation
cycle.

What is _currently_ considered experimental can at best be found out by proxy
via a `GHC User Guide's search for EXPERIMENTAL <https://downloads.haskell.org/ghc/latest/docs/users_guide/search.html?q=EXPERIMENTAL&check_keywords=yes&area=default>`_.


Proposed Change Specification
-----------------------------
- Introduce a new compiler flag ``-experimental``.
- Introduce a new compiler flag ``-no-experimental`` (default).
- Features not behind the ``-experimental`` flag will have to adhere to a
  deprecation cycle before introducing breaking changes.

``-experimental`` is a superset, therefore ``-experimental`` could
depend on code without ``-experimental``. The inverse does not hold.

Semantically the last ``-[no-]experimental`` flag passed to GHC will win.

Examples
--------
Consider a hypothetical experimental feature ``X``. Without the
``-experimental`` flag, trying to use ``X`` would result in a compiler error.
However, with the flag, the feature can be used, but with the understanding that
it might change in future releases. ::

  -- Without -experimental
  ghc Main.hs  -- This will throw an error if Feature X is used

  -- With -experimental
  ghc -experimental Main.hs  -- This will compile successfully with Feature X

  -- With -no-experimental
  ghc -experimental -no-experimental Main.hs -- This will throw an error if Feature X is used

  ghc -no-experimental Main.hs -package foo   -- This will throw an error if Feature X is used anywhere in Main.hs or the package foo.

Effect and Interactions
-----------------------
The introduction of this flag provides a clear distinction between stable and
experimental features. It ensures that developers are aware of the potential
risks of using experimental features while providing a sandbox for testing out
new GHC capabilities.

Costs and Drawbacks
-------------------
- There might be an initial learning curve for developers to understand the
  distinction between experimental and stable features.
- Maintaining two sets of features (stable and experimental) might increase the
  complexity of the compiler codebase.

The authors believe the benefit of clearly separating experimental and those
that adhere to a more rigourous change evolution easily offsets the costs for
both end users and feature developers. For developers of experimental feature
this also means they reserve the right to break them without warning in
backwards incompatible ways at any moment in time leading to significantly
lighter processes for experimental features.

Prior Work
----------
The ``-experimental`` feature is similar to solutions for experimental
features and implementation in other languages, and GHC would not have a
bespoke solution here, but share a common approach with many other compilers.
Do note that the provided approaches mix editions, channels, and feature gates.
The goal is to illustrate that the concept of separating experimental features
explicitly behind opt-in gates.

- **Rust**: Rust has so called channels, which delineate stability by stable,
  beta, and nightly. To opt-in locally to nightly features you'd use the
  ``+nightly`` toolchain modifier. Effectively using a different Rust compiler
  provided by ``rustup``.
- **Java**: Java has the ``--enable-preview`` flag since Java 9, which allows
  opt-in to experimental features.
- **Gnu Compiler Collection**: GCC provides flags to opt-in to experimental
  features through the ``-std=...`` flag. For example, it has flags like
  ``-std=c++1z`` for experimental C++ features before they were standardized in
  C++17.

Backward Compatibility
----------------------
The introduction of this flag is expected to have minimal impact on existing
code. The initial set of features behind the ``-experimental`` flag will be
the empty set. Follow up proposals will be required to move existing features
behind the ``-experimental`` flag. New features should by default be behind
the ``-experimental`` flag, unless the respective authors consider them stable
and not subject to sudden change.

Conditional Compilation
--------------------------
For maintainers there will be a ``__GHC_EXPERIMENTAL__`` macro to use with ``CPP``
to allow for conditional compilation if ``-experimental`` is active. This can
be used with the usual version macros to tailor to specific GHC versions as needed.

Garbage Collection
------------------
A follow up proposal will need to address the lifecycle of features behind the
``-experimental`` flag.  How features (not just extensions as in `#601`_),
will either be removed or migrate from ``-experimental`` into the stable
compiler.

Relationship to ``Haskell98``, ``Haskell2010``, ``GHC2021``, and ``-fglasgow-exts``
-----------------------------------------------------------------------------------
GHC today provides the option to *opt-in* to a collection of Extensions
considered stable. It does *not* permit the exclusion of experimental features.
The proposed ``-experimental`` provides the assurances on the other end of
the spectrum. It provides a binary option to those who want to stay out of
highly experimental features, still allowing them to augment existing
collections within the limits of *stable* extensions.

Alternatives
------------
1. **Feature Flags for Each Experimental Feature**: Instead of a single flag for
   all experimental features, individual flags for each feature could be
   introduced. However, this could lead to a proliferation of flags and increase
   complexity.
2. **Separate GHC Builds**: Provide separate GHC builds for experimental
   features. This ensures a clear separation but might be cumbersome for
   developers to manage multiple GHC installations.

Unresolved Questions
--------------------

.. _`#601`: https://github.com/ghc-proposals/ghc-proposals/pull/601

1. What is the process for moving a feature from experimental to stable? Or from
   stable to experimental? This is being addressed in `#601`_.


Risks
-----

If ``-experimental`` ends up becoming the default because library authors
and other end up depending on experimental features, the value of having
``-experimental`` diminishes greatly.  It is therefore prudent to follow
up with `#601`_, to provide a clear path for experimental features transitioning
into the stable compiler.

Implementation Plan
-------------------
The authors will try to provide a PoC for an implementation.

Endorsements
-------------
- Erik de Castro Lopo (@erikd)
- Kevin Hammond
- Jens Petersen (@juhp)
- Hécate (@kleidukos)
- Arnaud Bailly (@abailly)
- Matthias Benkort (@KtorZ)