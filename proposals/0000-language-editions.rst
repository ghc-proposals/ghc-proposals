Fortified Language Editions
===========================

.. author:: Richard Eisenberg (with some contributions by Simon Peyton Jones)
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/636>`_.
.. sectnum::
.. contents::

.. _manifesto: https://github.com/ghc-proposals/ghc-proposals/pull/628

I propose to lean more heavily on *language editions*. This proposal describes
how a language edition can control more than just extensions (importantly, one
can control *warnings* as well), and proposes several concrete language editions.
One of the proposals is for ``Stable2024``, which will continue to be supported,
without change to the set of user programs accepted, for at least 3 years.

This is extracted from a longer `manifesto`_ about how GHC should treat language
extensions. For some introductory text describing the mindset that leads to this
design, readers may want to skim the introductory sections of that manifesto.
However, the effective payload of this proposal is all in this one document.

Motivation
----------

The primary motivation behind the use of language editions is that they
can succinctly inform GHC what kind of user it's faced with, so GHC
can behave accordingly.

Though the details are spelled out below, it's necessary to introduce
some of the language editions I'm proposing:

* ``Stable2024``: Code compiled in the ``Stable2024`` edition will be
  expected to compile (assuming stability of libraries) under that
  same language edition on all GHCs release for 6 years,
  until the beginning of 2030. (We may end up falling short of this
  expectation in corner cases, but it will remain a reasonable expectation
  to have.)

* ``Experimental2024``: Switching to the ``Experimental`` series of
  editions gives you access to experimental features of GHC, which might
  reasonably evolve and break existing programs.

* ``Latest``: This language edition is the latest and greatest that GHC
  has to offer. Compiling with ``Latest`` might break between releases.

* ``Student2024``: The student edition of the language has extra guardrails.
  Furthermore, any code written in the ``Student`` edition is expected
  to compile with all future versions of GHC for 10 years, meant to
  echo the expected lifetime of a textbook.

With that out of the way, here are some scenarios that the language editions
model helps us to address:

* **Stability**. Once we include a feature in a ``Stable`` edition, it
  will not change. However, that does not mean GHC cannot evolve. If we
  identify a ``Stable`` feature that nevertheless deserves an upgrade, we
  can do so, provided we continue to support the old behavior. So
  type inference around ``FlexibleInstances``, say, might work one way in
  ``Stable2024`` and differently in ``Stable2027``. If we release a new
  ``Stable`` edition every 3 years, users have three years to upgrade before
  we no longer guarantee support. A three-year timeline is long enough that
  we might imagine contributors writing upgrade tools in that timeframe.

* **Error messages**. Because the language edition describes the user (very
  coarsely!) we can tailor error messages to be appropriate for them. The
  ``Student`` editions will not advise ``DataKinds``. The ``Stable`` editions
  will not advise ``LinearTypes``.

* **Warning evolution**. One challenge in adding features to GHC is to
  know how to evolve the warning system. Does a new warning get added to
  ``-Wall``? And what's the deal with ``-Wcompat``?

  Language editions make
  these questions easier to answer: If we think that a warning should be
  enabled for users going forward, we can turn it on by default, but only
  in appropriate language editions, such as ``Experimental`` or ``Latest``.

  For ``-Wcompat``, we add warnings that describe features that will change
  in the next edition in a given series. So the ``Stable2024`` edition of
  ``-Wcompat`` will warn about features changing in ``Stable2027``. This
  set of warnings will likely grow between 2024 and 2027; that's fine.
  (We do *not* guarantee that all ``Stable`` code remain warning-free, especially
  with ``-Wcompat`` enabled.) Now if a user is running ``Stable2024`` in
  2028 with ``-Wcompat``, they won't see warnings about changes due in
  2030; they'll see just the warnings they need to upgrade to ``Stable2027``.

* **Simplicity**. The goal is that a vast majority of our users will be
  able to specify a language edition, and that's it. No extensions. No
  warning flags. This simplifies what a user needs to think about when
  setting up a Haskell project, removing the paralysis of choice that
  can reign today.


Proposed Change Specification
-----------------------------

-  We refresh the concept of *language
   edition*. Existing language editions ``Haskell98``, ``Haskell2010``, and
   ``GHC2021`` will continue to be language editions, to which we
   add the following:

   * ``Stable2024``
   * ``Experimental2024``
   * ``Latest``
   * ``Student2024``

- A language edition can be specified using the extensions syntax, by
  passing e.g. ``-XStable2024`` on the command line or putting
  ``Stable2024`` in a language pragma.

- Every file is compiled with respect to precisely one language
  edition. Specifying more than one edition is an error. If a user
  specifies no edition during compilation, then a default is chosen:

  * In all versions of GHC released before the end of 2025, the default
    language edition will be ``GHC2021``.

  * A new warning
    ``-Wmissing-language-edition``, will inform users
    that they should specify a language edition. This warning will be
    off by default in GHC, but it is expected that cabal will turn it on.
    In this way, someone experimenting with a standalone Haskell file doesn't
    have to fuss about language editions, but anyone building a package
    (presumably for publication) is encouraged to choose.

  * Starting in 2026, the default language edition when none is specified
    will be the latest ``Stable`` edition available. (The warning continues,
    unchanged.)

  * In GHCi, the default language edition will be the latest ``Stable``
    edition. ``-Wmissing-language-edition`` will be off by default.

- In GHCi, the top-level can have a distinct language edition from loaded
  files. If a file is being loaded and does not specify its own language
  edition, it inherits from the language edition in force at the top-level.

- A language edition can control almost all behaviors of GHC. The meaning
  (or existence) of other flags can depend on language edition. While
  we will not implement it this way, we can imagine that GHC becomes
  a set of programs that happen to share a binary; the choice of which
  program is chosen by the language edition.

  The one restriction on the expressive power of language editions
  is that build products of different language editions (within the
  same version of GHC) must be
  compatible. We expect the Haskell ecosystem to contain packages
  compiled with a variety of language editions, and they must work
  together. The word *compatible* above is doing some heavy lifting,
  in that it contains a notion that the interface (i.e. exported
  symbols, their types, etc.) can be translated from one language
  edition to another. This aspect of Haskell's design has always
  been present, in that different files can be compiled with different
  langauge extensions. We often don't notice this, because we have
  been careful with language extensions not to make the translation
  apparent.

  Although language editions have wide authority, we must be tasteful
  in how they work. It would be problematic to have ``Stable2024``
  and ``Stable2027`` disagree on the meaning of widely used features.
  Yet I think it's best to be maximally expressive here, relying on
  our future selves not to abuse our power.

  (In the conversation, it was suggested that a ``Python`` language
  edition would be beyond reason... but actually I think such a thing
  would be lovely, if the necessary work was done to translate the
  interfaces between Python and Haskell.)

- For backward compatibility, a language edition can be specified
  at an arbitrary place in a command-line invocation of GHC, or in a
  ``LANGUAGE`` pragma in a file. Even though it might come later in
  a command line, the edition can affect the meaning of command-line
  arguments that precede it.

- A cabal file will allow a new
  field ``language-edition``, available both at top-level and in
  build-product stanzas. This will specify the language edition. To
  support backward compatibility, this will use the ``default-language``
  setting if that is available, and omitting the ``language-edition`` will
  use the default. At some point, it is expected that ``language-edition``
  will become required.

- Any new language features invented once e.g. ``Stable2027`` is released
  will not be available with the 2024 editions. That is, if we introduce a new
  feature ``-XDependentTypes`` in 2028, then enabling ``-XDependentTypes``
  with ``Stable2024`` (or even ``Experimental2024``) will be an error.
  This policy encourages users to upgrade their editions in order
  to access GHC's new features.

- Once an edition has been eclipsed by newer models (that is, once the
  calendar reads 2028 but someone is still using ``2024``), we know that
  we are compiling older code. Our priority for such maintenance modes
  is stability, not, say, making sure that the set of warnings conforms
  to the latest standard.

- We introduce several coarse-grained *semantic bundles* that group
  together similar features. The individual choices are detailed below.
  When using one of the new language
  editions, error messages suggest opting into one of these bundles,
  rather than suggesting individual extensions.

  The bundles described below are all just combinations of existing
  language extensions and warnings. I expect that bundles will
  remain as such (though it's conceivable that we might imagine
  language features that do not get their own extension, just a spot
  in a bundle). It's also conceivable that the bundles will evolve
  to encompass more expressive power (such as controlling optimization
  flags or the meaning of ``import Prelude``).

  Unlike language editions, a user may specify any number of bundles
  when compiling a file. In order to simplify their processing, a
  bundle cannot control the interpretation of other command-line
  flags. Otherwise, though, a bundle can potentially control arbitrary
  behavior of GHC, just like a language edition. The big difference
  between them is that a file is compiled against exactly one language
  edition, but it can be compiled against an arbitrary number of bundles.

- When printing out the namne of a warning flag as part of a warning,
  we also include any bundle that also controls the warning.

Detailed specification
~~~~~~~~~~~~~~~~~~~~~~

The text above defines language editions generally. This section
instantiates the general design with some specifics. These language
editions affect language features (as described by extension name or
warning flag in the chart below), as well as error messages (not described
in any detail). They do not affect other aspects of GHC.

The chart below classifies all current extensions and warnings.

* The GHC2021 column has an X for extensions enabled as part of GHC2021.
  For warnings, it lists if the warning is part of a current warning bucket.

* The next four columns describe ``Student2024``, ``Stable2024``, ``Experimental2024``, and ``Latest``,
  respectively. Here is the key for these columns:

  * Y: an extension is turned on

  * blank: the feature is available, but not on

  * N: trying to enable the feature is an error

  * W: the warning warns by default

  * E: the warning is an error by default

  * W!: the warning warns by default and cannot be turned off

  * E!: the warning is an error by default and cannot be turned off or made into a warning

* The next several columns describe semantic bundles of options.

  * ``FancyTypes``: The user should enable this if they want fancy types. By enabling
    this semantic bundle, the user takes responsibility for understanding type inference
    and interactions at a deeper level. In an enterprise setting, the user takes the
    responsibility for training future collaborators in these techniques.

    Includes the following: ``DataKinds``, ``ExistentialQuantification``, ``GADTs``,
    ``GADTSyntax``, ``ImpredicativeTypes`` (allowing polytypes in visible type applications
    and in type parameters, no Quick Look), ``QuantifiedConstraints``, ``RankNTypes``,
    ``RequiredTypeArguments``, ``RoleAnnotations``, ``TypeAbstractions``, ``TypeData``,
    ``TypeFamilies``, ``-Wterm-variable-capture``.

  * ``DoSyntax``: This enables extra syntactic support around ``do``-notation. Someone
    who enables this option takes responsibility for understanding the extra syntax and
    for training others to understand that syntax.

    Includes the following: ``Arrows``, ``QualifiedDo``, ``RecursiveDo``

  * ``Classic``: In some places, Haskell has recently evolved, and the recommendation of
    the new language editions are to use the new syntax or features. In a few places, users
    can enable ``Classic`` mode to use older-style Haskell. A user enabling this feature
    takes responsibility for keeping abreast with today's Haskell best practices and determining
    when it is appropriate to migrate to the new style.

    Includes the following: ``FieldSelectors``, ``NoPolyKinds``, ``StarIsType``, ``-Wno-deriving-typeable``,
    ``-Wno-prepositive-qualified-module``, ``-Wno-type-equality-out-of-scope``

  * ``LowLevel``: This bundle enables a suite of features that allow users access to low-level
    details, mainly around unlifted types. A user enabling this
    option takes responsibility for knowing about strictness vs laziness and for knowing about
    how Haskell values are represented in memory.

    Includes the following: ``ExtendedLiterals``, ``MagicHash``, ``UnboxedSums``, ``UnboxedTuples``,
    ``UnliftedDatatypes``, ``UnliftedNewtypes``

  * ``Overload``: This bundle enables more overloading of Haskell operation. A user specifying
    this option takes responsibility for fixing any inference failures that arise from too much
    overloading, often by adding type annotations.

    Includes the following: ``MonadComprehensions``, ``OverloadedLabels``, ``OverloadedLists``,
    ``OverloadedRecordUpdate`` (only with ``Experimental2024`` or ``Latest``), ``OverloadedStrings``

  * ``Sugar``: This bundle enables a small suite of syntactic niceties. A user enabling this option
    takes responsibility for knowing the new syntax and having any new collaborators also learn
    this syntax.

    Includes the following: ``GADTSyntax``, ``MultiWayIf``, ``ParallelListComp``, ``PatternGuards``,
    ``PatternSynonyms``, ``PostfixOperators``, ``RecordWildCards``, ``TransformListComp`` (but not
    in ``Stable2024``), ``TupleSections``, ``TypeOperators``, ``UnicodeSyntax``, ``ViewPatterns``

    (I am the least confident about this group.)

  * ``FFI``: This enables Haskell's foreign function interface. A user enabling this option will need
    to understand the details of the FFI to be effective.

    Includes the following: ``CApiFFI``, ``ForeignFunctionInterface``, ``InterruptibleFFI``, ``JavaScriptFFI``,
    ``UnliftedFFITypes``

  * ``TH``: This enables Haskell's *Template Haskell* feature. A user enabling this option takes
    responsibility for understanding that recompilation will become more frequent, as well as understanding
    how staging issues can affect code reuse. The user also forgoes the possibility of doing
    cross-compilation.

    Includes the following: ``QuasiQuotes``, ``TemplateHaskell``

  * ``Unused``: This turns off a suite of warnings that tell the user when part of their code is
    redundant. Users enabling this option take responsibility for monitoring their own code for unused
    variables and other constructs.

    Includes the following: ``-Wno-unused-do-bind``, ``-Wno-unused-foralls``, ``-Wno-unused-imports``,
    ``-Wno-unused-local-binds``, ``-Wno-unused-matches``, ``-Wno-unused-packages``, ``-Wno-unused-pattern-binds``,
    ``-Wno-unused-record-wildcards``, ``-Wno-unused-top-binds``, ``-Wno-unused-type-patterns``

  * ``Explicit``: This option causes GHC to warn when it makes an assumption. To get code to compile
    with ``Explicit``, users must rely less on inference and more on their own annotations. Users specifying
    ``Explicit`` take on the responsibility of writing and maintaining these extra annotations.

    Includes the following: ``-Wmissing-deriving-strategies``, ``-Wmissing-exported-pattern-synonym-signatures``,
    ``-Wmissing-exported-signatures``, ``-Wmissing-export-lists``, ``-Wmissing-import-lists``,
    ``-Wmissing-kind-signatures``, ``-Wmissing-local-signatures``, ``-Wmissing-pattern-synonym-signatures``,
    ``-Wmissing-role-annotations``, ``-Wmissing-signatures``

  * ``Complete``: There are a number of places where programmers can omit parts of their program
    and still get it to compile. The ``Complete`` option makes these into errors. All such features
    are warnings by default (within the language editions, at least); ``Complete`` turns them into
    errors. Users enabling this option will have to fix such errors long before deployment. (Other
    warnings require fixing only for deployment, not during development.)

    Includes the following: ``-Werror=incomplete-patterns``, ``-Werror=incomplete-record-selectors``,
    ``-Werror=incomplete-record-updates``, ``-Werror=incomplete-uni-patterns``, ``-Werror=missing-fields``,
    ``-Werror=missing-methods``

  * ``T1``, ``T2``, and ``T3``. These options are meant to be analogous to optimization levels
    ``O1``, ``O2``, and ``O3``, but for type inference. When a user specifies optimization, they
    understand that the runtime behavior of their program may be less related to when they wrote and
    thus harder to preduct, but presumably will be faster. Similarly, specifying a type inference
    level beyond 0 means that GHC will work harder to accept their program, but the exact types inferred
    become harder to predict.

    * ``T1``: Type inference is expected to be stable, though it is harder (in corner cases) for users to guess
      the inferred type. The type checker may also run forever, but it will never produce a program that
      does so (unless the expressions in the program indeed loop).

      Includes the following: ``FlexibleContexts``, ``FlexibleInstances``, ``LiberalTypeSynonyms``,
      ``MultiParamTypeClasses``, ``TypeSynonymInstances``, ``UndecidableInstances``, ``UndecidableSuperClasses``

    * ``T2``:  It is possible (though unlikely) that type inference
      will change between major releases. This level enables *functional dependencies*, which
      allows type inference to propagate information in new, sometimes unexpected ways.

      Includes the following: ``FunctionalDependencies``, ``ImpredicativeTypes`` (Quick Look only),
      ``NoMonoLocalBinds``, ``OVERLAP`` pragmas, ``TypeFamilyDependencies``

    * ``T3``: The type inference engine is now allowed, in some scenarios, to make guesses
      between two valid possibilities. These guesses might even influence runtime behavior.
      Enabling this level of type inference should be done only by users who are confident
      in writing confluent sets of class instances.

      Includes the following: ``DeepSubsumption``, ``IncoherentInstances`` (but without implying
      ``INCOHERENT`` on every instance), ``NoMonomorphismRestriction``, ``-Wno-warn-orphans``

  Here is the key for the "semantic bundle" columns:

  * X: The feature is enabled.

  * O: The feature is disabled.

  * W: The warning warns.

  * E: The warning is an error.

* The notes column carries brief notes. For some extensions, it says *flag*. This means that the extension
  doesn't fit into the rubric of "taking on responsibility", but instead expresses the user's preference
  for how to interpret a program. I expect these extensions to remain as independent extensions (not bundled)
  into perpetuity.

* Though not captured in this table, I also think that language editions should control error messages.
  That is, the ``Student`` edition might have error messages more tuned to students' needs, over
  experts' needs. I do not intend to give a specification here of the details, but it's something
  I think we should keep in mind as considering all of this.

+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|                                               |GHC2021|Student|Stable|Exper|Latest|FancyTypes|DoSyntax|Classic|LowLevel|Overload|Sugar|FFI|TH|Unused|Explicit|Complete|T1|T2|T3|Notes                             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``AllowAmbiguousTypes``                        |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag* This is a bug.             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``AlternativeLayoutRule``                      |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``AlternativeLayoutRuleTraditional``           |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ApplicativeDo``                              |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``Arrows``                                     |       |       |      |     |  Y   |          |   X    |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``AutoDeriveTypeable``                         |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``BangPatterns``                               |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``BinaryLiterals``                             |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``BlockArguments``                             |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``CApiFFI``                                    |       |       |      |     |      |          |        |       |        |        |     | X |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ConstrainedClassMethods``                    |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ConstraintKinds``                            |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``CPP``                                        |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``CUSKs``                                      |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Deprecated.                       |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DataKinds``                                  |       |       |      |     |  Y   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DatatypeContexts``                           |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Deprecated.                       |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeepSubsumption``                            |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |X |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DefaultSignatures``                          |       |   N   |  N   |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Prefer ``DerivingVia``.           |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveAnyClass``                             |       |   N   |  N   |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Prefer ``DerivingVia``.           |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveDataTypeable``                         |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveFoldable``                             |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveFunctor``                              |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveGeneric``                              |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveLift``                                 |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DeriveTraversable``                          |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DerivingStrategies``                         |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DerivingVia``                                |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DisambiguateRecordFields``                   |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DoAndIfThenElse``                            |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``DuplicateRecordFields``                      |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``EmptyCase``                                  |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``EmptyDataDecls``                             |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``EmptyDataDeriving``                          |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ExistentialQuantification``                  |   X   |       |      |     |  Y   |   X      |        |       |        |        |     |   |  |      |        |        |  |  |  | [#]_                             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ExplicitForAll``                             |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ExplicitNamespaces``                         |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ExtendedDefaultRules``                       |       |   Y   |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ExtendedLiterals``                           |       |       |      |     |  Y   |          |        |       |   X    |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``FieldSelectors``                             |   X   |       |      |     |      |          |        |   X   |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``FlexibleContexts``                           |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |X |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``FlexibleInstances``                          |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |X |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ForeignFunctionInterface``                   |   X   |       |      |     |      |          |        |       |        |        |     | X |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``FunctionalDependencies``                     |       |       |      |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |  | X|  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``GADTs``                                      |       |       |      |     |  Y   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``GADTSyntax``                                 |   X   |       |  Y   |  Y  |  Y   |    X     |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``GeneralizedNewtypeDeriving``                 |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``GHCForeignImportPrim``                       |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``HexFloatLiterals``                           |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ImplicitParams``                             |       |       |      |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ImplicitPrelude``                            |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ImportQualifiedPost``                        |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ImpredicativeTypes``                         |       |       |      |     |  Y   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |``forall`` in args & applications |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ImpredicativeTypes``                         |       |       |      |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |X |  | Quick Look algorithm             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``IncoherentInstances``                        |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |X |Does not imply ``INCOHERENT``     |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``InstanceSigs``                               |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``InterruptibleFFI``                           |       |       |      |     |      |          |        |       |        |        |     | X |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``JavaScriptFFI``                              |       |       |      |     |      |          |        |       |        |        |     | X |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``KindSignatures``                             |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``LambdaCase``                                 |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``LexicalNegation``                            |       |       |      |     |      |          |        | Note  |        |        |     |   |  |      |        |        |  |  |  | [#]_                             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``LiberalTypeSynonyms``                        |       |       |      |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |X |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``LinearTypes``                                |       |       |  N   |     |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``MagicHash``                                  |       |       |      |     |  Y   |          |        |       |   X    |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``MonadComprehensions``                        |       |       |      |     |      |          |        |       |        |   X    |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``MonoLocalBinds``                             |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |O |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``MonomorphismRestriction``                    |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |O |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``MultiParamTypeClasses``                      |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |X |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``MultiWayIf``                                 |       |       |  Y   |  Y  |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NamedFieldPuns``                             |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NamedWildCards``                             |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  | See ``partial-type-signatures``  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NegativeLiterals``                           |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NondecreasingIndentation``                   |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NPlusKPatterns``                             |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NullaryTypeClasses``                         |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Use ``MultiParamTypClasses``      |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NumDecimals``                                |       |       |  N   |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``NumericUnderscores``                         |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``OverlappingInstances``                       |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  | X|  |T2 enables ``OVERLAP`` pragmas    |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``OverloadedLabels``                           |       |       |      |     |      |          |        |       |        |    X   |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``OverloadedLists``                            |       |       |      |     |      |          |        |       |        |    X   |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``OverloadedRecordDot``                        |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``OverloadedRecordUpdate``                     |       |       |  N   |     |      |          |        |       |        |   (X)  |     |   |  |      |        |        |  |  |  |Only with experimental/latest     |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``OverloadedStrings``                          |       |       |  Y   |  Y  |  Y   |          |        |       |        |    X   |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PackageImports``                             |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Ugly, but sometimes necessary     |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ParallelArrays``                             |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ParallelListComp``                           |       |       |      |     |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PartialTypeSignatures``                      |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |See ``partial-type-signatures``   |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PatternGuards``                              |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PatternSignatures``                          |       |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Enable only pattern signatures    |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PatternSynonyms``                            |       |       |      |     |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PolyKinds``                                  |   X   |   Y   |  Y   |  Y  |  Y   |          |        |   O   |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``PostfixOperators``                           |   X   |       |  Y   |  Y  |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``QualifiedDo``                                |       |       |      |     |  Y   |          |    X   |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``QuantifiedConstraints``                      |       |       |      |     |  Y   |     X    |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``QuasiQuotes``                                |       |       |      |     |      |          |        |       |        |        |     |   | X|      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RankNTypes``                                 |   X   |       |      |     |  Y   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RebindableSyntax``                           |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RecordWildCards``                            |       |       |      |     |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |Affects scoping. Own category?    |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RecursiveDo``                                |       |       |      |     |      |          |   X    |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RelaxedLayout``                              |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RequiredTypeArguments``                      |       |       |  N   |     |  Y   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |FancyTypes does not affect Stable |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``RoleAnnotations``                            |       |       |  Y   |  Y  |  Y   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``Safe``                                       |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  | [#]_                             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ScopedTypeVariables``                        |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``StandaloneDeriving``                         |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``StandaloneKindSignatures``                   |   X   |   Y   |  Y   |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``StarIsType``                                 |   X   |       |      |     |      |          |        |   X   |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``StaticPointers``                             |       |       |  N   |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should probably be removed.       |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``Strict``                                     |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``StrictData``                                 |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |*flag*                            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TemplateHaskell``                            |       |       |      |     |      |          |        |       |        |        |     |   |X |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TemplateHaskellQuotes``                      |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Its own category                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TraditionalRecordSyntax``                    |   X   |   Y   |   Y  |   Y |   Y  |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TransformListComp``                          |       |       |   N  |     |      |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |Sugar doesn't enable in Stable    |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``Trustworthy``                                |       |  N    |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |See note on ``Safe``              |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TupleSections``                              |   X   |       |  Y   |  Y  |   Y  |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeAbstractions``                           |       |       |  N   |     |  Y   |     X    |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeApplications``                           |   X   |       |  Y   |  Y  |   Y  |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeData``                                   |       |       |      |     |   Y  |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeFamilies``                               |       |       |      |     |   Y  |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeFamilyDependencies``                     |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  | X|  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeInType``                                 |       |   N   |   N  |   N |   N  |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Use ``PolyKinds``                 |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeOperators``                              |   X   |       |   Y  |  Y  |   Y  |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``TypeSynonymInstances``                       |   X   |       |   Y  |  Y  |  Y   |          |        |       |        |        |     |   |  |      |        |        | X|  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UnboxedSums``                                |       |       |      |     |  Y   |          |        |       |    X   |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UnboxedTuples``                              |       |       |      |     |  Y   |          |        |       |    X   |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UndecidableInstances``                       |       |       |      |     |  Y   |          |        |       |        |        |     |   |  |      |        |        | X|  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UndecidableSuperClasses``                    |       |       |      |     |  Y   |          |        |       |        |        |     |   |  |      |        |        | X|  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UnicodeSyntax``                              |       |       |      |     |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UnliftedDatatypes``                          |       |       |      |     |  Y   |          |        |       |   X    |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UnliftedFFITypes``                           |       |       |      |     |      |          |        |       |        |        |     | X |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``UnliftedNewtypes``                           |       |       |      |     |  Y   |          |        |       |   X    |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``Unsafe``                                     |       |   N   |  N   |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |See note on ``Safe``              |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ViewPatterns``                               |       |       |      |     |  Y   |          |        |       |        |        |  X  |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``all-missed-specialisations``                 |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``alternative-layout-rule-transitional``       |default|   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``ambiguous-fields``                           |default|   E   |   E! |  E  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``auto-orphans``                               |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``badly-staged-types``                         |default|   E   |   E! |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Will become an error.             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``compat-unqualified-imports``                 |compat |       |   W! |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``cpp-undef``                                  |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |No opinion.                       |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``data-kinds-tc``                              |default|   E   |   E! |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Will become an error.             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``deferred-type-errors``                       |default|   W   |   W! |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |[#]_                              |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``deferred-out-of-scope-variables``            |default|   W   |   W! |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |See note directly above.          |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``deprecated-flags``                           |default|       |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``deprecated-type-abstractions``               |compat |   E   |   E! |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Will become an error.             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``deriving-defaults``                          |default|   E   |   E! |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``deriving-typeable``                          |       |   E   |   W  |  W  |  W   |          |        |   O   |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``dodgy-exports``                              |   W   |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``dodgy-foreign-imports``                      |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``dodgy-imports``                              |   W   |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``duplicate-constraints``                      |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  | Use ``redundant-constraints``    |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``duplicate-exports``                          |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``empty-enumerations``                         |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``forall-identifier``                          |default|   E   |   E! |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Will become an error.             |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``gadt-mono-local-binds``                      |default|       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Not needed with ``MonoLocalBinds``|
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``hi-shadowing``                               |all    |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Deprecated.                       |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``identities``                                 |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``implicit-kind-vars``                         |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Deprecated.                       |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``implicit-lift``                              |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Debugging aid.                    |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``implicit-prelude``                           |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Seems pointless. Remove?          |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``implicit-rhs-quantification``                |compat |   E   |   W! |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``inaccessible-code``                          |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``incomplete-export-warnings``                 |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``incomplete-patterns``                        |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |    E   |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``incomplete-record-selectors``                |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |    E   |  |  |  |``FieldSelectors`` is off.        |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``incomplete-record-updates``                  |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |    E   |  |  |  |Should experiment; might be noisy.|
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``incomplete-uni-patterns``                    |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |    E   |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``inconsistent-flags``                         |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``inferred-safe-imports``                      |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``inline-rule-shadowing``                      |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``invalid-haddock``                            |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``loopy-superclass-solve``                     |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |This has been removed.            |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``misplaced-pragmas``                          |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missed-extra-shared-lib``                    |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missed-specialisations``                     |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-deriving-strategies``                |       |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-exported-pattern-synonym-signatures``|       |       |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-exported-signatures``                |       |       |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-export-lists``                       |       |       |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-fields``                             |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |   E    |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-home-modules``                       |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |For internal use by Cabal         |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-import-lists``                       |       |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |Maybe add ``(..)`` import list?   |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-kind-signatures``                    |       |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-local-signatures``                   |       |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-methods``                            |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |   E    |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-monadfail-instances``                |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-pattern-synonym-signatures``         |all    |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-poly-kind-signatures``               |       |       |   W  |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-role-annotations``                   |       |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |Should warn for abstract types.   |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-safe-haskell-mode``                  |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-signatures``                         |all    |       |      |     |      |          |        |       |        |        |     |   |  |      |    W   |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``missing-space-after-bang``                   |default|   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``monomorphism-restriction``                   |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Too noisy.                        |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``name-shadowing``                             |all    |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``noncanonical-monadfail-instances``           |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``noncanonical-monad-instances``               |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``noncanonical-monoid-instances``              |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``operator-whitespace``                        |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``operator-whitespace-ext-conflict``           |default|       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Covered by ``operator-whitespace``|
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``orphans``                                    |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |O |T3 turns this *off*               |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``overflowed-literals``                        |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``overlapping-patterns``                       |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``partial-fields``                             |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |``FieldSelectors`` is off.        |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``partial-type-signatures``                    |default|   W   |   W! |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``prepositive-qualified-module``               |       |   W   |   W  |  W  |  W   |          |        |   O   |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``redundant-bang-patterns``                    |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``redundant-constraints``                      |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``redundant-record-wildcards``                 |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``redundant-strictness-flags``                 |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``safe``                                       |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``semigroup``                                  |compat |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Does nothing; should be removed.  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``simplifiable-class-constraints``             |default|   W   |   W! |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``star-binder``                                |default|       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``star-is-type``                               |default|       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``tabs``                                       |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``term-variable-capture``                      |       |       |      |     |  W   |    X     |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``trustworthy-safe``                           |all    |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``typed-holes``                                |default|   W   |  W!  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``type-defaults``                              |all    |       |      |     |      |          |        |       |        |        |     |   |  |      |   W    |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``type-equality-out-of-scope``                 |compat |   E   |   E! |  E  |  E   |          |        |   O   |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``type-equality-requires-operators``           |default|   E   |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unbanged-strict-patterns``                   |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unicode-bidirectional-format-characters``    |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unrecognised-pragmas``                       |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unrecognised-warning-flags``                 |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unsafe``                                     |       |   N   |   N  |  N  |  N   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |Should be removed.                |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unsupported-calling-conventions``            |default|   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unsupported-llvm-version``                   |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unticked-promoted-constructors``             |       |       |      |     |      |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-do-bind``                             |all    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-foralls``                             |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-imports``                             |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-local-binds``                         |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-matches``                             |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-packages``                            |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-pattern-binds``                       |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-record-wildcards``                    | all   |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-top-binds``                           |  W    |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``unused-type-patterns``                       |       |   W   |   W  |  W  |  W   |          |        |       |        |        |     |   |  |  O   |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+
|``wrong-do-bind``                              |default|   E   |   E  |  E  |  E   |          |        |       |        |        |     |   |  |      |        |        |  |  |  |                                  |
+-----------------------------------------------+-------+-------+------+-----+------+----------+--------+-------+--------+--------+-----+---+--+------+--------+--------+--+--+--+----------------------------------+

Effect and Interactions
-----------------------

By leaning on language editions, we achieve the following:

* Users can declare, at the top of their file or in their cabal file,
  that they are working in the stable subset of Haskell.

* By clearly describing the stable subset of Haskell, this proposal
  allows us to uphold the stability principles `articulated
  elsewhere <https://github.com/ghc-proposals/ghc-proposals/blob/wip/general-rules/principles.rst#3ghc-stability-principles>`_.

* If we ever feel the need to evolve the meaning of an extension, we
  can do so, by keying the meaning of the extension to the language
  edition. This allows forward evolution without sacrificing stability.
  (There may be some implementation duplication to achieve this. There
  is a cost to stability, but one I think is worth paying.)

* We have a mechanism for introducing new warnings. Over the years,
  we've been skittish about adding new warnings on by default, lest
  they cause trouble for older programs. With language editions, we
  can tell whether a program is old or new, enabling new warnings only
  in a newer edition (if we so choose).

* There is no need for a language edition simply to be a collection
  of language extensions and warnings. We might imagine features
  that just live in an edition, without a separate flag controlling
  it. (Indeed, this happens in this original proposal a bit, by
  splitting up different aspects of ``-XImpredicativeTypes``.) If
  language editions prove popular, they open up new ways of evolving
  GHC without the fuss of language extensions.

* Coding tools (e.g. editor integrations, HLS) can choose to target,
  say, just the stable subset of Haskell. While support for all of
  the language is preferred, of course, having editions helps provide
  guidance to tool authors for where to focus their efforts.

* The ``Stable`` edition is somewhat restrictive, with warnings that
  cannot be disabled and extensions that cannot be turned on. Yet
  there is always an escape hatch: use ``Experimental`` instead.
  The motivation here is that we want ``Stable`` to mean exactly that.
  If a ``Stable`` module were allowed to twiddle settings that violate
  the stability goals, then the user might unintentionally make their
  program unstable. By making ``Stable`` restrictive, we force the
  user to be aware of the stability impact of their settings.

* Because a language edition can affect all aspects of GHC, it
  can also affect the module that is implicitly imported in Haskell
  files. Maybe it's ``Prelude``. Maybe it's ``Prelude2024``, which
  might have some new exports. Or maybe importing a module named
  ``Prelude`` is something like a keyword whose behavior depends
  on the edition. (This way, you can effectively change the language
  edition for modules with an explicit ``import Prelude`` and still
  change what's imported.) Such effects of language editions are
  not included for the editions spelled out in this proposal, but
  editions offer a tantalizing way to grow or alter the ``Prelude``
  over time.

Costs and Drawbacks
-------------------

* Reducing the effective configuration space ("effective": my goal
  is for users to leverage new language editions and semantic
  bundles, but the old options remain) means that users will be less
  precise in specifying their language. Maybe a user is happy to
  have GADTs but not type families in their programs; the blunderbuss
  ``-XFancyTypes`` is too broad for them. Yet other languages have no
  fine-grained mechanism for controlling language features. I think
  this is a luxury we can live without, gaining more simplicity in
  its place.

* Because of the expressive power of language editions -- with their ability
  to control all aspects of GHC's behavior -- we now have potentially an
  even larger configuration space to worry about as implementers. ("Does
  the bug happen with ``-XWeevils`` under ``Stable`` or just ``Experimental``?")
  If we expected users to use language editions *and* the 100+ language
  extensions, this would indeed be problematic. But, again, the goal is
  to eschew language extensions for the coarser semantic bundles, reducing
  the space down to something more manageable.

* This proposal takes us further from where Haskell started, as a base
  language supported by multiple compilers, each of which could introduce
  its own extensions. While nothing stops another compiler from supporting
  the same language editions as proposed here, the coarse nature of them
  makes it harder to enter the field. Yet the reality is that GHC is the
  only industrial strength Haskell compiler, and even it doesn't compile
  standard Haskell (because of the ``Applicative`` superclass to ``Monad``,
  at least).

* If language editions come out only every three years, then there is more
  pressure to get an idea into the next edition, instead of having to wait.
  This is a real problem. It is mitigated a bit by the existence of ``Latest``,
  which will evolve with every GHC release (possibly even minor releases). To
  me, we're striking the right balance between due caution and our desire for
  evolution, but individual experiences and opinions will differ.


Backward Compatibility
----------------------

This proposal causes no change in behavior to existing programs, except
for a warning that users should specify a language edition.

Alternatives
------------

* The current proposal makes ``Stable2024`` and ``Experimental2024``
  into language editions. In the future, we'll have, say, ``2027``
  editions, too. A different way to slice this cake is to specify
  the language varietal and its vintage separately, via two flags.

  There's something appealing about the separation, as it makes
  two axes navigable independently. Maybe it's a better user interface
  than what I've proposed, but I think the payload doesn't change:
  the combination of varietal and vintage would together make a language
  edition, and all the aspects of a language edition that I've
  described would be controlled by this little product.

* ``Stable`` and ``Experimental`` are central to this proposal; ``Student``
  and ``Latest`` less so. I can see forgoing either of these two. I like
  keeping them, but maybe others disagree.

* Perhaps we want to phase in the no-language-edition-specified warning.

* There are a number of other proposals in this space, including the
  description of a ``GHC2024``.

Unresolved Questions
--------------------

* What's the right cadence for updates? This proposal assumes a 3-year
  cadence, but there's no great reason to believe that's the right one.
  Three years feels, to me, about the right balance between cost (each
  one of these has maintenance and specification costs) and expressiveness,
  but I can't defend this feeling.

* How soon to drop support for an old language edition? Elsewhere in this
  text, I assume that at 2030, we'll phase out 2024 editions. Maybe that's
  too soon.

* Should specifying ``-XComplete`` also change the ``Prelude`` to exclude
  partial functions? I think probably not -- I'm worried about opaqueness
  introduced by having these bundles be too expressive -- but it's fun to
  dream about.

Resolved Questions (i.e. FAQ)
-----------------------------

* Q: Why can't we just define language editions and semantic bundles as
  sets of extensions and warnings?

  A: This proposal almost does this. There are a few places, though, where
  the current meaning of extensions cuts across several different ideas that
  might usefully be separated. (Specifically, ``ImpredicativeTypes`` both
  allows newer fancier types and also newer fancier type inference; these
  aspects can usefully be separated, as they have different expectations
  around stability.) Looking forward, I think it's useful to give ourselves
  the freedom to change other aspects of GHC beyond what is typically controlled
  by extensions or warnings -- such as error messages.

  In the future, if users end up largely migrating away from using language
  extensions and warnings and toward using the coarser interfaces (editions
  and bundles) introduced here, then maybe we can describe editions and bundles
  just as extensions, because we would have the freedom to invent lots of new
  extensions (e.g. ``ImpredicativeTypeDefinitions`` and ``ImpredicativeTypeInference2024``)
  without impacting users.

* Q: Why not just keep ``default-language`` in cabal instead of changing it to
  ``language-edition``?

  A: Because of the word "default". That word implies that this is the
  language for all modules except those that specify some other
  language, but I think the cabal file should be authoritative about
  language edition. That is, I want to be able to know that a package
  is stable just by looking at the cabal
  file. Maybe this is problematic if someone really wants a package
  mixing language editions? I suppose if that is a use case we want to
  support, there could be ``language-edition:
  as-specified-in-each-module`` or something. Alternatively, we could
  say this field is not required by cabal. Regardless, I don't think
  the field should specify an overridable default.

* Q: Doesn't this proposal lose the ability to refer to a feature by
  its language extension?

  A: Yes, though not irrevocably. That is, under this proposal, we might
  imagine evolving the meaning of ``-XTribbles`` between language editions.
  Thus, talking about the "Tribbles" feature would be underspecified, as
  we'd have to name both "Tribbles" and the language edition. For implementors,
  this could be problematic, and we may to invent a term "Tribbles2024" to talk
  about a specific version of the Tribbles feature. But for users, the goal
  is for them not to worry about these details. Indeed, they might never specify
  ``Tribbles`` anywhere, instead using editions and bundles to describe what
  kind of language they want, without sweating the details.

  Put another way (adopting text from Simon Marlow): We must not
  change an extension's meaning for a given language edition, but we
  could change the meaning in a new language edition. We might well
  decide that changing extensions from one edition to the next is too
  confusing to contemplate (and I would agree!) but given that in the
  past we've felt it necessary to change the meaning of extensions
  from one GHC release to the next it seems plausible that we might
  want to make changes in the future. We could decide that those kind
  of changes can only be made by adding a new extension, which would
  be arguably better.

* Q: What's the binary compatibility guarantee?

  A: The restriction on the expressive power of language editions is that build
  products of different language editions must be compatible. The compatibility
  guarantee applies only *within* one release of GHC. That is, GHC 9.12 will
  produce binaries compatible with other binaries produced by GHC 9.12, regardless
  of the language edition. However, *no* guarantee is provided between GHC 9.12
  and GHC 9.14, even under the same language edition.

* Q: With the power embodied in language editions, might we be inviting a
  state of chaos, as these can evolve arbitrarily over time?

  A: Technically, yes, we're giving ourselves the power to make arbitrary
  changes from one version of GHC to the next. But this is nothing new! We've
  always had the ability to change the meaning of language extensions (or the
  base language) between releases. However, we have striven to make tasteful
  such changes. We will continue to do so.



Implementation Plan
-------------------

This will be easy to implement; I volunteer to do so.

.. [#]
   With any of the new language editions specified (that is, anything
   other than ``Haskell98``, ``Haskell2010``, or ``GHC2021``),
   ``ExistentialQuantification`` will allow only existential quantification,
   not also (and accidentally) GADTs.

.. [#]
   ``LexicalNegation`` fixes a wart in the language around unary ``-``, by
   requiring unary ``-`` not to have any whitespace after it. However, unary
   ``-`` remains the only symbolic unary function in the language. I hereby
   propose fixing the wart in the other direction, by just killing off unary
   ``-``. In the presence of ``NegativeLiterals``, we can still write ``-5``
   for negative 5. But if you want to negate ``x``, just ``negate x``.

   Thus with any of the new language editions, users cannot write a unary
   ``-``, unless ``Classic`` mode is on.

.. [#]
   It is time for the ``Safe`` ecosystem to be dismantled. We should warn
   on the use of any of the ``Safe`` language extensions, but otherwise
   ignore them. With the introduction of the language editions proposed
   here, GHC will allow a ``Safe`` module to depend on any module, thus
   allowing for a transition period, even if some libraries eagerly adopt
   these language editions.

 .. [#]
   These warnings cannot be disabled in ``Stable`` mode, because the mechanism
   for producing them may conceivably change over time. Recall that packages
   cannot be released with warnings (without the user disabling this mechanism)
   and so this means that users cannot release code with deferred errors.

   These are not affected by ``Complete``, because deferring these errors
   is already opt-in.
