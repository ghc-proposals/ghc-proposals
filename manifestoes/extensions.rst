.. sectnum::
.. contents::

.. _PR: https://github.com/ghc-proposals/ghc-proposals/pull/628

**GHC Language Configuration Renovation**

This document is discussed at this PR_.

Preamble
--------

This document was chiefly written by Richard Eisenberg; "I" refers
to Richard. Some sections (exactly which ones are now lost to the
mists of time) evolved from text originally written by Simon Peyton
Jones. Simon should not be considered a co-author, however, as he
has not reviewed the document in its entirety before public posting.

Goals
~~~~~

This document lays out a number of changes to GHC's mechanism for
configuring which language the compiler accepts, with the following
goals:

-  Make users' decisions around extensions – which to use and why –
   easier and more well-informed.

-  Make it easier for GHC to remain both stable for users who want
   stability and fast-moving for users who want innovation.

-  Rationalize historical language-configuration capabilities (e.g.
   extensions, warnings, several language-changing flags), by putting
   everything into one consistent framework.

Motivation
~~~~~~~~~~

Do we need to spend yet more time worrying about how extensions are used
in GHC? I argue "yes", for the following reasons:

- The end result of this line of work will **allow users to declare that
  their code is written in a stable subset of Haskell.** The ability to
  do so signals to potential enterprise adopters that they can expect
  a smoother upgrade experience than has historically been the case
  for Haskell.

- The language extension mechanism is a significant part of our user interface.
  As it is distinctive to Haskell, it's a part of what people consider when
  choosing Haskell over another language. Getting it wrong may be pushing
  people away.

- The Haskell community is currently weighed down by the tension between
  users who want stability and users who want innovation. The key idea
  threaded throughout this document -- *language editions* -- appear to
  be a way to satisfy both sets of users, setting Haskell up for many more
  years of innovation while simultaneously becoming a more attractive setting
  for enterprise codebases.

- Innovation in Haskell is often bogged down by long debates about extensions,
  warnings, and defaults. I believe that the new framing of these aspects of
  language design as presented here can cut through these debates and make
  the way forward clearer, accelerating and simplifying language innovation.

- Pulling all of these together, **I see this document as describing the end
  of Haskell's adolescence.** During adolescence, we experiment with all manner
  of ideas, mixing work and play, and following fun ideas wherever they might
  lead. In adulthood, however, we segregate hobbies from work, knowing that
  our work will stand the test of time (while still enjoyably engaging in hobbies
  away from work). **If we accept these proposals, we declare to the world that
  Haskell has grown up and is ready to work.** (But don't fret: there is still plenty
  of room for fun and hobbies in adulthood!)

Structure of this document
~~~~~~~~~~~~~~~~~~~~~~~~~~

This document contains a number of chapters. The document warms
up by laying out the particular take on the world that motivates
the main proposal.

My goal in posting this is not to get the main
proposal accepted as-is, but instead to seek consensus that the broad
outlines are a direction worth exploring. The details are all here,
but I worry that debating details before agreeing on a general direction
may be counter-productive.

The end of the document includes a few older ideas that I no longer think
are so important; they are included in case they are of interest to
readers.

How to read this document
~~~~~~~~~~~~~~~~~~~~~~~~~

This is big! How should you get the most out of your time? My recommended
route through this document is this:

1. Read the bold sentences in the opinion chapters. These are the main payload
   of those chapters.

2. For any chapter where you are unsold on the bold sentences, skim through the
   rest of the chapter, reading it more thoroughly if you're inspired to. If you're
   in agreement with the bold sentences, the rest of the chapter may be redundant
   for you.

3. Glance at the proposal chapters, to get a high-level overview for what they're
   trying to achieve. You're probably best off *not* reading the details.

4. If you're in broad agreement with the directions proposed here -- chiefly,
   to lean more heavily on *language editions*, not *language extensions*,
   as the way we think about language configuration -- post on the `PR`_ this is
   attached to saying so.

   If you're not in agreement with this, post on the `PR`_, as well, of course!

Because we will not be voting on any part of this document, right now, as a proposal,
the details are meant to illustrate the main point. If you agree in broad strokes but
think, say, that a particular extension should be treated differently than I've done
so here, there is no need, now, to highlight this difference. I'm seeking broad consensus
on the direction of travel, first, and then choosing details later.

Actual proposals for change will be forthcoming, based on the initial discussion
on the main `PR`_. Once we get to those proposals, I welcome debate on the details!

Opinion: Haskell, not GHC
----------------------------

When thinking about the user interface of Haskell, **it is vital to consider
that the user is interacting with Haskell, not just GHC.** Users, in general,
do not distinguish between the multiple tools that together comprise Haskell,
encountering some subset of GHC, cabal, stack, HLS, editor integrations, and
Hackage, all in the pursuit of doing some Haskell programming. We should keep
this experience front and center as we're considering any changes we might
make to the language and its ecosystem: addressing just one tool in isolation
is almost surely not the right way to think about design.

Background: Language configuration
---------------------------------------------

This is a document about *language configuration*. Let's tease apart and
understand both of those words:

**Language**. As we all learned in elementary school, a *language* is a
set of strings, preferably a recursively enumerable one. In the context
of a compiler, however, we mean more than this: we really mean a
function mapping strings either to error messages or to semantics. [#]_
Now we must tease these parts apart:

*Error messages*. An error message is a string presented to the user in
the event that the compiler cannot make sense of their program. At a
minimum, the message should explain why the compiler was unable to make
progress. However, even better is when the message gives the user a way
to repair their program to make it acceptable. The key correctness
criterion about an error message is its usefulness to the reader, not
whether it is factually accurate (which is a rather low bar).
Accordingly, **error messages must communicate at the level of
abstraction the user is working in**. We don’t talk about voltages,
resistance, and incandescence; we instead say we flipped a light switch
-- unless we’re in an electricity lab, of course! The goal of tuning our
error messages to the user’s level of abstraction suggests that **we
should change our messages depending on the configuration of the
compiler**.

*Semantics.* The semantics of an accepted program encompasses several
details.

-  *Static semantics, or typing*. At the level of thinking about a
   compiler’s output, static semantics describes a compilation unit’s
   interface, dictating what other compilation units are acceptable in
   the context of this one. When thinking about stability, the key
   detail here is how one compilation unit affects others, not internal
   details. That is, if a change to GHC affects the way a unit’s types
   are printed — but not how the compiler behaves on other compilation
   units — then that change does not decrease GHC’s stability.

-  *Evaluation semantics*. This describes the Haskell values a function
   evaluates to, when the result is forced (or non-termination or
   exceptions thrown). Because of Haskell’s purity, [#]_ just talking
   about values encompasses effects and their ordering. In thinking
   about stability, we want to ensure that a function called on a value
   continues to produce the same output in future versions of GHC.

-  *Performance semantics*. In a real execution, the amount of time,
   memory, and power a function requires to reduce to a value matters.
   In thinking about stability, we should guarantee that there are no
   asymptotic regressions across GHC versions. However, this may be hard
   to achieve (in the presence of asymptotically helpful but fragile
   rewrite rules, for example), and even so it may be insufficient for
   some use cases (constant factors matter). In order to make stability
   promises about GHC, we will have to think carefully about what to say
   about performance. I think ignoring the problem is to our detriment,
   however. The challenge here is a concrete downside of how clever GHC
   is at optimizing our code.

There are other semantic properties one might imagine, including
security properties, debuggability, ABI, etc. We ignore these details in
this document, leaving stability guarantees about such features as a
future goal.

**Configuration**. Haskell compilation can be configured in many ways.
Here are some:

1. Via a ``LANGUAGE`` pragma.

2. Via an ``OPTIONS`` or ``OPTIONS_GHC`` pragma.

3. In a command-line invocation with compiler flags.

4. In a ``ghc-options`` stanza in a cabal file.

5. With a ``--ghc-options=`` flag passed to a cabal invocation.

6. In a ``ghci.conf`` file.

7. With various editor-specific settings, passed via HLS.

8. Through custom ``DynFlags`` operations available in the GHC API.

I’m sure there are more. One might argue that most of these are just
glosses over the fact that flags are passed to GHC on the command line.
However, to a user, all of these are distinct. One of the goals of this
document is to make the interaction between all of these different
settings loci predictable.

Opinion: User experience around configuration
-----------------------------------------------

For its lifetime, the design of Haskell has been propelled forward by
the work of language researchers. This focus on wild new features makes
Haskell a thought leader in several programming technologies (practical
programming with purity, arrows, laziness, expressive types, etc).
However, it has also meant that much of the structure of the language
and its compiler is dictated by the sensibilities of language
researchers -- at a somewhat far remove from everyday programmers.

Because of this history, and because volunteer contributors tend to
implement their new idea in a new language extension, we have ended up
with a great wealth of extensions.

A challenge for users is that the extension menagerie can be hard to
keep track of. If GHC says ``UndecidableInstances`` might help with solving
a type inference puzzle, what should a user do? Maybe they enable the
extension, and maybe it does help in that case. Is the user’s program
now more susceptible to crashing? Will it run slower? Will it compile
slower? Might type inference fail in a different way now? We compiler
developers know the answers to these questions (only in very rare
circumstances; no; GHC is doing more now, so a bit slower, but not
really; existing type inference will continue to work) but users have no
good way of accessing this knowledge. Furthermore, users are generally
ill equipped to make the decision of enabling each extension
independently.

Instead of thinking at the level of extensions, I like to think of users
worried about *responsibility*. A compelling reason to work in a typed
language is that a type-checker absolves users of the responsibility of
avoiding a large class of errors, like adding an integer to a bool.
Similarly, working in a terminating language absolves users of worrying
about infinite loops. To me, the key question a user might be thinking
about is: what new responsibility am I assuming by enabling this
compiler feature? Decisions around compiler configuration should be
designed to center this notion of responsibility; doing so will make
decisions easier for both hobbyist and enterprise users. (Student users
usually don’t have the freedom of choice; their instructor makes this
decision for them.)

I thus propose **feature bundles**. **A feature bundle is a collection
of Haskell features all require the same assumption of
responsibility from users.** It is my hope that, once these are
established, users will focus on these bundles, not individual
extensions. For now, the fine-grained extension control will remain;
this is all too much of an experiment for some time going forward. But
if the experiment is successful -- if users like the idea of these
bundles -- then perhaps we can lean more heavily on them in the future.

The details about what the bundles are, and what extensions belong in
each, is presented in a later proposal chapter.

Opinion: Warnings
-------------------

We must fit the notion of language configuration around the way GHC
already is, with its current set of configuration options. Today’s
language extensions very clearly are a part of language configuration.
The warnings facility also forms a part. By changing the warning
settings (in particular, when using ``-Werror``), we can change the set of
programs GHC accepts.

The presence of warnings muddies the definition of *language* given above,
which says that a language is a mapping from strings to either error messages
or semantics of a program. In the presence of warnings, a language is a
mapping from strings to either error messages or semantics *and warnings*.
We'll use this definition going forward.

Why do we have warnings at all? If something is wrong with a user program,
shouldn't we just stop compilation? It is tempting to say that a warning
is a *recoverable* problem with a user program. That is, an error is a problem
where the compiler can't make sense of the user's program, while a warning
doesn't prevent the compiler from understanding the user program -- it's just
identifying a potential problem.

Yet that doesn't quite hold up under scrutiny:
with ``-fdefer-type-errors``, GHC is perfectly capable of making sense of a
program with type errors. With ``-XNoDeriveFunctor``, GHC is similarly capable
of making sense of a program with ``deriving Functor``. Yet in both of these
scenarios (and many more), GHC issues an error.

I thus argue that the line between warnings and (some) errors is blurry. We
can imagine a spectrum of code problems going from spelling of identifiers
through the kinds of problems identified by Hlint through GHC's warnings through
features enabled by GHC extensions through features enabled by flags like ``-fdefer-type-errors``
through errors GHC simply cannot recover from (like parser errors). Embodied
in the current way these potential problems are distributed between different
reporting mechanisms are value judgments on what the GHC (and other tool) developers
think about the problems at hand. It is thus reasonable to expect people to differ
on these points, as with any value judgments. My point here is simply calling
something an "error" or a "warning" or some other problem is not very informative.
Accordingly, we should strive for a *uniform treatment* of these potential problems;
doing so would simplify Haskell's user interface.

Let's also reflect on why we have warnings at all. I claim the usefulness of
warnings depends on what our user is trying to accomplish. Here are some
possibilities:

1. The user is actively developing the code in question.
2. The user is compiling the code in question in order to use the compilation
   output (as a library or an application); the user is not a developer of the
   code.
3. The user is running CI and/or publishing their work.

In scenario (1), warnings are great. They give the user information they might
use to improve their code. Indeed warnings are better than errors for them, because
perhaps their focus at the moment is on a different part of the code than where
the warning arose, or perhaps the user is experimenting and is happy for e.g. an
import to be unused.

In scenario (2), warnings are mostly useless. The user doesn't care about the
code being compiled, and doesn't need to know that it has a few unused variables.
The only reason that warnings aren't completely useless here is that conscientious
users might reach out to library authors to tell them that their code is warning.

In scenario (3), the fact that warnings do not stop compilation is actively harmful. When going through final checks
before merging a pull request or posting on Hackage, an unresolved warning is a
potential problem in code that the programmer might have missed. This might be
disastrous.

Happily, we can accommodate all of these usage scenarios and treat warnings
appropriately, as detailed in a proposal below. The key points can be summarized
in two sentences:

* **Preparing a package for uploading to Hackage (e.g. ``cabal sdist``) should
  fail if any warnings are produced.**

* **Language edition should control which warnings are printed.**

Opinion: Other bits of language configuration
---------------------------------------------

Beyond just warnings, GHC offers other mechanisms for controlling the language
that is compiled. These include, for example, the optimization flags and flags
controlling GHC's chosen back end (i.e. target platform). Though these do indeed
control the semantics of accepted programs, **I consider optimizations and
choices of back end out-of-scope** for this document. The reason is essentially
practical: though users might reasonably want stability around optimizations between
releases, ensuring this is likely to be too expensive to be practical.

Proposal: Language editions
-----------------------------

GHC has, for some time, support language editions. Today's language
editions are ``Haskell98``, ``Haskell2010``, and ``GHC2021``. Other
than the fact that only one of these can be specified at a time, language
editions are not special: they are just language extensions that imply
hosts of others.

This proposal seeks to change that, making a language edition capable
of controlling all aspects of GHC's behavior.

Motivation
~~~~~~~~~~

The primary motivation behind the use of language editions is that they
can succinctly inform GHC what kind of user it's faced with, so GHC
can behave accordingly.

Though the details are spelled out below, it's necessary to introduce
some of the language editions I'm proposing:

* ``Stable2024``: Code compiled in the ``Stable2024`` edition will be
  expected to compile (assuming stability of libraries) for 6 years,
  until the beginning of 2030.

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

Proposed specification
~~~~~~~~~~~~~~~~~~~~~~

-  We refresh the concept of *language
   edition*. Existing language editions ``Haskell98``, ``Haskell2010``, and
   ``GHC2021`` will continue to be language editions, to which we
   add the following:

   * ``Stable2024``
   * ``Experimental2024``
   * ``Latest``
   * ``Student2024``

- Every file is compiled with respect to precisely one language
  edition. If a user specifies no edition during compilation, the
  latest ``Stable`` edition is used. If a user specifies two or more
  editions, an error is reported.

- A language edition can control almost all behaviors of GHC. The meaning
  (or existence) of other flags can depend on language edition. While
  we will not implement it this way, we can imagine that GHC becomes
  a set of programs that happen to share a binary; the choice of which
  program is chosen by the language edition.

  The one restriction on the expressive power of language editions
  is that build products of different language editions must be
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

- Once e.g. ``Stable2027`` is released, new language features will *not*
  be available with the 2024 editions. That is, if we introduce a new
  feature ``-XDependentTypes`` in 2028, then enabling ``-XDependentTypes``
  with ``Stable2024`` (or even ``Experimental2024``) will be an error.
  This policy gently encourages users to upgrade their editions in order
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

Effects
~~~~~~~

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

Drawbacks
~~~~~~~~~

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

Alternatives
~~~~~~~~~~~~

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

Open questions
~~~~~~~~~~~~~~

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

Proposal: Make warnings into errors for deployment
--------------------------------------------------

According to the analysis above, having warnings not stop compilation
is most useful during interactive development. Since a warning indicates
a problem of some sort in your code, though, it should stop deployment.

Thus, I propose:

* ``cabal sdist`` fails if any warnings are produced during compilation.

* Tools that perform Haskell CI (such as `haskell-ci <https://github.com/haskell-CI/haskell-ci>`_)
  should turn warnings into errors.

These are not GHC proposals. But I think it may be useful to consider
the language editions proposed above with respect to the design here.
I am not an expert in cabal's or haskell-ci's UI, so I will not specify
the details here. However, I would expect all such tools to offer
some way to opt out of the erroring behavior, just to allow e.g. an
old package with lots of warnings to still be updated on Hackage.

Effects
~~~~~~~

The stable language edition allows users to include ``-fdefer-type-errors``
or to use partial type signatures. Yet these features are not tightly
specified and could reasonably evolve between GHC releases; it would be
poor if such evolution broke existing programs. This is why the language
edition chart says that the correspnoding warnings cannot be disabled.
If we cannot deploy a Haskell package that has warnings, we are effectively
prevented from persisting the unstable behavior here -- while still allowing
programmers to benefit from the features during interactive development.

More generally, I think this new default will clarify our attitude around
warnings: issues that should not impede development but should either be
fixed or suppressed before deployment.

**Retracted** Proposal: Make extension flags order-independent
--------------------------------------------------

This proposal changes the way extensions work in order to make their
interpretation independent of the order in which they are written.

I wrote it at an earlier phase of my thinking about all of this. However,
if we lean heavily on language editions and semantic bundles, then
there is no need to fuss about option order. I leave this here in case
anyone is interested.

**Goals**

Order-independence would mean that we don't need to specify the
order! There are a lot of sources, including

-  ``LANGUAGE`` pragma

-  ``OPTIONS_GHC`` pragma

-  Command line to invocation of GHC

-  ``default-extensions`` field of Cabal file

-  Cabal's ``--ghc-options``

Other things being equal, not having to specify the order in which
they are processed would be good.

Scope
~~~~~~~~~~

This proposal covers both changes to GHC and to the cabal library (i.e.
the format and handling of .cabal files). We recognize these are
different projects; an acceptance of this proposal by the GHC Steering
Committee does not imply an obligation of acceptance by the cabal
maintainers, and the proposal is designed to be useful even if cabal
rejects the proposal.

This choice is made because it is helpful to consider both cabal and GHC
together, as this is how users experience their interactions with
Haskell.

Motivation
~~~~~~~~~~~~~~~

Currently, the order in which language extensions are written matters.
This is most obvious with e.g. ``-XGADTs -XNoGADTs``, where the last one
written wins. But order-dependence more insidiously affects extensions
with implications: because ``-XGADTs`` implies ``-XMonoLocalBinds``, saying
``-XNoMonoLocalBinds -XGADTs`` leads to a different end interpretation than
``-XGADTs -XNoMonoLocalBinds``. Yet the reader has to know of the
implication in order to know that these two extensions do not commute.

By moving to an order-independent interpretation, we gain the ability to
redesign aspects of the extension system without worrying about
ordering. There are also other nice UI benefits, as detailed in the
“Effects” section.

Background
~~~~~~~~~~~~~~~

Some language extensions imply others. No extension that begins ``No``
implies another. With only one exception, the implied exceptions also do
not begin with ``No``; the exception is that ``RebindableSyntax`` implies
``NoImplicitPrelude``. No other extension implies ``ImplicitPrelude``. (This
arrangement is important for maintaining a high degree of backward
compatibility.)

There are cases where extension A implies extension B, which implies
extension C. In these cases, we just say that extension A implies B and
C. Example: ``TypeFamilyDependencies`` implies ``TypeFamilies``, which implies
``MonoLocalBinds``. For this proposal, though, we will just say that
``TypeFamilyDependencies`` implies ``MonoLocalBinds``.

Proposed change specification for GHC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Running example: We have Ext.hs beginning ::

  {-# LANGUAGE TypeFamilies, FunctionalDependencies, NoMonoLocalBinds #-}

  {-# OPTIONS_GHC -XNoGADTs -XNoMultiParamTypeClasses #-}

and compiled with ``ghc -XGADTs``.

1. Let an *extension priority level* be a natural number, such that a
   lower number implies "more important".

2. Let an *extension specification* be a place where an extension is
   specified. In our example, there are 6 extension specifications (3 in
   a ``LANGUAGE`` pragma, 2 in a ``OPTIONS_GHC`` pragma, and one in a
   command-line argument ``-XGADTs``).

3. Each extension specification is associated with an extension
   *priority level*.

4. Each item in the list of extensions in ``LANGUAGE`` is assigned priority
   level 0.

5. As command-line options are processed, GHC maintains an extension
   priority level, which is assigned to each flag that enables (or
   disables) a language extension. This level starts out as 1. The new
   flag ``-extension-priority-level=N`` expects N to be a natural number
   literal and sets the extension priority level to N. To change the
   level multiple times within the command line, pass multiple
   ``-extension-priority-level=N`` flags. N is allowed to be 0. The
   ``OPTIONS_GHC``\ /\ ``OPTIONS`` pragmas also start processing their extension
   specifications at level 1; the level can be changed with
   ``-extension-priority-level``. (It is expected that users never pass
   ``-extension-priority-level``; only tooling such as cabal will use this
   flag. Errors from GHC should take care not to mention it.)

   In our example, ``TypeFamilies``, ``FunctionalDependencies``, and
   ``NoMonoLocalBinds`` all have priority 0, while ``GADTs``,
   ``NoMultiParamTypeClasses``, and ``NoGADTs`` have priority level 1.

6. After all extension specifications are processed, each extension
   priority level will be associated with an unordered set of extension
   specifications. If there are any conflicts within a level, report an
   error and stop compilation. In our example, there is a conflict at
   level 1, and so we would reject with an error, saying that we don't
   know whether the user wants ``GADTs`` on or off.

   We then process the priority levels in decreasing order. For each
   priority level, we compute all implied extensions and enable or
   disable them. Then we look at the extension specifications at the
   current level and enable or disable extensions. In this way, implied
   extensions can be overridden by explicit request, but lower priority
   specifications take precedence over higher priority ones.

   Let's modify our example to remove the ``-XNoGADTs``, so that the error
   above would not happen. Then we would do the following sequence:

   1. Enable ``MonoLocalBinds``, as implied by ``GADTs``.

   2. Enable ``GADTs`` and disable ``MultiParamTypeClasses``.

   3. Enable ``MonoLocalBinds`` (as implied by ``TypeFamilies``) and
      ``MultiParamTypeClasses`` (as implied by ``FunctionalDependencies``).

   4. Enable ``TypeFamilies`` and ``FunctionalDependencies``, and disable
      ``MonoLocalBinds``.

   We are thus left with the following enabled: ``GADTs``,
   ``MultiParamTypeClasses``, ``TypeFamilies``, and ``FunctionalDependencies``. Note
   that we indeed enable ``MultiParamTypeClasses`` despite the priority-1
   ``NoMultiParamTypeClasses`` and disable ``MonoLocalBinds`` despite it being
   implied at both level 1 and 0.

Proposed Change Specification for .cabal Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Add a new setting to the ``library``, ``executable``, and ``test``
   specifications: ``required-extensions`` are extensions that are in effect
   for all files in a project. If a file tries to countermand this (e.g.
   with ``required-extensions: MonoLocalBinds`` and ``{-# LANGUAGE
   NoMonoLocalBinds #-}``), GHC will report an error.

Implementation: The required-extensions are passed to GHC preceded by
``-extension-priority-level=0``.

Effects
~~~~~~~~~~~

This design offers the following improvements over the status quo:

1. Order of extension specifications no longer matters. This means that
   future proposals in this chain do not have to think about how the new
   designs interact with ordering.

2. An author of a cabal file can now effectively prevent the use of an
   extension in any Haskell files in the project -- even if those files
   try to enable the extension. This makes it easier for a company to
   monitor and control what extensions are used in a Haskell codebase.

The design here is intended to be backward compatible in most cases. It
handles the possibility that a programmer has implied an extension and
then reversed the implication (e.g. with ``-XTypeFamilies
-XNoMonoLocalBinds``) due to the special handling of implied extensions.
The set of implied extensions can never be contradictory in itself, as
no extension is implied turned on by one extension and implied turned
off by another.

This design is not fully backward compatible, however: today a user
might write ``-XArrows -XNoArrows`` (or similar in a ``LANGUAGE`` pragma), and
this has a well-defined semantics. WIth this proposal, such a
specification would be rejected. I consider this is an improvement.

This design also addresses some corner cases that exist today. For
example, if I have ``default-extensions`` in a cabal file and some
extensions specified by ``—-ghc-options`` in the command-line invocation of
cabal, which takes precedence? I don’t see this addressed in the cabal
documentation. This proposal defines the semantics of such an event.

Drawbacks
~~~~~~~~~~~~~

This makes the extension mechanism seem more complicated. However, I
argue that this complication is already there, but we just don’t see it.
The complication is in the fact that extensions can be specified in so
many different places, and a full understanding of the extensions
mechanism requires knowing in what order all these places are processed.
With this proposal, all that is simplified away. And the expectation is
that users know nothing more than “``LANGUAGE`` takes priority over other
places, and contradictions are errors”. Simple!

Implementation thoughts
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It’s unclear at the time of writing how bad the backward incompatibility
would be. Maybe this affects many projects in the wild. Hopefully not —-
because this would negatively affect only projects with contradictions
in their extension specifications.

It would not be hard to implement this proposal. And so we can
implement this alongside the existing extension processing algorithm,
and then see if the set of extensions implied by the new algorithm is
different than the one implied by the old algorithm, or if any new
errors arise. We can then make a final decision on whether to adopt
this new plan.

**Retracted** Proposal: Unifying warnings and language extensions
-----------------------------------------------------

This proposal unifies language extensions and warnings into one
mechanism, thus simplifying GHC's user interface while offering more
expressiveness, including allowing the language edition to specify a
default set of warnings.

I have decided to retract this proposal as unnecessary in light of
language editions and their ability to control both extensions and
warnings. (It was originally written when we were expecting language
editions just to control language extensions.) Nevertheless, this
proposal still adds some expressiveness, by giving the user the ability
to warn on the use of certain constructs. In the end, though, I think
the upheaval this would cause (even while being backward compatible)
isn't worth it.

Motivation
~~~~~~~~~~~~~~~

This proposal discusses both **language extensions** and **warning
flags.** These features may seem distinct, but in fact (see `this
discussion <https://github.com/ghc-proposals/ghc-proposals/issues/615>`__):

-  many extensions could have been implemented as warnings instead (such
   as ``-XMultiParamTypeClasses``, ``-XDeriveFunctor`` or ``-XAllowAmbiguousTypes``)

-  many warnings could have been implemented as extensions (such as
   ``-Wname-shadowing``)

It is something of a historical accident that one thing has ended up in
"warnings" while another has ended up in "language extensions". However
although language extensions and warning flags overlap in functionality,
they also have some distinctively different functionality:

-  Language extensions can change the behavior of a program; warnings
   cannot.

-  Warnings can warn. Language extensions cannot: they can only error.

This proposal seeks to unify the features of language extensions and
warnings, and then present a simplified user interface over the unified
feature.

**"Good" and "bad"**. In general, language extensions often enable a
good new behavior, while disabling a warning enables a bad new
behavior. But "good" and "bad" are clearly subjective, and we've
already gotten this wrong a few times. (For example, ``-XDatatypeContexts``
allows a bad behavior and ``-Wno-unticked-promoted-constructor`` allows a
good one, at least in my opinion.) So I propose the following
principle:

   **Principle of neutrality.** GHC itself should not have an opinion
   about "good" and "bad", for example by categorising one as a language
   extension and the other as a warning flag; rather language editions
   should express that choice.

*Not in scope:* There are flags that control GHC's language that we are
not (yet) including, such as ``-fdefer-type-errors`` and ``-fpedantic-bottoms``
that control GHC's behavior. While they fit within the overall
framework here, there is no great need to consider them now and will
only serve to complicate the debate.

Proposed design
~~~~~~~~~~~~~~~~~~~~

-  **Extensions can warn.** For any given language extension, say ``GADTs``:

   -  ``-XGADTs`` allows GADTs

   -  ``-XNoGADTs`` errors on a use of GADTs

   -  ``-XWarnGADTs`` warns on a use of GADTs

-  **Warnings are just extensions.** Almost all current warnings, such
   as ``-Wname-shadowing``, become a language extension
   ``-XWarnNameShadowing``, with the obvious algorithmic name conversion.

- Back-compat: all existing warning-flag syntax remains (perhaps
  indefinitely); but almost all are re-interpreted as a synonym for
  language extension flags. For example -Wname-shadowing is a synonym
  for ``-XWarnNameShadowing``. ("Almost all" because a few warnings are
  extra-linguistic, such as ``-Winconsistent-flags``.)

-  **Implications.** A language extension may imply others. This is true
   today; for example ``-XTypeFamilyDependencies`` implies ``-XTypeFamilies``.
   The warning form has a similar dependency:
   ``-XWarnTypeFamilyDependencies`` implies ``-XWarnTypeFamilies``.

-  **Conservative and non-conservative extensions.** A conservative
   extension adds a feature to the language, without affecting the
   meaning of any existing program; a non-conservative extension changes
   the meaning of a program.

-  **Non-warnable extensions**. Some language extensions are
   *non-warnable*, so you are not allowed to say
   ``-XWarnAlternativeLayoutRule`` for example.

   The vast majority of extensions are warnable; in particular, all
   conservative extensions are warnable. Most non-conservative
   extensions could usefully be made warnable, although it might take
   extra work to do so. Examples:

   -  ``-XWarnMonomorphismRestriction``: we already have a warning when this
      "bites", and it did indeed take extra work.

   -  ``-XWarnRebindableSyntax``: this would be new, but we would warn on
      every use of a rebindable construct that does not refer to the
      appropriate name from base.

   -  ``-XWarnDeepSubsumption``: would warn when deep subsumption was
      actually used, and simple subsumption would not have sufficed.

The meaning of ``-W`` and ``-Wall`` would continue to be "enable all recommended
warnings" and "enable all reasonable warnings", just as in GHC today.
These lists may vary with GHC version and language edition.

Consequences and benefits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  The tension between warnings and language extensions disappears.
   For example, at the top of a module we can write ::

     {-# LANGUAGE GADTs, NoIncompletePatterns #-}

   rather than ::

     {-# LANGUAGE GADTs #-}
     {-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-  A language edition could choose to allow, but warn, about a language
   extension, e.g ``-XDeriveFunctor``. That is not possible today.

Questions
~~~~~~~~~~~~~

-  How does this play with the new user defined warning categories?

-  A new language extension for each warning, and a new warning for each
   language extension. Two long lists (extensions and warnings) combined
   into one even longer list. Could feel intimidating.

-  Will we end up supporting something for longer? Eg ``-Wmonad-fail``. It
   lived only for a few releases, it warned you if you didn't write your
   code in a forward compatible way. E.g ``-Wstar-is-type``. The language
   extension ``-XStarIsType`` could go away entirely.

.. [#]
   The semantics also come with compilation outputs, but we’ll ignore
   that important detail in this document.

.. [#]
   Of course, Haskell isn’t completely pure, supporting unsafePerformIO
   and friends. The behavior of such functions is often important in
   Haskell programs. In thinking about stability, we should be attuned
   to possible changes here. If we identify any particular patterns of
   impure functions that we wish to guarantee future support for, we
   should document them.

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
