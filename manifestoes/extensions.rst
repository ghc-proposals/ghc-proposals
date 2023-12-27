.. sectnum::
.. contents::
.. title:: GHC Language Configuration Renovation

.. _PR: TODO

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

This document contains a number of chapters. Some chapters are
exposition: they describe the world as it is, or include some
definitions, or lay out some assumptions. Some chapters are actual
proposals for changes to GHC. Proposal chapters are labeled in the
chapter heading. I expect that it will be easiest to debate the
proposals serially, rather than all at once. Yet they are incorporated
into one document so that we can see our final destination (in the
context of the exposition chapters) from the outset. I expect that the
details of the individual proposals will evolve during debate; future
proposals may have to be revised to accommodate those changes. Each
proposal here assumes the previous proposals have been accepted, though
there may be ways of reordering them (with appropriate edits).

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
function mapping strings either to error messages or to semantics. [1]_
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
   exceptions thrown). Because of Haskell’s purity, [2]_ just talking
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

In scenario (3), warnings are actively harmful. When going through final checks
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

Proposal: Unifying warnings and language extensions
-----------------------------------------------------

TODO: Consider removing.

This proposal unifies language extensions and warnings into one
mechanism, thus simplifying GHC's user interface while offering more
expressiveness, including allowing the language edition to specify a
default set of warnings.

This proposal is *optional*: it unifies an aspect of GHC's interface,
but if we allow the language edition to control the set of warning flags
(not just, say, enable or disable extensions), then we do not need to
do this.

Motivation
~~~~~~~~~~~~~~~

This proposal discusses both **language extensions** and **warning
flags.** These features may seem distinct, but in fact (see `this
discussion <https://github.com/ghc-proposals/ghc-proposals/issues/615>`__):

-  many extensions could have been implemented as warnings instead (such
   as -XMultiParamTypeClasses, -XDeriveFunctor or -XAllowAmbiguousTypes)

-  many warnings could have been implemented as extensions (such as
   -Wname-shadowing)

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
already gotten this wrong a few times. (For example, -XDatatypeContexts
allows a bad behavior and -Wno-unticked-promoted-constructor allows a
good one, at least in Richard's opinion.) So we propose the following
principle:

   **Principle of neutrality.** GHC itself should not have an opinion
   about "good" and "bad", for example by categorising one as a language
   extension and the other as a warning flag; rather language editions
   should express that choice.

*Not in scope:* There are flags that control GHC's language that we are
not (yet) including, such as -fdefer-type-errors and -fpedantic-bottoms
that control GHC's behavior. While they fit within the overall
framework here, there is no great need to consider them now and will
only serve to complicate the debate.

Proposed design
~~~~~~~~~~~~~~~~~~~~

-  **Extensions can warn.** For any given language extension, say GADTs:

   -  -XGADTs allows GADTs

   -  -XNoGADTs errors on a use of GADTs

   -  -XWarnGADTs warns on a use of GADTs

-  | **Warnings are just extensions.** Almost all current warnings, such
     as -Wname-shadowing, become a language extension
     -XWarnNameShadowing, with the obvious algorithmic name conversion.
   | Back-compat: all existing warning-flag syntax remains (perhaps
     indefinitely); but almost all are re-interpreted as a synonym for
     language extension flags. For example -Wname-shadowing is a synonym
     for -XWarnNameShadowing. ("Almost all" because a few warnings are
     extra-linguistic, such as -Winconsistent-flags.)

-  **Implications.** A language extension may imply others. This is true
   today; for example -XTypeFamilyDependencies implies -XTypeFamilies.
   The warning form has a similar dependency:
   -XWarnTypeFamilyDependencies implies -XWarnTypeFamilies\ **.**

-  **Conservative and non-conservative extensions.** A conservative
   extension adds a feature to the language, without affecting the
   meaning of any existing program; a non-conservative extension changes
   the meaning of a program.

-  | **Non-warnable extensions**. Some language extensions are
     *non-warnable*, so you are not allowed to say
     -XWarnAlternativeLayoutRule for example.
   | The vast majority of extensions are warnable; in particular, all
     conservative extensions are warnable. Most non-conservative
     extensions could usefully be made warnable, although it might take
     extra work to do so. Examples:

   -  -XWarnMonomorphismRestriction: we already have a warning when this
      "bites", and it did indeed take extra work.

   -  -XWarnRebindableSyntax: this would be new, but we would warn on
      every use of a rebindable construct that does not refer to the
      appropriate name from base.

   -  -XWarnDeepSubsumption: would warn when deep subsumption was
      actually used, and simple subsumption would not have sufficed.

-  **Non-negatable extensions**. Some language extensions are
   *non-negatable*; for example, you cannot say -XNoSafe. (This is the
   case today, because someone might want to ensure that all files are
   compiled Safely, and an individual module should not be able to opt
   out.) (With the proposal about unordered extension specifications,
   the rule would be slightly different: it would just say that -XSafe
   is always at priority level 0.)

-  **Incompatible extensions**. Two language extensions can be *mutually
   incompatible*. For example -XSafe and -XUnsafe. It is an error to
   specify both at "warn" level or above.

-  **A language edition**, like -XGHC2024, simply implies a bunch of
   other extensions, just as today. Each language edition is
   incompatible with other language editions, so you can specify at most
   one language edition.

..

   Any particular version of GHC comes with its own "default language
   edition". For example, GHC 9.8 has default language edition GHC2021.
   What that means is that the language extensions implied by GHC2021
   are switched on; *but GHC2021 itself is not*, so that the user can
   say ghc -XGHC2024 without an incompatible-extension warning.

Extensions are processed in order, as today (but see Proposal 3 for an
alternative).

The meaning of -W and -Wall would continue to be "enable all recommended
warnings" and "enable all reasonable warnings", just as in GHC today.
These lists may vary with GHC version.

Consequences and benefits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  | The tension between warnings and language extensions disappears.
     For example, at the top of a module we can write
   | {-# LANGUAGE GADTs, NoIncompletePatterns #-}

..

   | rather than
   | {-# LANGUAGE GADTs #-}
   | {-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-  A language edition fixes a set of warnings, unlike the situation
   today. For example, -XGHC2024 could include warnings about incomplete
   patterns.

-  A language edition could choose to allow, but warn, about a language
   extension, e.g -XDeriveFunctor. That is not possible today.

-  A language edition could choose to error on what is today a warning,
   such as -XNoMissingMethods. (Today you can say
   -Werror=missing-methods, but you can't do that in a language
   edition.) An opt-in change of this nature is the purpose of `GHC
   Proposal
   571 <https://github.com/ghc-proposals/ghc-proposals/pull/571>`__

-  We could add a non-warnable non-negatable language extension -XStable
   that is defined to be incompatible with all Experimental extensions,
   but otherwise does nothing at all. Thus, adding -XStable will ensure
   that no experimental extensions can be used, which is (close to) the
   goal of `GHC Proposal
   617 <https://github.com/angerman/ghc-proposals/blob/std-experimental/proposals/0000-std-experimental.rst>`__.

-  A language edition could, if we wanted, choose to be incompatible
   with some experimental extension (e.g. -XLinearTypes), or even with
   all experimental extensions (via -XStable).

-  -Wcompat turns on warnings that will be enabled by default in the
   future, but remain off in normal compilations for the time being. It
   can continue to do so. But under this proposal, warnings "enabled by
   default in the future" will simply be part of the default language
   edition.

Questions
~~~~~~~~~~~~~

-  How does this play with the new user defined warning categories?

-  A new language extension for each warning, and a new warning for each
   language extension. Two long lists (extensions and warnings) combined
   into one even longer list. Could feel intimidating.

-  Will we end up supporting something for longer? Eg -Wmonad-fail. It
   lived only for a few releases, it warned you if you didn't write your
   code in a forward compatible way. E.g -Wstar-is-type. The language
   extension -XStarIsType could go away entirely.

   -  Policy idea: Support the past three language editions, but drop
      support for earlier ones.

   -  Currently dropping warnings is seen as no-fuss-required; but if
      warnings were language extensions, we'd need to treat them much
      more carefully.

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

- A language edition can control arbitrary behavior of GHC. The meaning
  (or existence) of other flags can depend on language edition. While
  we will not implement it this way, we can imagine that GHC becomes
  a set of programs that happen to share a binary; the choice of which
  program is chosen by the language edition.

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



Proposal 3: Make extension flags order-independent
--------------------------------------------------

This proposal changes the way extensions work in order to make their
interpretation independent of the order in which they are written.

**Goals**

-  Order-independence would mean that we don't need to specify the
   order! There are a lot of sources, including

   -  LANGUAGE pragma

   -  OPTIONS_GHC pragma

   -  Command line to invocation of GHC

   -  default-extensions field of Cabal file

   -  Command to invocation of Cabal --ghc-options

..

   Other things being equal, not having to specify the order in which
   they are processed would be good.

3.1. Scope
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

.. _motivation-1:

3.2. Motivation
~~~~~~~~~~~~~~~

Currently, the order in which language extensions are written matters.
This is most obvious with e.g. -XGADTs -XNoGADTs, where the last one
written wins. But order-dependence more insidiously affects extensions
with implications: because -XGADTs implies -XMonoLocalBinds, saying
-XNoMonoLocalBinds -XGADTs leads to a different end interpretation than
-XGADTs -XNoMonoLocalBinds. Yet the reader has to know of the
implication in order to know that these two extensions do not commute.

By moving to an order-independent interpretation, we gain the ability to
redesign aspects of the extension system without worrying about
ordering. There are also other nice UI benefits, as detailed in the
“Effects” section.

3.3. Background
~~~~~~~~~~~~~~~

Some language extensions imply others. No extension that begins No
implies another. With only one exception, the implied exceptions also do
not begin with No; the exception is that RebindableSyntax implies
NoImplicitPrelude. No other extension implies ImplicitPrelude. (This
arrangement is important for maintaining a high degree of backward
compatibility.)

There are cases where extension A implies extension B, which implies
extension C. In these cases, we just say that extension A implies B and
C. Example: TypeFamilyDependencies implies TypeFamilies, which implies
MonoLocalBinds. For this proposal, though, we will just say that
TypeFamilyDependencies implies MonoLocalBinds.

3.4. Proposed change specification for GHC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Running example: We have Ext.hs beginning

{-# LANGUAGE TypeFamilies, FunctionalDependencies, NoMonoLocalBinds #-}

{-# OPTIONS_GHC -XNoGADTs -XNoMultiParamTypeClasses #-}

and compiled with ghc -XGADTs.

1. Let an *extension priority level* be a natural number, such that a
   lower number implies "more important".

2. Let an *extension specification* be a place where an extension is
   specified. In our example, there are 6 extension specifications (3 in
   a LANGUAGE pragma, 2 in a OPTIONS_GHC pragma, and one in a
   command-line argument -XGADTs).

3. Each extension specification is associated with an extension
   *priority level*.

4. Each item in the list of extensions in LANGUAGE is assigned priority
   level 0.

5. As command-line options are processed, GHC maintains an extension
   priority level, which is assigned to each flag that enables (or
   disables) a language extension. This level starts out as 1. The new
   flag -extension-priority-level=N expects N to be a natural number
   literal and sets the extension priority level to N. To change the
   level multiple times within the command line, pass multiple
   -extension-priority-level=N flags. N is allowed to be 0. The
   OPTIONS_GHC/OPTIONS pragmas also start processing their extension
   specifications at level 1; the level can be changed with
   -extension-priority-level. (It is expected that users never pass
   -extension-priority-level; only tooling such as cabal will use this
   flag. Errors from GHC should take care not to mention it.)

..

   In our example, TypeFamilies, FunctionalDependencies, and
   NoMonoLocalBinds all have priority 0, while GADTs,
   NoMultiParamTypeClasses, and NoGADTs have priority level 1.

6. After all extension specifications are processed, each extension
   priority level will be associated with an unordered set of extension
   specifications. If there are any conflicts within a level, report an
   error and stop compilation. In our example, there is a conflict at
   level 1, and so we would reject with an error, saying that we don't
   know whether the user wants GADTs on or off.

..

   We then process the priority levels in decreasing order. For each
   priority level, we compute all implied extensions and enable or
   disable them. Then we look at the extension specifications at the
   current level and enable or disable extensions. In this way, implied
   extensions can be overridden by explicit request, but lower priority
   specifications take precedence over higher priority ones.

   Let's modify our example to remove the -XNoGADTs, so that the error
   above would not happen. Then we would do the following sequence:

1. Enable MonoLocalBinds, as implied by GADTs.

2. Enable GADTs and disable MultiParamTypeClasses.

3. Enable MonoLocalBinds (as implied by TypeFamilies) and
   MultiParamTypeClasses (as implied by FunctionalDependencies).

4. Enable TypeFamilies and FunctionalDependencies, and disable
   MonoLocalBinds.

..

   We are thus left with the following enabled: GADTs,
   MultiParamTypeClasses, TypeFamilies, and FunctionalDependencies. Note
   that we indeed enable MultiParamTypeClasses despite the priority-1
   NoMultiParamTypeClasses and disable MonoLocalBinds despite it being
   implied at both level 1 and 0.

SPJ: This is all very confusing.

-  I have no idea what -extension-priority-level is for

-  I think you intend that implied extensions are one level of priority
   down

-  You give a priority level to LANGUAGE flags but not to command line
   flags of OPTIONS_GHC flags.

-  I think the algorithm is:

   -  Collect all (Extension, Setting, Priority) triples

   -  For a given extension, take the highest priority triples; ignore
      the others

   -  Complain if they conflict.

I like the general idea, but I'm not sure if this change is worth the
complexity. It solves a problem that no one (as far as I know) has
reported.

RAE: That’s because it was unfinished! Read on.

2.5 Proposed Change Specification for .cabal Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Add a new setting to the library, executable, and test
   specifications: required-extensions are extensions that are in effect
   for all files in a project. If a file tries to countermand this (e.g.
   with required-extensions: MonoLocalBinds and {-# LANGUAGE
   NoMonoLocalBinds #-}), GHC will report an error.

Implementation: The required-extensions are passed to GHC preceded by
-extension-priority-level=0.

3.6 Effects
~~~~~~~~~~~

This design offers the following improvements over the status quo:

1. Order of extension specifications no longer matters. This means that
   future proposals in this chain do not have to think about how the new
   designs interact with ordering.

2. An author of a cabal file can now effectively prevent the use of an
   extension in any Haskell files in the project — even if those files
   try to enable the extension. This makes it easier for a company to
   monitor and control what extensions are used in a Haskell codebase.

3. A language edition (as defined in a later proposal) can effectively
   be incompatible with various extensions, by disabling them at
   priority 0.

The design here is intended to be backward compatible in most cases. It
handles the possibility that a programmer has implied an extension and
then reversed the implication (e.g. with -XTypeFamilies
-XNoMonoLocalBinds) due to the special handling of implied extensions.
The set of implied extensions can never be contradictory in itself, as
no extension is implied turned on by one extension and implied turned
off by another.

This design is not fully backward compatible, however: today a user
might write -XArrows -XNoArrows (or similar in a LANGUAGE pragma), and
this has a well-defined semantics. WIth this proposal, such a
specification would be rejected. RAE thinks this is an improvement.

This design also addresses some corner cases that exist today. For
example, if I have default-extensions in a cabal file and some
extensions specified by —-ghc-options in the command-line invocation of
cabal, which takes precedence. I don’t see this addressed in the cabal
documentation. This proposal defines the semantics of such an event.

3.7 Drawbacks
~~~~~~~~~~~~~

This makes the extension mechanism seem more complicated. However, I
argue that this complication is already there, but we just don’t see it.
The complication is in the fact that extensions can be specified in so
many different places, and a full understanding of the extensions
mechanism requires knowing in what order all these places are processed.
With this proposal, all that is simplified away. And the expectation is
that users know nothing more than “LANGUAGE takes priority over other
places, and contradictions are errors”. Simple!

3.8 Implementation thoughts
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It’s unclear at the time of writing how bad the backward incompatibility
would be. Maybe this affects many projects in the wild. Hopefully not —
because this would negatively affect only projects with contradictions
in their extension specifications.

It would not be hard to implement this proposal. And so we can implement this alongside the existing extension processing algorithm, and then see if the set of extensions implied by the new algorithm is different than the one implied by the old algorithm, or if any new errors arise. We can then make a final decision on whether to adopt this new plan.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Proposal 4: Semantic bundles
----------------------------

This proposal attempts to group extensions into *semantic bundles*,
which operate at a coarser granularity but correspond to *risks* and
*responsibilities* the programmer is taking on. It is a hope that this
characterization will be helpful to users and will rationalize the
interface to the language.

.. _motivation-2:

3.1. Motivation
~~~~~~~~~~~~~~~

Users of GHC should be able to decide how to configure GHC without deep
knowledge of its language design choices. When they do have to make
specific decisions (e.g. changing a language extension or warning flag),
these decisions should be easy to understand and with clear
ramifications.

For example, if a user writes x :: Just Int, they get a suggestion to
enable -XDataKinds. But the user is given no guidance about what
-XDataKinds means or the consequences of enabling it. (Note: the error
has an error code, and the Haskell Error Index could, and perhaps
should, explain the consequences.)

One possible ramification of enabling an extension is instability. For
example, a use of f :: Int %1-> Int might hint to add -XLinearTypes; the
user has no idea that this extension is unstable and that by enabling
it, they are opening themselves up for instability.

This proposal thus describes a way to help users declare that they wish
to work in a stable subset of Haskell.

To simplify users' lives, we want to build on the notion of a **language
edition**, such as -XGHC2024. The clear aspiration is that if a program
compiles with ghc-10.6 -XGHC2024 then it should compile with ghc-11.2
-XGHC2024. The language edition fixes the language, and future GHCs
should honor that language. (This aspiration is hard to meet today
because each GHC comes with its own base package; but that too will be
fixed in time.)

3.2. Why we have warnings
~~~~~~~~~~~~~~~~~~~~~~~~~

Given an input program (and an environment), GHC produces one of three
results: a successful (and silent) compilation, a failure with errors,
or a successful compilation with warnings. Why do we (and every other
compiler) have this third option? After all, if there are warnings,
clearly *something* is wrong – maybe the program will crash (or worse)
on execution. Yet we have warnings because they are useful during the
development cycle. That is, we often (always, basically) work on
incomplete code. There may be unused imports. Other parts of our project
might not yet be updated. We might have unused variables. When we're in
this intermediate state, it is nevertheless very useful for compilation
to succeed, so that we might run our code and experiment.

This characterization of warnings leads to two design conclusions:

1. GHC should succeed during compilation whenever possible (to avoid
   impeding development)

2. There should be no emitted warnings when releasing a project (to
   avoid releasing buggy code)

Conclusion (1) suggests that we've gotten the design wrong around
language extensions. If I say deriving Functor but do not have
-XDeriveFunctor, I shouldn't get an error; I should get a warning. This
would mean compilation can continue, I can get other warnings, and I can
even experiment with my running program. This would also mean that we
learn about all of the plethora of missing extensions all in one go,
rather than one at a time – because compilation isn't aborted after the
first missing extension.

Conclusion (2) suggests that e.g. cabal sdist should temporarily turn on
-Werror. This would guarantee that a released package has no warnings.
When we say cabal sdist, we're done in the iterative development cycle
and believe everything should work. Of course, if some warnings persist
and the programmer wishes to accept the warnings, that's fine: they can
disable the warnings in their .cabal file. (Perhaps there would be a new
sdist-options field so that users can continue to get the warnings
during their next development session.)

.. [1]
   The semantics also come with compilation outputs, but we’ll ignore
   that important detail in this document.

.. [2]
   Of course, Haskell isn’t completely pure, supporting unsafePerformIO
   and friends. The behavior of such functions is often important in
   Haskell programs. In thinking about stability, we should be attuned
   to possible changes here. If we identify any particular patterns of
   impure functions that we wish to guarantee future support for, we
   should document them.
