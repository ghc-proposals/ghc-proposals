Integrate language extensions and warnings
===========================================

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header::
.. sectnum::
.. contents::



This proposal unifies language extensions and warnings into one mechanism, thus simplifying GHC's user interface while offering more expressiveness, including allowing the language edition to specify a default set of warnings.

It is dicussed at `this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/620>`_.

Motivation
-------------
This proposal discusses both language extensions and warning flags. These features may seem distinct, but in fact (see `this discussion <https://github.com/ghc-proposals/ghc-proposals/issues/615>`_:
many extensions could have been implemented as warnings instead (such as -XMultiParamTypeClasses, -XDeriveFunctor or -XAllowAmbiguousTypes)
many warnings could have been implemented as extensions (such as -Wname-shadowing)
It is something of a historical accident that one thing has ended up in "warnings" while another has ended up in "language extensions".  However although language extensions and warning flags overlap in functionality, they also have some distinctively different functionality:
Language extensions can change the behaviour of a program; warnings cannot.
Warnings can warn.  Language extensions cannot: they can only error.

This proposal seeks to unify the features of language extensions and warnings, and then present a simplified user interface over the unified feature.

An important direction of travel is that we want a **language edition**, such as ``-XGHC2021``, to fix a particular language specification.
In particular, our aspirational goal is that if a program compiles with ``ghc-9.8 -XGHC2021`` then it should also compile with ``ghc-9.10 -XGHC2021``.  This proposal moves us towards that goal, but does not fully accomplish it.

*"Good" and "bad"*.  In general, language extensions often enable a good new behaviour, while disabling a warning enables a bad new behaviour. But "good" and "bad" are clearly subjective, and we've already gotten this wrong a few times. (For example, ``-XDatatypeContexts`` allows a bad behaviour and ``-Wno-unticked-promoted-constructor`` allows a good one, at least in Richard's opinion.)  So we propose the following principle:

  **Principle of neutrality.**  GHC itself should not have an opinion about "good" and "bad", for example by categorising one as a language extension and the other as a warning flag.  Instead, a language edition should express that choice.

Not in scope: There are a few non-language-extension flags that affect the semantics of the program, such as ``-fdefer-type-errors`` and ``-fpedantic-bottoms``. They could be included within the overall framework described here, there is no great need to consider them now and will only serve to complicate the debate.



Background information
-----------------------
This section contains some definitions and other background information about the status quo.

* **Order of specification**.  Extensions are processed left-to-right in the following order:

  * ``default-extensions`` field of Cabal file
  * ``--ghc-options`` command-line argument to ``cabal``
  * ``-X`` Command-line argments to ``ghc``
  * ``LANGUAGE`` pragma or ``OPTIONS_GHC`` pragma in the module itself

* **Implications**.  A language extension may imply others.  For example ``-XTypeFamilyDependencies`` implies ``-XTypeFamilies``.

* **Conservative and non-conservative extensions**.   A conservative extension adds a feature to the language, without affecting the meaning of any existing program; a non-conservative extension may change the meaning of a program.

* **Non-negatable extensions**. Some language extensions are non-negatable; for example, you cannot say ``-XNoSafe``.  This is the case because someone might want to ensure that all files are compiled Safely, and an individual module should not be able to opt out.

* **Incompatible extensions**.  Two language extensions can be mutually incompatible.  For example ``-XSafe`` and ``-XUnsafe``.

* **Language editions**.  A language edition, like ``-XGHC2024``, simply implies a bunch of other extensions.  However language-edition extensions are treated slightly differently to other extensions:

  * Any particular version of GHC comes with its own "default language edition". For example, GHC 9.8 has default language edition ``GHC2021``.
  * This default is overridden if a language edition is specified explicitly, e.g. ``-XGHC2010``.
  * The language-edition extension (if present) is always treated as if it came first.  Thus, ``-XNoPatternGuards -XGHC2021`` and ``-XGHC2021 -XNoPatternGuards`` are equivalent.


Proposed Change Specification
-----------------------------

We propose the following changes:

1. **Extensions can warn**. For any given language extension, say GADTs:

   * ``-XGADTs`` allows GADTs.
   * ``-XWarnGADTs`` allows GADTs, but warns if they are used.
   * ``-XNoGADTs`` errors on a use of GADTs.

   *Implied extensions*: when a language extension implies others, its warning form has a similar dependency.
   For example, ``-XTypeFamilyDependencies`` implies ``-XTypeFamilies``, and hence ``-XWarnTypeFamilyDependencies`` implies ``-XWarnTypeFamilies``.

2. **Non-warnable extensions**.  A few extensions cannot warn; the
   *non-warnable extensions*.  For example, you are not allowed to say ``-XWarnAlternativeLayoutRule``.

   The *vast majority* of extensions are warnable; in particular, all conservative extensions are warnable.

   Moreover, most existing non-conservative extensions could usefully be made warnable, although it might take extra work to do so.  Examples:

   * ``-XWarnMonomorphismRestriction``: we already have a warning when this "bites", and it did indeed take extra work.
   *  ``-XWarnRebindableSyntax``: this would be new, but we would warn on every use of a rebindable construct that does not refer to the appropriate name from base.
   * ``-XWarnDeepSubsumption``: would warn when deep subsumption was actually used, and simple subsumption would not have sufficed.

3. **Warnings are just extensions**. Almost all current warnings, such as ``-Wname-shadowing``, become a language extension ``-XWarnNameShadowing``, with the obvious algorithmic name conversion.

   * Back-compat: all existing warning-flag syntax remains (perhaps indefinitely); but almost all are re-interpreted as a synonym for language extension flags.   For example ``-Wname-shadowing`` is a synonym for ``-XWarnNameShadowing``.
   * We say that "almost all" current warnings can become extensions, because a few warnings are extra-linguistic, such as ``-Winconsistent-flags``.

4. **Clarifying extension negation**.  For extensions that imply others, GHC's manual does not specify what happes if they are switched off.  For example, ``-XGADTs`` implies ``-XMonoLocalBinds``; so does ``-XNoGADTs`` imply ``-XNoMonoLocalBinds``?  In the current implementation, the answer is "no"; but we propose to make the answer "yes", so that the semantics lines up with warnings (item 1 above).  That is, if ``-XWombat`` implies ``-XSquirrel`` then ``-XNoWombat`` implies ``-XNoSquirrel``.

   There is one exceptional case. Currrently ``-XRebindableSyntax`` implies ``-XNoImplicitPrelude``.  So what does ``-XNoRebindableSyntax`` mean?  Presumbly it just restores ``-XImplicitPrelude``.

5. **Warning categories**.  `Accepted GHC proposal 541 on warning categories <https://github.com/adamgundry/ghc-proposals/blob/tweak-warning-category-syntax/proposals/0541-warning-pragmas-with-categories.rst>`_ introduces so-called *warning categories*, allowing you to say; ::

       {-# WARNING in "x-partial" tail "This is a partial function" #-}

   and then enable/disable the warning with ``-Wx-partial`` and ``-Wno-x-partial``.  We propose to adapt proposal 541 for the new scheme, as follows:

   * The pragma would look like ::

        {-# WARNING in "U-Partial" tail "This is a partial function" #-}

   * Warning enabled with ``-XWarnU-Partial``.
   * Warning disabled (partial functions allowed) with ``-XU-Partial``.
   * Warning is an error (partial functions disallowed) with ``-XNoU-Partial``.

   The prefix ``U-`` (for "user") seems better than ``X-``, to separate user-defined categories from other built-in extensions, because ``X`` is already being used to signal language extensions, e.g. ``-XGADTs``.

Extensions are processed in order, as today.  (Richard has a separate proposal in preparation, to make extensions order-independent.)

The meanings of ``-W`` and ``-Wall`` continue to be "enable all recommended warnings" and "enable all reasonable warnings", just as in GHC today.
These lists may therefore vary with GHC version.  That is, they are not fixed by a language edition,
so a later GHC version may warn about things that an earlier GHC version does not.  If you use (say) ``-XGHC2021`` you will, of course, get all the warnings that are in ``-XGHC2021``. You would only use ``-Wall`` to get *extra* ones, meaning "warn about anything smelly, including stuff not warned about in GHC2021".

We also propose that we become more systematic about specifying extension properties. Specifically, for each language extension X you should specify:

* **Warnable**. Whether or not X is warnable.
* **Negatable**. Whether or not X is negatable
* **Compatibility**. List the other extensions wrt which X is incompatible.
* **Implications**.  List which other extensions are implied by X.

The change here is mostly one of perspective: rather than these
properties being ad-hoc, one-off behaviours of particular extensions,
they are systematically specified for each extension.

Consequences and examples
--------------------------

This design has the following happy consequences.

* The tension between warnings and language extensions disappears.  For example, at the top of a module we can write::

	{-# LANGUAGE GADTs, NoIncompletePatterns #-}

  rather than::

	{-# LANGUAGE GADTs #-}
	{-# OPTIONS_GHC -Werror=incomplete-patterns #-}


* A language edition fixes a set of warnings, unlike the situation today.  For example, ``-XGHC2024`` could include warnings about incomplete patterns.


* A language edition could choose to error on what is today a warning, such as ``-XNoMissingMethods``.   (Today you can say ``-Werror=missing-methods``, but you can't do that in a language edition.)   An opt-in change of this nature is the purpose of `GHC Proposal 571 <https://github.com/ghc-proposals/ghc-proposals/pull/571>`_.

* A language edition could choose to allow, but warn about, the use of a language extension, e.g ``-XDeriveFunctor``.  That is not possible today.

* We could add a non-warnable non-negatable language extension ``-XStable`` that is defined to be incompatible with all Experimental extensions, but otherwise does nothing at all.   Thus, adding ``-XStable`` will ensure that no experimental extensions can be used, which is (close to) the goal of `GHC Proposal 617 <https://github.com/ghc-proposals/ghc-proposals/pull/617>`_.

* A language edition could, if we wanted, choose to be incompatible with some experimental extension (e.g. ``-XLinearTypes``), or even with all experimental extensions (via ``-XStable``).

* ``-Wcompat`` currently turns on warnings that will be enabled by default in the future, but remain off in normal compilations for the time being.  It can continue to do so.  But under this proposal, warnings "enabled by default in the future" will simply be part of the default language edition. 

* Today language editions are not mutually incompatible -- you can say ``-XGHC2010 -XGHC2021`` without complaint.  (The rightmost one "wins".)  They really should be incompatible, and that would be an easy change with this proposal.


Unresolved questions
----------------------

* Currently we have two long lists: one for extensions and one for warnings.  Under this proposal we would have one list, but twice as long.  Maybe that woul feel more uniform; but it might also feel intimidating.

* Will we end up supporting something for longer?   Eg ``-Wmonad-fail``.  It lived only for a few releases, it warned you if you didn't write your code in a forward compatible way.
  * Policy idea: Support the past three language editions, but drop support for earlier ones.

* Currently dropping warnings is seen as no-fuss-required; but if warnings were language extensions, we'd need to treat them much more carefully.
