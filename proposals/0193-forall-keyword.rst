Make ``forall`` a keyword
=========================

.. author:: Richard Eisenberg
.. date-accepted:: 2019-02-14
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/merge_requests/363
.. implemented:: 8.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/193>`_.
.. contents::

Having accepted proposals for `dependent visible quantification`_
(`discussion <https://github.com/ghc-proposals/ghc-proposals/pull/81>`_) and
for the `dot type operator`_
(`discussion <https://github.com/ghc-proposals/ghc-proposals/pull/173>`_), we now have the potential for
situations where a type can change meaning depending on what extensions are enabled. This is unfortunate.
This proposal suggests that ``forall`` should be considered a keyword in types always, regardless
of extensions.

.. _`dependent visible quantification`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0035-forall-arrow.rst
.. _`dot type operator`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0039-dot-type-operator.rst

Motivation
------------
Consider these type signatures::

  f1 :: forall a. a -> a
  f2 :: forall a -> a -> a

What do they mean? It depends on what extensions are in force.

* ``f1`` is rejected in Haskell2010. But with ``-XTypeOperators`` enabled (and
  the `dot type operator`_ feature implemented), it means ``((.) (forall a) a) -> a``,
  where ``forall`` is an ordinary type variable. With ``-XExplicitForAll`` enabled,
  we get quantification over ``a``. Both interpretations are sensible
  and potentially well-kinded.

* ``f2`` is accepted in Haskell2010, with ``forall`` being understood as an ordinary
  type variable. But with `dependent visible quantification`_ in effect, that type signature
  quantifies over ``a`` and then expects an argument of type ``a``. (The dependent visible
  quantification proposal does not say what extension enables the feature. I am assuming it
  comes with ``-XExplicitForAll``.)

What's troublesome here is that a user might write one of these signatures, intending for quantification,
but forgetting to enable ``-XExplicitForAll``. Instead of getting a helpful error message asking that
they enable ``-XExplicitForAll``, they would get obscure type errors.

Proposed Change Specification
-----------------------------
The lexeme ``forall`` is understood to be a keyword in types, always. Similarly, the unicode variant
of ``forall`` would be understood as a keyword in types, always.

Effect and Interactions
-----------------------
* This proposal would potentially break some existing programs, if any program uses a type variable
  spelled ``forall``. These would be easily fixed, simply by using a different type variable name.
  This proposal author doubts the existence of any such programs in the wild.

* There is precedent for this kind of syntax-stealing behavior.

  - Several lexemes are already pseudo-keywords in types. These include ``family`` and ``role``.
    With ``-XTypeOperators`` (but no other extensions), one might assume that you could write
    ``type family + x = Either family x`` to make ``+`` a synonym for ``Either``. Yet GHC rejects
    this because it expects ``type family`` to begin a type family declaration.

  - If you have a term-level function named ``forall``, you cannot write ``RULES`` for it. For example,
    if we have ``forall x = x``, you might want ``{-# RULES "forall" forall = id #-}``. Yet GHC
    interprets the word ``forall`` as introducing term-level quantified variables in a rewrite rule.
    This syntax is not allowed.

  Admittedly, these examples require non-standardized syntax, whereas the current proposal interferes
  with the standard.

* Error messages would improve with this change, as GHC would be able to unambiguously detect when
  the user wants quantification.

* This proposal does *not* interact meaningfully with the ``-XExplicitForAll``. It does *not* turn
  the extension on automatically. It simply reserves the lexeme ``forall``. This means that any
  program with the lexeme ``forall`` appearing in a type will be in error if ``-XExplicitForAll``
  is not enabled.

Costs and Drawbacks
-------------------
* The major drawback is that it moves us further from the standard. However, this particular deviation
  seems slight.

Alternatives
------------
* **Do nothing**. The status quo includes no programs that are ambiguous to GHC (or other tooling), because
  these tools can always know what extensions are in effect. Yet, programs may be confusing or ambiguous
  to poor humans, who might not always know what extensions are in effect.

* **Hide this feature behind an extension**. We could introduce ``-XKeywordForall`` that enables this new
  behavior. In order to satisfy the needs in the Motivation, this extension would have to be enabled by default.
  It should also, logically, be disabled by ``-XHaskell2010`` and ``-XHaskell98``. However, it is now
  common practice to specify a "default language" in ``.cabal`` files, and ``cabal`` builds files with one
  of these extensions specified. So, if we did this, any users compiling via ``cabal`` would not reap the
  benefits of the better error messages this proposal would enable.

* **Make ``forall`` a keyword in all contexts**. Should ``forall`` be a keyword everywhere? This alternative
  is more future-compatible with the possibility of dependent types. Yet it would break known programs
  (e.g., Idris, which has a function named ``forall``). I'm open to this possibility, but in the end,
  I currently think it's better to just do this in types, for now.

Unresolved Questions
--------------------
None at this time.

Implementation Plan
-------------------
This would likely be implemented alongside the implementations for either `dependent visible quantification`_
or the `dot type operator`_.
