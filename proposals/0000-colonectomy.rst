=============
 Colonectomy
=============

.. proposal-number::
.. trac-ticket:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/118>`_.
.. sectnum::
.. contents::

Despite its name and deliberate timing, this is a completely serious proposal. I am open to changing both the name of the proposal and the name of the flags.

Haskell is the only language in modern use that employs a double colon (``::``) to separate a term from its type signature. All other languages (Agda, Idris, ML, Coq, Scala, etc.) use a single colon (``:``). Haskell's use of the double colon seems to be an historical artifact dating from a time in which list cons (which is represented by ``:`` in Haskell) was perhaps considered more important or common than a type signature. According to `A History of Haskell: Being Lazy With Class <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf>`_:

  We adopted from Miranda the convention that data constructors are
  capitalized while variables are not, and added a similar convention
  for infix constructors, which in Haskell must start with a colon. The
  latter convention was chosen for consistency with our use (adopted
  from SASL, KRC, and Miranda) of a single colon : for the list
  "cons" operator. (The choice of ":" for cons and "::" for type
  signatures, by the way, was a hotly contested issue (ML does the
  opposite) and remains controversial to this day.)

Haskell has evolved through improvements to the core language and numerous extensions, and the Haskell community embraces change, yet this fundamental aesthetic problem has not yet been addressed. In this proposal I argue that a fix is both technically simple and provides a natural migration path toward a future in which the double colon for a type signature will be only a quaint memory.

Throughout this proposal I will use OCC (Old Colon Convention) to refer the Haskell's current use of ``:`` for list cons and ``::`` for type signature, and NCC (New Colon Convention) for the proposed reversal in which ``:`` is used for type signature and ``::`` for list cons. Because in the important case of type signature the double colon is being replaced by a single colon, I have named this proposal *colonectomy*.


Motivation
==========
Haskell is the only language in contemporary use to follow the OCC. Every other language follows the NCC. This causes some brief confusion for newcomers, with the inevitable question of why the double colon for a type signature, for which there is no answer except that's how it was historically. For experienced programmers switching between languages, there is a small amount of mental overhead to going back and forth between the two conventions. Personally after writing Agda code for a while I find myself writing type signatures in Haskell with a single colon, only to be reminded when I get a compiler error.

In addition all research papers on type theory (except those specifically writing about Haskell) follow the NCC.

Aside from uniformity with other languages and theoretical papers, it is good design practice to use the shorter, simpler construct in the more common use case; this also saves time typing (and disc space!), even if it is a tiny amount. In the first edition of `The C Programming Language <https://archive.org/details/TheCProgrammingLanguageFirstEdition>`_, Kernighan and Ritchie state regarding ``=`` and ``==`` (page 17):

  Since assignment is about twice as frequent as equality testing in typical C programs,
  it's appropriate that the operator be half as long.

In modern Haskell programs, even though the language supports type inference as a major feature, it is important as documentation to include at least top-level type signatures and often type signatures of variables declared within functions. This also ensures the program is doing what the programmer expected. In contrast list usage has decreased as lists have been replaced by more performant data structures. Although I have not attempted to measure this, I would assert that the ratio of type signatures to list cons in a typical Haskell program is far greater than two to one.

It is notable that both Agda and Idris, dependently typed functional languages inspired by and implemented in Haskell, chose to use NCC. Dependent types are now being added to Haskell, and it is time to bring Haskell's syntax in line with these more modern languages as well.


Proposed Change Specification
=============================
The change is very simple and straightforward, and provides a clear and natural migration path from OCC to NCC.

There are two issues to address:

1. Parsing individual files according to OCC or NCC as appropriate.
2. Displaying messages to the user in accordance with OCC or NCC as desired.

Parse
-----

A new language extension ``-XColonectomy`` can be added to ``libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs`` with ``compiler/main/DynFlags.hs`` updated appropriately. These are both one line changes.

The remainder of the changes are in ``compiler/parser/Lexer.x``. Aside from a few lines to define ``colonectomyEnabled``, the only changes necessary are the following:

.. code-block:: haskell

  sym :: (FastString -> Token) -> Action
  sym con span buf len =
    case lookupUFM reservedSymsFM fs of
      Just (keyword, exts) -> do
        extsEnabled <- extension exts
        let !tk | extsEnabled = keyword
                | otherwise   = con fs
        swapColon <- extension colonectomyEnabled -- NEW CODE
        let !tk' | swapColon = colonectomy tk     -- NEW CODE
                 | otherwise = tk                 -- NEW CODE
        return $ L span tk'
      Nothing ->
        return $ L span $! con fs
    where
      !fs = lexemeToFastString buf len

  -- Swap : and ::
  colonectomy :: Token -> Token
  colonectomy ITcolon      = ITdcolon UnicodeSyntax -- need to maintain char count?
  colonectomy (ITdcolon _) = ITcolon
  colonectomy token        = token

This has been implemented on a private branch of the latest GHC. With the above modifications to GHC the following example compiles correctly. Note that list cons promoted to type level works fine.

.. code-block:: haskell

  {-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
  {-# LANGUAGE Colonectomy #-}

  mycons : a -> [a] -> [a]
  mycons x xs = x :: xs

  data HList : [*] -> * where
    HNil  : HList '[]
    HCons : a -> HList t -> HList (a ':: t)

Display
-------

To display message to the user following NCC, add a dynamic flag ``-fprint-colonectomy`` (internally represented as ``Opt_PrintColonectomy``) to ``compiler/main/DynFlags.hs`` (a two-line change). Also add the function:

.. code-block:: haskell

  performColonectomy :: DynFlags -> Bool
  performColonectomy = gopt Opt_PrintColonectomy

and export it through ``compiler/main/DynFlags.hs-boot``.

Then in the file ``compiler/utils/Outputable.hs``, add the following function:

.. code-block:: haskell

  -- If Opt_PrintColonectomy is not set:
  --   Outputs :  if origColon == true
  --           :: if origColon == false
  -- If Opt_PrintColonectomy is set:
  --   Outputs :: if origColon == true
  --           :  if origColon == false
  colonectomy :: Bool -> SDoc
  colonectomy origColon = sdocWithDynFlags $ \dflags ->
    if performColonectomy dflags
    then if origColon then dcolon else colon
    else if origColon then  colon else dcolon
    where
      colon  = docToSDoc $ Pretty.colon
      dcolon = unicodeSyntax (char '∷') (docToSDoc $ Pretty.text "::")
  
Finally replace the definitions of ``colon`` and ``dcolon`` (whose original definitions are in the ``where`` clause ``colonectomy``) with the following.

.. code-block:: haskell

  colon  = colonectomy True
  dcolon = colonectomy False

Here is an example of the effect of the change (note that unicode and explicit forall are also set):

.. code-block:: haskell

  *Main> :t HCons
  HCons ∷ ∀ {a} {t ∷ [★]}. a → HList t → HList ((':) ★ a t)

  *Main> :set -fprint-colonectomy
  *Main> :t HCons
  HCons : ∀ {a} {t : [★]}. a → HList t → HList ((':) ★ a t)

Note that ``':`` should be printed as ``'::``. See `Unresolved Questions`_.

Effect and Interactions
=======================

There should be no interaction with any other language or compiler features.

The proposed change enables a migration path to a future in which NCC is the standard for Haskell. The use of the language extension ``-XColonectomy`` on a per-file basis allows programmers to write new code using the NCC if desired, and it will interoperate seamlessly with other files using either the OCC or NCC. Existing files using OCC can be converted to NCC at any time, and it would help to write a tool to do this automatically. It is envisioned that within a few years the GHC codebase itself, libraries, and standard packages will all have been migrated to NCC. Papers about Haskell meanwhile can use the NCC with a passing mention of the convention, as Hinze has already done in his papers (see `Costs and Drawbacks`_).

At some point it may then make sense to introduce another extension, perhaps with a boring name like ``OldColonConvention``, to denote files still following the historical OCC. At the point of some future major release the NCC could become the default, no longer requiring an extension, and only ``OldColonConvention`` retained as an extension for those who prefer to live in the past.

As for the flag ``-fprint-colonectomy``, this could also be made the default at some point and a flag ``-fprint-oldcolonconvention`` added to revert to the historical output behavior.


Costs and Drawbacks
===================

The primary argument against this proposal would be potential confusion caused by the use of OCC in some places and NCC in others. I argue that there should be no genuine confusion. Type signatures and list cons are sufficiently distinct that humans can tell them apart regardless of the exact punctuation used. Similarly the parser stage can distinguish the two (this could be taken advantage of as noted in `Alternatives`_), although it is simpler to direct the lexer to make the switch if necessary. The presence or absence of the ``-XColonectomy`` extension at the start of the file makes it clear which convention the file will follow.

There is also the issue that almost all existing written documentation of Haskell uses ``::`` for type signatures. Again this should cause no problem for modern readers. There are already examples of research papers using Haskell in which the notation has been corrected, such as Ralf Hinze's works. In `Adjoint folds and unfolds--An extended study <https://www.cs.ox.ac.uk/ralf.hinze/publications/SCP-78-11.pdf>`_ he specifically notes:

  In the Haskell code, the conventions of the language are adhered to,
  with one notable exception: I have taken the liberty to typeset '::' as ':'....

It is possible there would be a small performance penalty for checking the flags, but this should be negligible.

Copying and pasting from older code, either on web pages or existing codebases that have not yet been converted, takes more time as the colon convention must be swapped. Again a tool could help with this.

Third-party tools outside the Haskell ecosystem such as syntax highlighting would have to be updated, and as they may follow fairly primitive rules it may not be easy for them to distinguish files using OCC and NCC.

Alternatives
============

The main alternative is to simply do nothing and maintain the status quo. The question is simply whether or not we want to address this issue. There are certainly short-term costs to doing something now, but I believe long-term gain of cleaner syntax which is consistent with other modern languages is worth it. In any case now seems a good time to at least make a decision.

An intriguing alternative is to be flexible and allow either ``:`` or ``::`` in either the type signature or list cons context, since the two can be distinguished at the parser stage. In this case the lexer would pass the tokens as written and the parser would make the appropriate fixes if necessary. The advantage is that this requires no flag whatsoever for parsing (one would still want the flag ``-fprint-colonectomy`` for displayed output), but it defeats the purpose of moving Haskell toward the NCC as there would be no requirement to use that convention uniformly.


Unresolved Questions
====================

1. I still need to add code to ensure that NCC list cons ``::`` and promoted list cons ``'::`` are displayed correctly.
2. In replacing ``::`` with ``:`` (converting list cons NCC to OCC to work with GHC's internal code), make sure the positions and length are updated correctly. Note that we can replace ``:`` with unicode ``∷`` and maintain character count, although perhaps this would cause problems on systems not supporting unicode (are there any at this point?).
3. Why is ``-fprint-colonectomy`` not in the ``GHCi-specific dynamic flag settings`` section with the other flags there even though I defined it in the identical manner as the others using ``flagSpec``?

.. code-block:: haskell
   
  *Main> :set
  options currently set: none.
  base language is: Haskell2010
  with the following modifiers:
    -XNoDatatypeContexts
    -XKindSignatures
    -XNondecreasingIndentation
    -XUnicodeSyntax
  GHCi-specific dynamic flag settings:
    -fprint-explicit-foralls
    -fprint-explicit-kinds
    -fprint-unicode-syntax
  other dynamic, non-language, flag settings:
    -fexternal-dynamic-refs
    -fignore-optim-changes
    -fignore-hpc-changes
    -fimplicit-import-qualified
    -fprint-colonectomy
  warning settings:
   


Implementation Plan
===================

I volunteer to implement, test, and document this extension.
