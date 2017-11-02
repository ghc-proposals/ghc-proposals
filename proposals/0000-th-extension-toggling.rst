.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/88>`_.

.. contents::

Locally toggle language extensions with Template Haskell
========================================================

Using Template Haskell to splice in code that requires the use of language extensions can be extremely painful, as GHC requires the module which contains the splice to enable any extension that the spliced-in code makes use of. Rather than shifting this burden onto end-users of Template Haskell code, this proposal introduces a way to have Template Haskell locally enable or disable extensions when compiling spliced code so that users don't have to do this themselves.

Motivation
------------
Template Haskell's current design often forces those who use it to enable dozens of language extensions in order to successfully splice certain things. As a motivating example, consider this example usage of the ``singletons`` library: ::

    {-# LANGUAGE TemplateHaskell #-}
    import Data.Singletons.TH

    $(singletons [d| data Unit = MkUnit |])

Intuitively, this program should only require the ``TemplateHaskell`` extension. But alas, GHC will scream quite loudly if you attempt to compile that program, as the code which that Template Haskell splice *generates* requires several dozen additional extensions to compile, including ``TypeInType``, ``ScopedTypeVariables``, ``ExistentialQuantification``, and more (see `here <https://github.com/goldfirere/singletons/tree/d4c522708e9ac0bf0399537ea718291d3dc90583#compatibility>`_ for a full list). This is the subject of `Trac #8510 <https://ghc.haskell.org/trac/ghc/ticket/8510>`_.

What's worse, GHC usually doesn't tell you all of the extensions you need to enable all at once, so one inevitably winds up in a cycle of:

1. Compile the (broken) file.
2. See what language extensions GHC recommends enabling in the resulting error messages.
3. Enable those extensions.
4. Repeat.

This isn't fun, and moreover, it shouldn't be necessary. From the user's perspective, what the ``singletons`` function does is its own business, so it ought to be able to manage its own language extension affairs. This proposal would grant Template Haskell the ability to do just that.

Proposed Change Specification
-----------------------------
The user-facing side of this change would involve the addition of one new constructor to each of four various AST forms in the ``template-haskell`` library: ::

    data Exp -- Expressions
      = ...
      | WithExtsE [OnOff Extension] Exp

    data Type -- Types
      = ...
      | WithExtsT [OnOff Extension] Type

    data Pat -- Patterns
      = ...
      | WithExtsP [OnOff Extension] Pat

    data Dec -- Declarations
      = ...
      | WithExtsD [OnOff Extension] [Dec]

Here, the ``Extension`` type, which comes from ``GHC.LanguageExtensions``, is an enumeration of all of GHC's language extensions. The ``OnOff`` data type (which currently lives internally inthe GHC API, but would be exposed publicly with this proposal) is defined as: ::

    data OnOff a
      = On a
      | Off a

Where the ``On`` constructor is interpreted to mean "please enable this locally", and the ``Off`` constructor is interpreted to mean "please disable this locally".

One might ask why this propsal only adds ``WithExts*`` forms for ``Exp``, ``Type``, ``Pat``, and ``Dec``, and not other AST forms. The primary reason is that these are the four varities of AST forms that can be quoted in Template Haskell (through the ``[e| ... |]``, ``[t| ... |]``, ``[p| ... |]``, and ``[d| ... |]`` forms, respectively) and spliced (through ``$(...)``), so ``WithExts*`` is most likely to be useful in these scenarios. Of course, one could conceivably have ``WithExts*`` constructors for other AST forms—see the "Unresolved questions" section for further discussion.

Here is an example of how ``WithExtsD`` might be used: ::

    {-# LANGUAGE TemplateHaskell #-}

    $(pure [ WithExtsD [On GADTs, On KindSignatures]
               [d| data Foo :: * -> * where
                     MkFoo :: a -> Foo a |]
           ])

This will splice in the ``Foo`` data type, and critically, only the ``TemplateHaskell`` language extension needs to be enabled at the top of the module. Note that ``WithExtsD`` toggles on the ``GADTs`` and ``KindSignatures`` language extensions locally, so when ``Foo`` passes through GHC's renamer, it will behave as if ``GADTs`` and ``KindSignatures`` are enabled.

How will this work internally? Just as new constructors are added to various ``template-haskell`` ASTs, corresponding constructors must also be added to GHC's internal source ASTs. That is, we must add: ::

    data HsExpr p
      = ...
      | HsWithExtsE [OnOff Extension] (LHsExpr p)

    data HsType p
      = ...
      | HsWithExtsT [OnOff Extension] (LHsType p)

    data Pat p
      = ...
      | WithExtsPat [OnOff Extension] (LPat p)

    data HsDecl id
      = ...
      | WithExtsD [OnOff Extension] [LHsDecl id]

These ASTs are manipulated during four important compiler passes: parsing*, renaming, typechecking, and desugaring. Importantly, the vast majority of language extensions only affect these passes! (There are some oddballs like ``StaticPointers`` which affect later passes, but they make up a small minority of extensions.) Therefore, by tracking language extensions in these AST nodes, we can easily determine which things need to be toggled on or off during each of these compiler passes.

Note that I put an asterisk* after "parsing" because while there are some extensions that do affect how GHC lexes/parses source code (``MagicHash`` comes to mind, for instance), we won't be able to toggle any language extensions locally during parsing with the techniques in this proposal. The reason is because Template Haskell quotes produce ASTs *post-parsing*. In other words, the following code would not be accepted by GHC: ::

    {-# LANGUAGE TemplateHaskell #-}

    $(pure [ WithExtsD [On MagicHash]
               [d| f :: Int# -> Int#
                   f x = x |]
           ])

This is because GHC must first parse all of the source code first (including the bits within ``[d| ... |]``) before it can process the Template Haskell splice, so we are powerless to affect parsing with this technique. Nevertheless, I don't envision this being a huge problem in practice, since it's quite easy to work around the issue by writing an explicit TH AST instead of a quote, and language extensions that affect renaming/typechecking/desugaring far outnumber those that affect parsing.

Note that the ``[OnOff Extension]`` lists have a left-to-right semantics. That is, in the following code: ::

    {-# LANGUAGE TemplateHaskell #-}

    $(pure [ WithExtsD [Off DataKinds, On DataKinds]
               [d| f :: Proxy True
                   f = Proxy ]
           ])

Within the ``[d| ... |]`` quote, GHC will have ``DataKinds`` enabled. That is because before GHC performs any compiler pass over a ``WithExts*`` constructor, it first processes the language extensions from left to right, toggling each one it sees. So before renaming ``f``, GHC will locally disable ``DataKinds`` (due to ``Off DataKinds``) and then immediately enable ``DataKinds`` (due to ``On DataKinds``). If the language extensions had been given in the reverse order (``[On DataKinds, Off DataKinds]``), then GHC would have rejected the program, as the last thing GHC would do before renaming ``f`` is disable ``DataKinds``, which is needed for the ``Proxy True`` type.

Effect and Interactions
-----------------------
For the most part, this change would be orthogonal to other GHC features, as the internal changes are relegated to extra AST constructors which, aside from their ability to toggle language extensions, have no additional semantics, so there shouldn't be too many surprises in that department. Moreover, one has to opt in to using this feature with Template Haskell, so most Haskell programs should be unaffected by this.

There is something of an open question about how each language extension should behave on a "local" basis. For extensions like ``DataKinds``, it's not so difficult to imagine how they would behave locally, as ``DataKinds`` operates on a per-promoted-type basis. For extensions like ``Safe`` or ``Trustworthy``, this is perhaps less clear (see the "Unresolved questions" section for more details).

Costs and Drawbacks
-------------------
This would be a rather heavy change to GHC's source ASTs, as we'd need a new constructor for each of ``HsExpr``, ``HsType``, ``Pat``, and ``HsDecl``, plus corresponding changes to Template Haskell. Moreover, since these new AST forms can be appear anywhere post-splicing, we'd need to add quite a bit of plumbing to accommodate these new forms. (This is perhaps no different than adding any other AST form, I suppose.)

Alternatives
------------
Instead of modifying ASTs to accomplish this, one could imagine adding a new class method to ``Quasi``: ::

   class Quasi q where
     ...
     qWithExts :: [OnOff Extension] -> q a -> q a

Where ``qWithExts exts q`` indicates that when ``q`` is renamed, typechecked, and desugared (post-splicing), the extensions in ``exts`` will be toggled. If this were possible, it would be a far more flexible solution, since we wouldn't need to change ASTs at all (and indeed, this would be applicable to *any* computation which lives in ``Quasi``, and not just expressions, types, patterns, and declarations). However, I have not been successful in implementing such an idea, and I am doubtful that the staging of it all even makes this idea feasible.

One of the problems that this is addressing (being able to toggle language extensions at a finer granularity) would almost certainly be better addressed by a solution outside of Template Haskell, which comes with its own set of downsides. But no one seems to have a particular (non-TH) syntax in mind in `the discussion in Trac #602 <https://ghc.haskell.org/trac/ghc/ticket/602>`_, so until that time comes, I believe we should have *some* kind of solution, and this happens to be one.

Unresolved questions
--------------------
Currently, this proposal only extends to toggling language extensions at the expression, type, pattern, and declaration level. Would users desire further control and want to be able to toggle extensions at other granularities? (For instance, at the type variable binder level, the case alternative level, etc.)

This proposal currently only grants the ability to toggle language extensions, and not other GHC flags (e.g., ``-Wincomplete-patterns``). Should we include other GHC flags under the scope of this proposal as well? If so, how would this affect the Template Haskell API? Currently, we have AST forms that take ``[OnOff Extension]`` arguments—would we need some other data type that is richer than ``Extension`` if other sorts of flags were allowed? Something like this, perhaps? ::

    data GhcFlag =
        Extension Extension
      | Option Option -- for -Wincomplete-patterns, etc.

(If we did pursue this option, we'd need to expose a datatype in the ``GHC.*`` namespace which reflects all of the option flags that GHC currently uses, as currently there's only ``GHC.LanguageExtensions`` for language extension-specific flags.)

Does every language extension have a "local" semantics? For example, the ``Safe``/``Trustworthy`` extensions currently operate on a per-module basis. (I haven't tested this theory out, but my suspicion is that if one were to attempt to use ``Safe``/``Trustworthy`` in a local fashion with the techniques in this proposal, that they wouldn't have any effect.) Or would it be acceptable for certain language extensions to not have any local semantics at all?

Implementation Plan
-------------------
I volunteer to implement. I currently have a prototype implementation of these ideas `here <https://github.com/RyanGlScott/ghc/commit/2db8e9423e7f5b930922ba5f0261b44dab32a240>`_. This prototype only contains ``WithExtsE`` (for expressions) at the moment, but I imagine the amount of effort needed to add ``WithExtsT`` and ``WithExtsP`` (for types and patterns, respectively) would be similar. (Adding ``WithExtsD`` would be slightly more involved since GHC awkwardly represents top-level declarations in the source AST, but I believe that this difficulty could be overcome with enough elbow grease.)
