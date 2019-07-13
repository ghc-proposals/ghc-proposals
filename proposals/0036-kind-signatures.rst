Top-level kind signatures
=========================

.. proposal-number:: 36
.. author:: Richard Eisenberg
.. date-accepted:: 2018-09-30
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/54>`_.
.. sectnum::
   :start: 36
.. contents::


This proposal adds *top-level kind signatures* allowing users to declare the kind of
type-level declarations introduced with ``type``, ``data``, ``newtype``, or ``class``.

For example::

  type MonoTagged :: Type -> Type -> Type
  data MonoTagged t x = MonoTagged x

  type Id :: forall k. k -> k
  type family Id x where
    Id x = x

  type C :: (k -> Type) -> k -> Constraint
  class C a b where
    f :: a b

  type TypeRep :: forall k. k -> Type
  data TypeRep a where
    TyInt   :: TypeRep Int
    TyMaybe :: TypeRep Maybe
    TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

Declarations that have a top-level kind signature (with no wildcards)
can use polymorphic recursion; declarations
without such signatures are understood to have inferred kinds, and polymorphic
recursion is not available. Note that the last example above, ``TypeRep``, uses
polymorphic recursion and would be rejected without the top-level kind signature.

This proposal replaces GHC's current notion of syntactic
CUSKs_.

.. _CUSKs: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion


Motivation
------------
This is a simplification over the current story around CUSKs, which are fiddly and
unpredictable. For example, here_ are_ some_ tickets_ borne of confusion around CUSKs.

.. _here: https://gitlab.haskell.org/ghc/ghc/issues/12928
.. _are: https://gitlab.haskell.org/ghc/ghc/issues/10141
.. _some: https://gitlab.haskell.org/ghc/ghc/issues/13109
.. _tickets: https://gitlab.haskell.org/ghc/ghc/issues/13761

This new proposal makes the choice of whether or not to infer a kind much simpler.
Even better, this proposal makes type-level polymorphic recursion have the same rules
as term-level polymorphic recursion: you *must* have a top-level signature (with no
wildcards).

Proposed Change Specification
-----------------------------

1. Introduce a new top-level declaration form ::

     type <name> :: <kind>

   It is distinguishable from a type synonym by the lack of an ``=`` on the line. A
   type-level declaration with a top-level kind signature may use polymorphic recursion;
   one without is considered to have an inferred kind and may not use polymorphic recursion.

   The kind given is checked against the declaration of the type. All kind generalization
   is done before ever examining the full declaration, just like how GHC treats type
   signatures.

   Associated types may be given kind signatures within their classes.

   Unlike type signatures, the type variables brought into scope in a type-level kind
   signature do *not* scope over the type definition.

   Top-level kind signatures are enabled with the extension ``-XTopLevelKindSignatures``.

2. Introduce a new extension ``-XCUSKs``, on by default, that detects CUSKs as they
   currently exist. A CUSK will be treated identically to a top-level kind signature.

   When ``-XNoCUSKs`` is specified, only a top-level kind signature enables
   polymorphic recursion.

3. Plan to turn ``-XCUSKs`` off by default in GHC 8.8 and to remove it sometime thereafter.

Effect and Interactions
-----------------------
This is largely a simplification over the status quo, eventually eliminating the need for
the fiddly definition and detection of CUSKs. It allows users to control whether they want
inference or specification in a more conspicuous way than CUSKs do.

Note that a top-level kind signature, by itself, is insufficient in describing a type-level
construct in, say, an hs-boot file. The kind signature omits details like

* whether the type is generative and/or injective

* whether the type is open or closed

* whether the type must be applied to a certain prefix of arguments

I don't foresee intricate interactions with other features.

Template Haskell will need to be updated accordingly.

Note that this proposal depends on `#81`_, which adds a bit of syntax necessary
to give, e.g., ``data ProxyVis k (a :: k)`` a kind signature.

.. _`#81`: https://github.com/ghc-proposals/ghc-proposals/pull/81

Costs and Drawbacks
-------------------
Implementation should be rather straightforward, as this is a new syntactic construct.

Parsing may be slightly complicated by the similarity to a type synonym, but I doubt this
will pose more than an hour's delay in implementation.

Checking and generalizing the kind can be done by already-written code (in TcHsType).

The hardest part will be complicating the code in TcTyClsDecls, which is already somewhat
involved; however, I don't think this change will be invasive, as it will just affect the
code that currently checks for CUSKs.

Alternatives
------------

* Don't do anything. I find the current situation to be confusing, though, generating
  several confused users yearly.

* A previous version of this proposal introduced a new type former ``~>``, which denoted
  *matchable* functions. Using ``~>``, a top-level kind signature could differentiate
  between the parameters of a type family that are required to be saturated and any others.
  However, this particular choice of syntax was bound to create confusion and disagreement.
  Furthermore, the particular way the syntax was designed was based on issues around
  *future*\-compatibility, and so was likely to end up being wrong, regardless.

* We don't need the ``type`` keyword to introduce non-symbolic kind signatures, as the
  capital letter can tip GHC off. Perhaps omit.

* With top-level kind signatures, some aspects of type declarations are redundant.
  (For example, the ``a b c`` in ``data T a b c where ...``.) One could imagine removing
  these as an extension to this proposal.

* Other transition plans are welcome. We could just abandon CUSKs entirely, asking the
  few users who play in this dark corner to use some CPP.

* Instead of introducing wholly new syntax, we could just tell GHC when to look for a CUSK.
  That is, we could have a new pragma ``{-# CUSK T #-}`` that tells GHC that ``T`` has a
  CUSK. If the pragma is absent, ``T`` does not have a CUSK. (Or, we could have a
  ``NO_CUSK`` pragma to countermand current behavior. This might have an easier transition
  story.) If ``T`` is labeled as having a CUSK, but does not, reject.

  This idea might be an improvement on my original proposal (it was inspired by a comment
  made on the original), but it still means that types have a different treatment from
  terms, which is aesthetically displeasing to me.


Unresolved questions
--------------------
These are essentially considered in the "Alternatives" section.


Implementation Plan
-------------------
I (or a close collaborator) will implement.
