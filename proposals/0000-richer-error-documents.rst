Richer (error) documents
========================

.. author:: Alp Mestanogullari
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/307>`_.
.. contents::

Enrich GHC's document types (``Doc`` and ``SDoc``) with support for
annotations of some arbitrary type ``a``, and use this new capability
to emit error messages with bits of expressions, types and more
embedded in the documents, as AST values, as opposed to their
textual form which is not amenable to any form of inspection or
post-processing.

This proposal drew a lot of inspiration from the discussion at
`ghc#8809 <https://gitlab.haskell.org/ghc/ghc/issues/8809>`_.

Motivation
----------

GHC's whole pretty printing infrastructure is based around the following
types:::

    data Doc
      = Empty                                            -- empty
      | NilAbove Doc                                     -- text "" $$ x
      | TextBeside !TextDetails {-# UNPACK #-} !Int Doc  -- text s <> x
      | Nest {-# UNPACK #-} !Int Doc                     -- nest k x
      | Union Doc Doc                                    -- ul `union` ur
      | NoDoc                                            -- The empty set of documents
      | Beside Doc Bool Doc                              -- True <=> space between
      | Above Doc Bool Doc                               -- True <=> never overlap

    data SDocContext = SDC
      { sdocStyle      :: PprStyle
      , sdocLastColour :: PprColour
      , sdocDynFlags   :: DynFlags
      }
    newtype SDoc = SDoc { runSDoc :: SDocContext -> Doc }

``SDocContext`` gives access to all sorts of pretty-printing and compiler
configuration. These types allow us to specify how sub-documents should be
placed relative to one another once rendered and have been great to describe
all sorts of textual documents in GHC.

On the other hand, authors of tools in the IDE/editor-integration space
have been wondering about the possibility of enriching those error
documents, with several applications in mind -- we will mention a few in
the **Effects and Interactions** section. All of those applications require
that we keep some information around, stored in good old ADTs, that we can
freely inspect and consume: bits of expressions, types, constraints, etc.

To bridge that gap, this proposal suggests that we add support for
embedding annotations (non-textual values) directly into our documents,
as described in the next section.

Proposed Change Specification
-----------------------------

Basic idea
~~~~~~~~~~

This proposal suggests that we modify ``Doc`` to extend it with a
constructor for embedding arbitrary values into documents, which we refer
to as "annotations".::

    data Doc a
      = Empty
        -- empty
      | ...
      | Pure a -- new! embed an annotation of type 'a'

Such a document model comes with useful instances for standard
classes, among which ``Functor``, ``Aplicative``, ``Monad``, ``Foldable``
and ``Traversable``, giving us standard names for functions that
transform annotations into other annotations or into whole new sub-documents.

``SDoc`` would similarly be updated to take a type parameter for the
annotation type:::

    newtype SDoc a = SDoc { runSDoc :: SDocContext -> Doc a }

and would come with similar instances as ``Doc`` (since with this definition,
``SDoc`` is isomorphic to ``ReaderT SDocContext Doc``), except for ``Foldable``
and ``Traversable``. The main addition to the public API of the ``Outputable``
module would be a function for constructing a document from an annotation:::

    embed :: a -> SDoc a
    embed = pure

We could then create an annotation type for error documents, so as to
embed bits of expressions, types and more, in their AST form, right into
the documents, even supporting embedding ASTs from all 3 GHC passes. We could
for example "categorise" error annotations according to the type of entity
(expression, type, ...) and the AST phase (parser, renamer, typechecker):::

    data PassWrapper f
      = WrapperPs (f GhcPs) -- parser AST
      | WrapperRn (f GhcRn) -- renamer AST
      | WrapperTc (f GhcTc) -- typechecker AST

    data ErrAnnotation
      = ErrHsExpr (PassWrapper HsExpr)
        -- ^ expression annotation, from any of the 3 phases
      | ...

Or alternatively build up a large sum type of all possible types of AST chunks
that the current error infrastructure emits (as text).::

    data ErrAnnotation
      = ErrTyCon TyCon
      -- ^ type constructor annotation, used in "Couldn't match expected type"
      --   error documents and possibly others
      | ...

The exact definition of this type will be best figured out while implementing
this proposal, informed by all the error documents that GHC builds.

We could then start emitting ``ErrAnnotation`` values in our documents
instead of rendering expressions, types, and friends directly.
GHC's error storage and reporting infrastructure could then be updated to
emit ``SDoc ErrAnnotation`` values, with the nice side effect that GHC API users
(such as developers of IDE tooling) would now get to deal with error documents
with that type. Most users will quite likely then want to use ``>>=`` to process
those annotations and render them using GHC's default rendition or a custom
one:::

    (>>=) :: SDoc a -> (a -> SDoc b) -> SDoc b

This ``Monad`` instance lets us substitute all the annotations in a document
by fresh, new subdocuments that can depend on the annotation values. Of
particular interest is the case where ``b`` is ``Void``, indicating
the "annotation-free" nature of the resulting document.  We could even imagine
tweaking the functions that actually print documents to only accept
annotation-free documents (``SDoc Void``), so as to force users to interpret
annotations one way or another before getting the documents printed somewhere.

Annotation-free documents
~~~~~~~~~~~~~~~~~~~~~~~~~

In fact, GHC uses ``SDoc`` in other contexts than error messages, a major
one being code generation. In those cases, we will never want to emit
any annotation, just pure text, and as fast as possible. This would be another
case where we would want to deal with ``SDoc Void`` values. It is therefore
desired that any potential implementation of this proposal doesn't drastically
change the performance of document construction and rendering when no annotation
is involved.

Producers of annotation-free (annotation-agnostic) documents should produce
documents polymorphic in the annotation type, whenever there might be any
chance that the said documents are used with different annotation types.
Consumers of annotation-free documents should take ``SDoc Void`` values,
to force the instantiation of a potentially polymorphic annotation type
to ``Void``, therefore guaranteeing the absence of ``Pure`` nodes. Some
explicit conversion functions like
``fmap absurd :: forall a. SDoc Void -> SDoc a`` would quite likely be supplied,
to cover the cases where we are explicitly bootstrapping an annotated document
from an annotation-free one.

A problem
~~~~~~~~~

However, with the changes described so far, we would run into a problem. While
the semantics of the existing ``Doc`` constructors are pretty clear in terms of
layout, there is no good answer when interpreting a ``Pure a``. We will never
know for sure what text this annotation is going to end up being replaced with,
since the point of this proposal exactly consists in sprinkling non-textual
Haskell values all over our documents and rendering them later. This in turns
means that we can't reliably "guess" whether our annotation is going to end up
being rendered over several lines, nor how many columns or levels of nesting it
will involve.

A possible solution
~~~~~~~~~~~~~~~~~~~

To work around this problem, we suggest to adopt the trick used in the
`wl-pprint-extras <https://hackage.haskell.org/package/wl-pprint-extras>`_
library, which consists in introducing constructors that allow users to
introduce (sub-)documents that are dependent on the current column number,
nesting level, ribbon length, etc. For instance, this is how we would define
the constructor that "suspends" a sub-document on the column number of the
current position in the textual rendering of a larger document:::

    data Doc a
      = ...
      | Pure a
      | Column (Int -> Doc a) -- also new!
      | ...

While this now prevents ``Doc`` from being ``Foldable`` or ``Traversable``
(which isn't that big of a deal since ``SDoc`` wasn't going to support those
operations anyway, and it is the type that we manipulate the most),
it nicely solves the problem of having to "guess" properties about the
textual rendition of an annotation, allowing us to preserve the current
layout semantics by building up continuations that will be consumed when
rendering the document.

Consuming annotations
~~~~~~~~~~~~~~~~~~~~~

If we can ensure that the public API for ``Doc`` doesn't allow users to build
documents that may or may not have some annotations, depending e.g on
the column number, we can have our ``Foldable`` instance. This
assumption is just what we need to have the permission to call all those
continuations with arbitrary numbers, e.g ``0``, and be able to keep inspecting
the sub-documents "behind the lambdas" with the guarantee that
we will end up looking at all the annotations. An implementation of this
proposal would try to establish and maintain that invariant, so as to be able to
offer such a simple API.

For ``SDoc``, the situation is a bit different: one cannot conjure up an
``SDocContext`` out of thin air, to get to the underlying ``Doc``. In partcular,
some ``DynFlags`` might significantly affect the contents of error documents,
one simply cannot guess the flags and hope to get the right annotations. This
suggests that document consumers should supply an ``SDocContext`` explicitly
and that the entire annotation consumption API of ``SDoc`` would have to take
such an argument, e.g ``collectAnns :: SDocContext -> SDoc a -> [a]``.

If, on the other hand, we cannot maintain the invariant described above, then we
necessarily would have to render the documents (including the annotations) and
collect all the annotation values as we go, in order to be able to return a list
of them, e.g
``showSDocAnns :: (a -> SDoc Void) -> SDocContext -> SDoc a -> (String, [a])``.

An alternative to returning just the annotations would be to
pair them with the location of their textual rendition, e.g::

  data RenderedAnn a = RenderedAnn
    { raSpan :: RealSrcSpan -- or a more appropriate type
    , raAnn  :: a
    } deriving (Functor, Foldable, Traversable)

  showSDocRenderedAnns
    :: (a -> SDoc Void)
    -> SDocContext
    -> SDoc a
    -> (String, [RenderedAnn a])

This variant is strictly more general than ``showSDocAnns`` (which can
be written in terms of ``showSDocRenderedAnns`` by just dropping location
information) and is implementable regardless of whether our invariant
holds. The simpler, ``collectAnns`` -style API on the other hand would
only be guaranteed to work if the invariant holds, and this additional API
would therefore only be available in that case.

Effect and Interactions
-----------------------

The main point of adding support for annotations as described
above is to give a chance to tooling authors to easily access
AST fragments that today are simply pretty-printed as part of
some error messages, and this is indeed made possible by this
proposal. GHC's main error message data type is ``ErrMsg``,
which contains useful metadata and the actual error message
document(s), of type ``ErrDoc``.::

    type MsgDoc = SDoc

    data ErrDoc = ErrDoc {
            -- | Primary error msg.
            errDocImportant     :: [MsgDoc],
            -- | Context e.g. \"In the second argument of ...\".
            errDocContext       :: [MsgDoc],
            -- | Supplementary information, e.g. \"Relevant bindings include ...\".
            errDocSupplementary :: [MsgDoc]
            }

Changing the definition of ``MsgDoc`` to
``type MsgDoc = SDoc ErrAnnotation`` and "fixing all the
resulting type errors" will make it possible to build error
messages that contain annotations. Since such an ``MsgDoc``
*could* contain annotations but doesn't necessarily have to,
we could start emitting annotations incrementally, completing
this effort over several patches, as many as we want.

Updating all the error messages should not be very complicated: the famous
``Couldn't match expected type`` error message is currently emitted by the
following code, from ``compiler/typecheck/TcErrors.hs``.::

  misMatchMsg :: Ct -> Maybe SwapFlag -> TcType -> TcType -> SDoc
  misMatchMsg ct oriented ty1 ty2
    | Just NotSwapped <- oriented
    = misMatchMsg ct (Just IsSwapped) ty2 ty1

    -- These next two cases are when we're about to report, e.g., that
    -- 'LiftedRep doesn't match 'VoidRep. Much better just to say
    -- lifted vs. unlifted
    | isLiftedRuntimeRep ty1
    = lifted_vs_unlifted

    | isLiftedRuntimeRep ty2
    = lifted_vs_unlifted

    | otherwise  -- So now we have Nothing or (Just IsSwapped)
                 -- For some reason we treat Nothing like IsSwapped
    = addArising orig $
      pprWithExplicitKindsWhenMismatch ty1 ty2 (ctOrigin ct) $
      sep [ text herald1 <+> quotes (ppr ty1)
          , nest padding $
            text herald2 <+> quotes (ppr ty2)
          , sameOccExtra ty2 ty1 ]
    where
      herald1 = conc [ "Couldn't match"
                     , if is_repr     then "representation of" else ""
                     , if is_oriented then "expected"          else ""
                     , what ]
      herald2 = conc [ "with"
                     , if is_repr     then "that of"           else ""
                     , if is_oriented then ("actual " ++ what) else "" ]
      padding = length herald1 - length herald2

      is_repr = case ctEqRel ct of { ReprEq -> True; NomEq -> False }
      is_oriented = isJust oriented

      orig = ctOrigin ct
      what = case ctLocTypeOrKind_maybe (ctLoc ct) of
        Just KindLevel -> "kind"
        _              -> "type"

      conc :: [String] -> String
      conc = foldr1 add_space

      add_space :: String -> String -> String
      add_space s1 s2 | null s1   = s2
                      | null s2   = s1
                      | otherwise = s1 ++ (' ' : s2)

      lifted_vs_unlifted
        = addArising orig $
          text "Couldn't match a lifted type with an unlifted type"

To emit annotations that contain the structured types (instead of their
textual rendition, like above), we could instead do:::

  data ErrAnnotation = ... | TcTypeAnn TcType

  tyTypeAnn :: TcType -> SDoc ErrAnnotation
  tyTypeAnn = embed . TcTypeAnn

  misMatchMsg :: Ct -> Maybe SwapFlag -> TcType -> TcType -> SDoc ErrAnnotation
  misMatchMsg ct oriented ty1 ty2
    | ...

    -- we just change the 'otherwise' clause, using 'tcTypeAnn'
    -- to embed the TcType values as annotations
    | otherwise
    = addArising orig $
      pprWithExplicitKindsWhenMismatch ty1 ty2 (ctOrigin ct) $
      sep [ text herald1 <+> tcTypeAnn ty1 -- <- HERE
          , nest padding $
            text herald2 <+> tcTypeAnn ty2 -- <- HERE
          , sameOccExtra ty2 ty1 ]

    ... everything else stays the same ...

We essentially changed the return type of ``misMatchMsg`` and
turned ``quote (ppr xxx)`` into ``tcTypeAnn xxx``, twice.

Once the annotations are emitted, GHC API consumers would
be able to get their hands on them when a compilation
returns non-empty bags of ``ErrMsg`` or ``WarnMsg`` values, and could
decide to use them to apply the following ideas or others in the same spirit.

* A REPL front-end or IDE tool might implement color-coded output,
  choosing a token's color by its syntactic class (e.g. type constructor,
  data constructor, or identifier), its name or some other criterion
  entirely.

* A REPL front-end or IDE tool might allow users the ability to
  interactively navigate a type in a type error and, for instance,
  allow the user to interactively expand type synonyms, show kind
  signatures, etc.

* A REPL front-end or IDE tool might allow users the ability to toggle a
  setting in order to display expressions, types and other AST related
  entities in their AST form instead of pretty-printed. This could be useful
  for anyone working on plugins or GHC itself.

Below is a simple example of a GHC API program that loads ``M.hs``,
collects the annotations contained in the errors and prints them (assuming
an ``Outputable`` instance for ``ErrAnnotation`` and that we do have
``collectAnns``).::

  import Bag
  import DynFlags
  import GHC
  import GHC.Paths ( libdir )
  import HscTypes

  main :: IO ()
  main = do
    res <- run
    case res of
      Right _   -> return ()
      Left anns -> putStrLn $ "Got " ++ show (length anns) ++ " annotations"

  run :: IO (Either [ErrAnnotation] SuccessFlag)
  run = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    target <- guessTarget "M.hs" Nothing
    setTargets [target]
    handleSourceErrors (return . Left . handleErrs dflags)
                       (Right <$> load LoadAllTargets)

  handleErrs :: DynFlags -> SourceError -> [ErrAnnotation]
  handleErrs dflags e = concatMap (errMsgAnns dflags)
                      $ bagToList (srcErrorMessages e)

  errMsgAnns :: DynFlags -> ErrMsg -> [ErrAnnotation]
  errMsgAnns dflags errmsg = collectAnns sdocctx sdoc

    where -- we get the error document (consisting of several 'SDoc's)
          errdoc :: ErrDoc
          errdoc = errMsgDoc errmsg

	  -- we "format" the ErrDoc as a single SDoc
	  sdoc :: SDoc ErrAnnotation
          sdoc    = formatErrDoc dflags errdoc

	  -- we create a suitable context for producing calling 'collectAnns'
	  sdocctx :: SDocContext
	  sdocctx = initSDocContext dflags (defaultUserStyle dflags)

Costs and Drawbacks
-------------------

The ``Outputable`` class in GHC lets us specify how to render values of all
sorts of types as documents:::

    class Outputable a where
        ppr :: a -> SDoc

One drawback of our approach is that we can't allow ``Outputable`` instances
to emit annotations without either using the same annotation type everywhere
(and changing ``ppr`` to return a document with such annotations), or
introducing a type family or functional dependency to map each ``a`` to a
corresponding annotation type. That still would not be good enough, as some
values end up being used in error messages (``ErrAnnotation``) as well as
in GHC-generated dumps (``Void`` annotations) -- e.g expressions, types.

What we will instead have to do is change ``Outputable`` as follows:::

    class Outputable a ann where
        ppr :: a -> SDoc ann

By making the annotation type a parameter of the typeclass, we get just the
flexibility we need. We can define a textual, annotation-free interpretation of
a given bit of typechecker information, as well as a "rich" one that wraps the
data in a suitable way to be embedded as an ``ErrAnnotation`` annotation:::

  instance Outputable TcType Void where
    ppr ... = ...
  -- alternatively: instance {-# OVERLAPPABLE #-} Outputable TcType a where ...

  instance Outputable TcType ErrAnnotation where
    ppr = tcTypeAnn

  -- If we need another interpretation for another annotation type, we just
  -- write the corresponding Outputable instance.

The ``OutputableBndr`` class would have to be updated in a similar manner:::

  class Outputable a ann => OutputableBndr a ann where
    pprBndr :: BindingSite -> a -> SDoc ann
    pprBndr _b x = ppr x

    pprPrefixOcc, pprInfixOcc :: a -> SDoc ann

    bndrIsJoin_maybe :: a -> Maybe Int
    bndrIsJoin_maybe _ = Nothing

We can see that there would be a problem with ``bndrIsJoin_maybe``, whose type
doesn't mention ``ann``, the annotation type. This could be fixed by adding a
dummy argument to ``bndrIsJoin_maybe`` (``Proxy :: Proxy ann`` or
``Nothing :: Maybe ann``) or putting that method in its own class. (This method
is given an explicit definition only a few times in the entire codebase.)

A good chunk of the work required for implementing this proposal will most
likely consist in adapting a lot of code in GHC that takes or returns
``SDoc`` values, and decide whether the annotation type should be
``Void``, ``ErrAnnotation`` or left polymorphic. Any implementation of this
proposal should also make sure that the current rendering of error messages
and IR dumps is not affected, in particular by the changes to the
pretty-printing infrastructure that are going to be required to perform
accurate layout computations in the presence of annotations.

Alternatives
------------

The design for annotated documents as described in this proposal is based
on the approach used by the *wl-pprint-extras* library, and lets us stick
annotations at the leaves of our "document trees", and is sometimes referred
to as the "*pointed* annotations" approach. An alternative design, used for
example in the Idris compiler, conists in introducing *scoped* annotations:::

    data Doc a
      = ...
      | Ann a (Doc a)

where the annotation wraps a sub-document, attaching non-textual information
to it. This approach has a few drawbacks in our case:

* We want to delay rendering, and the two most obvious ways to use this design
  would be to attach an annotation to either an empty document to emulate
  our pointed annotations approach, or to a textual version of the annotation.
  We are not guaranteed that this text is the one that's going to be used
  further down the road when reporting errors, since one of the applications of
  this proposal is to allow tooling authors to customize how some error
  message entities are rendered.

* This variant of ``Doc`` does not seem to come with lawful ``Applicative``
  and ``Monad`` instances, which provide a familiar and rich toolbox for
  introducing, transforming and eliminating annotations.

Unresolved Questions
--------------------

The only aspect of the implementation that is not crystal clear at this point
is the handling of annotation nodes in a few key functions from
``compiler/utils/Pretty.hs``. Fortunately, any implementation that does not
preserve the current layout bit for bit will quite likely be caught by the
testsuite, if we modify the driver so as to be able to make tests fail when
their output doesn't match the expected one, including situations where the only
differences are additional or missing whitespaces. We are quite confident that
this can be figured out with careful thinking, and by using the literature and
the implementation of the *wl-pprint-extras* library as inspirations.

Implementation Plan
-------------------

Well-Typed LLP will implement this proposal with financial support from
Richard Eisenberg, under NSF grant number 1704041.
