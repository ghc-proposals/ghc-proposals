Template Haskell Quotes as patterns
===================================

.. author:: John Ericson
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

The Template Haskell AST is inherently unstable.
Let's make that less of an issue.

Motivation
----------

As is well known, the Template Haskell AST is inherently unstable.
Changes that are backwards compatible in surface Haskell nonetheless compatibility issues with the TH AST.

Here is a `recent change <https://github.com/reflex-frp/reflex/pull/472/files#diff-2c01379db9dd160bd3f212e3ce06c34bdeee89c19e08f41fdc94f7e66cca5aa8>`_ I made to support GHC 9.2 in a library illustrating the problem:

.. code-block:: diff

  --- a/src/Reflex/Dynamic/TH.hs
  +++ b/src/Reflex/Dynamic/TH.hs
  @@ -45,5 +45,12 @@ qDynPure qe = do
     (e', exprsReversed) <- runStateT (gmapM f e) []
     let exprs = reverse exprsReversed
         arg = foldr (\a b -> ConE 'FHCons `AppE` snd a `AppE` b) (ConE 'FHNil) exprs
  -      param = foldr (\a b -> ConP 'HCons [VarP (fst a), b]) (ConP 'HNil []) exprs
  +      param = foldr (\a b -> conPCompat 'HCons [VarP (fst a), b]) (conPCompat 'HNil []) exprs
     [| $(return $ LamE [param] e') <$> distributeFHListOverDynPure $(return arg) |]
  +
  +conPCompat :: Name -> [Pat] -> Pat
  +#if MIN_VERSION_template_haskell(2, 18, 0)
  +conPCompat name = ConP name []
  +#else
  +conPCompat = ConP
  +#endif

``ConP`` got an additional parameter so I had to introduce some CPP and a `conPCompat` function.
Even if such functions are changed to pattern synonyms and factored out into a library to avoid everyone reinventing the wheel (generally the best we can do for smoothing about data type changes), they is still an annoying source of boilerplate / chores for release management.

But Template Haskell gives us a better option, which I used in a `subsequent commit <https://github.com/reflex-frp/reflex/commit/4cd322604596ac652f35bbe72c1ad8fe42f2efdc>`:

.. code-block:: diff

  --- a/src/Reflex/Dynamic/TH.hs
  +++ b/src/Reflex/Dynamic/TH.hs
  @@ -44,15 +44,14 @@ qDynPure qe = do
     (e', exprsReversed) <- runStateT (gmapM f e) []
     let exprs = reverse exprsReversed
  -      arg = foldr (\a b -> ConE 'FHCons `AppE` snd a `AppE` b) (ConE 'FHNil) exprs
  -      param = foldr (\a b -> conPCompat 'HCons [VarP (fst a), b]) (conPCompat 'HNil []) exprs
  -  [| $(return $ LamE [param] e') <$> distributeFHListOverDynPure $(return arg) |]
  +      arg = foldr
  +        (\(_, expr) rest -> [e| FHCons $(pure expr) $rest |])
  +        [e| FHNil |]
  +        exprs
  +      param = foldr
  +        (\(name, _) rest -> [p| HCons $(pure $ VarP name) $rest |])
  +        [p| HNil |]
  +        exprs
  +  [| (\ $param -> $(pure e')) <$> distributeFHListOverDynPure $arg |]
  -
  -conPCompat :: Name -> [Pat] -> Pat
  -#if MIN_VERSION_template_haskell(2, 18, 0)
  -conPCompat name = ConP name []
  -#else
  -conPCompat = ConP
  -#endif

Notes that ``conPCompat`` is gone entirely!
By using quotes and splices like this, one avoids the AST and its instability problems.
Quotes and splices are much more stable for the same reason the surface language is.
The overall method of this proposal is to allow using them to solve more problems, so the AST becomes less necessary to use, and thus TH code in practice is less likely to break.

Just a few lines above, however, there was more TH AST usage I couldn't get rid of::

  let f :: forall d. Data d => d -> StateT [(Name, Exp)] Q d
      f d = case eqT of
        Just (Refl :: d :~: Exp)
          | AppE (VarE m) eInner <- d
          , m == 'unqMarker
          -> do n <- lift $ newName "dynamicQuotedExpressionVariable"
                modify ((n, eInner):)
                return $ VarE n
        _ -> gmapM f d
  (e', exprsReversed) <- runStateT (gmapM f e) []

Perhaps we can take solace in a claim that ``AppE`` and ``VarE`` are less likely to change, but that isn't satisfactory --- what about more complex patterns?
The fundamental problem here is that in *positive* position (expressions), we have a choice of either using regular syntax or quotes,
but in *negative* position (patterns), we only have the option regular syntax.

The solution is simple: let's allow quotes too!
This would allow:

.. code-block:: diff

         Just (Refl :: d :~: Exp)
  -        | AppE (VarE m) eInner <- d
  +        | [e| $(VarE m) $eInner |] <- d
           , m == 'unqMarker
           -> do ...

or even going further:

.. code-block:: diff

         Just (Refl :: d :~: Exp)
  -        | AppE (VarE m) eInner <- d
  -        , m == 'unqMarker
  +        | [e| unqMarker $eInner |] <- d
           -> do ...

In this way, we also avoid the use of the AST.

Note we do have quotes *of patterns* today (``[p| ... |]``), but that is orthogonal.
This is quotes *as patterns*, the type of syntax being quoted doesn't matter and could be anything.
The point is the quotes are in negative position.

With this change put together, the hope is that a significant portion of TH out in the wild is going to be more stable across GHC versions.
This is all accomplished without trying to minimize TH AST changes, which is quite a hopeless task and also a perverse incentive for the rest of language development.

Proposed Change Specification
-----------------------------
Specify the change in precise, comprehensive yet concise language. Avoid words
like "should" or "could". Strive for a complete definition. Your specification
may include,

* BNF grammar and semantics of any new syntactic constructs
  (Use the `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/>`_ or GHC's ``alex``\- or ``happy``\-formatted files
  for the `lexer <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser/Lexer.x>`_ or `parser <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y>`_
  for a good starting point.)
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler
  features, in case that is otherwise ambiguous

Strive for *precision*. The ideal specification is described as a
modification of the `Haskell 2010 report
<https://www.haskell.org/definition/haskell2010.pdf>`_. Where that is
not possible (e.g. because the specification relates to a feature that
is not in the Haskell 2010 report), try to adhere its style and level
of detail. Think about corner cases. Write down general rules and
invariants.

Note, however, that this section should focus on a precise
*specification*; it need not (and should not) devote space to
*implementation* details -- there is a separate section for that.

The specification can, and almost always should, be illustrated with
*examples* that illustrate corner cases. But it is not sufficient to
give a couple of examples and regard that as the specification! The
examples should illustrate and elucidate a clearly-articulated
specification that covers the general case.

Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

Effect and Interactions
-----------------------
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Alternatives
------------
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.

Future Work
-----------

Split or wrap the ``template-haskell`` library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The previous change hopefully allows far less usage of the Template Haskell AST than today without loss of expressive power.
But even if that's the case, users will just encounter another source of new GHC busywork.
The ``template-haskell`` library contains more stable items and the AST alike, and the latter forces a major version bump every release.
Even when one doesn't use the AST, or any other part of the library with a breaking change, they still need to adjust bounds to deal with this version bump.

We should instead split or wrap the Template Haskell library so that more stable core functionality is accessible in a more stable library.
Then users which no longer need the unstable bits don't have to pay their costs in the form of major version churn.

The exact interface of such a library is more a Core Library Committee than GHC steering committee matter, so I defer any further details to a separate posting in that venue.

More valid splice positions and quote types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There will still be a long tail of scenarios where the AST is needed, but over time we can add more flexible forms of quoting and splicing to shrink that tail.
A goo ways to figure out what is needed could be trying to convert existing in-depth code generators like Alex and Happy, and seeing what is possible and what isn't.

The goal is for the TH AST to increasingly be a historical artifact, or debugging aid, that doesn't unlock any additional expressive power.

Implementation Plan
-------------------

I lack the time capacity to implement these changes all by myself, and would submit this to the Haskell Foundation as part of whatever fund as part of whatever https://discourse.haskell.org/t/pre-hftt-ongoing-focus-on-migration-tools/4626 becomes.

Endorsements
-------------
