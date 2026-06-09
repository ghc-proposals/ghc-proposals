Template Haskell quotes as patterns
===================================

.. author:: John Ericson
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/529>`_.
.. sectnum::
.. contents::

The Template Haskell AST is inherently unstable.
Let's make that less of an issue.
We can do that by allowing Quotes to be used instead of Template Haskell AST data constructors in patterns.
This is a well-known technique used in many other languages' macro systems.

Motivation
----------

As is well known, the Template Haskell AST is inherently unstable.
Changes that are backwards compatible in surface Haskell nonetheless cause compatibility issues with the TH AST.

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

But Template Haskell gives us a better option, which I used in a `subsequent commit <https://github.com/reflex-frp/reflex/commit/4cd322604596ac652f35bbe72c1ad8fe42f2efdc>`_:

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

Quotes as patterns
~~~~~~~~~~~~~~~~~~

With the new extension ``TemplateHaskellQuotesAsPatterns``, slightly modified quotes are usable in pattern position.

The first difference is that quotes as pattern match raw syntax, not (monadic) actions producing syntax.
The tying rules are as follows:

- ``'...`` or ``''...``,. where "..." is a name, is a pattern that matches ``Name``
- ``[| ... |]`` or ``[e| ... |]``, where "..." is an expression, is a pattern that matches ``Exp``
- ``[p| ... |]``, where "..." is a pattern, is a pattern that matches ``Pat``
- ``[t| ... |]``, where "..." is a type, is a pattern that matches ``Type``
- ``[d| ... |]``, where "..." is a top-level declaration, is a pattern that matches a ``List Dec`` with a single item.

The second difference is that splices within these quotes contain patterns instead of expressions::

  <pat> ::= ...

  <apat> ::= [| ... $(p) ... |]
          |  ...

The third and final difference is that names in quotes must all be uses, never bindings.
The semantics of matching a name, either standalone, or inside a larger quote, is name equality.
This is formalized on the deguaring of a case alternative below::

  ''name -> ...

  =>

  __internalName | __internalName == ''name -> ...

Optional: Fine-grained Quotation constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _`Proposal #246`: ./0246-overloaded-bracket.rst

*This is optional, but fits well with the rest.*

`Proposal #246`_ made it so that quotes are polymorphic, e.g. ``[| 1 + 1 |] :: Quote m => m Exp``.
The ``Quote`` class has a ``newName`` method, and is just used when binding local variables.

Relax the rules so that TH Quotes only impose a ``Quote`` constraint when ``newName`` is in fact needed.
Otherwise, merely impose a ``Monad`` constraint.

[This was a `alaternative that was rejected <./0246-overloaded-bracket.rst#alternatives>` of Proposal #246, but now we have additional movation for it (as detailed in "Effectas and Interactions") below.]

Examples
--------

Quotes as Patterns
~~~~~~~~~~~~~~~~~~

Quotes of names as patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. This is allowed::

     f ''foo = ...

     =>

     f __secret | __secret == ''foo = ...

Quotes of expressions as patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. This is allowed::

     f [| $(x) $(y) |] = ...

     =>

     f (AppE x y) = ...

#. This is conditionally allowed::

     f [| name |] = ...

     =>

     f (VarE __n) | __n == 'name = ...

   on ``'name`` being bound in the scope the bracket is written in (bound lexically, not dynamically at the splice site).

#. This is not allowed::

     f [| \x -> x |] = ...

   It is disallowed because the first ``x`` in the quote is a binding not a use.

#. This is allowed::

     isLambda [| \$(_) -> $(_) |] = True
     isLambda _                   = False

   `isLambda` returns `True` for any single parameter lambda.

#. This is allowed::

     isLambda [| \_ -> $(_) |] = True`
     isLambda _                = False

   `isLambda` returns `True` for any lambda with a single `_` parameter.
   Not the same!

#. Question: is::

     isLambdaLambda [| \$(_) -> \$(_) -> $(_) |] = True
     isLambdaLambda _                            = False

   equivalent to::

     isLambdaLambda [| \$(_) $(_) -> $(_) |] = True
     isLambdaLambda _                        = False

   ?

   Those would *not* be equivalent because the syntax being matched is different, even though both syntaxes have the same semantics.

Quotes of types as patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. This is allowed::

     f [t| $(x) -> $(y) |] = ...

     =>

     f (AppT x y) = ...

#. This is not allowed::

     f [t| forall x. x |] = ...

   It is disallowed because the first ``x`` in the quote is a binding not a use.

Quotes of patterns as patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. This is allowed::

     f [p| !$(x) |] = ...

     =>

     f (BangP x) = ...

Quotes of declarations as patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. This is allowed::

     f [d| _ = $(x) |] = ...

     =>

     f [ValD WildP (NormalB x) []] = ...

#. This is allowed::

     f [d| instance $(t) |] = ...

     =>

     f [InstanceD Nothing [] t []] = ...

#. This is not allowed::

     f [d| _ = $(x); _ = $(y) |] = ...

   because matching on lists of multiple declarations is left as future work.

Optional: Fine-grained Quotation constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. ``[| 1 + 1 |]`` would have type ``Monad m => m Exp`` instead of ``Quote m => m Exp``.

#. ``[| \x -> x |]`` however would continue to have type ``Quote m => m Expr``.

(Note, we could relax ``Monad`` to ``Applicative`` at an any point, but ``Monad`` is the current superclas of ``Quote``.)

Effect and Interactions
-----------------------

``TemplateHaskell`` will not imply ``TemplateHaskellQuotesAsPatterns``, though it does imply ``TemplateHaskellQuotes``.

The banned binding constructs are precisely those which would need ``newName`` in expression position.

Optional Proposed Change: Fine-grained Quotation constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The optional proposed relaxation of the expression position rules is supposed to make these pattern-position restrictions more familiar to the programmer.
Specifically, by distinguishing the same subset of quotes in two ways (they're the only ones allowed in pattern position, they get a more general type in expresssion position), we give programmers two different ways to learn the difference between them and quotes in general.

If we go with the alternative of supporting local variables, however, this extra motivation dries up.
The cases where ``newName`` is needed in expression position will not no longer correspond to anything obvious in pattern position in this alternative.
The originally motivation from `Proposal #246`_ of more flexible typing for flexibility's sake still stands, however.

Costs and Drawbacks
-------------------

Action asymmetry
~~~~~~~~~~~~~~~~

The lack of symmetry where expression create actions but patterns only bind plain AST values is annoying.
But the fixes for this might be too radical?

Alternatives
------------

Pattern match on actions(!)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

I hypothesize that we could do better than the proposed actions vs no action asymmetry by meditating on the ways pattern matching relates to optics.
For example::

  [| a + $(x) |]

could match any ``t Exp`` where ``t`` is ``Traversable`` and bind ``x :: t Expr`` with this desugaring::

  f [| a + $(x) |] = ...

  =>

  f (traverse __inner -> Just x) = ...

  __inner (AppE (VarE __a) x) | __a == 'a = Just x
  __inner _                               = Nothing

This behavior seems overwrought, as we are doing the as-proposed behavior *plus* an additional traversal.
But this matches the fact that expression-position quotes are do what idiom brackets do (implicit ``Applicative``) in addition to base quoting.

Support local variables
~~~~~~~~~~~~~~~~~~~~~~~

Quotes that bind local variables do in fact have an interpretation as non-linear patterns::

  f [| \x -> x |] = ...

  =>

  f (LamE [VarP __x0] (VarE __x1) | nameBase __x0 == "x" && __x0 == __x1 = ...

This gets especially interesting with multiple scopes::

  f [| (\x -> x, \x -> x) |] = ...

  =>

  f (TupE [ Just (LamE [VarP __x0] (VarE __x1))
          , Just (LamE [VarP __x2] (VarE __x3))
          ])
    | nameBase __x0 == "x" && __x0 == __x1
    | nameBase __x2 == "x" && __x2 == __x3
    = ...

Note how ``__x0`` is related to ``__x1`` and  ``__x2`` likewise to ``__x3``, but the former two are *not* related to the latter two.
This respects the two independent scopes.

This is perhaps convenient, but it rather baroque.
It is also unclear whether the ``nameBase _ == "x"`` is useful in practice, or whether it is better to just "bake in" alpha equivalence and not care whether the local variable is an "x" or not provided the usage lines up with the binding.

Finally, the non-linear patterns trick is not a true dual because it merely checks whether the variables "ended up" being the same *once the action is run*, rather than pattern matching on the action *itself*.
For example, De Bruijn indices encode actions in a way that makes equality between the actions *themselves*, rather than their results, easily decidable.
This would be a less hacky solution.

Note that without this alternative, we still don't have to full back completely on not using our new feature for this use-case.
The second example we can write as::

  f [| (\$(VarP x0) -> $(VarE x1), \$(VarP x2) -> $(VarE x3)) |]
    | nameBase x0 == "x" && x0 == x1
    | nameBase x2 == "x" && x2 == x3
    = ...

which, while not as terse, is still an improvement.

---------

Both alternatives are tempting, but I rather wait for more research on patterns, optics, and "dualizing" ``Applicative`` and effects like ``Quote`` in general, so we can better understand the theory of what's going on.
If and when we understand the lay-of-the-land better, we can make some new extensions and deprecate the old ones accordingly.

Unresolved Questions
--------------------

None at this time.

Related Work
------------

Examples of languages that also implement this feature:

Scheme
~~~~~~

- ``syntax-rules``, see https://docs.racket-lang.org/guide/pattern-macros.html#%28part._define-syntax_and_syntax-rules%29 .
- ``syntax-case``, see https://docs.racket-lang.org/guide/syntax-case.html .

Racket
~~~~~~

``syntax-parse`` is a more powerful alternative to ``syntax-case``.
See:

- Docs: https://docs.racket-lang.org/syntax/Parsing_Syntax.html

- Paper: `"Fortifying Macros" by Culpepper and Felleisen <https://www2.ccs.neu.edu/racket/pubs/icfp10-cf.pdf>`_

There's a Haskell implementation of ``syntax-parse``'s core as part of the Crucible language's `concrete syntax <https://github.com/GaloisInc/crucible/blob/master/crucible-syntax/src/Lang/Crucible/Syntax/ExprParse.hs>`_.

Rust
~~~~

- Pattern-based macros (like ``syntax-rules``) https://doc.rust-lang.org/reference/macros-by-example.html .

- Procedural macros don't have great pattern matching, but https://github.com/nrc/proc-macro-rules is an library-level experiment to try to bridge the gap.


Idris
~~~~~

- https://davidchristiansen.dk/pubs/type-directed-elaboration-of-quasiquotations.pdf

Lean
~~~~

- https://dl.acm.org/doi/pdf/10.1145/3110278

- https://arxiv.org/pdf/2001.10490.pdf

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

Typed Template Haskell
~~~~~~~~~~~~~~~~~~~~~~

This could also work for typed template haskell expressions.
For example, in::

  f :: Code m alpha -> delta
  f [|| $$(x) $$(y) ||] = ... :: delta

``x`` would be bound as ``x :: Code m (beta -> alpha)``, and
``y`` would be bound as ``y :: Code m beta``.

This doesn't seem too hard, but is left as future work.

Implementation Plan
-------------------

I lack the time capacity to implement these changes all by myself, and would submit this to the Haskell Foundation to fund as part of whatever https://discourse.haskell.org/t/pre-hftt-ongoing-focus-on-migration-tools/4626 becomes.

That said, I would be happy to pair / code review / etc. with whoever does end up working on it.
I likewise have been pitching in while @tek is leading the charge on `Proposal #285`_, and that process has felt very good to me.

.. _`Proposal #285`: ./0285-no-implicit-binds.rst

Endorsements
-------------
