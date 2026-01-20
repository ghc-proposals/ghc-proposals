More Symmetrical Pattern Synonyms
=================================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/138>`__.
.. sectnum::
.. contents::

Pattern synonyms (language extension ``-XPatternSynonyms``) are a powerful tool. However, they are not as expressive as they could be, and conceptually simple synonyms are often much more complicated than they have to be. This proposal attempts to alleviate this by making pattern synonyms more "symmetrical".

Motivation
----------
Currently, many pattern synonyms must be written using view patterns (extension ``-XViewPatterns``). These are needlessly complicated and make conceptually simply synonyms hard to read and write.

1. Adapted from `this StackOverflow answer <https://stackoverflow.com/a/49805742/5684257>`__

   ::

     newtype ZipList a = ZipList [a] -- Like the one in Control.Applicative

     pattern ZNil :: ZipList a
     pattern ZNil = ZipList []
     pattern ZCons :: a -> ZipList a -> ZipList a
     pattern ZCons x xs <- ZipList (x : (ZipList -> xs))
       where ZCons x (ZipList xs) = ZipList $ x : xs

   The purpose of these synonyms is to work with ``ZipList``\s without accidentally using a typeclass instance for ``[]``, which is easy to do with a pattern like ``ZipList (x:xs)``. ``ZNil``, as expected, is quite simple. However, even though it seems reasonable that ``ZCons`` would be similarly simple, it must be written as an explicitly bidirectional synonym, and it requires a view pattern.

2.
   ::

     data D = D1 String Bool | D2 String Int | D3 Int

     _DString :: D -> Maybe String
     _DString (D1 s _) = Just s
     _DString (D2 s _) = Just s
     _DString _ = Nothing
     pattern DString :: String -> D -- get the String, if possible
     pattern DString s <- (_DString -> Just s)

   It is impossible for a pattern synonym to cover multiple cases without using a view pattern. The function used by the view pattern must be total, or else the pattern synonym would diverge instead of just failing, so it uses ``Maybe``. However, this is not intuitive.

3. Adapted from `this StackOverflow answer <https://stackoverflow.com/a/50548724/5684257>`__

   ::

     -- type of numbers n such that xs !! n = x
     data Elem (x :: k) (xs :: [k]) where
       Here  :: Elem x (x : xs)
       There :: Elem x xs -> Elem x (y : xs)
     -- sum type represented by a value and a tag for which variant it is
     data Sum :: [*] -> * where
       Sum :: Elem t ts -> t -> Sum ts

     -- Either-style Left
     Inl :: forall ts. () =>
            forall t ts'. (ts ~ (t : ts')) =>
            t -> Sum ts
     pattern Inl x = Sum Here x -- again, very simple

     -- Either-style Right
     data Inr' ts = forall t ts'. (ts ~ (t : ts')) => Inr' (Sum ts')
     _Inr :: Sum ts -> Maybe (Inr' ts)
     _Inr (Sum Here _) = Nothing
     _Inr (Sum (There tag) x) = Just $ Inr' $ Sum tag x
     pattern Inr :: forall ts. () =>
                    forall t ts'. (ts ~ (t : ts')) =>
                    Sum ts' -> Sum ts
     pattern Inr x <- (_Inr -> Just (Inr' x))
       where Inr (Sum tag x) = Sum (There tag) x

   This example is more arcane, but the pattern synonym ``Inr`` is even worse. It forces the creation of a whole new existential wrapper ``Inr'`` and of a preprocessing function ``_Inr`` (which must be total even though the pattern synonym won't be complete). This is entirely non-obvious to write, and is quite hard to read.

Proposed Change Specification
-----------------------------
In the following, unidirectional pattern synonyms are those defined by ``<-`` without an expression form defined in a ``where`` clause. An explicitly bidirectional synonym is one defined with ``<-`` with an expression form defined in a ``where`` clause. An implicitly bidirectional synonym is one defined with ``=``. The phrase "bidirectional synonym", when unqualified, refers to both implicitly and explicitly bidirectional synonyms.

Pattern synonyms already depend on the idea of "invertible patterns", or patterns that can also be interpreted as expressions. Invertible patterns occur on the RHS of an implicitly bidirectional pattern synonym. They currently do not have a precise specification, so this proposal first defines them. There should be no changes to the existing behavior:

* A pattern of the form ⟨gcon⟩ ⟨apat\ :subscript:`1`\⟩ ... ⟨apat\ :subscript:`k`\⟩ is invertible if ⟨gcon⟩ is either a data constructor or a bidirectional pattern synonym and every ⟨apat\ :subscript:`i`\⟩ is also an invertible pattern.
* A pattern of the form ⟨lpat⟩ ⟨qconop⟩ ⟨pat⟩ is invertible if ⟨qconop⟩ is a data constructor or a bidirectional pattern synonym, and both ⟨lpat⟩ and ⟨pat⟩ are invertible patterns.
* A pattern of the form ⟨var⟩ is invertible.
* "Constant" patterns, including ⟨literal⟩, negative literals, unboxed literals, etc. are invertible.
* A pattern of the form ``(``\⟨pat⟩\ ``)`` (a parenthesized pattern) is invertible if ⟨pat⟩ is.
* A pattern of the form ⟨qcon⟩ ``{`` ⟨fpat\ :subscript:`1`\⟩\ ``,`` ... ⟨fpat\ :subscript:`k`\⟩\ ``,`` [``..``] ``}`` (a record pattern) is invertible if all of the following are true:

  * ⟨qcon⟩ is either a data constructor or a bidirectional pattern synonym.
  * Every ⟨fpat\ :subscript:`i`\⟩ has either no pattern (``-XNamedFieldPuns``) or an invertible pattern.
  * Either every record field has an associated ⟨fpat\ :subscript:`i`\⟩ or there is a ``..`` at the end (``-XRecordWildcards``).

* A pattern of the form ⟨lpat⟩ ``::`` ⟨type⟩ is invertible if ⟨lpat⟩ is invertible.

(NB: Things that are *not* invertible patterns include bang-patterns, irrefutable patterns, as-patterns, view patterns, unidirectional pattern synonyms, wildcards, and n+k patterns.) The expression interpretation of an invertible pattern is fairly obvious and will not be outlined.

Syntax
~~~~~~

A function is defined by a sequence of equations, each of which has a sequence of (potentially non-invertible) patterns on its LHS and a (potentially non-invertible) expression on its RHS. Unidirectional pattern synonyms are redefined to be the opposite: they are made of a sequence of "equations", each with a sequence of expressions on the LHS and a single pattern on the RHS. A single unidirectional pattern synonym "equation" can be of one these forms ("equation" because there is no ``=``, just ``<-``, but it is conceptually similar):

* ``pattern`` ⟨qcon⟩ ⟨aexp\ :subscript:`1`\⟩ ... ⟨aexp\ :subscript:`k`\⟩ ``<-`` ⟨pat⟩ for *k* ≥ 0.
* ``pattern`` ⟨aexp⟩ ⟨qconop⟩ ⟨aexp⟩ ``<-`` ⟨pat⟩
* ``pattern`` ⟨qcon⟩ ``{`` ⟨fbind\ :subscript:`1`\⟩ ... ⟨fbind\ :subscript:`k`\⟩ ``}`` ``<-`` ⟨pat⟩ for *k* ≥ 1.

A full unidirectional pattern synonym declaration may consist of several of these. Like functions, the equations for a single synonym must be contiguous, and their order matters. Further, all of the equations must be of the same type: either all of them are of the first kind, all are of the second kind, or all of them are of the third kind. Each equation must have the same number of fields, and, if applicable, the record fields must be the same, and in the same order. The difference from the current syntax is that, instead of just variable names, the LHS can contain arbitrary expressions, and there can be more than one equation.

Bidirectional pattern synonyms combine functions with unidirectional pattern synonyms. Implicitly bidirectional synonyms do so by "taking the intersection": the LHS arguments and the RHS body must all be invertible. They, too, can now have multiple equations. A single equation looks like one of:

* ``pattern`` ⟨qcon⟩ ⟨apat\ :subscript:`1`\⟩ ... ⟨apat\ :subscript:`k`\⟩ ``=`` ⟨pat\ :subscript:`r`\⟩ for *k* ≥ 0, where all of ⟨apat\ :subscript:`i`\⟩ and ⟨pat\ :subscript:`r`\⟩ are invertible.
* ``pattern`` ⟨apat⟩ ⟨qconop⟩ ⟨apat⟩ ``<-`` ⟨pat\ :subscript:`r`\⟩, where both ⟨apat⟩s and ⟨pat\ :subscript:`r`\⟩ are invertible.
* ``pattern`` ⟨qcon⟩ ``{`` ⟨fpat\ :subscript:`1`\⟩ ... ⟨fpat\ :subscript:`k`\⟩ ``}`` ``<-`` ⟨pat\ :subscript:`r`\⟩ for *k* ≥ 1, where ⟨pat\ :subscript:`r`\⟩ is invertible and every ⟨fpat\ :subscript:`i`\⟩ either has no pattern or an invertible pattern. Additionally, the LHS must be linear, in that no term variable is bound more than once. For compatibility, a ``-XNamedFieldPuns`` style binding is allowed even when the extension is disabled.

For multiple equations, the restrictions are the same as those for unidirectional synonyms.

Explicitly bidirectional synonyms are another way of combining unidirectional synonyms and functions. They consist of a unidirectional synonym and a function simply stuck together under one name. This proposal does not change the function part, and the synonym part changes in the same way as standalone unidirectional pattern synonyms (this includes multiple equations).

Since 1 equation is trivially a sequence of equations, and since variables are invertible patterns, all existing pattern synonyms should continue to work.

Semantics
~~~~~~~~~
To the informal semantics of pattern matching, outlined in the Haskell Report §3.17.2, add this rule:

* Matching the pattern ⟨con⟩ ⟨pat\ :subscript:`1`\⟩ ... ⟨pat\ :subscript:`n`\⟩ against a value ``v``, where ⟨con⟩ is a pattern synonym with RHSs ``r``\ :subscript:`1` ... ``r``\ :subscript:`m`, ordered from the first declared to the last, proceeds as follows:

  1. The value ``v`` is matched against each ``r``\ :subscript:`i` in turn, until one of them succeeds. If one of them diverges before a success is found, the whole match diverges. If all of them fail, the whole match fails. The matching RHS's corresponding LHS expressions are now called ``l``\ :subscript:`1` ... ``l``\ :subscript:`n`. This match also binds some variables.
  2. Match ⟨pat\ :subscript:`1`\⟩ with ``l``\ :subscript:`1` (which may refer to the previously bound variables) ... match ⟨pat\ :subscript:`n`\⟩ with ``l``\ :subscript:`n`. If any fail or diverge, so does the whole match.
  3. If all of these matches succeed, so does the whole match, binding the variables bound by all the ⟨pat\ :subscript:`i`\⟩. Note that the variables bound by ``ri`` are not bound by the whole match; they remain local to the pattern synonym. Also note that the argument patterns do not affect which equation is chosen.

More formally, for a pattern synonym ``P`` with RHSs ``r1`` ... ``rn``, where each ``ri`` binds variables ``xi1`` ... ``xim`` and has corresponding LHS expressions ``li1`` ... ``lil``, the following equation (in the style of the Haskell Report §3.17.3) holds:

::

  case v of
       P v1 ... vl -> e
       _ -> e'
  =
  case v of
       [f11/x11]...[f1m/x1m]r1 -> case [f11/x11]...[f1m/x1m]l11 of
                                        v1 -> ... case [f11/x11]...[f1m/f1m]l1l of
                                                       vl -> e
                                                       _ -> e'
                                        _ -> e'
       ... [fn1/xn1]...[fnm/xnm]rn -> case [fn1/xn1]...[fnm/xnm]ln1 of
                                           v1 -> ... case [fn1/xn1]...[fnm/xnm]lnl of
                                                     vl -> e
                                                     _ -> e'
                                           _ -> e'
       _ -> e'

where ``[a/b]`` denotes substituting ``a`` in place of ``b``, and all of ``fij`` are fresh variables. This equation also holds for all current pattern synonyms. The only difference now is that all of ``lij`` can be expressions, and there is the possibility of multiple equations.

If ``P`` is an explicitly bidirectional synonym, a function application to ``P`` simply goes to the function part of its definition. If it is an implicitly bidirectional synonym, then all of ``li`` are actually invertible patterns, and a function application acts as if ``P`` were a function defined by:

::

  P l11 ... l1l = r1
  ...
  P ln1 ... lnl = rn

Again, this is very similar to the current behavior, except ``P`` can now do pattern matching when used as a function, and can have multiple equations

Pattern synonym record selectors are defined as follows, where ``fi`` is a field of the pattern synonym ``P``:

::

  fi P {fi = fi} = fi

(The current rule is ``fi r = fi``, where ``fi`` needs to be bound by ``r``.)

The definition of pattern synonym record updates and pattern synonym record constructions do not change, as they are defined in terms of simple desugarings to pattern matches and function applications.

Typing
~~~~~~
Pattern synonyms have *pattern types*, which are of the form

::

  pattern P :: forall u1 ... un. -- universal type variables
               (req) => -- required context; may refer to all ui but none of ej
               forall e1 ... em. -- existential type variables
               (prv) => -- provided context; may refer to all of ui and ej
               a1 -> ... -> al -> -- matched values; may refer to all of ui and ej
               r -- result type; may only refer to ui

When typing a unidirectional pattern synonym, let the RHSs be ``r``\ :subscript:`1` ... ``r``\ :subscript:`a`, where each ``r``\ :subscript:`i` has corresponding LHS expressions ``l``\ :subscript:`i,1` ... ``l``\ :subscript:`i,l`. The result type ``r`` is the type of values that are matched by all of the ``r``\ :subscript:`i`. (I.e. if a function ``f`` were defined by the equations ``f r1 = undefined`` ... ``f ra = undefined``, ``r`` would be the argument type of ``f``). For each sequence of LHS expressions ``l``\ :subscript:`i,1` ... ``l``\ :subscript:`i,l`, there must be a substitution of the existential variables (``e``\ :subscript:`1` ... ``e``\ :subscript:`m`) such that the type of each ``l``\ :subscript:`i,j` unifies with ``a``\ :subscript:`j`. (This substitution may be different between equations, but must be consistent within each one.) The LHS expressions themselves are typed with the variables (terms and types) bound by the RHS in scope. Generalization places any unsolved type variables into the universal list. Any constraints that are required by any of the RHSs to match a value must appear in ``req``. Any constraints that all of the RHSs provide may appear in ``prv``. For every equation, any constraints that are required by the LHS but not provided by the RHS must appear in ``req``.

When the type of a unidirectional pattern synonym is being inferred, all of the type variables and constraints that are bound by all of the equations are added to the existentials and provided contexts, respectively. When the type is given explicitly, as usual, it may be less general than the inferred one. As with functions, the required context may be larger than needed, the type variables reordered, or some of the universals instantiated. Addtionally, the provided context may be shrunken, and so may be the existential variables.

Implicitly bidirectional synonyms are type checked in a similar way to unidirectional pattern synonyms. However, the handling of contexts is slightly different. If a constraint is provided by both the LHS and the RHS and required by neither, then it does not need to appear in either the required or provided contexts of the synonym.

Currently, explicitly bidirectional synonyms are type checked by checking the two parts (the unidirectional synonym and the function) separately, and then making sure they are consistent with one another. This does not change.

Effect and Interactions
-----------------------
``ZCons``, from above, becomes

::

  pattern ZCons x (ZipList xs) = ZipList (x : xs)

An example usage may look like the following:

::

  f (ZCons x (ZCons _ xs)) = ZCons x (f xs)
  f xs = xs

This is equivalent to

::

  f arg = case arg of -- desugar argument pattern; manipulate a bit further
               ZCons x (ZCons _ xs) -> ZCons x (f xs)
               _ -> case arg of
                         xs -> xs
        = case arg of -- semantics of pattern synonym matches (taken literally; this produces a mess)
               ZipList (fresh1:fresh2) -> case fresh1 of -- RHS of ZCons; now match the argument patterns (x, ZCons _ xs) with the LHS expressions (fresh1, ZipList fresh2)
                                               x -> case ZipList fresh2 of
                                                         ZipList (fresh3:fresh4) -> case fresh3 of -- RHS of ZCons; now match the argument patterns (_, xs) with the LHS expressions (fresh3, ZipList fresh4)
                                                                                         _ -> case ZipList fresh4 of
                                                                                                   xs -> ZCons x (f xs)
                                                                                                   _ -> case arg of xs -> xs -- failure case gets duplicated a bunch; usually unreachable
                                                                                         _ -> case arg of xs -> xs
                                                         _ -> case arg of xs -> xs
                                               _ -> case arg of xs -> xs
               _ -> case arg of xs -> xs
        = case arg of -- simplify
               ZipList (x:_:xs) -> case f xs of -- inline ZCons (as a function)
                                        ZipList xs -> ZipList (x : xs)
               xs -> xs


This is ``DString``, now

::

  pattern DString s <- D1 s _
  pattern DString s <- D2 s _

  -- usage
  f (DString s) = s
  f (D3 i) = show i
  -- equivalent
  f arg = case arg of
               DString s -> s
               _ -> case arg of D3 i -> show i
        = case arg of
               D1 s _ -> s
               D2 s _ -> s
               D3 i -> show i
  -- or
  f (D1 s _) = s
  f (D2 s _) = s
  f (D3 i) = show i

``Inr``'s transformation is more drastic

::

  pattern Inr (Sum tag x) = Sum (There tag) x

  -- usage
  double :: Sum [Int, Char, Float] -> Double
  double (Inl i) = fromIntegral i
  double (Inr (Inl c)) = fromIntegral $ fromEnum c
  double (Inr (Inr (Inl f))) = float2Double f
  -- equivalent
  double (Sum Here i) = fromIntegral i
  double (Sum (There free1) free2) = case Sum free1 free2 of -- Inr
                                          Sum Here c -> fromIntegral $ fromEnum c
                                          Sum (There free3) free4 -> case Sum free3 free4 of
                                                                          Sum Here f -> float2Double f
  -- or even
  double (Sum Here i) = fromIntegral i
  double (Sum (There Here) c) = fromIntegral $ fromEnum c
  double (Sum (There (There Here)) f) = float2Double f

There are some interactions with record syntax and its extensions, which should all be covered above. ``-Wincomplete-patterns`` will now warn if an implicitly bidirectional pattern synonym's LHSs are not covering. ``-Woverlapping-patterns`` will now warn if a unidirectional synonym's RHSs make an equation unreachable. It will also warn on implicitly bidirectional synonyms, but only if an equation is unreachable *both* in expression form and in pattern form. That is

::

  pattern P False = 0
  pattern P True  = 1
  pattern P False = 2

will emit no warnings. The final equation is unreachable when ``P`` is used as a function, as ``P False`` triggers the first equation. However, it is not unreachable when ``P`` is used as a pattern; ``case 2 of P b -> b`` is ``False``.

The typing rules are a bit labyrinthine, so here are a couple examples.

*
  ::

    pattern WellOfLies undefined <- _
    -- inferred
    -- pattern WellOfLies :: forall {a} {b}. a -> b -- {var} denoting variables not available to type application, as usual

  This is also the type of the synonym

  ::

    pattern WellOfLies' lie <- (const undefined -> lie)
    -- inferred
    -- pattern WellOfLies' :: forall {a} {b}. a -> b

  which is possible today.

*
  ::

    data Dict con = con => Dict
    pattern IdentityWithExtraSteps :: forall con. Dict con -> Dict con
    pattern IdentityWithExtraSteps Dict = Dict
    -- inferred
    -- pattern IdentityWithExtraSteps :: forall a b. a => b => Dict a -> Dict b

  The first thing is to understand the inferred signature. Note that there is no reason for the ``Dict``\s on the LHS and RHS to be of the same constraint. Thus, the one on the left has type ``_a => Dict _a`` for a unification variable ``_a``, and the one on the left ``_b => Dict _b`` for another variable ``_b``. These get generalized, so the inferred signature has two universals and no existentials. Note how either side can introduce new universal variables. ``b`` is provided by the RHS, so it appears in the provided contraints. ``a`` is required to construct the ``Dict a``, so it appears in the required constraints.

  Understanding why the supplied signature is legal takes a bit of thinking. First, instantiate ``a ~ b``. Then, rename them to ``con`` and shrink the provided contraints from ``con`` to ``()``. This leaves ``forall con. con => Dict con -> Dict con``. However, ``con`` is provided on both sides and is required on neither, so it can also be omitted from the required constraints, as when one side, acting as an expression, requires it, the other, as a pattern, provides it.

Costs and Drawbacks
-------------------
The learning curve for new users, if anything, is reduced, because the new syntax is more intuitive than the twistiness of view patterns. The nice symmetry with functions can only help.

The current implementation of pattern synonyms actually seems quite amenable to these changes. They are currently implemented as pairs of functions: a matcher that takes a success continuation, a failure continuation, and a scrutinee, matches on the scrutinee, and calls either the success continuation with the bound variables or the failure continuation, and a builder, which is already an arbitrary function (because of explicitly bidirectional synonyms). This proposal should be implementable, after the required parsing changes, by giving implicitly bidirectional synonyms' builders the ability to pattern match, and giving all matchers the ability to have multiple cases and to modify the bound values before calling the success continuation. However, this is added complexity, so something may always go wrong.

All existing pattern synonyms should continue to work, since they all have variables on the LHS, and variables are invertible patterns. It is a bug in this proposal if anything breaks.

Alternatives
------------
None so far.

Unresolved questions
--------------------
* n+k patterns should also be invertible, but

  ::

    pattern P a = a + 5

  is already rejected, and it's probably not worth the effort.
* Admitting as-patterns as invertible is possible but would require interesting contortions of the scoping rules and is currently not accepted. Is it worth it?

Implementation Plan
-------------------
TBA
