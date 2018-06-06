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

2. Adapted from `this StackOverflow answer <https://stackoverflow.com/a/50548724/5684257>`__

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

A function has a sequence of (potentially non-invertible) patterns on its LHS, and a (potentially non-invertible) expression on its RHS. Unidirectional pattern synonyms are redefined to be the opposite: they have a sequence of expressions on the LHS and a single pattern on the RHS. A unidirectional pattern synonym definition can be of one these forms:

* ``pattern`` ⟨qcon⟩ ⟨aexp\ :subscript:`1`\⟩ ... ⟨aexp\ :subscript:`k`\⟩ ``<-`` ⟨pat⟩ for *k* ≥ 0.
* ``pattern`` ⟨aexp⟩ ⟨qconop⟩ ⟨aexp⟩ ``<-`` ⟨pat⟩
* ``pattern`` ⟨qcon⟩ ``{`` ⟨fbind\ :subscript:`1`\⟩ ... ⟨fbind\ :subscript:`k`\⟩ ``}`` ``<-`` ⟨pat⟩ for *k* ≥ 1.

The difference from the current syntax is that, instead of just variable names, the LHS can contain arbitrary expressions.

Bidirectional pattern synonyms combine functions with unidirectional pattern synonyms. Implicitly bidirectional synonyms do so by "taking the intersection": the LHS arguments and the RHS body must all be invertible. They look like one of

* ``pattern`` ⟨qcon⟩ ⟨apat\ :subscript:`1`\⟩ ... ⟨apat\ :subscript:`k`\⟩ ``=`` ⟨pat\ :subscript:`r`\⟩ for *k* ≥ 0, where all of ⟨apat\ :subscript:`i`\⟩ and ⟨pat\ :subscript:`r`\⟩ are invertible.
* ``pattern`` ⟨apat⟩ ⟨qconop⟩ ⟨apat⟩ ``<-`` ⟨pat\ :subscript:`r`\⟩, where both ⟨apat⟩s and ⟨pat\ :subscript:`r`\⟩ are invertible.
* ``pattern`` ⟨qcon⟩ ``{`` ⟨fpat\ :subscript:`1`\⟩ ... ⟨fpat\ :subscript:`k`\⟩ ``}`` ``<-`` ⟨pat\ :subscript:`r`\⟩ for *k* ≥ 1, where ⟨pat\ :subscript:`r`\⟩ is invertible and every ⟨fpat\ :subscript:`i`\⟩ either has no pattern or an invertible pattern. Additionally, the LHS must be linear, in that no term variable is bound more than once. For compatibility, a ``-XNamedFieldPuns`` style binding is allowed even when the extension is disabled.

Similarly, the difference from the current syntax is that the LHS may contain arbitrary invertible patterns instead of just variables. Since variables are invertible patterns (and thus expressions), these changes should not break existing code.

Explicitly bidirectional synonyms are another way of combining unidirectional synonyms and functions. They consist of a unidirectional synonym and a function simply stuck together under one name. This proposal does not change the function part, and the synonym part changes in the same way as standalone unidirectional pattern synonyms.

Typing
~~~~~~
Pattern synonyms have *pattern types*, which are of the form

::

  pattern P :: forall u1 ... un. -- universal type variables
               (req) => -- required context; may refer to all ui but none of ei
               forall e1 ... em. -- existential type variables
               (prv) => -- provided context; may refer to all of ui and ei
               a1 -> ... -> an -> -- matched values; may refer to all of ui and ei
               r -- result type; may only refer to ui

If a pattern synonym is not given a signature, its type is currently inferred as if it were written as a unidirectional pattern synonym. This is changed, so the whole synonym is considered. Type *checking*, of course, continues to consider everything.

For a unidirectional patttern synonym, the result type ``r`` is the type of values that the RHS matches. The provided context ``prv`` is composed of the constraints provided by the matching of the RHS. The existentials are type variables that are provided by matching the RHS. The expressions on the LHS are typed with the variables (terms and types) and context matched from the RHS in scope. The types of the matched values ``ai`` are the types of the corresponding expressions. Any unsolved type variables on either side are added to the universal type variables. Any constraints required by the RHS must appear in the ``req`` constraints. Any constraints required by the LHS must either appear in the ``req`` constraints or must be matched from the RHS.

Implicitly bidirectional synonyms are type checked in a similar way to unidirectional pattern synonyms. However, the handling of contexts is slightly different. If a constraint is provided by both the LHS and the RHS and required by neither, then it does not need to appear in either the required or provided contexts of the synonym.

Explicitly bidirectional synonyms have their unidirectional synonym and function parts type checked separately. The whole synonym's type is formed by combining them. Every required constraint of the pattern synonym part must be in ``req``. Only the provided constraints can be in ``prv``. Any constraints required by the function part must appear in either ``req`` or ``prv``. The universal variables and the matched and result types are computed via unification of the unidirectional synonym's type and the function's type.

Semantics
~~~~~~~~~
To the informal semantics of pattern matching, outlined in the Haskell Report §3.17.2, add this rule:

* Matching the pattern ⟨con⟩ ⟨pat\ :subscript:`1`\⟩ ... ⟨pat\ :subscript:`n`\⟩ against a value ``v``, where ⟨con⟩ is a pattern synonym with RHS ``r`` and LHS expressions ``l``\ :subscript:`1` ... ``l``\ :subscript:`n`, proceeds as follows:

  1. The value ``v`` is matched against ``r``. If this fails or diverges, so does the whole match.
  2. Match ⟨pat\ :subscript:`1`\⟩ with ``l``\ :subscript:`1` (which may refer to the previously bound variables), ... match ⟨pat\ :subscript:`n`\⟩ with ``l``\ :subscript:`n`. If any fail or diverge, so does the whole match.
  3. If all of these matches succeed, so does the whole match, binding the variables bound by all the ⟨pat\ :subscript:`i`\⟩. Note that the variables bound by ``r`` are not bound by the whole match; they remain local to the pattern synonym.

More formally, for a pattern synonym ``P`` with RHS ``r``, which binds variables ``x1``, ... ``xn``, and LHS expressions ``l1``, ... ``lm``, the following equation (in the style of the Haskell Report §3.17.3) holds:

::

  case v of
       P v1 ... vm -> e
       _ -> e'
  =
  case v of
       [f1/x1]...[fn/xn]r -> case [f1/x1]...[fn/xn]l1 of
                                  v1 -> ... case [f1/x1]...[fn/fn]lm of
                                                 vm -> e
                                                 _ -> e'
                                  _ -> e'
       _ -> e'

where ``[a/b]`` denotes substituting ``a`` in place of ``b``, and all of ``fi`` are fresh variables. This equation also holds for all current pattern synonyms. The only difference now is that all of ``li`` can be expressions.

If ``P`` is an explicitly bidirectional synonym, a function application to ``P`` simply goes to the function part of its definition. If it is an implicitly bidirectional synonym, then all of ``li`` are actually invertible patterns, and a function application acts as if ``P`` were a function defined by:

::

  P l1 ... lm = r

Again, this is very similar to the current behavior, except ``P`` can now do pattern matching when used as a function.

Pattern synonym record selectors are defined as follows, where ``fi`` is a field of the pattern synonym ``P`` with corresponding LHS expression ``li`` and with RHS ``r``:

::

  fi r = li

(The current rule is ``fi r = fi``, where ``fi`` needs to be bound by ``r``. This obviously stops working here.)

The definition of pattern synonym record updates and pattern synonym record constructions do not change, as they are defined in terms of simple desugarings to pattern matches and function applications.

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

There are some interactions with record syntax and its extensions, which should all be covered above. ``-Wincomplete-patterns`` will now warn if an implicitly bidirectional pattern synonym's LHS is not covering.

Costs and Drawbacks
-------------------
The learning curve for new users, if anything, is reduced, because the new syntax is more intuitive than the twistiness of view patterns. The nice symmetry with functions can only help.

The current implementation of pattern synonyms actually seems quite amenable to these changes. They are currently implemented as pairs of functions: a matcher that takes a success continuation, a failure continuation, and a scrutinee, matches on the scrutinee, and calls either the success continuation with the bound variables or the failure continuation, and a builder, which is already an arbitrary function (because of explicitly bidirectional synonyms). This proposal should be implementable, after the required parsing changes, by giving implicitly bidirectional synonyms' builders the ability to pattern match, and giving all matchers the ability to modify the bound values before calling the success continuation. However, this is added complexity, so something may always go wrong.

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
* Admitting as-patterns as invertible is possible but would require interesting contorsions of the scoping rules and is currently not accepted. Is it worth it?

Implementation Plan
-------------------
TBA
