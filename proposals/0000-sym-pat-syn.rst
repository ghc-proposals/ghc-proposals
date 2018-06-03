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

    import Control.Applicative(ZipList(..))
    pattern ZCons :: a -> ZipList a -> ZipList a
    pattern ZCons x xs <- ZipList (x : (ZipList -> xs))
      where ZCons x (ZipList xs) = ZipList $ x : xs

  The purpose of this synonym is to work with ``ZipList``\s without accidentally using a typeclass instance for ``[]``, which is easy to do with a pattern like ``ZipList (x:xs)``. However, even though it is very simple, it must be written as an explicitly bidirectional synonym, and it requires a view pattern.

2. Adapted from `this StackOverflow answer <https://stackoverflow.com/a/50548724/5684257>`__

  ::

    -- type of numbers n such that xs !! n = x
    data Elem (x :: k) (xs :: [k]) where
      Here  :: Elem x (x : xs)
      There :: Elem x xs -> Elem x (y : xs)
    -- sum type represented by a value and a tag for which variant it is
    data Sum :: [*] -> * where
      Sum :: Elem t ts -> t -> Sum ts

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
* A pattern of the form ⟨qcon⟩ ``{`` ⟨fpat\ :subscript:`1`\⟩ ... ⟨fpat\ :subscript:`k`\⟩ ``}`` (a record pattern) is invertible if all of the following are true:
   * ⟨qcon⟩ is either a data constructor or a bidirectional pattern synonym.
   * Every ⟨fpat\ :subscript:`i`\⟩ has either no pattern (``-XNamedFieldPuns``) or an invertible pattern.
   * Either every record field has an associated ⟨fpat\ :subscript:`i`\⟩ or there is a ``..`` at the end (``-XRecordWildcards``).
* A pattern of the form ⟨lpat⟩ ``::`` ⟨type⟩ is invertible if ⟨lpat⟩ is invertible.

(NB: Things that are *not* invertible patterns include bang-patterns, irrefutable patterns, as-patterns, view patterns, unidirectional pattern synonyms, wildcards, and n+k patterns.) The expression interpretation of an invertible pattern is fairly obvious and will not be outlined.

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

For a unidirectional pattern synonym, all of the variables bound on the RHS (both terms and types) are in scope for the LHS expressions. For an implicitly bidirectional synonym, every variable bound on one side must appear on the other. This is a simple generalization of the existing behavior.

During a pattern match against a pattern synonym, the scrutinee is first matched against the synonym's RHS. If it suceeds, the expressions on the synonym's LHS (which may reference variables that were bound by the RHS) are matched against the corresponding patterns at the usage of the synonym. This is analagous to how pattern synonyms work currently, except the produced values may be expressions depending on the variables bound on the RHS instead of just the variables.

For a pattern synonym record field access, the value being scrutinized is matched against the RHS of the synonym, and the value of the expression associated with the field in question is returned. For a pattern synonym record field update, all the fields involved must be belong to the same synonym, or it is a compile-time error. The value being updated is matched against the RHS of the synonym, and the LHS gives associations bewteen fields and their values. These associations are updated with the given record update, and the new set of associations is turned back into a value by using the pattern synonym as a record constructor. Again, this is just how it works currently, except that the record fields are allowed to be associated with expressions instead of just being the bound variables.

When an implicitly bidirectional synonym is used as an expression (that is, as a function), or when it is used as a record constructor, the incoming values are matched against the corresponding patterns on the LHS, and the result is the value of the RHS expression with the values matched by the LHS. Currently, the values are substituted into the RHS directly, as the LHS cannot contain patterns. There is no change to the behavior of explicitly bidirectional synonyms in this regard.

Effect and Interactions
-----------------------
``ZCons``, from above, becomes

  ::

    pattern ZCons x (ZipList xs) = ZipList (x : xs)

Just for example, when matching ``ZipList [1,2,3]`` against ``ZCons 1 ys``, the value is first matched against ``ZCons``'s RHS, causing ``x = 1`` and ``xs = [2,3]``. The expression ``x`` is matched against ``1``, which succeeds. The expression ``ZipList xs`` is matched against ``ys``, causing ``ys = ZipList [2,3]``.

``Inr``'s transformation is more drastic

  ::

    pattern Inr (Sum tag x) = Sum (There tag) x

When evaluating ``Inr (Sum Here 'a')``, everything proceeds as with a function. The value is matched against the LHS, producing ``tag = Here`` and ``x = 'a'``. The result is the RHS with the appropriate substiutions: ``Sum (There Here) x``.

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
