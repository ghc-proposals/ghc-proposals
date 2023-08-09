Or Patterns
==============

.. author:: Sebastian Graf, David Knothe
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/609>`_.
.. sectnum::
.. contents::

Instead of spelling out all the trivial cases of a function, we often use wildcards to summarise shared behaviour. This however becomes inconvenient once we extend the domain datatype by adding new constructors: the function in question does still compile as the newly added constructor is subsumed in the function's wildcard pattern. This is often unwanted and makes refactoring hard, because existing tools provide no way to find functions that may need to be adjusted after the new constructor was added. `GHC issue #21572 <https://gitlab.haskell.org/ghc/ghc/-/issues/21572>`_ is a recent example.

We propose a new syntax extension for "Or patterns": *an Or pattern matches
a list of patterns, none of which matches any variables.*
Compared to writing out one clause per pattern, Or patterns endorse code reuse of the shared right-hand side.
Compared to wildcard patterns, Or patterns list matched patterns explicitly, to support refactorings in the above sense.

Motivation
----------
Suppose we had this type:

::

    data T = T1 String | T2 Int | T3 Int

We might want to write a function on this like

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT _      = Nothing

Now suppose that some time later we add a new constructor:

::

    data T = T1 String | T2 Int | T3 Int | T4 String

We need to update ``stringOfT`` but unfortunately we don't get a warning because
we used a wildcard (``_``) pattern.

Wildcard patterns make code harder to maintain. They essentially mean "match
every other pattern", which also includes "patterns that may be enabled in the
future" e.g. when a new constructor is added to a type.

In our experience this is rarely the intention; usually, when a new constructor
is added, we need to revisit functions on the type and update them accordingly.
But if functions use ``_`` patterns this is not easy as we won't be getting any
pattern match warnings about functions we need to update.

As a result, adding a new constructor to an
existing type means many compile-run-edit cycles, with no compile-time help.

We propose a new language extension, `-XOrPatterns`, to solve this problem by allowing
programmers to explicitly match a list of constructors in a concise way. For the above
function we get

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s)        = Just s
    stringOfT (T2{}; T3{}) = Nothing

This function doesn't match ``T4``, so we get our warning in the very first compile
cycle or (even faster) in our IDE powered by a language server implementation.


Proposed Change Specification
-----------------------------

Changes in the grammar
~~~~~~~~~~~~~~~~~~~~~~

We consider this as an extension to GHC's .
We consider this as an extension to `Haskell 2010 grammar
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_.
This proposal adds one more production to the nonterminal ``pat``: ::

    pat -> pat_1; ...; pat_n (n >= 2)

Regarding pattern synonym declarations, we propose to use ``infixpat`` instead of ``pat`` on the right side of the pattern synonym.

Concretely, as Haskell 2010 grammar does not cover pattern synonyms, in GHC's `Parser.y <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y>`_, the ``pattern_synonym_decl`` nonterminal would then have the following productions: ::

    pattern_synonym_decl -> 'pattern' pattern_synonym_lhs '=' infixpat
                          | 'pattern' pattern_synonym_lhs '<-' infixpat
                          | 'pattern' pattern_synonym_lhs '<-' infixpat where_decls

where ``infixpat`` is a nonterminal only delegating to ``infixexp``.


N.B.: For the concrete implementation in `Parser.y <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y>`_, we would need to amend both the ``aexp`` and the ``pat`` nonterminals.
This is an effect of the ECP parsing. In particular, ::

  aexp -> ( orpats )

and ::

  pat -> pat ';' orpats

where ``orpats`` is a new nonterminal: ::

   orpats -> exp
           | exp ';' orpats

**Some examples that this new grammar produces:**

Or patterns with parentheses: ::

  case e of (T1; T2{}; T3 a b) -> ...

  f :: (Int, Int) -> Int
  f (5, (6;7)) = 2

Unparenthesized Or patterns: ::

  case e of
    1; 2; 3 -> x
    4; (5; 6) -> y

Unparenthesized Or patterns using layout: ::

  sane e = case e of
    1
    2
    3 -> a
    4
    5;6 -> b
    7;8 -> c

  insane e = case e of
    A _ _; B _
    C -> 3
    (D; E (Just _) Nothing)
     -> 4
    F -> 5

N.B.: Unparenthesized Or patterns only work in some places where patterns are expected. For example, in ::

  g x = do
    A; B <- x
    return 1

the ``A; B <- x`` is interpreted as two statements. Parentheses would have to be used around ``A; B`` to make it denote an Or pattern.


N.B.: The new grammar allows Or patterns which bind variables. These will however be rejected in `2.2`_.

.. _2.2:

Static semantics of Or pattern matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or patterns which bind variables are rejected in the renamer.


We give the static semantics in terms of *pattern types*. A pattern type has the form ``Γ, Σ ⊢ pat : τ ⤳ Γ,Σ,Ψ`` where

 - Γ is an in/out param that corresponds to a binding context that is populated with match vars
 - Σ is an in/out param that collects Given constraints. So Σ\ :sub:`in`\  is used to discharge Θ\ :sub:`req`\  and Σ\ :sub:`out`\  contains any Θ\ :sub:`prov`\  unleashed by the match.
 - Ψ collect existential variables

Then the typing rule for Or patterns is:
::

      Γ0, Σ0 ⊢ pat_i : τ ⤳ Γ0,Σi,Ψi
    ---------------------------------
    Γ0, Σ0 ⊢ ( pat_1; ...; pat_n ) : τ ⤳ Γ0,Σ0,∅



Dynamic semantics of Or pattern matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Informal semantics in the style of `Haskell 2010 chapter 3.17.2: Informal
Semantics of Pattern Matching
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-600003.17.2>`_:

- Matching the pattern ``(p1; ...; pk)`` against the value ``v`` is the result of matching ``v`` against ``p1`` if it is not a failure, or the result of
  matching ``(p2; ...; pk)`` against ``v`` otherwise. We require that ``p1``, …, ``pk`` bind no variables.
- Matching the pattern ``(p1)`` against the value ``v`` performs a normal pattern match.


Here are a few examples: ::

    (\ (1; 2) -> 3) 1 => 3
    (\ (Left 0; Right 1) -> True) (Right 1) => True
    (\ (([1]; [2, _]); ([3, _, _]; [4, _, _, _])) -> True) [4, undefined, undefined, undefined] => True
    (\ (1; 2; 3) -> True) 3 => True

We do not employ backtracking in Or patterns. The following would yield ``"no backtracking"``: ::

 case (True, error "backtracking") of
   ((True, _); (_, True)) | False -> error "inaccessible"
   _ -> error "no backtracking"

Examples
--------

- GHC has lots of code like this: (taken from
  ``HS/Pat.hs``, slightly simplified) ::

    isIrrefutableHsPat pat
      = go pat
      where
        go (L _ pat) = go1 pat

        go1 (WildPat {})        = True
        go1 (VarPat {})         = True
        go1 (LazyPat {})        = True
        go1 (BangPat pat)       = go pat
        go1 (CoPat _ pat _)     = go1 pat
        go1 (ParPat pat)        = go pat
        go1 (AsPat _ pat)       = go pat
        go1 (ViewPat _ pat _)   = go pat
        go1 (SigPatIn pat _)    = go pat
        go1 (SigPatOut pat _)   = go pat
        go1 (TuplePat pats _ _) = all go pats
        go1 (SumPat pat _ _  _) = go pat
        go1 (ListPat {})        = False
        go1 (PArrPat {})        = False
        go1 (ConPatIn {})       = False
        go1 (ConPatOut{ pat_con = L _ (RealDataCon con), pat_args = details }) = ...
        go1 (ConPatOut{ pat_con = L _ (PatSynCon _pat) }) = ...
        go1 (LitPat {})         = False
        go1 (NPat {})           = False
        go1 (NPlusKPat {})      = False
        go1 (SplicePat {})      = urk pat

        urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

  Using Or patterns this code can be simplified to: ::

    isIrrefutableHsPat pat
      = go pat
      where
        go (L _ pat) = go1 pat

        go1 (WildPat{}; VarPat{}; LazyPat{})
          = True

        go1 (PArrPat{}; ConPatIn{}; LitPat{}; NPat{}; NPlusKPat{}; ListPat{})
          = False

        go1 (BangPat pat)       = go pat
        go1 (CoPat _ pat _)     = go1 pat
        go1 (ParPat pat)        = go pat
        go1 (AsPat _ pat)       = go pat
        go1 (ViewPat _ pat _)   = go pat
        go1 (SigPatIn pat _)    = go pat
        go1 (SigPatOut pat _)   = go pat
        go1 (CoPat _ pat _)     = go1 pat
        go1 (TuplePat pats _ _) = all go pats
        go1 (ConPatOut{ pat_con = L _ (RealDataCon con), pat_args = details }) = ...
        go1 (ConPatOut{ pat_con = L _ (PatSynCon _pat) }) = ...
        go1 (SplicePat {})      = urk pat

        urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)


GHC also has wildcard patterns in many places (here  ``Core.hs``):

::

 hasCoreUnfolding (CoreUnfolding {}) = True
 hasCoreUnfolding (DFunUnfolding {}) = True
 hasCoreUnfolding _                  = False

 isValueUnfolding (CoreUnfolding { uf_is_value = is_evald }) = is_evald
 isValueUnfolding _                                          = False

 isEvaldUnfolding (OtherCon _)                               = True
 isEvaldUnfolding (CoreUnfolding { uf_is_value = is_evald }) = is_evald
 isEvaldUnfolding _                                          = False

 isConLikeUnfolding (OtherCon _)                             = True
 isConLikeUnfolding (CoreUnfolding { uf_is_conlike = con })  = con
 isConLikeUnfolding _                                        = False

 hasSomeUnfolding NoUnfolding   = False
 hasSomeUnfolding BootUnfolding = False
 hasSomeUnfolding _             = True

 neverUnfoldGuidance UnfNever = True
 neverUnfoldGuidance _        = False

 ...

Would ``Unfolding`` be expanded by another constructor, all these functions would still compile but some would become semantically wrong, laying an additional burden on the code author.

Actually, a `recent issue <https://gitlab.haskell.org/ghc/ghc/-/issues/21831>`_ (point 1) has to do with ``isEvaldUnfolding`` and ``isValueUnfolding`` returning ``False`` for too many input values.
Had we had Or patterns, the code authors probably would have thought more thoroughly about the other cases instead of using a wildcard pattern.


Effect and Interactions
-----------------------

The main effect of Or patterns is twofold:

1. With Or patterns developers can avoid ``_`` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. Or patterns allow more code reuse as right hand sides can be shared by many patterns.


GADTs & View Patterns
~~~~~~~~~~~~~~~~~

With existential quantification and GADTs, patterns can not only bind values, but also equality constraints, dictionaries and existential type variables. We described in `2.2`_ how these new constraints are handled: required constraints of the individual patterns are merged while provided constraints are deleted.

So the following example would not type check because the Or pattern doesn't provide the constraint ``a ~ Int``:

::

 data GADT a where
     IsInt1 :: GADT Int
     IsInt2 :: GADT Int

 foo :: a -> GADT a -> a
 foo x (IsInt1 {}; IsInt2 {}) = x + 1


Considering view patterns, these do work seamlessly with Or patterns. As specified in `5`_, Or patterns will just merge the required constraints which come from view patterns. This would work: ::

 f :: (Eq a, Show a) => a -> a -> Bool
 f a (((== a) -> True); (show -> "yes")) = True
 f _ _ = False

.. _5:

Costs and Drawbacks
-------------------
The cost is a small implementation overhead. Also, as Or patterns are syntactic sugar, they add to the amount of syntax Haskell beginners have to learn.
We believe however that the mentioned advantages more than compensate for these disadvantages.
Or patterns are available in all of the top seven programming languages on the TIOBE index (Python, Java, Javascript, C#, C, etc.), which suspects that the concept won't be particularly troublesome for beginners to learn.

Additionally, the changes to the ``pattern_synonym_decl`` nonterminal are required to allow unparenthesized Or patterns in the syntax. This breaks pattern synonym declarations using a pattern signature such as: ::

  pattern X <- 42 :: Int

One would now have to parenthesize the right side of the pattern synonym and write ::

  pattern X <- (42 :: Int)

A search on Hackage shows that this syntax is currently only used (without enclosing parentheses) in `haskell-src-exts-simple <https://hackage-search.serokell.io/?q=%5Epattern.*%3C-.*%3A%3A%5B%5E%5C%29%5D*%24>`_.

The syntax change also makes 3 tests in the `patsyn` testsuite fail, out of 250 total `patsyn` tests. The fix is always to enclose the right side in parentheses.

Alternatives
------------

There have been proposed a **lot** of alternatives in regard to the exact syntax of Or patterns (see the discussion at https://github.com/ghc-proposals/ghc-proposals/pull/585).

After performing two community votes (https://github.com/ghc-proposals/ghc-proposals/issues/587 and https://github.com/ghc-proposals/ghc-proposals/issues/598), the relative majority voted for the here-proposed ``(p1; p2)`` syntax, with ``(p1 | p2)`` being close behind (with 48-43 votes).
So, a suitable alternative would be to use the syntax ``(p1 | p2)``.

While ``|`` is a pretty natural choice regarding an *or* operation, the semicolon does a better job in showing the asymmetry of the pattern as later alternatives are only evaluated when earlier ones fail to match.

Also, the ``(p1 | p2)`` syntax could be better used by a future "guards in patterns" proposal.

Another great advantage of ``;`` over ``|`` is the use of the layout rule: in a layout context introduced by ``of``, semicolons are automatically inserted into equally-indented lines. This makes it possible to write ::

  f x = case x of
    1
    2
    3 -> x

where the Or pattern is implicitly parsed as ``1; 2; 3``.
This resembles the ``switch/case``-syntax known from languages like C and Java.

No unparenthesized Or patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As described in `5`_, the changes to the ``pattern_synonym_decl`` nonterminal are breaking changes in some rare cases.
We could avoid introducing these breaking changes by requiring Or patterns to be parenthesised. This means, the concrete changes to Parser.y would not include the following production ::

      pat -> pat ';' orpats

and would not include the changes to the ``pattern_synonym_decl`` nonterminal.

Beware that it would still be possible to use the layout rule even with parenthesized Or patterns as follows: ::

    case a of
      (A
      B
      C) -> 1

This is an artifact of the layout rule and is not intended to be used.

When disallowing the unparenthesized syntax ``p1; p2``, we do not see much advantage of the ``;`` separator over the ``|`` separator, except that the unparenthesized syntax could be added some time in the future.

Binding pattern variables
~~~~~~~~~~~~~~~~~~

The `parent proposal <https://github.com/ghc-proposals/ghc-proposals/pull/43>`__ allowed Or patterns to bind variables as long as they are shared by all individual patterns:

::

 data T = T1 Int | T2 Int | T3 | T4

 getInt (T1 a; T2 a) = Just a
 getInt (T3; T4) = Nothing

This is a non-goal of this proposal: with binding pattern variables come challenges like binding existential constraints. Correctly specifying the semantics is hard and caused the parent proposal to become dormant after no progress has been made.

Future proposals could build on the current one and further specify it to eventually allow binding pattern variables.

Alternative semantics using view patterns
~~~~~~~~~~~~~~~~~~~~~~

We think the following semantics in terms of view patterns is equivalent.
We could define the semantics of Or patterns as a simple desugaring to view
patterns. The desugaring rule is: ::

    (p1; ...; pk)
    =
    ((\x -> case x of p1 -> True; p2 -> True; …; pk -> True; _ -> False)
        -> True)

The desugaring rule defines both static and dynamic semantics of Or patterns:

An Or pattern type checks whenever the desugared pattern type checks; the dynamic semantics of an Or pattern is the same as the dynamic semantics of its desugared pattern.

But because of forward compatibility we decided not to define it in this way.

Using pattern synonyms
~~~~~~~~~~~~~~~~~~~~~~

Why not just use pattern synonyms? With these we can even bind variables, which is not possible with Or patterns currently!

While true, pattern synonyms require lots of boilerplate code. Wherever we'd use an Or pattern, we would have to write a pattern synonym, a view pattern and a ``COMPLETE`` pragma. Example: ::

 t2OrT3 T2{} = True
 t2OrT3 T3{} = True
 t2OrT3 _    = False

 pattern T2OrT3 :: T
 pattern T2OrT3 <- (t2OrT3 -> True)
 {-# COMPLETE T1, T2OrT3 #-}

It seems that most developers would rather continue conveniently using wildcard patterns instead of making the extra effort required to use pattern synonyms everywhere.

Unresolved Questions
--------------------

Not any at this time.


Implementation Plan
-------------------

Or patterns have been fully implemented by `@knothed <https://github.com/knothed>`__ and `@sgraf812 <https://github.com/sgraf812>`__ [here](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9229).

Endorsements
-------------

Not any so far.
