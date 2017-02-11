.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Or patterns
===========

(`Discussion <https://github.com/ghc-proposals/ghc-proposals/pull/43>`_)

We propose a new syntax extension for "or-patterns". An or pattern is
essentially a list of patterns, where patterns match exactly the same set of
variables of same types [#]_. The right hand side is shared by all of these
patterns, and can refer to the variables matched by the patterns.

Main advantages of this extension are:

1. With or patterns we can avoid ``_`` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. It allows more code reuse as right hand sides can be shared by many
   patterns.

An or pattern is an ordinary pattern and it can appear anywhere that a pattern
can appear (top-level function argument positions, ``LambdaCase`` patterns,
left-hand side of ``<-`` in guards etc.). To solve the ambiguity between a
pattern guard and an or pattern, we require parenthesis around or patterns.
This makes this extension backwards-compatible even when it's enabled.

.. [#] While in theory it may be possible to generalize this to type check
       patterns that bind same variables of different types, in this first iteration
       we want to keep things as simple as possible and only consider the case where
       all patterns have same set of binders and binders have same types. In addition,
       all patterns should bind same existentials and constraints. I think this
       version already covers majority of use cases.

Motivation
----------

There are two motivations as summarised in the abstract.

**First,** ``_`` patterns make code harder to maintain. They essentially mean "match
every other pattern", which also includes "patterns that may be enabled in the
future" e.g. when a new constructor is added to a type.

In my experience this is rarely the intention. Usually, when a new constructor
is added, the programmer needs to revisit functions on the type and update them
accordingly. But if functions use ``_`` patterns this is not easy as she won't be
getting any compile time warnings about functions she needs to update.

This is also against the Haskell way of refactoring programs. Haskell is well
known for its features that make refactoring easier than most other languages
[#]_, but ``_`` patterns actually make refactoring harder.

As an example, GHC developers would know that adding a new constructor to an
existing type means many compile-run-edit cycles, with no compile-time help,
because of ``_`` patterns. Given that by default we don't get stack traces in
Haskell, and also GHC takes a lot of time to build, this wastes GHC developers'
time.

Or patterns solve this problem by allowing programmers to explicitly match a
list of constructors in a concise way. As an example, suppose we had this type:

::

    data T = T1 String | T2 | T3

We might want to write a function on this like

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT _      = Nothing

Now suppose that some time later we add a new constructor:

::

    data T = T1 String | T2 | T3 | T4 String

We need to update ``stringOfT`` but unfortunately we don't get a warning because
we used a ``_`` pattern.

Or patterns solve the problem by allowing us to do this:

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s)        = Just s
    stringOfT (T2{} | T3{}) = Nothing

This function doesn't match ``T4``, so we get our warning.

**Second,** or patterns allow more code reuse. In our previous example, we might
extend ``stringOfT`` to something like

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT (T4 s) = Just s
    stringOfT _      = Nothing

While this is not too bad (and we can always introduce new functions for similar
right hand sides), as the number of constructors increase this becomes
repetitive.

Or patterns can solve this problem like this

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s | T4 s) = Just s
    stringOfT (T2{} | T3{}) = Nothing

Now we have code reuse, and we will get nice warnings next time a new
constructor is added.

Real-world examples
-------------------

- GHC has lots of code like this: (this one taken from
  ``compiler/hsSyn/HsPat.hs``, slightly simplified) ::

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

  Using or patterns this code can be simplified to: ::

    isIrrefutableHsPat pat
      = go pat
      where
        go (L _ pat) = go1 pat

        go1 (WildPat{} | VarPat{} | LazyPat{})
          = True

        go1 (BangPat pat     | ParPat pat     | AsPat _ pat |
             ViewPat _ pat _ | SigPatIn pat _ | SigPatOut pat _ | SumPat pat _ _ _)
          = go pat

        go1 (PArrPat{} | ConPatIn{} | LitPat{} | NPat{} | NPlusKPat{} | ListPat {})
          = False

        go1 (CoPat _ pat _)     = go1 pat
        go1 (TuplePat pats _ _) = all go pats
        go1 (ConPatOut{ pat_con = L _ (RealDataCon con), pat_args = details }) = ...
        go1 (ConPatOut{ pat_con = L _ (PatSynCon _pat) }) = ...
        go1 (SplicePat {})      = urk pat

        urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

- Even worse from the previous example is code like this (taken from the same
  file): ::

    collectEvVarsPat :: Pat id -> Bag EvVar
    collectEvVarsPat pat =
      case pat of
        LazyPat  p        -> collectEvVarsLPat p
        AsPat _  p        -> collectEvVarsLPat p
        ParPat   p        -> collectEvVarsLPat p
        BangPat  p        -> collectEvVarsLPat p
        ListPat  ps _ _   -> unionManyBags $ map collectEvVarsLPat ps
        TuplePat ps _ _   -> unionManyBags $ map collectEvVarsLPat ps
        SumPat p _ _ _    -> collectEvVarsLPat p
        PArrPat  ps _     -> unionManyBags $ map collectEvVarsLPat ps
        ConPatOut {pat_dicts = dicts, pat_args  = args}
                          -> unionBags (listToBag dicts)
                                       $ unionManyBags
                                       $ map collectEvVarsLPat
                                       $ hsConPatArgs args
        SigPatOut p _     -> collectEvVarsLPat p
        CoPat _ p _       -> collectEvVarsPat  p
        ConPatIn _  _     -> panic "foldMapPatBag: ConPatIn"
        SigPatIn _ _      -> panic "foldMapPatBag: SigPatIn"
        _other_pat        -> emptyBag

  This has repeated cases like the previous example, and it also has a
  wildcard, which means this function will probably break next time a new
  constructor is added to ``Pat`` type (this happened many times during the
  implementation of unboxed sums).

Proposed Change
---------------

- Changes in the grammar:

  We consider this as an extension to `Haskell 2010 grammar
  <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_.
  Relevant non-terminal is ``apat``: ::

    apat    →    var [ @ apat]                     (as pattern)
            |    gcon                              (arity gcon  =  0)
            |    qcon { fpat1 , … , fpatk }        (labeled pattern, k ≥ 0)
            |    literal
            |    _                                 (wildcard)
            |    ( pat )                           (parenthesized pattern)
            |    ( pat1 , … , patk )               (tuple pattern, k ≥ 2)
            |    [ pat1 , … , patk ]               (list pattern, k ≥ 1)
            |    ~ apat

  Or patterns extension adds one more production: ::

            |    ( pat1 | … | patk )

  This means that or patterns are not treated any different than any other
  pattern during parsing.

  Some examples that this new grammar produces: ::

    -- in expression context
    case e of
      (T1 | T2{} | T3 _ _) -> ...

    -- in expression context
    let ([x] | (x : _ : _)) = e1 in e2

    -- pattern guards in declarations
    f x y
      | x@(T1 | T2) <- e1
      , guard x
      = e2

    -- nested or patterns
    case e1 of
      (((T1 | T2) | T3) | T4) -> e2

  Since extensions like `LambdaCase` and `MultiWayIf` (as patter guards) use
  the same pattern syntax, or patterns are enabled in those too.

  The new production doesn't add any ambiguities, because of the parentheses.

  One alternative to this syntax is using ``/`` instead of ``|`` to avoid
  parentheses in some cases (thanks to joe462 for the suggestion), but we can't
  completely eliminate parentheses around or patterns, as the following example
  demonstrates: ::

    f T1{} / T2{} / T3 T4 = ...

  This could mean one of these two: ::

    -- a function with two arguments
    f (T1{} / T2{} / T3) T4 = ...

    -- a function with one argument
    f (T1{} / T2{} / T3 T1) = ...

    -- where the argument is defined like
    data T = T1 | T2 | T3 T

Informal semantics of or pattern matching
-----------------------------------------

We define informal semantics as an extension to `Haskell 2010 chapter 3.17.2: Informal Semantics of Pattern Matching <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-600003.17.2>`_

- Matching the pattern ``p1 | p2 | ... | pn`` against the value ``v`` is the
  result of matching ``v`` against ``p1`` if it is not a failure, or the result
  of matching ``p2 | .. | pn`` against ``v`` otherwise.

Here are some examples: ::

    (\ (x | x) -> x) 0 => 0
    (\ ([x] | (x : _ : _)) -> x) [1, 2, 3] => 1
    (\ (Left x | Right x) -> x) (Left 1) => 1
    (\ (Left x | Right x) -> x) (Right 1) => 1
    (\ ((x, _) | (_, x)) -> x) (1, 2) => 1
    (\ (([x] | [x, _]) | ([x, _, _] | [x, _, _, _])) -> x) [1, \bot, \bot, \bot] => 1
    (\ (1 | 2 | 3) -> True) 3 => True

Interaction with guards
-----------------------

In the absence of or patterns, guards are tried sequentially and only if all of
the guards succeeded the corresponding RHS is evaluated. Example: ::

    f :: Maybe Int -> Maybe Int -> Maybe Int
    f (Just x) (Just y)                 -- first case
      | even x                          -- guard 1
      , even y                          -- guard 2
      = Just (x + y)
    f (Just x) _                        -- second case
      | even x                          -- guard 3
      = Just x
    f _ _
      = Nothing

To evaluate ``f (Just 2) (Just 1)`` first two guards of the first case is
tried. Because second guard fails, second case is tried and ``Just x`` is
evaluated as a result.

In the presence of or patterns, two different semantics is possible. Running
example: ::

    f :: (Int, Int) -> Bool
    f ((x, _) | (_, x))
      | even x
      = True
    f _
      = False

    main = print (f (1, 2))

**First semantics** is called "single-match" or "non-backtracking" or "left
priority". In this semantics guards are tried after a match in the or pattern.
If any of the guards fail, the whole match fails.

In this semantics the program above prints ``False``: matching the pattern
``(x, _)`` succeeds and the guard is tried. Because the guard is failed, the
match is considered as failed.

**Second semantics** is called "multi-match" or "backtracking". In this semantics
guards are tried for every succeeding pattern in an or pattern.

In this semantics the program above prints ``True``: matching the pattern ``(x,
_)`` succeeds and the guard is tried. Guard fails, so next pattern in the or
pattern, ``(_, x)`` is tried. Match succeeds and the guard is tried. Guard also
succeeds, so the corresponding expression ``True`` is evaluated.

Reference: `Haskell 2010 Chapter 3.13: Case Expressions
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13>`_

Formal semantics of or pattern matching
---------------------------------------

We give formal semantics of or patterns as a series of identities, in the style
of `Haskell 2010 Report chapter 3.17.3
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17.3>`_.
We give rules for both "backtracking" and "non-backtracking" semantics.

**Non-backtracking semantics**

We add one rule to chapter 3.17.3 Figure 3.2: ::

    (or_1) case v of { (p1 | … | pN) -> e; _ -> e' }
           =
           case v of { p1 -> e; …; pN -> e; _ -> e' }

This rule is enough to define non-backtracking semantics. As an example
evaluation of the running example from informal semantics section with this
rule, see Appendix A.

**Backtracking semantics**

TODO

Drawbacks
---------

TBD

Alternatives
------------

**Hiding constructors by providing destructor functions (eliminators)**

One way to have some of the benefits of or patterns is to hide constructors of
a type and provide constructor and destructor functions instead. Example: ::

    module T (T, t1, t2, t3, matchT) where

    data T a b = T1 | T2 a | T3 a b

    t1 = T1
    t2 = T2
    t3 = T3

    matchT :: T a b -> ret -> (a -> ret) -> (a -> b -> ret) -> ret
    matchT t on_t1 on_t2 on_t3 =
      case t of
        T1     -> on_t1
        T2 a   -> on_t2 a
        T3 a b -> on_t3 a b

This module gives no way to match on values of type ``T`` and case analysis
have to be done using ``matchT``. When a new constructor is added, type of
``matchT`` changes, and so all call sites generate a compile-time error.

However, this isn't as flexible as having or patterns. Most importantly, nested
patterns and guards can't be implemented as easily in this style. There're also
other smaller problems, for example, there's no direct translation of this
expression: ::

    case (x :: T Int Int) of
      T1 -> e1
      (T2 a | T3 _ a) -> e2

Closest expression to this is: ::

    matchT x e1 (\a -> e2) (\_ a -> e2)

which duplicates ``e2``.

Or patterns in other languages
------------------------------

**OCaml**

From `OCaml manual <http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html#sec108>`_:

    The pattern ``pattern1 | pattern2`` represents the logical “or” of the two
    patterns ``pattern1`` and ``pattern2``. A value matches ``pattern1 |
    pattern2`` if it matches ``pattern1 or pattern2``. The two sub-patterns
    ``pattern1`` and ``pattern2`` must bind exactly the same identifiers to
    values having the same types. Matching is performed from left to right.
    More precisely, in case some value v matches ``pattern1 | pattern2``, the
    bindings performed are those of ``pattern1`` when v matches ``pattern1``.
    Otherwise, value ``v`` matches ``pattern2`` whose bindings are performed.

OCaml implements "single-match" semantics. `OCaml manual chapter on guards
<http://caml.inria.fr/pub/docs/manual-ocaml/expr.html#sec123>`_ doesn't
explicitly mention or patterns, but it can be inferred from the text that
guards are tested once on a match.

`Ambiguous pattern variables
<http://gallium.inria.fr/~scherer/research/ambiguous_pattern_variables/ml_workshop_2016.abstract.pdf>`_
explains how single-match semantics can be confusing to users, and explains
design of the warning OCaml 4.03 prints when potentially confusing guard is used
with an or pattern. The warning works like this:

Suppose we have an or pattern ``p1 | p2 | p3 ... pN``, and a variable ``x`` used
in patterns.

- ``x`` is *stable* if in all of the patterns it's used in the same location.
  The paper gives this example: ::

    ((x, None, _) | (x, _, None))

  Note that for this to hold the pattern must match a product type.

- ``x`` is *stable* if none of the pattern can match at the same time. The
  paper gives this example: ::

    ((x, None, _) | (_, Some _, x))

  Another example is when matching different constructors of a sum type: ::

    (Left x | Right x)

If a variable used in an or pattern is not *stable*, it's *ambiguous* and
reported in a warning: ::

    Warning 57: Ambiguous or-pattern variables under guard;
    variable x may match different arguments. (See manual section 8.5)

If we choose to implement the single-match semantics we should implement a
similar warning.

**Rust**

Rust seems to support a simpler version of or patterns. `Relevant section in
the language reference
<https://doc.rust-lang.org/reference.html#match-expressions>`_ doesn't say much
about it, but the implementation seems to support or patterns only at the top
layer of patterns. These are fine: ::

    match i {
        Ok(1) | Ok(2) => {}
        _ => {}
    }

    enum T {
        T1(i32),
        T2(i32),
        T3(i32),
    }

    match x {
        T::T1(a) | T::T2(a) | T::T3(a) => { println!("{:?}", a); }
    }

But this fails with a parse error: ::

    match i {
        Ok(1 | 2) => {}
        _ => {}
    }

    error: expected one of `)`, `,`, `...`, or `..`, found `|`
      --> pat.rs:24:14
       |
    24 |         Ok(1 | 2) => { println!("ok"); }
       |              ^

Unresolved Questions
--------------------

- We need to figure how this interacts with

  - GADTs
  - Pattern synonyms
  - Existentials
  - ViewPatterns
  - BangPatterns
  - Irrefutable patterns

.. [#] For a recent talk on this topic, see https://www.youtube.com/watch?v=_K6UAq4hjAs

Appendix A: Evaluation of the running example
---------------------------------------------

**Non-backtracking semantics (rule or_1)** ::

    (original expression)
    case v of
      ((x, _) | (_, x))
        | even x
        -> True
      _ -> False

    ==> (rule b)

    case v of
      ((x, _) | (_, x))
        | even x
        -> True
      _ -> case v of
             _ -> False

    ==> (rule c)

    case False of
      y ->
        case v of
          ((x, _) | (_, x))
            case () of
              () | even x -> True
              _ -> y
          _ -> y
    (y fresh)

    ==> (rule v)

    case False of
      y ->
        case v of
          ((x, _) | (_, x)) -> if even x then True else y
          _ -> y

    ==> (rule or_1)

    case False of
      y -> case v of
             (x, _) -> if even x then True else y
             (_, x) -> if even x then True else y
             _ -> y

    ==> (rule b)

    case False of
      y -> case v of
             (x, _) -> if even x then True else y
             _ -> case v of
                    (_, x) -> if even x then True else y
                    _ -> case v of
                           _ -> y

At this point we don't have any or patterns and substituting ``(1, 2)`` for
``v`` and further simplifications using identities from the manual reveals that
this indeed implements non-backtracking semantics.
