Or Patterns
==============

.. author:: David Knothe, Ömer Sinan Ağacan, Sebastian Graf 
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/522>`_.
.. sectnum::
.. contents::

Instead of spelling out all the trivial cases of a function, we often use wildcards to summarise shared behaviour. This however becomes inconvenient once we extend the domain datatype by adding new constructors: the function in question does still compile as the newly added constructor is subsumed in the function's wildcard pattern. This is often unwanted and makes refactoring hard, because existing tools provide no way to find functions that may need to be adjusted after the new constructor was added. `GHC issue #21572 <https://gitlab.haskell.org/ghc/ghc/-/issues/21572>`_ is a recent example.

We propose a new syntax extension for "or patterns": *an or pattern matches
a list of patterns, none of which matches any variables.*
Compared to writing out one clause per pattern, or patterns endorse code reuse of the shared right-hand side.
Compared to wildcard patterns, or patterns list matched patterns explicitly, to support refactorings in the above sense.

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
    stringOfT (T2{} ; T3{}) = Nothing

This function doesn't match ``T4``, so we get our warning in the very first compile
cycle or (even faster) in our IDE powered by a language server implementation.


Proposed Change Specification
-----------------------------

Changes in the grammar
~~~~~~~~~~~~~~~~~~~~~~

We consider this as an extension to `Haskell 2010 grammar
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_.
The relevant non-terminal is ``apat``: ::

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

          |    ( pat1 ; … ; patk )                (or pattern, k ≥ 2)

The ``;`` between the parentheses have (shift) priority that is lower than any other ``apat``'s (reduction) priority.

Some examples that this new grammar produces: ::

  -- in expression context
  case e of
    (T1 ; T2{} ; T3 a b) -> ...

  -- in expression context
  let ([x] ; (x : y : z)) = e1 in e2

  -- pattern guards in declarations
  f x y
    | x@(T1 _ ; T2 a b) <- e1
    , guard x
    = e2

  -- nested or patterns
  case e1 of
    (((T1 ; T2) ; T3) ; T4) -> e2

The new production doesn't add any ambiguities because of the mandatory parentheses, just like for tuples.

NB: The new grammar allows or patterns which bind variables. These will however be rejected in `2.2`_.

.. _2.2:

Static semantics of or pattern matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or patterns which bind variables are rejected in the renamer.


We give the static semantics in terms of pattern types.

A pattern type, as defined `here <https://mpickering.github.io/pattern-synonyms-extended.pdf>`__ (chapter 6), has the form ``req => prov => arg_1 -> ... -> arg_k -> res``, where ``req`` denotes required constraints and ``prov`` denotes provided constraints.


When we have two patterns ``p1`` and ``p2`` with pattern types
::

    p1 :: forall xs1. T1 => prov1 => arg1_1 -> ... -> arg1_k -> res1,
    p2 :: forall xs2. T2 => prov2 => arg2_1 -> ... -> arg2_l -> res2

then the or pattern ``(p1; p2)`` has the pattern type ::

    (p1; p2) :: forall xs. (T1[xs/xs1], T2[xs/xs2]) => () => arg1_1[xs/xs1] -> ... -> arg_k[xs/xs1] -> res1[xs/xs1]


where we require ::

    l == k, arg1_i[xs/xs1] == arg2_i[xs/xs2] (1 ≤ i ≤ k), res1[xs/xs1] == res2[xs/xs2]

for successful typing.

An or pattern consisting of more than two parts works the same: the individual required constraints are merged together, the provided constraints are dropped and we require the remaining type signature to be identical modulo alpha conversion.


Dynamic semantics of or pattern matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Informal semantics in the style of `Haskell 2010 chapter 3.17.2: Informal
Semantics of Pattern Matching
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-600003.17.2>`_:

- Matching the pattern ``(p1 ; … ; pk)`` against the value ``v`` is the result of matching ``v`` against ``p1`` if it is not a failure, or the result of
  matching ``(p2 ; … ; pk)`` against ``v`` otherwise.

  ``p1``, …, ``pk`` bind no variables.

  NB: For k=1, the pattern ``(p1)`` is meant to denote a parenthesized pattern.

Here are a few examples: ::

    (\ (1 ; 2) -> 3) 1 => 3
    (\ (Left 0 ; Right 1) -> True) (Right 1) => True
    (\ (([1] ; [2, _]) ; ([3, _, _] ; [4, _, _, _])) -> True) [4, undefined, undefined, undefined] => True
    (\ (1 ; 2 ; 3) -> True) 3 => True

We do not employ backtracking in or patterns. The following would yield ``"no backtracking"``: ::

 case (True, error "backtracking") of
   ((True, _) ; (_, True))
     | False -> error "inaccessible"
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

  Using or patterns this code can be simplified to: ::

    isIrrefutableHsPat pat
      = go pat
      where
        go (L _ pat) = go1 pat

        go1 (WildPat{} ; VarPat{} ; LazyPat{})
          = True

        go1 (PArrPat{} ; ConPatIn{} ; LitPat{} ; NPat{} ; NPlusKPat{} ; ListPat {})
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


Effect and Interactions
-----------------------

The main effect of or patterns is twofold:

1. With or patterns developers can avoid ``_`` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. Or patterns allow more code reuse as right hand sides can be shared by many patterns.


GADTs & Existential quantification
~~~~~~~~~~~~~~~~~

With existential quantification and GADTs, patterns can not only bind values, but also equality constraints, dictionaries and existential type variables. We described in `2.2`_ how these new constraints are handled: required constraints of the individual patterns are merged while provided constraints are deleted.

So the following example would not type check because the or pattern doesn't provide the constraint ``a = Int``:

::

 data GADT a where
     IsInt1 :: GADT Int
     IsInt2 :: GADT Int

 foo :: a -> GADT a -> a
 foo x (IsInt1 {}; IsInt2 {}) = x + 1

Costs and Drawbacks
-------------------
Or patterns are a small feature which is quite simple to implement given that we forbid binding any variables. There are no obvious drawbacks.


Alternatives
------------

Alternative syntax
~~~~~~~~~~~~~~~~~~

In the `parent proposal <https://github.com/ghc-proposals/ghc-proposals/pull/43>`__, ``|`` had previously been suggested for the separator. However, ``|`` is
used for guards, so it's reserved for a future `proposal
<https://ghc.haskell.org/trac/ghc/wiki/ViewPatternsAlternative>`_ that
generalizes view patterns to allow guards inside patterns.

One nice thing about using ``;`` for the separator is that ``;`` is also used
for separating case alternatives, so it looks familiar. Example: ::

    case x of p1 -> e; p2 -> e
    case x of (p1 ; p2) -> e

An alternative to the originally proposed syntax is using ``/`` instead of ``|``
to avoid parentheses in some cases. This can't completely eliminate parentheses
around or patterns, as the following example demonstrates: ::

  f T1{} / T2{} / T3 T4 = ...

This could mean one of these two: ::

  -- a function with two arguments
  f (T1{} / T2{} / T3) T4 = ...

  -- a function with one argument
  f (T1{} / T2{} / T3 T1) = ...

  -- where the argument is defined like
  data T = T1 | T2 | T3 T

Another suggestion was to use curly braces around or patterns, instead of
parens. However, this causes ambiguities in the syntax. Two examples: ::

    -- Not clear if curly braces are for a do block or for a binding LHS
    do { ... } <- ...

    -- Not clear if curly braces are for a record pattern (where Foo is a record
    -- constuctor) or for an or pattern (matching the argument of Foo)
    case x of Foo { ... } -> ...

Yet another suggestion is to use the syntax ``T1 or T2`` by making ``or`` a keyword inside or patterns. This however leaves room for ambiguity: ``fun (T1 or T2) = 0`` could either denote an or pattern or a simple pattern matching on the binary constructor ``T1``. If we enforce it to denote an or pattern then this would be a breaking change.

Binding pattern variables
~~~~~~~~~~~~~~~~~~

The `parent proposal <https://github.com/ghc-proposals/ghc-proposals/pull/43>`__ allowed or patterns to bind variables as long as they are shared by all individual patterns:

::

 data T = T1 Int | T2 Int | T3 | T4

 getInt (T1 a ; T2 a) = Just a
 getInt (T3 ; T4) = Nothing

This is a non-goal of this proposal: with binding pattern variables come challenges like binding existential constraints. Correctly specifying the semantics is hard and caused the parent proposal to become dormant after no progress has been made.

Future proposals could build on the current one and further specify it to eventually allow binding pattern variables.

Alternative semantics using view patterns
~~~~~~~~~~~~~~~~~~~~~~

We could define the semantics of or patterns as a simple desugaring to view
patterns. The desugaring rule is: ::

    (p1; … ; pk)
    =
    ((\x -> case x of p1 -> True; p2 -> True; … ; pk -> True; _ -> False)
        -> True)

The desugaring rule defines both static and dynamic semantics of or patterns:

An or pattern type checks whenever the desugared pattern type checks; the dynamic semantics of an or pattern is the same as the dynamic semantics of its desugared pattern.

Unresolved Questions
--------------------

Not any at this time.


Implementation Plan
-------------------
The implementation will be done by `@knothed <https://github.com/knothed>`__ and `@sgraf812 <https://github.com/sgraf812>`__.

Endorsements
-------------

Not any so far.
