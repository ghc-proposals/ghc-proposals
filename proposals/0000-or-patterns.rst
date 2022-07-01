Or Patterns
==============

.. author:: Ömer Sinan Ağacan, Sebastian Graf, David Knothe
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
.. sectnum::
.. contents::

Instead of spelling out all the trivial cases of a function, we often use wildcards to summarise shared behaviour. This will however become inconvenient once we extend the domain datatype by adding new constructors: the function in question does still compile as the newly added constructor is subsumed in the function's wildcard pattern. This is often unwanted and makes refactoring hard.

We propose a new syntax extension for "or patterns": *an or pattern is
a list of patterns, none of which matches any variables.* Or patterns are syntactic sugar which support eradicating wildcard patters from your code. Or patterns additionally endorse code reuse as right hand sides can be shared by multiple patterns.


Or patterns are ordinary patterns and can appear anywhere that a pattern
can appear (top-level function argument positions, ``LambdaCase`` patterns,
left-hand side of ``<-`` in guards etc.)

Motivation
----------


Wildcard (``_``) patterns make code harder to maintain. They essentially mean "match
every other pattern", which also includes "patterns that may be enabled in the
future" e.g. when a new constructor is added to a type.

In our experience this is rarely the intention. Usually, when a new constructor
is added, we need to revisit functions on the type and update them accordingly.
But if functions use ``_`` patterns this is not easy as we won't be getting any
compile time warnings about functions we need to update.

This is also against the Haskell way of refactoring programs. Haskell is well
known for its features that make refactoring easier than most other languages,
but ``_`` patterns actually make refactoring harder.

As an example, GHC developers would know that adding a new constructor to an
existing type means many compile-run-edit cycles, with no compile-time help,
because of ``_`` patterns. Given that by default we don't get stack traces in
Haskell, and also GHC takes a lot of time to build, this wastes GHC developers'
time.

Or patterns solve this problem by allowing programmers to explicitly match a
list of constructors in a concise way. As an example, suppose we had this type:

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
we used a ``_`` pattern.

Or patterns solve the problem by allowing us to do this:

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s)        = Just s
    stringOfT (T2{} ; T3{}) = Nothing

This function doesn't match ``T4``, so we get our warning.


Proposed Change Specification
-----------------------------

Changes in the grammar
~~~~~~~~~~~~~~~~~~~~~~

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

 →       |    ( pat1 ; … ; patk )                (or pattern, k ≥ 2)

The ``;`` between the parenthesis have lower precedence than anything else.

Or patterns which bind variables are syntactically valid but will be refuted by the renamer.


Some examples that this new grammar produces: ::

  -- in expression context
  case e of
    (T1 ; T2{} ; T3 _ _) -> ...

  -- in expression context
  let ([x] ; (x : _ : _)) = e1 in e2

  -- pattern guards in declarations
  f x y
    | x@(T1 ; T2) <- e1
    , guard x
    = e2

  -- nested or patterns
  case e1 of
    (((T1 ; T2) ; T3) ; T4) -> e2

The new production doesn't add any ambiguities, because of the parentheses.

Semantics of or pattern matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Informal semantics in the style of `Haskell 2010 chapter 3.17.2: Informal
Semantics of Pattern Matching
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-600003.17.2>`_:

- Matching the pattern ``(p1 ; … ; pk)`` against the value ``v`` is the result of matching ``v`` against ``p1`` if it is not a failure, or the result of
  matching ``(p2 ; … ; pk)`` against ``v`` otherwise.

  ``p1``, …, ``pk`` bind no variables.

Here are a few examples: ::

    (\ (1 ; 2) -> 3) 1 => 3
    (\ (Left 0 ; Right 1) -> True) (Right 1) => True
    (\ (([1] ; [2, _]) ; ([3, _, _] ; [4, _, _, _])) -> True) [4, \bot, \bot, \bot] => True
    (\ (1 ; 2 ; 3) -> True) 3 => True

More formally, we define semantics of or patterns as a desugaring to view
patterns. The desugaring rule is: ::

    (p1; … ; pk)
    =
    ((\x -> case x of p1 -> True; p2 -> True; … ; pk -> True; _ -> False)
        -> True)

The desugaring rule defines both static and dynamic semantics of or patterns.
An or pattern type checks whenever the desugared pattern type checks. Dynamic
semantics of an or pattern is the same as the dynamic semantics of its
desugared pattern.

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

Would ``Unfolding`` be expanded by another constructor, all these functions would still compile but some would become semantically wrong.


Effect and Interactions
-----------------------

The main effect of or patterns is twofold:

1. With or patterns developers can avoid ``_`` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. Or patterns allow more code reuse as right hand sides can be shared by many patterns.


GADTs & Existential quantification
~~~~~~~~~~~~~~~~~

With existential quantification and GADTs, patterns can not only bind values, but also equality constraints, dictionaries and existential type variables. This however does not interfere with or patters in any way - binding variables (or equality constraints etc.) explicitly is a non-goal of this proposal.

Further extensions
~~~~~~~~~~~~~~~~~

Or patterns can be used in explicitly bidirectional pattern synonyms. For example: ::

    pattern Some <- (Left ; Right)


Since extensions like ``LambdaCase`` and ``MultiWayIf`` (in pattern guards) use the same pattern syntax, or patterns are enabled in those too.

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

Binding pattern variables
~~~~~~~~~~~~~~~~~~

The `parent proposal <https://github.com/ghc-proposals/ghc-proposals/pull/43>`__ allowed or patterns to bind variables as long as they are shared by all individual patterns:

::

 data T = T1 Int | T2 Int | T3 | T4

 getInt (T1 a ; T2 a) = Just a
 getInt (T3 ; T4) = Nothing

This is a non-goal of this proposal: with binding pattern variables come multiple challenges like binding existential constraints and handling backtracking sensibly. Correctly specifying the semantics is hard and caused the parent proposal to become dormant after no progress has been made.

Future proposals could build on the current one and further specify it to eventually allow binding pattern variables.

More
~~~~~~~~~~~~~~~~~~

Or patterns are just syntactic sugar but don't add any new functionality.
The trivial alternative therefore is to use view patterns manually to achieve the same thing as or patterns do automatically. 

Unresolved Questions
--------------------

n.a.


Implementation Plan
-------------------
The implementation will be done by `@knothed <https://github.com/knothed>`__ and `@sgraf812 <https://github.com/sgraf812>`__.

Endorsements
-------------

n.a.
