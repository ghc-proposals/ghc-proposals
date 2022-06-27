
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

This proposal is a derivation of `this one <https://github.com/osa1/ghc-proposals/blob/or_patterns/proposals/0000-or-patterns.rst>`_. In short, the other proposal allowed or patterns to bind variables, while this proposal does not.

Or Patterns
============

(`Discussion <https://github.com/ghc-proposals/ghc-proposals/pull/43>`_)

We propose a new syntax extension for "or patterns". An or pattern is
essentially a list of patterns, where **patterns match no variables**. The right hand side is shared by all of these patterns.

Main advantages of this extension are:

1. With or patterns we can avoid ``_`` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. It allows more code reuse as right hand sides can be shared by many
   patterns.

An or pattern is an ordinary pattern and it can appear anywhere that a pattern
can appear (top-level function argument positions, ``LambdaCase`` patterns,
left-hand side of ``<-`` in guards etc.). To avoid any ambiguities we require
parenthesis around or patterns.

Motivation
----------

``_`` patterns make code harder to maintain. They essentially mean "match
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
    stringOfT (T2{} ; T3{}) = Nothing

This function doesn't match ``T4``, so we get our warning.

----------

Ultimately, we'd like or patterns to also allow *binding variables*. Then when extending `T` by a new constructor we could write something like:

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s ; T4 s) = Just s
    stringOfT (T2{} ; T3{}) = Nothing

This however is **not** in the scope of this proposal.

See `here <https://github.com/ghc-proposals/ghc-proposals/pull/43#issuecomment-1127812720>`__ for the discussion on the original proposal. After the proposal petered out due to complexities in the specification and interplay with language extensions like GADTs, we decided to create this simplified proposal which would still be of a lot of use but much simpler to correctly specify and implement.

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

- Here's an example (taken from GHC's ``TmOracle.hs``) that reflects a
  programmer's frustration with the lack of or patterns: ::

    -- | Solve a complex equality.
    solveComplexEq :: TmState -> ComplexEq -> Maybe TmState
    solveComplexEq solver_state@(standby, (unhandled, env)) eq@(e1, e2) = case eq of
      -- We cannot do a thing about these cases
      (PmExprOther _,_)            -> Just (standby, (True, env))
      (_,PmExprOther _)            -> Just (standby, (True, env))
      ...
      _ -> Just (standby, (True, env)) -- I HATE CATCH-ALLS

Proposed change specification
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

          |    ( pat1 ; pat2 )

The ``;`` between the parenthesis have lower precedence than anything else. Or
patterns are associative, so N-ary version ``( pat1 ; … ; patN )`` is also
accepted.

TODO: forbid patterns matching variables

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

Since extensions like ``LambdaCase`` and ``MultiWayIf`` (in pattern guards) use
the same pattern syntax, or patterns are enabled in those too.

The new production doesn't add any ambiguities, because of the parentheses.

Semantics of or pattern matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Informal semantics in the style of `Haskell 2010 chapter 3.17.2: Informal
Semantics of Pattern Matching
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-600003.17.2>`_:

- Matching the pattern ``(p1 ; p2)`` against the value ``v`` is the result of
  matching ``v`` against ``p1`` if it is not a failure, or the result of
  matching ``p2`` against ``v`` otherwise.

  ``p1`` and ``p2`` bind no variables.

Here are a few examples: ::

    (\ (1 ; 2) -> 3) 1 => 3
    (\ (Left 0 ; Right 1) -> True) (Right 1) => True
    (\ (([1] ; [2, _]) ; ([3, _, _] ; [4, _, _, _])) -> True) [4, \bot, \bot, \bot] => True
    (\ (1 ; 2 ; 3) -> True) 3 => True

More formally, we define semantics of or patterns as a desugaring to view
patterns. The desugaring rule is: ::

    (p1; p2)
    =
    ((\x -> case x of p1 -> True; p2 -> True; _ -> False)
        -> True)

The desugaring rule defines both static and dynamic semantics of or patterns.
An or pattern type checks whenever the desugared pattern type checks. Dynamic
semantics of an or pattern is the same as the dynamic semantics of its
desugared pattern.

Interaction with other extensions
---------------------------------

Existential quantification and GADTs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

???

A pattern on a Haskell 98 data constructor (aka. a "vanilla" or "boring"
constructor) only binds values.

However with existential quanticiation and GADTs, patterns can also bind

- Equality constraints

  (``a ~ Int`` in GADT ``data Foo a where FooInt :: Int -> Foo Int``)

- Dictionaries

  (``Show a`` in GADT ``data Foo a where Foo :: Show a => a -> Foo a`` or in
  existential ``data Foo a = Show a => Foo a``)

- Existential type variables

  (``a`` in ``data Foo1 where Foo :: Default a => Foo`` or in existential ``data
  Foo = forall a . Default a => Foo``)

The desugaring rule implies that none of the above can be bound by an or
pattern. We thus support a limited form of pattern matching on GADT constructors
and constructors with existentials.

Below are some examples of accepted and rejected programs. Because GADTs subsume
existentials, we only use GADT syntax.

Accepted programs: ::

    data T2 a where
      C5 :: Int  -> T2 Int
      C6 :: Bool -> T2 Bool

    f3 :: T2 a -> a
    f3 (C5 x ; C6 x) = x
    -- desugared:
    f3_ds :: T2 a -> a
    f3_ds ((\x -> case x of C5 x -> Just x
                            C6 x -> Just x
                            _ -> Nothing) -> Just x) = x

    data T3 a where
      C7 :: a -> (a -> String) -> T3 String
      C8 :: Ord a => a -> T3 Int

    f4 :: T3 a -> String
    f4 (C7 _ _ ; C8 _) = "f4"
    -- desugared:
    f4_ds :: T3 a -> String
    f4_ds ((\x -> case x of C7 _ _ -> Just ()
                            C8 _ -> Just ()) -> Just ()) = "f4"

Rejected programs: ::

    data T1 where
      C1 :: a -> (a -> String) -> T1
      C2 :: Int -> (Int -> String) -> T1
      C3 :: Show a => a -> T1
      C4 :: String -> T1

    f1 :: T1 -> String
    f1 (C1 x g ; C2 x g) = g a
    -- desugared:
    f1_ds :: T1 -> String
    f1_ds ((\x -> case x of C1 x g -> Just (x, g)
                            C2 x g -> Just (x, g)
                            _ -> Nothing) -> Just (x, g)) = g x

    f2 :: T1 -> String
    f2 (C3 x ; C4 x) = show x
    -- desugared:
    f2_ds :: T1 -> String
    f2_ds ((\x -> case x of C3 x -> Just x
                            C4 x -> Just x
                            _ -> Nothing) -> Just x) = show x

Binding constraints, existentials, or dictionaries are not allowed even in
simplest cases like: ::

    data T1 where
      C1 :: Show a => a -> T1
      C2 :: Show a => a -> T1

    f :: T1 -> String
    f (C1 x ; C2 x) = show x
    -- desugared:
    f_ds :: T1 -> String
    f ((\x -> case x of C1 x -> Just x
                        C2 x -> Just x) -> Just x) = show x

Even though both patterns bind a dictionary of same type, to keep things simple
we currently reject this program. Pattern matching on GADTs in or patterns can
be generalized in the future in a backwards compatible way.

???


Alternatives
------------

The alternative is to allow or patterns to bind variables as long as every subpattern binds the same set of variables.
This however was already proposed `here <https://github.com/osa1/ghc-proposals/blob/or_patterns/proposals/0000-or-patterns.rst>`__, a dormant proposal which was closed 2 years ago. It petered out due to complexities in the specification and interplay with language extensions like GADTs and so we decided to create this simplified proposal which would still be of a lot of use but much simpler to correctly specify and implement.

Or patterns in other languages
------------------------------

See `here <https://github.com/osa1/ghc-proposals/blob/or_patterns/proposals/0000-or-patterns.rst#16or-patterns-in-other-languages>`__


Revisions
---------

The `original proposal <https://github.com/osa1/ghc-proposals/blob/or_patterns/proposals/0000-or-patterns.rst>`_ allowed or patterns to bind variables, as long as the individual patterns bound the same set of variables.
Due to difficulties in ???, this version now restricts itself to the simple but effective case of not binding any variables at all.

Implementation Plan
-------------------

Or patterns requires changes in the parser, type checker, pattern checker and
compiler (``match`` function). Lexer already generates ``;`` tokens so no
changes needed. There are no changes in Core.

A prototype implementation of the previous proposal is found here:
https://github.com/osa1/ghc/tree/or_patterns.

Parsing
~~~~~~~

Parsing is easily done by extending the production that generates (boxed or
unboxed) tuple and unboxed sum patterns (`example implementation
<https://github.com/osa1/ghc/commit/71831b4de5865529c819684d4215d0c02104679c#diff-72873ca71a4ec70caca296d4af035076>`_).

Type checking
~~~~~~~~~~~~~

TBD

Pattern checking
~~~~~~~~~~~~~~~~

TBD

Desugaring to GHC Core (match function)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

View patterns???

We take advantage of the recent join points work. When we see a match with an
or pattern, we first generate a join point for the RHS: ::

    case x of
      (P1 y ; P2 y) -> RHS1
      P3            -> RHS2

    ==>

    join {
        rhs1 :: ...
        rhs1 y = RHS1 } in
    case x of
      P1 y -> rhs1 y
      P2 y -> rhs1 y
      P3   -> RHS2

This is similar to how pattern errors for unhandled cases are compiled, except
we mark ``rhs1`` as join point explicitly during desugaring, rather that
relying on the optimizer, to avoid accidentally generating slow code.

An example with nested patterns: ::

    -- Haskell expression
    case x0 of
      ((Left x ; Right x), (Left y ; Right y)) -> e1

    ==>

    -- GHC Core
    case x0 of
      (x0_1, x0_2) ->
        join {
            rhs1 x =
              join {
                  rhs2 y = e1
              } in
              case x0_2 of
                Left  y -> rhs2 y
                Right y -> rhs2 y
        } in
        case x0_1 of
          Left  x -> rhs1 x
          Right x -> rhs1 x

Desugaring in the prototype implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The prototype implementation uses a pre-processing step for eliminating or
patterns, leaving `match` unchanged.

The trouble with changing `match` is

- Every single pattern group (e.g. "literals", "data constructors") need to
  handle or patterns. This requires quite invasive changes.

- Match function operates in `DsM` monad and otherwise don't allow accumulating
  new bindings during compilation (we need this to be able to introduce join
  points for RHSs).

A simpler alternative is to use a pre-processing step that eliminates or
patterns before leaving compilation to `match`. This steps runs in
`matchWrapper`. In summary, this pass does this:

- Check if the equation has any or patterns.

  - If it doesn't, nothing to do, just call `match`.

  - Otherwise introduce a join point for the RHS. This join point takes, as
    arguments, all of the binders in the equation. Then flatten the equation
    (eliminate or patterns), using the same RHS that jumps to the join point for
    all equations.

    For example, given this equation: ::

        [ (p1 ; p2), (p3 ; p4) ] -> RHS

    we flatten it as ::

        [ [ p1, p3 ] -> jump p1 bndrs
        , [ p1, p4 ] -> jump p1 bndrs
        , [ p2, p3 ] -> jump p1 bndrs
        , [ p2, p4 ] -> jump p1 bndrs
        ]

    where `p1` is the joint point and `bndrs` is all of the binders in an
    equation (remember that in an or pattern all alternatives bind exactly the
    same set of variables of same types, so equations in this exapanded form
    bind the same set of variables).

Disadvantages of this approach:

- Introducing a pre-processing step just for or patterns is ugly. The
  pre-processing step runs on every pattern matching expression, and adds a
  traversal cost in the best case (when equations don't have any or patterns).

- Flattening step potentially introduces exponential number of new equations.
  Unfortunately there's no way around that unless we change `Core` and `Stg` to
  support or patterns.