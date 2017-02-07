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

TBD

Drawbacks
---------

TBD

Alternatives
------------

TBD

Unresolved Questions
--------------------

- `As far as I can see
  <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4>`_,
  ``|`` is a reserved operator. So I think we can use it here, but we need to
  make sure.

- We need to figure how this interacts with

  - GADTs
  - Pattern synonyms
  - Existentials
  - ViewPatterns
  - BangPatterns
  - Irrefutable patterns

.. [#] For a recent talk on this topic, see https://www.youtube.com/watch?v=_K6UAq4hjAs
