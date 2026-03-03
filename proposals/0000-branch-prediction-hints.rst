Branch Prediction Hints
=======================

.. author:: Ian Duncan
.. date-accepted:: Leave blank.
.. ticket-url:: Leave blank.
.. implemented:: Leave blank.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0000>`_.
   **After creating the pull request, edit this file again, update the
   number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal adds branch prediction hints to GHC Haskell, allowing programmers
to communicate expected control flow to the compiler. A ``{-# WEIGHT n #-}``
pragma is added for case alternatives, function clauses, and guards. These
hints propagate through the entire compilation pipeline and drive code layout,
switch dispatch, register allocation, and LLVM metadata emission. A companion
set of heuristics infers weights automatically when no explicit annotation is
given.


Motivation
----------

Performance-sensitive Haskell code is at a disadvantage relative to C/C++,
which provides ``__builtin_expect`` (wrapped as ``likely()``/``unlikely()``
macros) to guide branch prediction and code layout. The Linux kernel alone
contains over 17,000 uses of these macros. GHC currently has no mechanism for
programmers to express branch likelihood, leading to several problems:

1. **Manual constructor ordering.** Library authors reorder data constructors
   to influence tag allocation and case dispatch order. For example,
   ``containers`` `reorders constructors
   <https://github.com/haskell/containers/blob/533c3c8d7d6df060d04468b8847de65ce04368c4/containers/src/Data/Set/Internal.hs#L116>`_
   for performance. This is fragile — the optimal ordering depends on
   GHC internals that change between releases.

2. **Suboptimal code layout.** Without likelihood information, the native code
   generator places basic blocks in an arbitrary order relative to the hot
   path, causing unnecessary branches and poor instruction cache utilization.

3. **Missed optimization opportunities.** Switch dispatch cannot prioritize
   hot cases, jump table density thresholds cannot be relaxed for hot regions,
   and the register allocator cannot prefer keeping hot-path values in
   registers.

4. **No path to profile-guided optimization.** A weight representation in
   the compiler's intermediate languages is a prerequisite for future PGO
   support. Inline annotations provide the simplest useful first step and
   do not preclude file-based profiles later.

Many branch likelihoods are obvious without profiling: in a list traversal, the
cons case is overwhelmingly more frequent than the nil case; error-checking
branches almost never fire on the fast path; the base case of a recursive
function is taken far less often than the recursive case.


Proposed Change Specification
-----------------------------

Two mechanisms are added, targeting different use cases.

``{-# WEIGHT n #-}`` pragma
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new pragma is accepted in three positions: before case alternative patterns,
after a function name and before its first pattern, and after a guard ``|``
and before the guard expression. ``n`` is a non-negative integer literal.

**Grammar (case alternatives):**

::

  alt → opt_weight pat '->' exp
  opt_weight → '{-# WEIGHT' INTEGER '#-}'
             | ε

**Grammar (function clauses):**

::

  funlhs_clause → funname opt_weight pats rhs

The weight appears between the function name and its first argument pattern.
This is consistent with pragma placement conventions elsewhere in GHC
(e.g. ``{-# UNPACK #-}`` before types).

**Grammar (guarded equations):**

::

  gdrh → '|' opt_weight guardquals '=' exp
  gdpat → '|' opt_weight guardquals '->' exp

**Semantics:**

- Weights are relative. Only the ratio between alternatives matters.
  ``{-# WEIGHT 2000 #-}`` and ``{-# WEIGHT 1 #-}`` on two alternatives is
  equivalent to ``{-# WEIGHT 4000 #-}`` and ``{-# WEIGHT 2 #-}``.

- A weight of 0 means "negligible probability, but not impossible." This is
  a candidate for cold-code placement.

- If any alternative in a case expression has a ``WEIGHT`` pragma, all
  alternatives without an explicit weight receive a default weight equal to
  the midpoint of the lowest and highest specified weights. For example,
  if two alternatives have weights 1 and 2000, unannotated alternatives
  receive ``(1 + 2000) / 2 = 1000`` (integer division, rounding down).

- Weights are stored as ``Word32``.

- For function clauses, weights attach to each clause. The desugarer
  translates these into case alternative weights when the function is
  compiled to a case expression.

**In Core**, each ``Alt`` carries an optional weight:

::

  data Alt b = Alt AltCon [b] (Expr b) !(Maybe Word32)

The weight is a ``Maybe Word32`` on each alternative rather than a separate
list on the ``Case`` constructor. This keeps weights colocated with their
alternatives and avoids the need to maintain two parallel lists in sync.

``likely`` and ``unlikely`` as library functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``likely`` and ``unlikely`` in the style of C/C++ and Rust are not built-in compiler primitives. They
can be expressed as ordinary Haskell functions using the ``WEIGHT`` pragma:

::

  likely :: Bool -> Bool
  {-# INLINE likely #-}
  likely c = case c of
    {-# WEIGHT 2000 #-} True  -> True
    {-# WEIGHT 1 #-}    False -> False

With case-of-case, the weights transfer correctly to the call site. Users
who want these convenience functions can define them locally or in a library.

Heuristic inference
~~~~~~~~~~~~~~~~~~~

When a ``Case`` expression reaches ``CoreToStg`` without explicit weights,
the compiler infers weights from two heuristics:

1. **Bottoming alternative** (~99% probability for the non-bottoming path):
   If an alternative's right-hand side is a known bottom (``error``,
   ``undefined``, ``patError``, etc.), it receives weight 1. All other
   alternatives receive weight 2000.

2. **Recursive call** (~89% for the recursive path):
   In a recursive binding, alternatives containing a self-call receive
   weight 2000. Alternatives without a self-call receive weight 100.

When both heuristics apply, the bottoming heuristic takes priority.

At the Cmm level, three additional static heuristics apply to conditional
branches that have no weights from earlier stages:

- **Dead-end successor:** If one successor has no outgoing edges
  (e.g. ``stg_gc_*``, ``stg_raise``), predict the other (weights 9:1).

- **Tag check:** A pattern ``(x & mask) == 0`` from closure tag checks
  predicts nonzero (weights 4:1).

- **Float non-equal:** Floating-point equality predicts non-equal
  (weights 7:3).

Explicit annotations always override inferred weights.

Corner cases
~~~~~~~~~~~~

- ``WEIGHT`` pragmas on ``\case`` and ``\cases`` alternatives follow the same
  grammar as ordinary case alternatives.

- Guards: a ``WEIGHT`` pragma on a guard (``| {-# WEIGHT n #-} guard = rhs``)
  attaches weight ``n`` to the ``True`` alternative of the guard's desugared
  case expression.

- Nested case expressions: weights apply only to the immediately annotated
  case. Inner case expressions are independent and may carry their own weights.

- **Marginalization across pattern columns.** When a function or ``\cases``
  expression matches on multiple arguments, the desugarer compiles clauses
  to nested case expressions (one per pattern column). The same applies to
  ordinary multi-argument function definitions. When a single outer
  constructor leads to multiple inner alternatives with different weights,
  the outer alternative's weight is the sum of the inner clause weights that
  share that outer constructor. For example::

    f {-# WEIGHT 100 #-}  Nothing  True  = a
    f {-# WEIGHT 2000 #-} Nothing  False = b
    f {-# WEIGHT 50 #-}   (Just _) True  = c
    f {-# WEIGHT 50 #-}   (Just _) False = d

  The outer case on the first argument gets ``Nothing`` with weight
  ``100 + 2000 = 2100`` and ``Just`` with weight ``50 + 50 = 100``. The
  inner case under ``Nothing`` gets ``True`` with weight 100 and ``False``
  with weight 2000.

  This corresponds to marginalization from probability theory. Treating
  clause weights as unnormalized joint probabilities over the pattern
  columns, the weight for an outer constructor :math:`c_i` is the marginal:

  .. math::

     w(c_i) = \sum_{j} w(c_i, p_j)

  where :math:`p_j` ranges over the inner patterns paired with :math:`c_i`.
  For the example above, :math:`w(\mathtt{Nothing}) = w(\mathtt{Nothing},
  \mathtt{True}) + w(\mathtt{Nothing}, \mathtt{False}) = 100 + 2000 = 2100`.
  This preserves the relative likelihoods: the compiler first decides which
  outer constructor is taken (with odds 2100 : 100, i.e. ``Nothing`` is
  ~21x more likely than ``Just``), then within that constructor decides
  among the inner alternatives (with odds 100 : 2000 under ``Nothing``).

  The rule generalizes to any number of pattern columns by repeated
  marginalization, and applies uniformly to multi-argument functions,
  ``\cases``, and any other source construct that the desugarer compiles to
  nested cases.

- Interaction with ``INLINE``/``INLINABLE``: when a function with ``WEIGHT``
  annotations on its clauses is inlined, the desugared per-Alt weights on
  the resulting case expressions are preserved. Weights are a property of
  the Core ``Alt``, not the source function, so inlining does not discard
  them.

Design principles
~~~~~~~~~~~~~~~~~

This design accords with the GHC `language design principles
<../principles.rst#2Language-design-principles>`_:

- **Orthogonality:** ``WEIGHT`` composes with all existing language features
  (``\case``, guards, view patterns, pattern synonyms). It does not
  introduce new evaluation semantics.

- **Predictability:** Weights affect only code layout and dispatch strategy,
  never program semantics. A program with incorrect weights produces the same
  values, just slower.

- **Conservatism:** The feature is purely opt-in. Without annotations, the
  compiler's behavior is unchanged except for heuristic inference at
  ``CoreToStg``, which improves layout without altering semantics.


Proposed Library Change Specification
-------------------------------------

No new library exports are required. Users who want convenience wrappers can define
them as ordinary Haskell functions using the ``WEIGHT`` pragma (see the
``likely``/``unlikely`` section above).

Template Haskell
~~~~~~~~~~~~~~~~

The ``Match`` type gains an optional weight field:

::

  data Match = Match [Pat] Body [Dec] (Maybe Word)

A new function ``matchW`` constructs a ``Match`` with a weight.
``Ppr`` emits ``{-# WEIGHT n #-}`` when a weight is present.


Examples
--------

Weighted function clauses
~~~~~~~~~~~~~~~~~~~~~~~~~

::

  lookup :: Eq k => k -> [(k, v)] -> Maybe v
  lookup _key []          = Nothing
  lookup  key ((x,y):xys)
    | key == x  = Just y
    | otherwise = lookup key xys

With weights:

::

  lookup :: Eq k => k -> [(k, v)] -> Maybe v
  lookup {-# WEIGHT 1 #-}    _key []          = Nothing
  lookup {-# WEIGHT 2000 #-} key ((x,y):xys)
    | key == x  = Just y
    | otherwise = lookup key xys

On a case expression directly:

::

  case xs of
    {-# WEIGHT 1 #-}    []   -> handleEmpty
    {-# WEIGHT 2000 #-} y:ys -> processElement y ys

Weights on a multi-way case
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  case cmd of
    {-# WEIGHT 5000 #-} Read   -> handleRead
    {-# WEIGHT 3000 #-} Write  -> handleWrite
    {-# WEIGHT 10 #-}   Delete -> handleDelete
    {-# WEIGHT 1 #-}    Admin  -> handleAdmin

The switch dispatch hoists the ``Read`` case (highest weight) as the first
equality check. The binary search tree uses weighted medians for splits.

Cold code with weight 0
~~~~~~~~~~~~~~~~~~~~~~~

::

  case result of
    {-# WEIGHT 2000 #-} Right val -> useValue val
    {-# WEIGHT 0 #-}    Left err  -> error ("impossible: " ++ show err)

The ``Left`` branch is a candidate for cold-code placement
(``.text.unlikely``). Weight 0 means negligible probability, not dead code.

Weighted guards
~~~~~~~~~~~~~~~

::

  classify :: Int -> String
  classify n
    | {-# WEIGHT 2000 #-} n > 0    = "positive"
    | {-# WEIGHT 100 #-}  n == 0   = "zero"
    | {-# WEIGHT 1 #-}    otherwise = "negative"

The guard weight attaches to the ``True`` alternative of the desugared
guard expression.

Heuristic inference (no annotation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With no explicit weights, the compiler infers automatically:

::

  safeDivide :: Int -> Int -> Either String Int
  safeDivide _ 0 = Left "division by zero"
  safeDivide x y = Right (x `div` y)

The bottoming-alternative heuristic does not apply here (neither branch
bottoms), but in:

::

  head :: [a] -> a
  head (x:_) = x
  head []    = error "empty list"

the ``error`` branch receives weight 1 and the cons branch receives weight
2000, without any annotation.

Template Haskell
~~~~~~~~~~~~~~~~

::

  -- Constructing a weighted match with Template Haskell:
  mkWeightedClause :: Q [Dec]
  mkWeightedClause = do
    let w = matchW 2000 [conP 'Just [varP (mkName "x")]]
                        (normalB [| useValue $(varE (mkName "x")) |]) []
    [d| f = $(lamCaseE [w]) |]

``matchW`` constructs a ``Match`` with the given weight. The quotation
``Ppr`` renders it as ``{-# WEIGHT 2000 #-} Just x -> useValue x``.

``\case`` alternatives
~~~~~~~~~~~~~~~~~~~~~~

::

  handle :: Maybe Response -> IO ()
  handle = \case
    {-# WEIGHT 2000 #-} Just resp -> processResponse resp
    {-# WEIGHT 1 #-}    Nothing   -> logTimeout


Effect and Interactions
-----------------------

This proposal addresses each of the motivating problems directly.
``WEIGHT`` pragmas make manual constructor ordering hacks (1) unnecessary by
expressing branch likelihood intent independent of data constructor tag
assignment. Code layout (2) improves because CFG edge weights derived from
branch weights guide the block layout algorithm to place hot paths as
fall-throughs. Switch dispatch and register allocation (3) benefit from
weight-aware hot-case hoisting, weighted binary search pivots, and
frequency-based spill selection. Finally, the per-Alt weight
representation in Core provides the prerequisite infrastructure for future
profile-guided optimization (4), and inline annotations serve as the
simplest useful first step without precluding file-based profiles later.

Simplifier
~~~~~~~~~~

The simplifier preserves per-Alt weights through case-of-case
transformations. When a case-of-known-constructor eliminates alternatives,
weights are discarded (only one branch remains). The ``CaseCtxt`` used for
inlining decisions carries branch weights, preparing for future
weight-aware inlining heuristics.

Core optimizations
~~~~~~~~~~~~~~~~~~

All existing Core-to-Core passes (float-in, float-out, CSE, demand analysis,
worker-wrapper, specialization, etc.) preserve per-Alt weights unchanged.
This is mechanically achieved because weights are colocated with their
alternatives and travel alongside them.

Switch lowering
~~~~~~~~~~~~~~~

The Cmm switch implementation uses weights to:

- Hoist hot cases (>50% of total weight) as the first equality test.
- Select weighted pivots for binary search trees.
- Relax jump table density thresholds for hot regions (``minJumpTableSize``
  3 instead of 5; ``maxJumpTableHole`` 14 instead of 7).
- Order bit-test masks by weight for sparse switches.

Register allocation
~~~~~~~~~~~~~~~~~~~

The linear register allocator uses CFG block frequencies (derived from
branch weights) to compute per-virtual-register costs. When a spill is
necessary, the cheapest register (least used on hot paths) is evicted first.

LLVM backend
~~~~~~~~~~~~

For conditional branches with two-element weights, the LLVM backend emits
``!prof !{!"branch_weights", i32 w_true, i32 w_false}`` metadata. LLVM's
``BranchProbabilityInfo`` uses this for block placement, if-conversion, and
other optimizations.

Native code generator
~~~~~~~~~~~~~~~~~~~~~

Branch weights flow into CFG edge weights via ``branchWeightsToEdgeWeights``.
The block layout algorithm (``-fblock-layout-cfg``, enabled at ``-O1`` and
above) uses these weights to place hot paths as fall-throughs and cold paths
after the hot code.

Hot/cold code splitting
~~~~~~~~~~~~~~~~~~~~~~~

When a basic block's incoming edge weight falls below a configurable
threshold relative to the function's total weight, it is emitted into a
separate cold section (``.text.unlikely`` on ELF, ``__text_cold`` on
Mach-O) rather than the default ``.text`` section. This has three benefits:

1. **Instruction cache pressure.** Cold blocks (error handlers, rarely-taken
   branches, diagnostic paths) occupy cache lines that would otherwise hold
   hot code. Moving them out of ``.text`` reduces the hot-code working set,
   improving i-cache hit rates.

2. **Page-level locality.** On systems with 4 KiB or 16 KiB pages, mixing
   hot and cold code causes entire pages to remain resident for the sake of
   a few cold bytes. Splitting lets the OS page out cold pages under memory
   pressure without evicting hot code.

3. **Link-time and post-link optimization.** Linkers (``lld``, ``gold``,
   ``mold``) can place ``.text.unlikely`` sections at the end of the binary,
   far from ``.text``, maximizing the density of hot code in contiguous
   pages. Tools like ``BOLT`` and ``AutoFDO`` benefit from a clean hot/cold
   partition as input.

The ``WEIGHT 0`` annotation is the primary user-facing signal for cold code.
Heuristic-inferred cold blocks (bottoming alternatives, dead-end Cmm
successors) also qualify. The threshold for splitting is controlled by a flag
(see Unresolved Questions).

Costs and Drawbacks
-------------------

- **Compile time:** Adding ``!(Maybe Word32)`` to each ``Alt`` in Core
  increases allocations by approximately 0.1% at ``-O2`` (measured in the
  original #182 discussion). The heuristic inference in ``CoreToStg`` is a
  single pass over alternatives with negligible cost.

- **Learnability:** ``{-# WEIGHT n #-}`` is a new pragma but follows the
  existing pragma syntax conventions. Users who want the familiar C/C++
  ``likely``/``unlikely`` API can define them as ordinary functions using
  ``WEIGHT`` pragmas and ``INLINE``. Beginners can ignore the feature
  entirely.

- **Maintenance burden:** Weights are threaded through Core mechanically.
  New Core passes must pattern-match on the per-Alt ``Maybe Word32`` field,
  but the default behavior (pass through unchanged) is straightforward.

- **Risk of misuse:** Incorrect weights can pessimize code. However, the
  heuristic inference provides good defaults without any annotation, and
  explicit weights are opt-in for performance-sensitive code.


Backward Compatibility
----------------------

This proposal meets the `GHC stability principles
<../principles.rst#3GHC-stability-principles>`_ fully: it introduces no new
warnings, changes no existing semantics, and deprecates nothing.

**Breakage level: 0.** No existing programs change behavior.

- The ``{-# WEIGHT n #-}`` pragma is new syntax; existing code cannot contain it.
- No new library exports are added, so there are no name collision concerns.
- The heuristic inference only affects code layout, not semantics.
- Template Haskell's ``Match`` gains an optional field with a default of
  ``Nothing``, which is backward-compatible.


Alternatives
------------

PGO-based weight files
~~~~~~~~~~~~~~~~~~~~~~

Instead of inline pragmas, weights could come from a separate profile data
file generated by instrumented execution. This was discussed at length in
the original proposal #182. The inline approach is complementary: it is
simpler, works without profiling infrastructure, and handles cases where
branch likelihood is obvious from code structure (error paths, recursive
cases). A future PGO implementation would reuse the same per-Alt weight
infrastructure and override inline annotations when profile data is
available.

Ticks
~~~~~

Branch weights could be represented as a new tick type in Core. This was
considered and rejected because ticks can be dropped by optimizations that
don't understand them, which would silently defeat the feature. Storing
weights directly in each ``Alt`` ensures they are always preserved.

Weights in AltCon
~~~~~~~~~~~~~~~~~

The original #182 discussion (with feedback from Simon PJ) considered putting
a ``Freq`` field in ``AltCon``. The current design adds a
``!(Maybe Word32)`` to each ``Alt`` tuple instead of modifying ``AltCon``.
This keeps ``AltCon`` simple and avoids a change to every pattern match on
``DataAlt``/``LitAlt``/``DEFAULT``. Each alternative is self-contained with
its own weight, avoiding the need to maintain parallel lists in sync.

Pragma on data constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Instead of annotating case alternatives, weights could be attached to
data constructor declarations. This would provide a default likelihood for
every case on that type. However, the optimal weights depend on the use site,
not the type definition. A list nil/cons split in a tight loop has very
different weights from one in a validation function. Per-case-site weights
are more precise.

Prior art in other languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The design draws on established practice in other compilers and languages:

- **GHC Proposal #182** (``https://github.com/ghc-proposals/ghc-proposals/pull/182``): "Add control
  flow hint pragmas" by Andreas Klebinger, open 2018–2025, closed dormant.
  The current proposal revives it (with a degree of different decisions, as I wasn't aware of it until writing this proposal myself).

- **GCC/Clang:** ``__builtin_expect(expr, val)`` and
  ``[[likely]]``/``[[unlikely]]`` attributes (C++20).

- **LLVM:** ``!prof !{!"branch_weights", i32 w1, i32 w2}`` metadata on
  ``br`` and ``switch`` instructions; ``llvm.expect.i1`` intrinsic.

- **Rust:** ``core::intrinsics::likely``/``unlikely`` (unstable, RFC 1131).

- **Zig:** ``@branchHint(.cold)`` builtin.

The ``WEIGHT`` pragma provides finer-grained control not available in any
of these systems except LLVM's raw metadata. Users who prefer the familiar
C/C++/Rust ``likely``/``unlikely`` API can define them as ordinary Haskell
functions using ``WEIGHT`` and ``INLINE``.


Unresolved Questions
--------------------

- **Interaction with ``-fno-case-merge`` and ``-fno-case-folding``:** Do
  these flags interact with weight propagation? The current implementation
  preserves weights through all transformations, so there should be no
  issue, but this deserves testing.

- **Hot/cold code splitting threshold:** The infrastructure for
  ``.text.unlikely`` sections exists. What edge-weight threshold (as a
  fraction of function total weight) should trigger cold placement? A
  natural default is weight 0 only (explicitly marked cold), but
  heuristic-inferred cold blocks (bottoming alternatives with weight 1
  out of 2001 total, ~0.05%) are also strong candidates. Should there be a
  ``-fhot-cold-threshold=N`` flag, and if so, what should the default be?
  Should cold splitting be gated behind an optimization level (e.g.
  ``-O2`` only)?


Implementation Plan
-------------------


I have a working prototype published to the GHC gitlab instance:


https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15614/diffs


The author (Ian Duncan) will implement the change.


Endorsements
------------


Acknowledgments
---------------

Andreas Klebinger (@AndreasPK) for the original proposal #182 and initial
implementation work (D4327). Simon Peyton Jones for design feedback on
weight representation in Core.
