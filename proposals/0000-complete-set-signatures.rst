COMPLETE set signatures
***********************

.. author:: Sebastian Graf
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/400>`_.
.. contents::

GHC allows users to add what looks like a type signature to a COMPLETE set declaration: ::

 {-# COMPLETE P, Q :: Either #-}

While this is documented purely as a means of disambiguation with the goal of
convenient implementation within GHC, it since has been abused by users, for
its refining effect: It allows the COMPLETE set ``{P,Q}`` to only apply at the
concrete data type constructor ``Either``.

It's impossible to bend the "type signature" to accomodate some legitimate
use cases, like requiring the type constructor to satisfy a type class. At
the same time, it is strictly an implementational vestige of the past: The
implementation to land in GHC 9.2 would not even need to look at it anymore!
It still will look at it for backwards-compatilibity, though.

Hence I propose to replace the strange COMPLETE set "type signatures" by a
mechanism that deserves to be called COMPLETE type signatures, requiring users to write ::

 {-# COMPLETE P, Q :: Either l r #-}

instead, e.g. with a regular type to the right of ``::``.

Motivation
----------

Imagine writing a library about list-like data structures (from
`ticket 14422 <https://gitlab.haskell.org/ghc/ghc/-/issues/14422#note_313198>`_):

::

 class IsList l where
   viewCons :: l a -> Maybe (a, l a)

 pattern (:<) :: IsList l => a -> l a -> l a
 pattern x :< xs <- (viewCons -> Just (x,xs))
 pattern Empty :: IsList l => l a
 pattern Empty <- (viewCons -> Nothing)

 instance IsList [] where
   viewCons []     = Nothing
   viewCons (x:xs) = Just (x,xs)

 f :: IsList l => l Int -> Int
 f Empty    = 0
 f (x :< _) = x

We declared a type class that abstracts pattern matching for such
``IsList`` data structure, as if it was, well, a list. There are
two pattern synonyms to this purpose, ``(:<)`` and ``Empty``, so that
pattern matches on a ``IsList`` have the syntactic sweetness of data
constructor matches. Nice. But GHC's pattern match checker will warn
that ``f`` lacks a wildcard match! Lucky for us, we can communicate
that having matched on ``Empty`` and ``(:<)`` makes the pattern match
complete, by way of declaring a *COMPLETE set*:

::

 {-# COMPLETE (:<), Empty #-}

Except... that doesn't work. GHC 9.0 needs us to disambiguate which type
constructor it should attach the COMPLETE set to:

::

 {-# COMPLETE (:<), Empty :: [] #-}

But that doesn't help us with ``f``, which is constrained on ``IsList``!
Moreover, we have to repeat that COMPLETE set for every type constructor.
But isn't it evident that we just want to say "don't warn whenever both
pattern synonyms are matched", regardless of the involved types? This is
very non-modular and an implementational smell of GHC 9.0 and before.

For the sake of the proposal, let's assume GHC can figure out where to store the
COMPLETE set by itself, so we can just write

::

 {-# COMPLETE (:<), Empty #-}

And users of the library can now declare ``IsList`` instances for ``Array``,
use the pattern snonyms and get accurate pattern match warnings. All seems well.
(This is what's implemented in GHC master at the moment.)

But imagine we want to broaden the scope and usefulness of our library and
support infinite containers:

::

 class IsList l => IsInfinite l where
   -- INVARIANT: `viewCons` always returns `Just`

 data Stream a = S a (Stream a)
 instance IsList Stream where
   viewCons (S x xs) = Just (x, xs)
 instance IsInfinite Stream where

 safeHead :: IsInfinite l => l a -> a
 safeHead (x :< _) = x

 {-# COMPLETE (:<) #-}

Note that we declared the matching COMPLETE set in order not to be warned about
``safeHead``.
But in doing so, the following ``unsafeHead`` will not emit a pattern match
warning anymore:

::

 unsafeHead :: [a] -> a
 unsafeHead (x :< _) = x

Urgh! We somehow want to say that the singleton COMPLETE set only applies
to type constructors satisfying ``IsInfinite``. But the type signature
syntax doesn't allow us to constrain on ``IsInfinite``! The only way out
is to declare the COMPLETE signature for all concrete type constructors
such as ``Stream``:

::

 {-# COMPLETE (:<) :: Stream #-}

And here goes repeating that declaration for all type constructors again, for us
as well as the users of our library. I'd much rather write

::

 {-# COMPLETE (:<), Empty :: IsList l => l a #-}
 {-# COMPLETE (:<) :: IsInfinite l => l a #-}

Once, inside the library. The constraint on the
latter makes sure it doesn't apply to the match
type in ``unsafeHead``, while it applies to any
match type that satisfies ``IsInfinite`` that a
user might write.

That is the new feature that I propose. Note the
specification of the full result type instead of
just the name of a data type constructor: It's a
breaking change with the current syntax, but one
that is trivially flagged by a kinding error.

Proposed Change Specification
-----------------------------

Syntax
======

Extend Syntax so that we are able to write the example from the previous section

::

 {-# COMPLETE (:<), Empty :: IsList l => l a #-} -- Context is allowed
 {-# COMPLETE (:<) :: Stream a #-}               -- TyCon App is allowed
 {-# COMPLETE (:<), Empty :: String #-}          -- Even type synonyms

Since there is no formal grammar for COMPLETE pragmas, here's how I propose to
change the happy grammar from

::

 sigdecl
   : ...
   | '{-# COMPLETE' con_list opt_tyconsig  '#-}'
 opt_tyconsig
   : {- -}
   | '::' gtycon
 gtycon
   : ntgtycon
   | '(' ')'
   | '(#' '#)'

to

::

 sigdecl
   : ...
   | '{-# COMPLETE' con_list opt_restysig  '#-}'
 opt_restysig
   : {- -}
   | '::' ctype
 atype
   : ntgtycon
   | '(' ')'
   | '(#' '#)'
   | ... many more ...

Where a ``ctype`` is a "for-all type", according to GHC's happy parser. It's the
same grammatical sort that constitutes the RHS of a ``::`` in a type signature
and since it constrains the result type of the relevant ConLikes, we call
it a "result type signature", often referring just to the type it carries.

Note that this change accepts strictly more syntax, because

- ``atype`` accepts a superset of ``gtycon``
- ``atype`` can be derived from ``ctype`` (via ``type``, ``btype``, ``infixtype``, ``ftype``)

Typing
======

The (type carried by the) result type signature must have kind ``TYPE r``, for
any runtime-representation ``r``.
Type variables obey the `forall-or-nothing rule`_ for quantification.

Examples for well-typed result type signatures:

::

 [a]
 IsInfinite l => l a
 forall l a. IsInfinite l => l a  -- equivalent to the former
 String
 (a ~ Int, Semigroup a) => a
 MPTC a b => a
 Int#

Examples for invalid result type signatures:

::

 Stream
 (->)
 "Symbol"
 Eq Int
 forall l. IsInfinite l => l a  -- forall-or-nothing: a is not in scope

Note that after type-checking

- We *accept* some of the previously well-typed syntax, like ``Int`` and other
  nullary data type constructors.
- We *reject* some of the previously well-typed syntax, like ``Stream`` and other
  non-nullary data type constructors.
- We *accept* new syntax, like ``IsInfinite l => l a``.

Pattern match checking
======================

A COMPLETE set with a result type signature ``{-# COMPLETE cls :: sig_ty
#-}`` is to be treated the same as one without, with one exception: When
the COMPLETE set is supposedly *covered* by a set of patterns in a pattern
match, we

1. Take the result type of the pattern match, ``ty``.
2. Check whether ``sig_ty`` subsumes ``ty``, as per the usual subsumption
   rules of GHC.
   If that is the case, then the COMPLETE set is *covered* by the pattern match.
   Otherwise, the COMPLETE set is *not covered* by the pattern match.

If *any* COMPLETE set is covered by a pattern match, then the pattern match is
exhaustive.

(This is very similar to how a pattern synonym with required constraints is
tested for applicability at a certain match type,
see `Note [Matching against a ConLike result type]`_.
The constraints of ``sig_ty`` should be handled very much like the required
constraints of a pattern snyonym.)

Examples
--------

The example from the introduction:

::

 class IsList l where
   viewCons :: l a -> Maybe (a, l a)

 pattern (:<) :: IsList l => a -> l a -> l a
 pattern x :< xs <- (viewCons -> Just (x,xs))
 pattern Empty :: IsList l => l a
 pattern Empty <- (viewCons -> Nothing)

 {-# COMPLETE (:<), Empty :: IsList l => l a #-} -- (1)

 instance IsList [] where ...

 f :: IsList l => l Int -> Int
 f Empty    = 0
 f (x :< _) = x

 class IsList l => IsInfinite l where
   -- INVARIANT: `viewCons` always returns `Just`

 data Stream a = S a (Stream a)
 instance IsList Stream where
   viewCons (S x xs) = Just (x, xs)
 instance IsInfinite Stream where

 {-# COMPLETE (:<) :: IsInfinite l => l a #-} -- (2)

 instance IsInfinite Stream where ...

 safeHead :: IsInfinite l => l a -> a
 safeHead (x :< _) = x

 safeHead2 :: Stream a -> a
 safeHead2 (x :< _) = x

 unsafeHead :: [a] -> a
 unsafeHead (x :< _) = x

This program passes type-checking. The compiler *should* emit a warning about
the definition of ``unsafeHead`` being incomplete, but not for ``f``,
``safeHead`` or ``safeHead2``:

- ``f`` has a case for ``Empty`` and ``(:<)``. COMPLETE set (1) is covered,
  because the type of the pattern match is ``IsList l => l a``, which is
  subsumed by itself. Thus, the pattern match of ``f`` is exhaustive.
- ``f`` has a case for ``(:<)``. COMPLETE set (2) is *not* covered,
  because the type of the pattern match is ``IsList l => l a``, which is not
  subsumed by ``IsInfinite l => l a``.
- ``safeHead`` has a case for ``(:<)``. COMPLETE set (2) is covered,
  because the type of the pattern match is ``IsInfinite l => l a``, which is
  subsumed by itself. Thus, the pattern match of ``safeHead`` is exhaustive.
- ``safeHead2`` has a case for ``(:<)``. COMPLETE set (2) is covered,
  because the type of the pattern match is ``Stream a``, which is
  subsumed by ``IsInfinite l => l a``.
  Thus, the pattern match of ``safeHead2`` is exhaustive.
- ``unsafeHead`` has a case for ``(:<)``. COMPLETE set (2) is *not* covered,
  because the type of the pattern match is ``[a]``, which is not
  subsumed by ``IsInfinite l => l a``.
- The lack of any COMPLETE set being covered by the the pattern match in
  ``unsafeHead`` means that its definition is flagged as inexhaustive.

Constraints in the signature
============================

Given the following example (full example in `#14422 <https://gitlab.haskell.org/ghc/ghc/-/issues/14422#note_296728>`_) ::

 pattern FZ :: () =>        n ~ 'S m      => Fin n
 pattern FZ <- (viewFin -> VZ) where
   FZ = Fin 0

 pattern FS :: () => n ~ 'S m => Fin m -> Fin n
 pattern FS m <- (viewFin -> VS m) where
   FS (Fin m) = Fin (1 + m)

 {-# COMPLETE FZ, FS #-}

 inc :: Fin n -> Fin (S n)
 inc FZ       = FS FZ
 inc n@(FS _) = FS n

What result type signature would I give the COMPLETE set that preserves its semantics?

- ``{-# COMPLETE FZ, FS :: Fin n #-}`` is the obvious and correct choice.
- ``{-# COMPLETE FZ :: Fin Z #-}; {-# COMPLETE FS :: Fin (S n) #-}`` Means that
  neither COMPLETE set applies at a match type of ``Fin n``, as long as there is
  no Given constraint that refines ``n``. The pattern-match in ``inc`` would be
  regarded as incomplete.
- ``{-# COMPLETE FZ :: n ~ Z => Fin n #-}; {-# COMPLETE FS :: n ~ S m => Fin n #-}``
  Exactly as the previous point; any constraints in the result type signature
  are to interpreted as *required* constraints. That's in stark contrast to
  the *provided* constraints in the pattern synonym definitions of ``FZ`` and
  ``FS``, which act as additional Given constraints in a pattern match.

Relation to `#399`_
===================

This proposal supersedes and obviates `#399`_ in that every example there should
work with our proposal, too.

The major difference is that this proposal wants more general result types.
E.g., we allow full forall types in the result type signature, to allow type
class constraints and feature parity with real type signatures.

Effect and Interactions
-----------------------

As the preceding example shows, the new mechanism allows to declare
each COMPLETE set once, while allowing to specify *exactly* when it
should apply.

It makes the old "type signature" mechanism obsolete, thus it should be
deprecated.

Costs and Drawbacks
-------------------
Implementation of the feature should be relatively straight-forward
once the proposal is settled. I don't expect any additional ongoing
maintenance cost. It's a strictly optional feature. Also it replaces
the very misleading "type signature" syntax with a principled design
that isn't just a leak of implementational detail.

Breaking changes
================

Note that the new result type signatures

1. *align* with the previous semantics of COMPLETE set signatures for nullary
   data type constructors like ``Int``
2. *diverge* from the previous semantics of COMPLETE set signatures for
   non-nullary data type constructors like ``Stream``, which is now ill-kinded.
3. *add considerable expressiveness*, as polymorphic result types like
   ``IsList l => l a`` are possible and have reasonable semantics.

(2) is a breaking change caught at compile-time with a clear and
trivial upgrade path. The compiler could even emulate the old
behavior by instantiating non-nullary data type constructors like ``Stream`` at
the programmer's behalf and emit a warning instead for a deprecation period.
E.g., ::

 {-# COMPLETE (:<) :: Stream #-}

::

 test.hs:42:1: warning:
    Non-nullary data type constructor implicitly instantiated to ``Stream a``
    In the result type signature of a COMPLETE pragma

Alternatives
------------

Different Syntax
================

An earlier version of this proposal used to invent new syntax to specify a
*constraint* on the result type constructor instead, e.g., ::

 {-# COMPLETE[forall l. IsList l] (:<), Empty #-}
 -- or just
 {-# COMPLETE[IsList] (:<), Empty #-}

This syntax is (more arcane, and) terser if the COMPLETE set is constrained by
a type class, whereas the syntax now described in this proposal (and `#399`_) is
terser if the COMPLETE set is constrained by an equality constraint, e.g., if
the result type signature is a concrete data type).

Different ``forall`` semantics
==============================

Consider ::

 {-# COMPLETE P :: forall a. [a] #-}
 pattern P = []

 ... case [] :: [Int] of
   P -> ...

The proposed design will find that the declared COMPLETE signature applies at
the match type ``[Int]``, because ``forall a. [a]`` subsumes ``[Int]`` (in that
we can instantiate ``x :: forall a. [a]`` to have type ``[Int]`` simply by
applying it to ``@Int``.

An alternative design might argue that *building* an expression of type
``forall a. [a]`` is harder than building one of type ``[Int]``; while ``[]``
inhabits both types, ``[4]`` only inhabits the latter.
Based on this relation, it might also make sense to say that the COMPLETE pragma
should not apply at ``[Int]``, because it is *less specific* (in the sense just
discussed) than ``forall a. [a]``.

We think the proposed design makes more sense, for its similarity to how
function type signatures work. Another more soft argument: pattern matching
feels a lot more like instantiating an expression than building one.

Unresolved Questions
--------------------
The design pretty much determines the implementation.

Implementation Plan
-------------------
@cgibbard has a promising prototype at
`!5095 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5095>`_
that I, Sebastian Graf, will shepherd.

Endorsements
------------

.. _`#399`: https://github.com/ghc-proposals/ghc-proposals/pull/399
.. _`Note [Matching against a ConLike result type]`: https://gitlab.haskell.org/ghc/ghc/-/blob/a9129f9fdfbd358e76aa197ba00bfe75012d6b4f/compiler/GHC/HsToCore/Pmc/Solver.hs#L1712
.. _`forall-or-nothing rule`: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html#the-forall-or-nothing-rule
