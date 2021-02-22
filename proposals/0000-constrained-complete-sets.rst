Constrained COMPLETE sets
*************************

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

GHC allows users to add what looks like a type signature to a COMPLETE set declaration.
While this is documented purely as a means of disambiguation with the goal of convenient
implementation within GHC, it since has been abused by users, for its refining effect:
It allows a COMPLETE set to only apply at a concrete data type constructor.

It's impossible to bend the "type signature" to accomodate some legitimate
use cases, like requiring the type constructor to satisfy a type class. At
the same time, it is strictly an implementational vestige of the past: The
implementation to land in GHC 9.2 does not even look at it anymore!

Hence I propose to deprecate COMPLETE set "type signatures" and offer a new
mechanism instead that allows the user to express arbitrary constraints on
the type constructor.

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

 {-# COMPLETE[forall f. IsList f] (:<), Empty #-}
 {-# COMPLETE[forall f. IsInfinite f] (:<) #-}

Once, inside the library. And that is the new feature that I propose. And also I
want to deprecate the "type signature" vestige in the process.

Proposed Change Specification
-----------------------------

Syntax
======

Extend Syntax so that we are able to write the example from the previous section

::

 {-# COMPLETE[forall f. IsList f] (:<), Empty #-}
 {-# COMPLETE[IsInfinite] (:<) #-}

Since there is no formal grammar for COMPLETE pragmas, here's how I propose to
change the happy grammar from

::

 sigdecl
   : ...
   | '{-# COMPLETE' con_list opt_tyconsig  '#-}'
 opt_tyconsig
   : {- -}
   | '::' gtycon

to

::

 sigdecl
   : ...
   | '{-# COMPLETE' opt_tycon_constraint con_list '#-}'
 opt_tycon_constraint
   : {- empty -}
   | '[' ctype ']'

Where a ``ctype`` is a "for-all type", according to GHC's happy parser. It's the
same grammatical sort that constitutes the RHS of a ``::`` in a type signature.

(Note that the ``opt_tyconsig`` was dropped; in practice I expect a deprecation
period. See Alternatives.)

Typing
======

The only addition is the new syntactic sort ``opt_tycon_constraint``. If it's
ommitted, then there's no change from the current semantics. If it's present,
then the declared ``ctype`` (let's call it ``tc_ct``) must match against kind
``k -> Constraint``, for any ``k`` that has result kind ``Type``.

Examples for valid ``tc_ct``s:

::

 IsInfinite
 forall f. () ~ f
 forall f. MPTC a f
 Monad
 forall f. (f ~ Int, Semigroup f)

Examples for invalid ``ctype``s:

::

 Int
 (->)
 "Symbol"
 Type

The idea is that we can take ``tc_ct``, apply it to some type constructor and
see if we can satisfy the resulting constraint.

Pattern-match checking
======================

A COMPLETE set with an ``opt_tycon_constraint`` is to be treated the same as one
without, with one exception: When the COMPLETE set is supposedly covered by a set
of patterns in a pattern match, we

  1. Take the result type of the pattern match, ``ty``.
  2. Take the type constructor ``tc`` in the head of ``ty``. If there is no such
     type constructor, the COMPLETE set is not covered by the pattern match.
  3. Apply the ``tc_ct`` to ``tc`` and check whether the constraint solver can satisfy the resulting constraint.
     If not, then the COMPLETE set is not covered by the pattern match.
     If the constraint is satisfiable, then the COMPLETE set is covered by the pattern match.

(Whether the constraint solver can satisfy the constraint naturally depends on
the implementation and which Given constraints it is fed.)

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

 {-# COMPLETE[IsList] (:<), Empty #-} -- (1)

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

 {-# COMPLETE[forall l. IsInfinite l] (:<) #-} -- (2)

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

  - ``f`` has a case for ``Empty`` and ``(:<)``. COMPLETE set (1) applies, because
    the TyCon of the type of the pattern match is ``l``, for which the constraint
    ``tc_ct @l === IsList l`` is satisfiable.
    (See Unresolved Questions for ``@l`` vs. ``l``)
  - ``f`` has a case for ``Empty`` and ``(:<)``. COMPLETE set (2) does *not* apply,
    because the TyCon of the type of the pattern match is ``l``, for which the
    constraint ``tc_ct @l === IsInfinite l`` is not satisfiable.
  - ``safeHead`` has a case for ``(:<)``. COMPLETE set (2) applies, because
    the TyCon of the type of the pattern match is ``l``, for which the constraint
    ``tc_ct @l === IsInfinite l`` is satisfiable.
  - ``safeHead2`` has a case for ``(:<)``. COMPLETE set (2) applies, because
    the TyCon of the type of the pattern match is ``Stream``, for which the constraint
    ``tc_ct @Stream === IsInfinite Stream`` is satisfiable.
  - ``unsafeHead`` has a case for ``(:<)``. COMPLETE set (2) does *not* apply,
    because the TyCon of the type of the pattern match is ``[]``, for which the constraint
    ``tc_ct @[] === IsInfinite []`` is not satisfiable.

#399
====

The examples from #399 would have the following COMPLETE pragmas:

::

 {-# COMPLETE[forall p. (p ~ Proxy a)] Empty #-}
 {-# COMPLETE[forall l. (l ~ [a])] Empty, Cons #-}

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

Alternatives
------------

We could just beef "type signature" syntax. Currently, ``opt_tyonsig`` only
allows the name of a type constructor. We could allow a general ``ctype``
there that should denote the *return type* of the pattern match where it
is applicable and write

::

 {-# COMPLETE (:<), Empty :: IsList l => l a #-}
 {-# COMPLETE (:<) :: IsInfinite l => l a #-}

That is what #399 proposes.
I'm not strongly against that. The only drawback is that it doesn't admit a proper
deprecation story for the old mechanism. E.g., today we write
``{-# COMPLETE (:<) :: Stream #-}``, but according to this alternative design,
that syntax will be ill-kinded: ``Stream`` has kind ``Type -> Type``.
The transition is rather mechanic, though, and coincides for monomorphic data
constructors.

To show that both approaches are equally expressive, realise the following
correspondence:

::

 {-# COMPLETE[forall t. p t] P #-}
 <=> (A)
 {-# COMPLETE P :: p t => t a b c #-}

 {-# COMPLETE Q :: T y z #-}
 <=> (B)
 {-# COMPLETE[forall t. (t ~ T y z)] Q #-}

Meaning: The two different syntaxes are intertranslatable by the
given correspondences. My syntax is terser if the COMPLETE set is
constrained by a type class (correspondence (A)), whereas #399 is
terser if the COMPLETE set is constrained by an equality constraint
(correspondence (B), e.g., if the return type in the type signature
is a concrete data type).

Unresolved Questions
--------------------
The design pretty much determines the implementation.

While writing up this proposal, I had to pause quite often and ask myself
"Is ``forall l. IsList l`` really the same as ``IsList``?" Well, one
quantifier is visible whereas the other is not, obviously. But at the time
of this writing, I'm not completely sure if I got the kinding right. If I
didn't, I'm sure someone of you will point that out :)
I'm open for other, maybe less ad-hoc constraint descriptions (e.g. what is
encoded in ``tc_ct``).

Maybe the alternative syntax is the better one.

Implementation Plan
-------------------
I will implement this proposal.

Endorsements
-------------
