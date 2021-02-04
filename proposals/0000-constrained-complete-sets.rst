Constrained COMPLETE sets
=========================

.. author:: Sebastian Graf
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
`ticket #14422<https://gitlab.haskell.org/ghc/ghc/-/issues/14422#note_313198>`_):

::

 class ListLike l where
   pattern (:<) :: a -> l a -> l a
   pattern Empty :: l a
   ...

 instance ListLike [] where ...

 f :: ListLike l => l Int -> Int
 f Empty    = 0
 f (x :< _) = x

We declared a type class that abstracts pattern matching for such
``ListLike`` data structure, as if it was, well, a list. There are
two pattern synonyms to this purpose, ``(:<)`` and ``Empty``, so that
pattern matches on a ``ListLike`` have the syntactic sweetness of data
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

But that doesn't help us with ``f``, which is constrained on ``ListLike``!
Moreover, we have to repeat that COMPLETE set for every data constructor.
But isn't it evident that we just want to say "don't warn whenever both
pattern synonyms are matched", regardless of the involved types? This is
very non-modular and an implementational smell of GHC 9.0 and before.

For the sake of the proposal, let's assume GHC can figure out where to store the
COMPLETE set by itself, so we can just write

::

 {-# COMPLETE (:<), Empty #-}

And users of the library can now declare ``ListLike`` instances for ``Array``,
use the pattern snonyms and get accurate pattern match warnings. All seems well.
(This is what's implemented in GHC master at the moment.)

But imagine we also are aware of ``NonEmpty`` and want to broaden the scope and
usefulness of our library:

::

 class ListLike l => NonEmptyLike l where ...
 instance NonEmptyLike NonEmpty where ...

 safeHead :: NonEmptyLike l => l a -> a
 safeHead (x :< _) = x

 {-# COMPLETE (:<) #-}

Note that we declared the matching COMPLETE set in order not to be warned about
``safeHead``.
But in doing so, the following ``unsafeHead`` will not emit a pattern match
warning anymore:

::

 unsafeHead :: [a] -> a
 unsafeHead (x :< _) = x

Urgh! We somehow want to say that the singleton COMPLETE set only applies to
``NonEmptyLike``s. But the type signature syntax doesn't allow us to constrain
on ``NonEmptyLike``! The only way out is to declare the COMPLETE signature for
all concrete data constructors such as ``NonEmpty``:

::

 {-# COMPLETE (:<) :: NonEmpty #-}

And here goes repeating that declaration for all data constructors again, for us
as well as the users of our library. I'd much rather write

::

 {-# COMPLETE[forall f. ListLike f] (:<), Empty #-}
 {-# COMPLETE[forall f. NonEmptyLike f] (:<) #-}

Once, inside the library. And that is the new feature that I propose. And also I
want to deprecate the "type signature" vestige in the process.

Proposed Change Specification
-----------------------------

Syntax
======

Extend Syntax so that we are able to write the example from the previous section

::

 {-# COMPLETE[forall f. ListLike f] (:<), Empty #-}
 {-# COMPLETE[NonEmptyLike] (:<) #-}

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

 NonEmptyLike
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

 class ListLike l where
   pattern (:<) :: a -> l a -> l a
   pattern Empty :: l a
   ...
 {-# COMPLETE[ListLike] (:<), Empty #-} -- (1)

 instance ListLike [] where ...

 f :: ListLike l => l Int -> Int
 f Empty    = 0
 f (x :< _) = x

 class ListLike l => NonEmptyLike l where ...
 {-# COMPLETE[forall l. NonEmptyLike l] (:<) #-} -- (2)

 instance NonEmptyLike NonEmpty where ...

 safeHead :: NonEmptyLike l => l a -> a
 safeHead (x :< _) = x

 safeHead2 :: NonEmpty a -> a
 safeHead2 (x :< _) = x

 unsafeHead :: [a] -> a
 unsafeHead (x :< _) = x

This program passes type-checking. The compiler *should* emit a warning about
the definition of ``unsafeHead`` being incomplete, but not for ``f``,
``safeHead`` or ``safeHead2``:

  - ``f`` has a case for ``Empty`` and ``(:<)``. COMPLETE set (1) applies, because
    the TyCon of the type of the pattern match is ``l``, for which the constraint
    ``tc_ct @l === ListLike l`` is satisfiable.
    (See Unresolved Questions for ``@l`` vs. ``l``)
  - ``f`` has a case for ``Empty`` and ``(:<)``. COMPLETE set (2) does *not* apply,
    because the TyCon of the type of the pattern match is ``l``, for which the
    constraint ``tc_ct @l === NonEmptyLike l`` is not satisfiable.
  - ``safeHead`` has a case for ``(:<)``. COMPLETE set (2) applies, because
    the TyCon of the type of the pattern match is ``l``, for which the constraint
    ``tc_ct @l === NonEmptyLike l`` is satisfiable.
  - ``safeHead2`` has a case for ``(:<)``. COMPLETE set (2) applies, because
    the TyCon of the type of the pattern match is ``NonEmpty``, for which the constraint
    ``tc_ct @NonEmpty === NonEmptyLike NonEmpty`` is satisfiable.
  - ``unsafeHead`` has a case for ``(:<)``. COMPLETE set (2) does *not* apply,
    because the TyCon of the type of the pattern match is ``[]``, for which the constraint
    ``tc_ct @[] === NonEmptyLike []`` is not satisfiable.

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

Syntax
======

Syntactically, we could repurpose the old "type signature" syntax instead of
placing the ``ctype`` in brackets after ``COMPLETE``.

I'm not strongly against that. Currently, GHC expects the name of a
data type constructor to the right of ``::``. But the ``tc_ct`` is
a *constraint* at its base! E.g., ``ListLike`` instead of ``[]``,
``NonEmptyLike`` instead of ``NonEmpty``. So arguably, the fact that
*no* old "type signature" has a valid semantics in the new syntax
makes the transition to the new semantics rather mechanic.

But one of the biggest drawbacks of the old syntax is that I find the analogy to
type signatures misleading, and that still is the case if we expect something of
kind ``k -> Constraint`` to the right of ``::``.

Unresolved Questions
--------------------
The design pretty much determines the implementation.

While writing up this proposal, I had to pause quite often and ask myself
"Is ``forall l. ListLike l`` really the same as ``ListLike``?" Well, one
quantifier is visible whereas the other is not, obviously. But at the time
of this writing, I'm not completely sure if I got the kinding right. If I
didn't, I'm sure someone of you will point that out :)
I'm open for other, maybe less ad-hoc constraint descriptions (e.g. what is
encoded in ``tc_ct``).

Implementation Plan
-------------------
I will implement this proposal.

Endorsements
-------------
