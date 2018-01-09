.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/104>`_.

.. contents::

Simplify rules for using record update syntax with GADTs
========================================================

The current rules for when record update syntax may be used for GADTs
are confusing. We can make them much simpler.

Motivation
------------
According to the GHC user's guide version 8.2.2
`section 10.4.7 <https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/glasgow_exts.html#gadt-style>`_

  The rule for record update is this:
  
      the types of the updated fields may mention only the universally-quantified type variables of the data constructor. For GADTs, the field may mention only types that appear as a simple type-variable argument in the constructor’s result type.
  
  For example:

  ::

    data T a b where { T1 { f1::a, f2::b, f3::(b,c) } :: T a b } -- c is existential
    upd1 t x = t { f1=x }   -- OK:   upd1 :: T a b -> a' -> T a' b
    upd2 t x = t { f3=x }   -- BAD   (f3's type mentions c, which is
                            --        existentially quantified)
    
    data G a b where { G1 { g1::a, g2::c } :: G a [c] }
    upd3 g x = g { g1=x }   -- OK:   upd3 :: G a b -> c -> G c b
    upd4 g x = g { g2=x }   -- BAD (f2's type mentions c, which is not a simple
                            --      type-variable argument in G1's result type)

The rules appear to ensure not only that updates are well-typed, but also that
the type of a function defined using a record update can always be *inferred*.
Although this may sound desirable, it has two major downsides: some entirely
reasonable updates are prohibited, and users have to learn and remember two
extra type rules.

Proposed Change Specification
-----------------------------
I propose that record updates for GADTs be treated purely as syntactic
sugar for pattern matching and construction, with no special type rules.
For example, given a GADT constructor

::

  C :: {f1 :: T1, f2 :: T2, f3 :: T3} -> ...

The update

::

  c { f1 = v1', f2 = v2'}

should be treated *identically* to

::

  case c of
    C {f1 = _v1, f2 = _v2, f3 = v3} -> C {f1 = v1', f2 = v2', f3 = v3}

Effect and Interactions
-----------------------
To the best of my knowledge, three things get better and one gets
worse.

The good news:

- The extra rules go away.

- Functions like ``upd4`` whose updates mention type indices become valid,
  although they will typically require a type signature.

- Legitimate updates involving existentials become valid without
  a hitch:

  ::

    data Same f where
      Same :: {p1 :: f a, p2 :: f a, p3 :: f Int} -> Same f

    swapout x y s = s {p1 = x, p2 = y}
    -- GHC infers swapout :: f a -> f a -> Same f -> Same f

The bad news:

Some types that were previously inferred no longer will be, so
some code may break. For example, suppose we have

::

  data Fish a b where
    Fish :: {x :: a, y :: a, Int} :: Fish a a

Today, we can write

::

  flounder c = Fish {x = 3, y = 4}

and GHC will infer the signature

::

  flounder :: Num a => Fish a b -> Fish a b

Under this proposal, the type of ``flounder`` would not be inferred, but
the user could write it using any of the following signatures:

::

  flounder :: Num a => Fish a b -> Fish a b
  flounder :: Num b => Fish a b -> Fish a b
  flounder :: Num c => Fish a b -> Fish c c

Costs and Drawbacks
-------------------
I suspect the development cost to be small and the maintenance cost
smaller. I believe this change will make the language easier to learn.
I predict the amount of code this breaks will be extremely small.

Alternatives
------------
No non-trivial alternatives come to mind at present.

Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
