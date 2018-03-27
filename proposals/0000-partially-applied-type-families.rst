.. proposal-number:: 

.. trac-ticket:: 

.. implemented:: 

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/52>`_.

Partially Applied Type Families
===============================
As of now type families aren't first-class entities in the typesystem, as they have to always appear fully applied. This proposal allows partial application of type families, with necessary restrictions to retain consistency in the typesystem.


Motivation
----------
Type level computations could be made much more flexible and powerful while retaining clean syntax (unlike the approaches listed in the alternatives_). One would be able to write type-level composition, folds, maps, zips, etc.

Currently, if one desires to write a type-level list fold, they must specialize exactly what function they're folding with. With this proposal one would write:

.. code-block:: haskell

  type family Foldr (f :: a ~> b ~> b) (z :: b) (xs :: [a]) :: b where
      Foldr f z '[] = z
      Foldr f z (x':xs) = f x (Foldr f z xs)

Notice how this uses ``~>`` in the kind of ``f``. I've come up with this idea `independently <https://mail.haskell.org/pipermail/haskell-cafe/2017-April/126893.html>`_, but it also comes up in `goldfire's much larger work on dependent types (Section 4.2.4) <http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf#page=73>`_. The implicit unmatchable ``~>`` arrows on term level are used for better compatibility with a possible Dependent Haskell extension. Without those, transition from this proposal to Dependent Haskell could be very problematic and backwards-incompatible.

Proposed Change Specification
-----------------------------

Matchable vs. Unmatchable Arrows
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Introduce a new kind of **unmatchable arrows** `(~>) :: * -> * -> *` with the same precedence/associativity as `(->)`, the kind of **matchable arrows**. `(->)` must be a subtype of `(~>)`.

Implicitly convert all type-level function arrows (in user-defined term functions) into `(~>)`, even if indicated otherwise.

.. code-block:: haskell

  fun1 x = x
  -- fun1 :: a ~> a

  fun2 :: a -> a
  fun2 x = x
  -- fun :: a ~> a

Pattern matching of function-kinded entities in type family RHS'es and typeclass instance bodies can only happen at `(->)` kind.

.. code-block:: haskell

  type family Id :: (a :: *) :: * where Id a = a

  type family F (f :: * -> *) :: *
  type instance F Maybe = () -- good
  type instance F Id = () -- bad: kind mismatch, expected (* -> *), got (* ~> *).

  foo :: F Maybe; foo = () -- good
  foo :: F Id -- bad: kind mismatch, expected (* -> *), got (* ~> *)

  type family G (f :: * ~> *) :: *
  type instance G Maybe = () -- bad: cannot pattern match at kind (* ~> *)

  type family H (f :: * ~> *) :: *
  type instance H f = f () -- good

  foo :: F Maybe; foo = () -- good, 'Maybe' supertyped to (* ~> *)
  foo :: H Id; foo = () -- good

Juxtaposition must operate at the `(~>)` kind, so that both matchable and unmatchable functions can be applied at either term or type level.

Flags
^^^^^
Introduce a language extension: `CurriedTypeFamilies`, that enables explicit use of the `(~>)` kind.

Introduce a flag `-fprint-explicit-arrows`, without which, `(~>)` arrows at type level must be rendered as `(->)`.

Interaction with DataKinds
^^^^^^^^^^^^^^^^^^^^^^^^^^
Promoted regular and GADT constructors (even partially applied) can be matched, so they must retain `(->)` arrows.

Interaction with ConstraintKinds
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Partially applied constraints can be matched, so they must retain `(->)` arrows.

Effect and Interactions
-----------------------
Full backwards compatibility with pre-`TypeInType` code. The `(~>)` arrow is never seen, and occurences of true `(->)` at term level would usually supertype to `(~>)`. On the type/kind level nothing is changed.

Costs and Drawbacks
-------------------
As far as maintenance goes, this is in agreement with the Dependent Haskell paper, so it is possible that Dependent Haskell would some day supersede this (still might be nice to have this extension around if you don't need the full wrath of dependent types).

Alternatives
------------
The `singletons package <https://hackage.haskell.org/package/singletons>`_ makes use of a custom function application type family and functions-as-tags, which leads to very unreadable code with a ton of boilerplate. 

`Oleg's type functions <http://okmij.org/ftp/Haskell/TTypeable/TTypeable.hs>`_ used a similar approach, but there were no datakinds back then, which means that on top of the above, the said functions are unkinded.

Unresolved questions
--------------------
Uncertain about the name of the `(~>)` kind, goldfire suggests `('->)`.

In presence of `-XTypeInType`, a type family like the following:

.. code-block:: haskell

  type family ArrowOf (x :: k) :: * -> * -> * where
      ArrowOf (x :: p k l) = p

can be used to extract a kind-level `(->)` arrow from, say, the kind of `Maybe`, and then the said arrow will mismatch with `(~>)` implicitly used on type level.

It is not clear whether unmatchability of term level arrows should be dealt with in this extension, or whether it should be deferred to Dependent Haskell, but I think handling it here would make a smoother transition.

Implementation Plan
-------------------
Probably me.
