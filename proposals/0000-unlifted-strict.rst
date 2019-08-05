Unlifted Data through ``Strict``
================================

.. author:: Sebastian Graf
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/257>`_.
.. sectnum::
.. contents::

This proposal is about introducing to `ghc-prim` the unlifted data type

::

 data Strict :: Type -> TYPE 'UnliftedRep where
   Force :: !a -> Strict a

that deprives itself and its argument of ⊥.

Motivation
----------
To quote the `unlifted data types wiki page <https://gitlab.haskell.org/ghc/ghc/wikis/unlifted-data-types#proposal-b4-levity-polymorphic-functions>`_:

Bob Harper `has written <https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/>`_:

    Haskell suffers from a paucity of types.  It is not possible in Haskell to
    define the type of natural numbers, nor the type of lists of natural numbers
    (or lists of anything else), nor any other inductive type!

The reason, of course, is that whenever you write ``data Nat = Z | S !Nat``, you
define a type of strict natural numbers, AS WELL AS bottom. Ensuring that an
``x :: Nat`` is never bottom requires all use-sites of this type to do strict
pattern matching / force the value appropriately. It would be nice if there was
some type-directed mechanism which specified that a value ``x`` was always
evaluated. This would give benefits, e.g. for code generation, where we can
assume that a pointer never points to a thunk or indirection and is thus
properly tagged.

For ``Nat`` above, having ``x :: Strict Nat`` is enough to know that the
inhabitant really only can be a natural number.

For another example, consider a binary tree data structure:

::

 data Tree a = Branch !(Tree a) !a !(Tree a)
             | Leaf

 tsum :: Tree Int -> Int
 tsum Leaf           = 0
 tsum (Branch l x r) = tsum l + x + tsum r

Since ``tsum undefined`` is a possible call site of ``tsum``, codegen can't
omit a tag check on the parameter of ``tsum``, although in all recursive calls
the tail should be properly tagged. Giving ``tsum`` the following type,
reflecting its strictness, gets rid of any tag checks, offloading the burden to
the caller:

::

 tsum :: Strict (Tree Int) -> Int
 tsum Force Leaf           = 0
 tsum Force (Branch l x r) = tsum l + x + tsum r

Note that this particular example would be less of an issue if we had
strictness analysis and worker/wrapper transformation work for sum types: The
argument would turn into an unboxed sum with arguably even better performance
characteristics.
The point is that we can do this for *any* strictly used argument of lifted
kind! There's an opportunity for worker/wrapper here.

Proposed Change Specification
-----------------------------
Add a new data type ``Strict :: Type -> TYPE 'UnliftedRep`` to ``GHC.Exts``
with a single constructor ``Force :: !a -> Strict a``.

As ``Strict`` is just one more unlifted data type, its semantics follow from 
the semantics of unlifted data types. In particular:

* When occuring in a constructor field (e.g. ``data T = MkT (Strict ())``), the
  semantics are identical to a field with a bang pattern
  (``data T = MkT !()``), modulo packing and unpacking of the ``Force``
  constructor.

* In an application ``f (Force a)``, the argument ``Force a`` is evaluated
  before the application is beta reduced. Since ``Force`` is strict in its
  field, this forces evaluation of the wrapped lifted expression ``a``. 

* In a let binding ``let x = Force e1 in e2``, the right-hand side ``Force e1``
  is evaluated before the body. Since ``Force`` is strict in its field, this
  forces evaluation of the wrapped lifted expression ``e1``. 

Examples
--------
Every unlifted data type will become syntactic sugar over a combination
of unlifted newtypes and ``Strict``, like

::

 data Ptr a = Ptr Addr#
 newtype Ptr# :: Type -> TYPE 'UnliftedRep where
   Ptr# :: Strict (Ptr a) -> Ptr# a

 newtype UPair :: Type -> Type -> TYPE 'UnliftedRep where
   UPair :: Strict (a, b) -> UPair a b

We can even recover ad-hoc forms of `unboxed strict tuples <https://gitlab.haskell.org/ghc/ghc/issues/17001>`_:

::

 (#! Either Int Bool, Char#, ByteArray# !#)
 ==>
 (# Strict (Either Int Bool), Char#, ByteArray# #)

In fact, ``Strict`` is somewhat similar to the unit unboxed strict tuple. It
crucially is a specialisation to lifted types, though, meaning it still has a
boxed representation. This is important for later endeavours into levity
polymorphism (rather than the current boxity polymorphism) over lifted and
unlifted types.

Another example again concerns the worker/wrapper transformation. Consider

::

 data SPair a b = SPair !a !b
 
 foo :: Int -> SPair Int Int
 foo x
   | even x
   = SPair (x+1)  x
   | otherwise
   = case foo (x-1) of
       SPair a b -> SPair (a+1) (b+1)
  data SPair a b = SPair !a !b

CPR analysis will discover that ``foo`` has the constructed product result
property. Hence WW will turn this function into (ignoring strictness and
inlining for the sake of simplicity)

::

 foo :: Int -> SPair Int Int
 foo x = case $wfoo x of (# a, b #) -> SPair a b

 $wfoo :: Int -> (# Int, Int #)
 $wfoo x
   | even x
   = (# (x + 1), x #)
   | otherwise
   = case $wfoo (x-1) of
       (# a, b #) -> (# a+1, b+1 #)

Compared to the original definition of ``foo``, ``$wfoo`` lost knowledge of the
fact that ``a`` and ``b`` in the recursive call are always evaluated, hence
tagged after `#16970 <https://gitlab.haskell.org/ghc/ghc/issues/16970>`_.
Meaning we could omit the tag check in the original definition (because
``SPair`` is strict in its fields), but not in the definition of ``$wfoo``,
because unboxed pairs are lazy in lifted fields.

With ``Strict``, WW could emulate strict unboxed tuples, hence preserve enough
information for Codegen to omit the tag checks:

::

 foo :: Int -> SPair Int Int
 foo x = case $wfoo x of (# Force a, Force b #) -> SPair a b

 $wfoo :: Int -> (# Strict Int, Strict Int #)
 $wfoo x
   | even x
   = (# Force (x + 1), Force x #)
   | otherwise
   = case $wfoo (x-1) of
       (# Force a, Force b #) -> (# Force (a+1), Force (b+1) #)

Finally, ``Strict`` provides a type-level mechanism to convey strictness of a
function to the compiler without having to resort to often superfluous bangs,
by encoding strictness in its calling convention:

::

 printAverage :: Strict Int -> Strict Int -> IO ()
 printAverage (Force sum) (Force count)
   | count == 0 = error "Need at least one value!"
   | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

Superficially, this doesn't seem to have an advantage over ``-XBangPatterns``,
but smililar to ``safeHead :: NonEmpty a -> a`` it offloads the burden of
evaluation to the caller, who is in a better position to decide if that ``seq``
is needed or not.

Effect and Interactions
-----------------------
Introduction of ``Strict`` means we can finally write code processing data types
that can be compiled as if we were in a strict language.

Strict constructor fields share considerable overlap with ``Strict``, yet they
proved unsufficient for encoding invariants for efficient code generation.

Many useful source language constructs, such as unlifted data types and strict
unboxed tuples, arise as syntactic sugar over the proposed mechanism. This
implies that subsequent proposals can work on implementing these syntactic
amenities after this proposal paved the way for a reference semantics.

This proposal consciously left out further work like a new specification for
levity polymorphism (every data type polymorphic over lifted types can
potentially be reused for unlifted, boxed data types!) and details of whether
we should eliminate the indirection in ``Force`` (we certainly should!) and to
what degree we could infer and let the user omit ``Force`` constructors.

Costs and Drawbacks
-------------------
I have no idea how long this will take to be implemented. Presumably all phases
of the compiler up to C-- are affected, but the change is atomic enough to be
implemented in a rather straightforward fashion. Since this isn't exactly new a
surface language extension, I don't think maintenance will be an issue.

Beginners won't have to touch ``Strict`` at all, unless they crave for better
performance in a custom data structure, at which point I wouldn't consider them
beginners anymore. There's precedent in going from unlifted to lifted by `Idris
<http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html>`_ with its
``Lazy`` data type.

Alternatives
------------
Implement `proper unlifted data types
<https://gitlab.haskell.org/ghc/ghc/wikis/unlifted-data-types#proposal-1-allow-data-types-to-be-declared-as-unlifted>`_.
This would have to anticipate all possible interactions with existing ways to
introduce (generalised) algebraic data types, from surface language issues
regarding syntax to code generation issues. Only adding ``Strict`` seems far
more compositional and handles one problem (that of semantics and code
generation) at a time.

Implement `strict unboxed tuples <https://gitlab.haskell.org/ghc/ghc/issues/17001>`_
instead of ``Strict``. As mentioned in the Examples section, although
``Strict`` is a specialisation of the strict unboxed unit tuple, the fact that
it still has a boxed representation opens up the possibility for improvements
to levity polymorphism in the future.

Unresolved Questions
--------------------
* It's unclear to me where the data type and its constructors whould live
  within ``GHC.Exts``. Should ``Force`` even be a bidirectional pattern synonym
  to some internal constructor? Should it become a language extension, like
  unboxed tuples?
* Unsure whether ``Strict(Force)`` is the best naming scheme, but it is neatly
  complementary to what `Idris does <http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html>`_.
* We really want to remove the indirection of ``Force`` wherever we can. Can we
  do this in the general case? What about interactions with
  reflection/``Typeable``?

Implementation Plan
-------------------
I will implement the changes, probably with a lot of help from #ghc.
Anyone is invited to join in on the effort, of course.
