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

Now for the rest of this proposal, we assume that the plan outlined in `#16970
<https://gitlab.haskell.org/ghc/ghc/issues/16970>`_ has landed, in particular
for its operational consequence that strict constructor fields always contain
tagged pointers (meaning their tag is never zero, like for thunks).

This is attractive in the following example involving a binary tree data
structure:

::

 data Tree a = Branch !(Tree a) !a !(Tree a)
             | Leaf

 tsum :: Tree Int -> Int
 tsum Leaf           = 0
 tsum (Branch l x r) = tsum l + x + tsum r

On each pattern match, the compiler has to generate code that checks if the
pointer to the scrutinee is properly tagged, and if not, *enter* the heap
object to evaluate it and get to know the constructor tag. We loosely refer to
this check as the "zero tag check", as in
`#16820 <https://gitlab.haskell.org/ghc/ghc/issues/16820>`_.
It looks like this in the generated C--:

::

           if (R1 & 7 != 0) goto c1fi; else goto c1fj;
       c1fj: // global
           call (I64[R1])(R1) returns to c1fi, args: 8, res: 8, upd: 8;
       c1fi: // global
           // rest of the code, assuming R1 is properly tagged

If the compiler can prove that the scrutinee (R1) is always tagged, it can omit
this check and remove a whole lot of dead code.

Not so in the example above: Since ``tsum undefined`` is a possible call site
of ``tsum``, codegen can't omit the zero tag check on the parameter of
``tsum``. But in the recursive calls the pointers are correctly tagged, since
they come from strict constructor fields ``l`` and ``r``! Giving ``tsum`` the
following type, reflecting its strictness, gets rid of any tag checks,
offloading the zero tag checking to the caller:

::

 tsum :: Strict (Tree Int) -> Int
 tsum (Force Leaf)           = 0
 tsum (Force (Branch l x r)) = tsum (Force l) + x + tsum (Force r)

Because ``Force`` is strict in its field, both the ``Strict`` box and the
wrapped thing are evaluated and tagged, so contrary to above there's no zero
tag check necessary. In the recursive call to ``tsum``, we now need to wrap
``l`` and ``r`` in ``Force``, implicitly evaluating and tagging them before
making the recursive call. This evaluation can immediately be optimised away,
because we know that ``l`` and ``r`` were already tagged to begin with. Thus no
zero tag checking has to happen on the recursive code path, at least saving us
a few instructions and relieving pressure on the branch predictor.

This particular example would be less of an issue if we had strictness analysis
and worker/wrapper transformation work for sum types: The argument would turn
into an unboxed sum with arguably even better performance characteristics. The
point is that we can do this for *any* strictly used argument of lifted kind!
There's an opportunity for worker/wrapper here.

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

Desugaring of unlifted data types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every unlifted data type will become syntactic sugar over a combination
of unlifted newtypes and ``Strict``, like

::

 data Ptr a = Ptr Addr#
 newtype Ptr# :: Type -> TYPE 'UnliftedRep where
   Ptr# :: Strict (Ptr a) -> Ptr# a

 newtype UPair :: Type -> Type -> TYPE 'UnliftedRep where
   UPair :: Strict (a, b) -> UPair a b

Ad-hoc desugaring of strict unboxed tuples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can recover ad-hoc forms of `unboxed strict tuples <https://gitlab.haskell.org/ghc/ghc/issues/17001>`_:

::

 (#! Either Int Bool, Char#, ByteArray# !#)
 ==>
 (# Strict (Either Int Bool), Char#, ByteArray# #)

In fact, ``Strict`` is somewhat similar to the unit unboxed strict tuple. It
crucially is a specialisation to lifted types, though, meaning it still has a
boxed representation. This is important for later endeavours into levity
polymorphism (rather than the current boxity polymorphism) over lifted and
unlifted types.

Worker/wrapper without loss taggedness information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
Meaning we could omit the zero tag check in the original definition (because
``SPair`` is strict in its fields), but not in the definition of ``$wfoo``,
because unboxed pairs are lazy in lifted fields.

With ``Strict``, WW could emulate strict unboxed tuples, hence preserve enough
information for Codegen to omit the zero tag checks:

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

Encoding strictness of a function in its type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Low-level code
~~~~~~~~~~~~~~

Consider the following rather low-level, performance sensitive code:

::
 
 {-# LANGUAGE MagicHash #-}

 module Lib where
 
 import GHC.Exts
 
 pack :: Bool -> Bool -> Int#
 pack False False = 0#
 pack False True  = 1#
 pack True  False = 2#
 pack True  True  = 3#

The programmer manually unboxed the resulting ``Int`` in desperate endeavour of squeezing out the last bit of performance.
This is the generated Core, which looks good enough:

::

 pack
   = \ (ds_d11d :: Bool) (ds1_d11e :: Bool) ->
       case ds_d11d of {
         False ->
           case ds1_d11e of {
             False -> 0#;
             True -> 1#
           };
         True ->
           case ds1_d11e of {
             False -> 2#;
             True -> 3#
           }
       }

STG looks similar. Now look what happens in C--:

::

       c1fp: // global
           if ((Sp + -16) < SpLim) (likely: False) goto c1fq; else goto c1fr;
       c1fq: // global
           R3 = R3;
           R2 = R2;
           R1 = Lib.pack_closure;
           call (stg_gc_fun)(R3, R2, R1) args: 8, res: 0, upd: 8;
       c1fr: // global
           I64[Sp - 16] = c1fi;
           R1 = R2;
           P64[Sp - 8] = R3;
           Sp = Sp - 16;
           if (R1 & 7 != 0) goto c1fi; else goto c1fj; <-- Zero tag check
       c1fj: // global
           call (I64[R1])(R1) returns to c1fi, args: 8, res: 8, upd: 8; <-- Dead enter if argument was always evaluted
       c1fi: // global
           _s1fa::P64 = P64[Sp + 8];
           if (R1 & 7 != 1) goto c1fn; else goto c1fm;
       c1fn: // global
           I64[Sp + 8] = c1fJ;
           R1 = _s1fa::P64;
           Sp = Sp + 8;
           if (R1 & 7 != 0) goto c1fJ; else goto c1fL; <-- Zero tag check
       c1fL: // global
           call (I64[R1])(R1) returns to c1fJ, args: 8, res: 8, upd: 8; <-- Dead enter if argument was always evaluted
       c1fJ: // global
           if (R1 & 7 != 1) goto c1fV; else goto c1fR;
       c1fV: // global
           R1 = 3;
           Sp = Sp + 8;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c1fR: // global
           R1 = 2;
           Sp = Sp + 8;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c1fm: // global
           I64[Sp + 8] = c1fu;
           R1 = _s1fa::P64;
           Sp = Sp + 8;
           if (R1 & 7 != 0) goto c1fu; else goto c1fw; <-- Zero tag check
       c1fw: // global
           call (I64[R1])(R1) returns to c1fu, args: 8, res: 8, upd: 8; <-- Dead enter if argument was always evaluted
       c1fu: // global
           if (R1 & 7 != 1) goto c1fG; else goto c1fC;
       c1fG: // global
           R1 = 1;
           Sp = Sp + 8;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c1fC: // global
           R1 = 0;
           Sp = Sp + 8;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;

Wow, that's quite a mouthful, all due to the lifted representation of ``Bool``!
Assuming that the call site can prove evaluatedness at a lower cost than
``pack``, we can wrap all ``Bool`` s in ``Strict`` and after removing dead code
(by hand, so no liability assumed) and freeing up stack space the C-- would
water down to:

::

       c1fr: // global
           R1 = R2;
           if (R1 & 7 != 1) goto c1fn; else goto c1fm;
       c1fn: // global
           R1 = R3;
           if (R1 & 7 != 1) goto c1fV; else goto c1fR;
       c1fV: // global
           R1 = 3;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c1fR: // global
           R1 = 2;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c1fm: // global
           R1 = R3;
           if (R1 & 7 != 1) goto c1fG; else goto c1fC;
       c1fG: // global
           R1 = 1;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c1fC: // global
           R1 = 0;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;

Much better! A decent backend should be able to turn this into a couple of
bitshifts on the tags.

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
