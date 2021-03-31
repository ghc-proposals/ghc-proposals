Unlifted Datatypes
================================

.. author:: Sebastian Graf
.. date-accepted:: 2020-01-30
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/19523
.. implemented:: 9.2
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/265>`_.
.. contents::

This proposal is about enabling users to define new unlifted, boxed data types
in source Haskell, like

::

 data SMaybe a :: TYPE (BoxedRep Unlifted) where
   SJust    :: !a -> SMaybe a
   SNothing :: SMaybe a

``SMaybe a`` has the same inhabitants as ``Maybe a``, with the exception that
it deprives itself and its arguments of ⊥. The advantage compared to using
bangs everywhere (and hence ``-XStrict``) is that code generation can assume
that every pointer is correctly tagged, as well as not having to write the
bangs in the first place.

There's a
`WIP implementation <https://gitlab.haskell.org/ghc/ghc/merge_requests/2218>`_
which works reasonably well already.

Motivation
----------

Consider the following implementation of a set of ``Int`` s, not unlike
``Data.Set.Strict`` specialised for ``Int``:

::
  
  data IntSet
    = Branch IntSet !Int IntSet
    | Leaf

  member :: Int -> IntSet -> Bool
  member !k Leaf = False
  member k (Branch l k' r)
    | k == k'   = True
    | k <  k'   = member k l
    | otherwise = member k r

The data structure is strict in the integer field so that it can be unboxed, to
squeeze out the last bit of performance. Yet if we look at the resulting C--,
we find that each pattern-match on ``IntSet`` is preceded with the following
check:

::

           if (R1 & 7 != 0) goto c1fi; else goto c1fj;
       c1fj: // global
           call (I64[R1])(R1) returns to c1fi, args: 8, res: 8, upd: 8;
       c1fi: // global

What does it do? It tests whether the scrutinee pointer in R1 has its tag bits
(the lower 3 bits) set. If not, the code will *enter* the heap object to
possibly evaluate it and get to know the constructor tag (i.e. 1 for ``Branch``
and 2 for ``Leaf``). Now, most of the time, the heap object has already been
evaluated and the pointer is in fact properly tagged (the GC for example will
tag every pointer to an evaluated heap object).

Nevertheless, we end up with the code above (which we loosely refer to as the
"zero tag check", as in
`#16820 <https://gitlab.haskell.org/ghc/ghc/issues/16820>`_), because a call
site like ``member k undefined`` is possible! Obviously, ``undefined`` is
neither evaluated nor tagged. Even making ``IntSet`` spine-strict by adding
bangs to the ``Branch`` constructor doesn't get rid of the problem: ``member k
undefined`` is still well-typed, so we have to generate code for it.
Fundamentally, ⊥ is an inhabitant of every data type in Haskell, so every data
declaration introduces a *lifted* type of kind ``TYPE (BoxedRep Lifted)``.

So omission of the zero tag check is only possible if ⊥ is not an inhabitant of
the type. This is exactly the condition for a type to be *unlifted*! Since we
are still talking about heap objects, we need unlifted, but boxed types, which
have kind ``TYPE (BoxedRep Unlifted)``. Indeed, for types of this kind GHC will
already omit the zero tag check today! So all that is left is to extend GHC in
a way that

::
  
  data IntSet :: TYPE (BoxedRep Unlifted) where
    Branch :: IntSet -> !Int -> IntSet -> IntSet
    Leaf   :: IntSet

  member :: Int -> IntSet -> Bool
  member !k Leaf = False
  member k (Branch l k' r)
    | k == k'   = True
    | k <  k'   = member k l
    | otherwise = member k r

does the Right Thing, namely disallowing unevaluated boxes to be passed around
by using call-by-value. This way we can have the guarantee that all unlifted
boxes are always properly tagged.

Preliminary `benchmarks <https://gitlab.haskell.org/ghc/ghc/merge_requests/2218#note_239994>`_
suggest that the unlifted variant is 5-10% faster, just due to omission of the
zero tag check.

Proposed Change Specification
-----------------------------

Henceforth, data type declaration refers to both data type and data family instance declarations.

The entire proposal assumes that the
`pointer rep proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0203-pointer-rep.rst>`_
and its
`amendment <https://github.com/ghc-proposals/ghc-proposals/pull/301>`_
have landed.

Static semantics
~~~~~~~~~~~~~~~~

Kind signatures for regular data type declarations must have a return kind of
``TYPE (BoxedRep Lifted)``. Activating ``-XUnliftedDatatypes`` will lift this
restriction to allow any return kind that unifies with ``TYPE (BoxedRep _)``. A
data type explicitly *may* have levity polymorphic kind
``TYPE (BoxedRep l)`` for some type variable ``l``. Example:

::

  data List a :: TYPE (BoxedRep Lifted) where
    Nil :: List a
    Cons :: a -> List a -> List a
  data SList a :: TYPE (BoxedRep Unlifted) where
    SNil :: SList a
    SCons :: a -> SList a -> SList a
  data PList a :: TYPE (BoxedRep l) where
    PNil :: PList a
    PCons :: a -> PList a -> PList a

Note that Haskell98-style data declarations can use standalone kind signatures
to specify the return kind. Example:

::

  type SList :: Type -> TYPE (BoxedRep Unlifted)
  data SList a = SNil | SCons a (SList a)

If the user provides no kind signature, the default
remains that the data type has lifted return kind (``TYPE (BoxedRep Lifted)``).
Specifying a kind signature is the only way to declare an unlifted or
levity-polymorphic data type.

The same applies to data family instances, in which case the data family
application's result kind must reduce to ``TYPE (BoxedRep _)``. Example:

:: 
 
  data family DF a :: TYPE (BoxedRep l)
  data instance DF Int :: TYPE (BoxedRep Unlifted) where
    TInt :: Int -> DF Int -- unlifted!
  data instance DF Char :: TYPE (BoxedRep Lifted) where
    TChar :: Char -> DF Char -- lifted!

See
`the section on data families in the UnliftedNewtypes proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0098-unlifted-newtypes.rst>`_
and ``Note [Implementation of UnliftedNewtypes]`` for details involving
type-checking the parent data family.

As usual for types of unlifted kind, values of unlifted data types may not be
declared at the top-level:

:: 

  -- rejected, `SList a` is not lifted 
  nil :: SList a
  nil = SNil 

  -- accepted, `a -> SList a -> SList a` is a function, thus lifted
  cons :: a -> SList a -> SList a
  cons x xs = SCons x xs 

The top-level restriction is not fundamental (see
`#17521 <https://gitlab.haskell.org/ghc/ghc/issues/17521>`_), but best
discussed in a separate proposal.

Dynamic Semantics
~~~~~~~~~~~~~~~~~

There is no change in GHC's existing dynamic semantics, namely

- Values of unlifted type are always computed eagerly
- Only values of types with a concrete ``RuntimeRep`` can be let-bound

Example:

::

  f x = let y = if odd 42 then SNil else SCons 42 SNil
        in ... y ...

Since the binding for ``y`` is unlifted, the ``let`` binding (is legal and) is
evaluated eagerly, without building a thunk.

This proposal simply allows data declarations to have kinds other than
``TYPE (BoxedRep Lifted)`` and the existing dynamic semantics of GHC takes care
of the rest.

Examples
--------

Declarations
~~~~~~~~~~~~

Here are a few example declarations that should all be accepted:

::
  
  data PList a :: TYPE (BoxedRep l) where
    PNil :: PList a
    PCons :: a -> PList a -> PList a
  -- alternative using a SAKS:
  -- type PList :: Type -> TYPE (BoxedRep l)

  -- This one is with visible quantification in a SAKS
  type DF :: forall l. Type -> TYPE (BoxedRep l)
  data family DF
  data instance DF Unlifted a where


Unlifted (call-by-value) semantics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

::

 data UPair a b :: TYPE (BoxedRep Unlifted) where
   UPair :: a -> b -> UPair a b

* When occuring in a constructor field (e.g.
  ``data T = MkT (UPair Int Bool)``), the semantics are identical to a strict
  field (``data T = MkT !(Int, Bool)``).

* In an application ::

    f (if odd 42
         then UPair a a
         else UPair b b)

  the unlifted argument is evaluated before the application is beta reduced. So
  call-by-value instead of call-by-need.

* In a let binding ``let x = UPair a b in e`` ::

    let x = if odd 42
              then UPair a a
              else UPair b b
    in e

  the unlifted right-hand side of ``x`` is evaluated before the body ``e``.

The Strict data type
~~~~~~~~~~~~~~~~~~~~

We get to define ``Strict``

::

 data Strict a :: TYPE (BoxedRep Unlifted) where
   Force :: !a -> Strict a

that deprives itself and its argument of ⊥.

``Strict`` is the very essence of this proposal: Every unlifted data type can
be defined in terms of lifted data types and ``Strict``, at the cost of an
additional indirection.

Whether we could define ``Strict`` as a ``newtype`` -- which would then mean we
coerce between kinds, meaning coercions aren't zero-cost -- is left for a
separate proposal.

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

The programmer manually unboxed the resulting ``Int`` in a desperate endeavour
of squeezing out the last bit of performance. This is the generated Core, which
looks good enough:

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
``pack``, we can define a new unlifted datatype ``SBool``:

::
 
 {-# LANGUAGE MagicHash #-}

 module Lib where
 
 import GHC.Exts

 data SBool :: TYPE (BoxedRep Unlifted) where
   STrue :: SBool
   SFalse :: SBool
 
 pack :: SBool -> SBool -> Int#
 pack SFalse SFalse = 0#
 pack SFalse STrue  = 1#
 pack STrue  SFalse = 2#
 pack STrue  STrue  = 3#

And then after removing dead code (by hand, so no liability assumed) and
freeing up stack space the C-- would water down to:

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

Data structure re-use
~~~~~~~~~~~~~~~~~~~~~

With levity polymorphism, we can even re-use currently lifted-only data structures:

::
  
  data PList a :: TYPE (BoxedRep l) where
    PNil :: PList a
    PCons :: a -> PList a -> PList a

  mapLifted :: (a -> b) -> PList a -> PList b -- assumes defaulting of Levity to @Lifted
  mapLifted f PNil         = PNil
  mapLifted f (PCons x xs) = PCons (f x) (mapLifted f xs)
  
  mapUnlifted :: (a -> b) -> PList @Unlifted a -> PList @Unlifted b
  mapUnlifted f PNil         = PNil
  mapUnlifted f (PCons x xs) = PCons (f x) (mapUnlifted f xs)
  
There no chance of sharing the ``map`` definition (not this one anyway)
currently, because we don't have levity polymorphism in expressions yet, which
should be tackled in a separate proposal.

Effect and Interactions
-----------------------

Introduction of user-defined unlifted data types means we can finally write
code processing data types that can be compiled as if we were in a strict
language.

**Strict constructor fields** share considerable overlap with ``Strict``, yet they
proved insufficient for encoding invariants for efficient code generation.

This proposal consciously left out further work like a new specification for
**levity polymorphic code**. Similar to data types, functions can be levity
polymorphic, too. There's
`#15532 <https://gitlab.haskell.org/ghc/ghc/issues/15532>`_,
which wants to weaken the restrictions we have in place for runtime-rep
polymorphic code.

**Pattern match checking** with unlifted types will be weird in some edge cases.
Consider the following example:

::
 
  data SVoid :: TYPE (BoxedRep Unlifted)
  f :: SVoid -> ()
  f _ = ()

Should this program be accepted without warning? It would be accepted for
lifted ``Void``, because ``f (error "boom")`` is a valid call and would
evaluate to ``()``. But with unlifted ``Void`` this doesn't make sense anymore:
Because of call-by-value, the ``error`` thunk will be evaluated before entering
``f``, resulting in a crash. In that regard, it's similar to the situation with

::

  data Void
  f :: Void -> ()
  f !_ = ()

And should probably elicit an inaccessible RHS warning. I guess this is accurate
for unlifted functions as well as long as we don't allow functions without
bindings.

**-XStrict/-XStrictData** could implicitly turn all data declarations
into unlifted ones. I see two potential problems:

* If a data type is exported, it's now an unlifted type. That's a breaking change.
* For data family instances, this is only possible if the parent data family
  was kind polymorphic. Plus it's a strange thing to do change kinds of a
  declaration just by switching on a language extension.

So rather dreadfully, we probably shouldn't "augment" ``-XStrict``.

**Lazy pattern matches** ``let ~t = ... in ...`` should not be allowed if ``t``
is unlifted. That is exactly the behavior that is currently implemented in GHC.

Concerning GHC's implementation (*only*) of **Data con wrappers**: The wrappers
of nullary constructors carrying some constraint, like

:: 

  data T a :: TYPE (BoxedRep Unlifted) where
    TInt :: T Int

will become unlifted top-level bindings in Core:

::

  -- data constructor worker:
  TInt :: a ~ Int => T Int
  -- data constructor wrapper:
  $WInt :: T Int
  $WInt = TInt $d

which GHC currently forbids in source syntax. I *think* these will always be
properly tagged, though, so it's just an implementation detail. The Core
formalism should only be minimally affected by such unlifted bindings at the
top-level. Either regard them as evaluated on start up or just allow normal
forms (modulo lifted fields) See
`#17521 <https://gitlab.haskell.org/ghc/ghc/issues/17521>`_.
Another way forward would be to turn wrappers of nullary unlifted constructors
into functions by adding a `Void#` argument.

Costs and Drawbacks
-------------------
Thanks to previous work on unlifted types and ``-XUnliftedNewtypes``, this
proposal seems rather easy to implement, with the majority of changes happening
in the parser and type-checker. Notably the backend is not only affected **at
all**. Very good cost to benefit ratio.

As for the risk of making the language harder to learn: Beginners won't come in
touch with unlifted datatypes at all. Unless they crave for better performance
in a custom data structure, at which point I wouldn't consider them beginners
anymore. There's precedent in going from unlifted to lifted by
`Idris <http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html>`_ or
`OCaml <https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html>`_ with
their ``Lazy`` data type.

Alternatives
------------
Implement
`the Strict data type only <https://github.com/ghc-proposals/ghc-proposals/pull/257>`_.
Doing so provides the same semantics at the cost of more syntactic overhead,
plus we can't get rid of the additional box.

Implement `strict unboxed tuples <https://gitlab.haskell.org/ghc/ghc/issues/17001>`_
instead. Rules out the promising direction of levity polymorphism in the
future, though.

Introduce ``unlifted`` as a new contextual keyword ("special id") after
``data``, like in a previous state of this proposal. Apart from the negligible
disadvantage that it steals syntax (``data unlifted a => T = ...`` could parse
``unlifted`` as ``unlifted :: Type -> Constraint``) it seems inconsistent
compared to ``-XUnliftedNewtypes``, which uses kind signatures and would not
support the syntax. Also, levity polymorphic data type declarations would be
impossible.

Unresolved Questions
--------------------
* Should we allow data family instances without kind signatures when the
  return kind can be inferred to be ``TYPE (BoxedRep _)``? We do so for
  ``Type`` and unlifted newtype instances. The user would need to write the
  kind signature according to this proposal.

Implementation Plan
-------------------
I will implement the changes. There is a working implementation at
`!2218 <https://gitlab.haskell.org/ghc/ghc/merge_requests/2218>`_, but it may
still be a little rough around the edges.
