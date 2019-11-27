Unlifted Datatypes
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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/265>`_.
.. sectnum::
.. contents::

This proposal is about enabling users to define new unlifted, boxed data types
in source Haskell, like

::

 data unlifted SMaybe 
   = SJust !a
   | Nothing

``SMaybe a`` has the same inhabitants as ``Maybe a``, with the exception that
it deprives itself and its arguments of ⊥. The advantage compared to using
bangs everywhere (and hence ``-XStrict``) is that code generation can assume
that every pointer is correctly tagged.

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
declaration introduces a *lifted* type of kind ``TYPE 'LiftedRep``.

So omission of the zero tag check is only possible if ⊥ is not an inhabitant of
the type. This is exactly the condition for a type to be *unlifted*! Since we
are still talking about heap objects, we need unlifted, but boxed types, which
have kind ``TYPE 'UnliftedRep``. Indeed, for types of this kind GHC will
already omit the zero tag check today! So all that is left is to extend GHC in
a way that

::
  
  data unlifted IntSet
    = Branch IntSet !Int IntSet
    | Leaf

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

Syntax
~~~~~~

The language extension ``-XUnliftedDatatypes`` introduces a new contextual
keyword ``unlifted``, only to be used in data type declarations. Revised
grammar rules:

::

 topdecl   -> 'data' [ 'unlifted' ] [ context => ] ...
 topdecl   -> 'data' 'instance' [ 'unlifted' ] [ context => ] ...
 decl_inst -> 'data' [ 'instance' ] [ 'unlifted' ] [ context => ] ...

GADT-style declarations can optionally specify a kind signature.
TODO: Also allow ``lifted`` for symmetry?

Static semantics
~~~~~~~~~~~~~~~~

Name resolution can ignore the ``unlifted`` keyword.

Similar to ``-XUnliftedNewtypes``, the return kind of a data type declaration's
kind signature (which may be given explicitly by the user or be inferred) is
``TYPE 'UnliftedRep`` when there was a leading ``unlifted`` keyword.

Data family instances may be declared unlifted, in which case the data family
application's result kind must reduce to ``TYPE 'UnliftedRep``. See
`the section on data families in the UnliftedNewtypes proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0098-unlifted-newtypes.rst>`_
and ``Note [Implementation of UnliftedNewtypes]`` for details involving
type-checking the parent data family.

The static semantics of other types of unlifted kind, such as the inability to
delare them at the top-level, apply. The top-level restriction is not
fundamental (see `#17521 <https://gitlab.haskell.org/ghc/ghc/issues/17521>`_),
but best discussed in a separate proposal.

Dynamic Semantics
~~~~~~~~~~~~~~~~~

Unliftedness (i.e., the absence of divergence) in general implies the need for
an eager evaluation semantics, which GHC implements in expression of kind
``#``.

Thus, call-by-value semantics are already well established within GHC. The
novelty is pattern matching on and construction of unlifted data types, but
that's exactly the same as it is for lifted data types. 

Examples
--------

Unlifted (call-by-value) semantics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example:

::

 data unlifted UPair a b = UPair a b

* When occuring in a constructor field (e.g.
  ``data T = MkT (UPair Int Bool)``), the semantics are identical to a field
  with a bang pattern (``data T = MkT !(Int, Bool)``).

* In an application ``f (UPair a b)``, the argument ``UPair a b`` is evaluated
  before the application is beta reduced. So call-by-value instead of
  call-by-need.

* In a let binding ``let x = UPair a b in e``, the right-hand side ``UPair a b``
  is evaluated before the body.

The Strict data type
~~~~~~~~~~~~~~~~~~~~

We get to define ``Strict``

::

 data unlifted Strict a = Force !a

that deprives itself and its argument of ⊥.

``Strict`` is the very essence of this proposal: Every unlifted data type can
be defined in terms of lifted data types and ``Strict``.

It can be used to encode evaluatedness in the type system and thus has a very
favorable interaction with the worker/wrapper transformation. Consider

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

Major caveat: This will only be a worthwhile thing to do if we manage to
eliminate the indirection in all cases, which is impossible to do in
polymorphic scenarios (think of RTS hacks like ``tagToEnum#``).

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
``pack``, we can wrap all ``Bool`` s in ``Strict`` (see above) or define a new
unlifted ``SBool`` and then after removing dead code (by hand, so no liability
assumed) and freeing up stack space the C-- would water down to:

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

Introduction of user-defined unlifted data types means we can finally write
code processing data types that can be compiled as if we were in a strict
language.

**Strict constructor fields** share considerable overlap with ``Strict``, yet they
proved insufficient for encoding invariants for efficient code generation.

This proposal consciously left out further work like a new specification for
levity polymorphism (every data type polymorphic over lifted types can
potentially be reused for unlifted, boxed data types!) and details of whether
we should eliminate the indirection in constructors like ``Force`` (we
certainly should!) and to what degree we could infer and let the user omit
``Force`` constructors.

**Pattern match checking** with unlifted types will be weird in some edge cases.
Consider the following example:

::
 
  data unlifted SVoid 
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
into ``unlifted`` ones. I see two potential problems:

* If a data type is exported, it's now an unlifted type. That's a breaking change.
* For data family instances, this is only possible if the parent data family
  was kind polymorphic. Plus it's a strange thing to do change kinds of a
  declaration just by switching on a language extension.

So rather dreadfully, we probably shouldn't "augment" ``-XStrict``.

**-UnliftedNewtypes** introduces unlifted *newtypes*, but does so simply by
inferring the kind of its single constructor's field type, no ``unlifted``
needed. Now with the new ``unlifted`` keyword, we could potentially allow
syntax like ``newtype unlifted Foo (a :: TYPE r) = Foo a``. What are its semantics?
Can we still have ``Foo Int#``? That wouldn't exactly be ``UnliftedRep`` (which
this proposal is all about), but the unlifted, unboxed runtime-rep ``IntRep``.
Similarly, do we allow ``Foo Int``? That would be boxed and lifted, seemingly
contradicting the declaration.

So I suggest that we (somewhat ironically) *disallow* ``unlifted`` syntax for
Newtype delcarations and instead suggest to activate ``-XUnliftedNewtypes``,
which will automatically infer the generalised kind.

Costs and Drawbacks
-------------------
Thanks to previous work on unlifted types and ``-XUnliftedNewtypes``, this
proposal seems rather easy to implement, with the majority of changes happening
in the parser and type-checker. Notably the backend is not only affected **at
all**. Very good cost to benefit ratio.

As for the risk of making the language harder to learn: Beginners won't come in
touch with unlifted newtypes at all. Unless they crave for better performance
in a custom data structure, at which point I wouldn't consider them beginners
anymore. There's precedent in going from unlifted to lifted by
`Idris <http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html>`_ with its
``Lazy`` data type.

Alternatives
------------
Implement
`the Strict data type only <https://github.com/ghc-proposals/ghc-proposals/pull/257>`_.
Doing so provides the same semantics at the cost of more syntactic overhead.

Implement `strict unboxed tuples <https://gitlab.haskell.org/ghc/ghc/issues/17001>`_
instead. Rules out the promising direction of levity polymorphism in the
future, though.

Unresolved Questions
--------------------
* We really want to remove the indirection of constructors like ``Force``
  wherever we can. Can we do this in the general case? What about interactions
  with reflection/``Typeable``?

Implementation Plan
-------------------
I will implement the changes. There is a working implementation at
`!2218 <https://gitlab.haskell.org/ghc/ghc/merge_requests/2218>`_, but it may
still be a little rough around the edges.
