OPAQUE pragma
=============

.. author:: Christiaan Baaij
.. date-accepted:: 2021-05-17
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/415>`_.
.. contents::

To paraphrase
https://gitlab.haskell.org/ghc/ghc/-/issues/19553#note_339736:

Being able to mark a (global) binder *opaque*, thus:

::

   {-# OPAQUE f #-}

What does OPAQUE mean?

-  Don’t inline it
-  Type-class specialisation is a no-op, as is SpecConstr
-  Don’t perform any w/w on it, not even cast w/w in the early stage
   simplifier.
-  Let any caller know that while the annotated function might be head
   strict, it will not unbox its arguments – e.g. if
   ``f :: Int -> Int -> blah`` was strict, callers would expect its
   wrapper to inline and unbox, but it won’t (For NOINLINE GHC carefully
   does w/w for this reason! See
   ``Note [Worker-wrapper for NOINLINE functions]`` in
   ``GHC.Core.Opt.WorkWrap``).

To quote
https://github.com/ghc-proposals/ghc-proposals/pull/415#issuecomment-802650289:

   The ultimate goal is that every call of ``f`` generates, well, a call
   of ``f``, not of some name-mangled variant.

Motivation
----------

The base/prelude library of the Clash Haskell-to-Hardware compiler has
multiple functions where Clash should *not* translate the definitions of
those functions to hardware, but use a specific-to-that-function
hard-coded translation instead. An example of such a function would be
multiplication for Clash’ arbitrary-sized signed number type (See
https://github.com/clash-lang/clash-compiler/blob/ab6dd31dc61fc96bb058e57dc117db6f5626d3a9/clash-prelude/src/Clash/Sized/Internal/Signed.hs):

.. code:: haskell

   module Clash.Sized.Internal.Signed where

   newtype Signed (n :: Nat) = S { unsafeToInteger :: Integer}

   {-# NOINLINE times# #-}
   times# :: Signed m -> Signed n -> Signed (m + n)
   times# (S a) (S b) = S (a * b)

How does Clash know it should pick a hard-coded translation for that
``times#`` function? It reads in files such as
https://github.com/clash-lang/clash-compiler/blob/ab6dd31dc61fc96bb058e57dc117db6f5626d3a9/clash-lib/prims/vhdl/Clash_Sized_Internal_Signed.primitives
which contains entries such as:

.. code:: json

   { "BlackBox" :
       { "name"      : "Clash.Sized.Internal.Signed.times#"
       , "template"  : "~IF~AND[~SIZE[~TYP[0]],~SIZE[~TYP[1]]]~THEN~ARG[0] * ~ARG[1]~ELSEsigned'(~SIZE[~TYPO]-1 downto 0 => '0')~FI"
       }
   }

which tells the Clash compiler that whenever it sees an application of
``Clash.Sized.Internal.Signed.times#`` it should use the string-template
that’s given by the ``template`` entry, as opposed to looking up the
definition of ``Clash.Sized.Internal.Signed.times#`` and translating
that.

Clash uses the GHC API as a frontend, and also uses regular GHC+Cabal to
install libraries. As such, Clash critically depends on GHC preserving
applications of ``Clash.Sized.Internal.Signed.times#``, and not inlining
such functions.

One might assume that the ``NOINLINE`` pragma is sufficient, but as
https://gitlab.haskell.org/ghc/ghc/-/issues/19553 shows, it is not.
That’s because worker/wrapper transformations translate definitions such
as the above ``times#`` to:

.. code:: haskell

   times# :: Signed m -> Signed n -> Signed (m + n)
   times# = times#1 |> coercion

   times#1 :: Signed m -> Signed n -> Integer
   times#1 (S a) (S b) = a * b

where the ``NOINLINE`` pragma on ``times#`` is dropped. GHC actually
does this for good reasons:
https://gitlab.haskell.org/ghc/ghc/-/commit/6d49d5be904c0c01788fa7aae1b112d5b4dfaf1c

All of this is framed in the Clash use case, but I can image that other
GHC (API) users needs/wants to recognize applications of certain
functions, and thus doesn’t want those functions inlined (nor
transformed by w/w).

Proposed Change Specification
-----------------------------

-  Add a new ``OPAQUE`` pragma
-  Binders with an ``OPAQUE`` pragma are not inlined, just like
   ``NOINLINE``.
-  Unlike ``NOINLINE``, binders with an ``OPAQUE`` pragma are not w/w
   transformed, not even the cast w/w that happens in the early stage
   simplifier (which even happens for ``-O0`` runs).
-  Unlike ``NOINLINE``, ``OPAQUE`` pragmas can *not* have a phase
   activation, i.e. there is no ``OPAQUE[1]``.
-  Type-class specialisation is a no-op for binders annotated with an
   ``OPAQUE`` pragma, as is SpecConstr.
-  Let any caller of an ``OPAQUE`` annotated function know that while
   the annotated function might be head strict, it will not unbox its
   arguments.
-  ``OPAQUE`` is mutually exclusive with any of: ``INLINE``,
   ``NOINLINE``, ``SPECIALISE``, ``CONLIKE``
-  For the reason above, and unlike ``NOINLINE``, ``OPAQUE`` does *not*
   form a two word pragma together with ``SPECIALIZE`` *nor* does it
   form a two word pragma with ``CONLIKE``.

Examples
--------

To indicate that ``times#`` really should not be inlined:

.. code:: haskell

   module Clash.Sized.Internal.Signed where

   newtype Signed (n :: Nat) = S { unsafeToInteger :: Integer}

   {-# OPAQUE times# #-}
   times# :: Signed m -> Signed n -> Signed (m + n)
   times# (S a) (S b) = S (a * b)

Effect and Interactions
-----------------------

We can now have binders that will: \* Not be inlined \* Not be
type-class specialised \* Not be specialised by SpecConstr \* Not be w/w
transformed, and thus not have a w/w wrapper that can be inlined

In addition, callers of that binding will: \* No longer expect a wrapper
to inline and unbox.

Costs and Drawbacks
-------------------

Implementation should be straightforward: 

- Add an ``Opaque`` constructor to ``GHC.Types.Basic.InlineSpec`` 
- Extend the parser and lexer to process the ``OPAQUE`` pragma 
- Treat ``Opaque`` like ``NoInline`` throughout the rest of the GHC 
  compiler, with the exception of W/W and specialisation related code. 
- Add guards to W/W related code to not transform ``Opaque`` annotated 
  binders 
- Add guards to specialisation related code to not specialise ``Opaque`` 
  annotated binders.

Drawbacks: \* W/W transformation is quite important for performance, and
disabling it seems like something you need extremely rarely. This pragma
will only make sense for those with “deep” knowledge of GHC.

Alternatives
------------

There’s already an ``-fno-worker-wrapper`` flag to disable “regular”
W/W, but that’s quite a blunt instrument as the smallest granularity you
have with applying it is complete modules.

In addition, as highlighted in
https://gitlab.haskell.org/ghc/ghc/-/issues/19553
``-fno-worker-wrapper`` does not stop cast W/W as that happens in the
early stage simplifier, i.e. cast W/W is also performed during ``-O0``
runs. One suggestion was to have ``-fno-worker-wrapper`` also stop cast
W/W, but that would affect ``-O0`` runs compared to the status quo.
Another options was to add a ``-fno-pre-worker-wrapper`` to stop W/W
transformations in the early stage simplifier: cast W/W would then only
be elided during ``-O0 -fno-pre-worker-wrapper`` runs. However, such a
``-fno-pre-worker-wrapper`` flag suffers from the same bluntness as the
``-fno-worker-wrapper`` flag.

Unresolved Questions
--------------------
- Should there be the two-word pragma combinations ``OPAQUE SPECIALIZE`` 
  and ``OPAQUE CONLIKE``?

No, they do not make sense, given that:

   The ultimate goal is that every call of ``f`` generates, well, a call
   of ``f``, not of some name-mangled variant.

Implementation Plan
-------------------

I or one of my colleagues at QBayLogic will implement this change and
submit an MR to the GHC gitlab.

Endorsements
------------
