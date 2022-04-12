Simplify subsumption
====================

.. author:: Simon PJ
.. date-accepted:: 2020-01-24
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/17775
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/287>`_.
.. contents::


This proposal argues that GHC’s type system is a bit too liberal; in
particular, the subsumption judgement is too liberal. It adds a little
bit of convenience for the programmer, but at quite a high cost in terms
of changing semantics and type-system complexity. So I propose that we
should simplify the type system, by removing \* Covariance of function
types \* Contravariance of function types \* Deep skolemisation \* Deep
instantiation

All four features are implemented in GHC.

However, *all four change the semantics of Haskell*, by performing
eta-expansion, changing bottom into non-bottom. That’s a pretty serious
change, which needs pretty serious motivation. But actually all we get
in exchange is a minor improvement in programming convenience. So I
argue that:

-  **The benefit, in terms of programming convenience, is small**. In
   particular, while using these features accepts some more programs,
   any such program is easily accepted without these features, after a
   small edit.

-  **The cost is large**. There is extra complexity in the compiler.
   There is extra complexity in the formal description of the type
   system, and in any paper we write about it. But, worst of all, the
   features change the semantics of the program.

-  A recent new cost is that the `Quick Look Impredicativity
   proposal <https://github.com/ghc-proposals/ghc-proposals/pull/274>`__
   gains extra power by not having contravariance. More details in the
   `Quick Look
   paper <https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/>`__

Here is a thought experiment. Suppose GHC lacked all four features, and
someone proposed adding them. That proposal would never leave the
launchpad. A minor change in programming convenience, in exchange for
changing language semantics? No. If that’s true, then the only issue
would be back-compat issues: how many libraries would be affected, and
how painful they would be to fix. We have collected data on this,
presented in

Motivation
----------

The baseline for this proposal is `Practical Type Inference for
Arbitrary Rank
Types <https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/>`__,
and specifically Section 4.6 which concerns the subsumption judgement.
That section discusses \* **Covariance** of the function arrow (Fig 7,
rule FUN) \* **Contravariance** of the function arrow (Fig 7, rule FUN)
\* **Deep skolemisation** (Fig 7, rule DEEP-SKOL) \* **Deep
instantiation**. Section (4.7.3) says that we don’t need deep
instantiation, but GHC actually does that too, for reasons sketched
below.

The proposal argues to remove all four. The following subsections
descuss each in turn.

Deep skolemisation
~~~~~~~~~~~~~~~~~~

Consider these types (paper, 4.6.1):

::

   f :: ∀ab.a → b → b
   g :: (∀p.p → (∀q.q → q)) → Int

Is ``(g f)`` well typed? Notice that ``g`` requires an argument with a
forall to the right of an arrow. With deep skolemisation, the answer is
“yes”. But remember that GHC elaborates to System F. Here is its
elaboration of ``(g f)``:

::

   g (/\p. \(x:p). /\q.\(y:q). f @p @q x y)

The lambdas do the impedence matching to turn ``f`` into an argument of
exactly the type that ``g`` expects.

BUT suppose ``f`` and ``g`` are defined like this:

::

   f = bottom
   g f = f `seq` 0

You would expect ``g f`` to diverge, since it seq’s on bottom. But it
won’t! it’ll return 0. Yikes.

Without deep skolemisation, ``(g f)`` is rejected. But the programmer
can easily repair it by manual eta-expansion, to

::

   g (\x y. f x y)

and now, of course, it is not surprising that the expression evaluates
to 0.

Deep instantiation
~~~~~~~~~~~~~~~~~~

Suppose ``f :: Int -> forall a. a -> a``. Again, notice the forall to
the right of the arrow. Now consider this definition, which lacks a type
signature:

::

   g x = f

What type would you expect to infer for ``g``? The obvious answer (and
the one we’d get without deep instantiation) is

::

   g :: forall b. b -> Int -> forall a. a -> a

But GHC actually deeply instantiates ``f`` (for no very well-explained
reason), so we get

::

   g :: forall b a. b -> Int -> a -> a

with the buried foralls pulled to the top. Perhaps that type is a tiny
bit more explicable to the programmer. But again, to produce that type,
GHC must elaborate ``g`` to

::

   g = /\ b a. \(x:b). \(i:Int). f i @a

GHC has eta-expaned ``f``, which changes the semantics. Yikes.

Contravariance
~~~~~~~~~~~~~~

Suppose you have

::

   g :: ((forall a. a -> a) -> R) -> S
   f :: (Int -> Int) -> R

Now, is ``(g f)`` well typed? That depends on whether

::

   (Int -> Int) -> R   <=    (forall a. a -> a) -> R

where ``<=`` is pronounced “is more polymorphic than” see the paper
sections 4.4. and 4.6.

Well, according to rule FUN of Figure 7, using contravariance of
``(->)``, that is true if

::

   forall a. a -> a    <=      Int -> Int

and that is certainly true. But again, to witness that proof GHC needs
to eta-expand during elaboration. We get this elaboration of ``(g f)``:

::

   g (\(h : forall a. a->a).  f (h @Int))

Again we have changed the semantics. Yikes.

Again, lacking covariance the program would be rejected, but is easily
fixed by manual eta-expansion, thus ``g (\h -> f h)``

Covariance
~~~~~~~~~~

Fig 7 in the paper also supports covariance of the function arrow, but
exactly the same eta-expansion issues arise.

Proposed Change Specification
-----------------------------

There are no syntactic changes.

The changes to the type system is to simplify the subsumption judgement
by removing

-  Covariance of function types
-  Contravariance of function types
-  Deep skolemisation
-  Deep instantiation

Thinking about a transition, it is very difficult to accept all current
programs, while providing a warning for programs that will need to be
changed when the propsal is adopted. Doing so would amount to compiling
every program twice, which does not seem acceptable.

It would be possible to offer a flag that restored the old behaviour,
but that still means changing the .cabal file, or adding a LANGUAGE
pragma. It seems more straightforward simply to change the source code
to work with the new restrictions. These changes turn out to be
extremely minor, and fully backward compatible.

Examples
--------

See Motivation above.

Effect and Interactions
-----------------------

-  See Section 7 of the `Quick Look
   paper <https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/>`__
   for a detailed analysis of the practical impact of these changes.
-  Everything (specification, implementation) becomes a bit simpler
-  Quick Look Impredicativity gains more power

Key conclusions of the practical impact (details in the paper) are: \*
Where programs require changes under this proposal, those changes are
simple, local, and arguably desirable anyway. \* The changes are
backward-compatible: if you change a package to accommodate this
proposal, it’ll still compile with earlier GHC’s too.

Costs and Drawbacks
-------------------

The main user-facing cost is that some existing programs will require
some manual eta-expansion.

There are some implementation consequences:

-  ``TcUnify.matchExpectedFunTys`` would need to be extended to deal
   with the possiblity of a ``forall``. Very straightforward.
-  The ambiguity check would need a bit more code than at present.
   Currently, we just check whether ``ty <= ty`` using the existing
   subsumption check: if this check fails, the type is ambiguous. With
   but with a simpler subsumption check ``Int -> forall a. String``
   would be a sub-type of itself, even though it’s plainly ambiguous. So
   we’d have to write a proper ambiguity checker. Not hard!

Examples of back-compat issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We found an example of a back-compat problem in ``cabal-doctest``. In
``Cabal:Distribution/Compat/Prelude`` we have:

::

   type CabalIO a = HasCallStack => IO a

(actually the definition re-uses ``IO`` as the name, but that’s just
confusing, so I’ve renamed it ``CabalIO`` here.) Then in
``Cabal:Ditribution.Simple.LocalBuildInfo`` we have

::

   withLibLBI :: PackageDescription -> LocalBuildInfo
              -> (Library -> ComponentLocalBuildInfo -> CabalIO ()) -> CabalIO ()

Finally, in ``cabal-doctest``, a function has a local definition, with
no type signature, looking like

::

      let getBuildDoctests withCompLBI mbCompName compExposedModules
                           compMainIs compBuildInfo = ...
      in
      ...(getBuildDoctests withLibLBI ...)...

Now, lacking a type signature on ``getBuildDoctests``, GHC infers the
type of the function to have plain arrows in its type, something like

::

      getBuildDoctests :: (PackageDescription -> LocalBuildInfo -> blah -> IO ())
                       -> ...blah...

but in the call the actual argument ``withLibLBI`` has type

::

   withLibLBI :: PackageDescription -> LocalBuildInfo -> blah -> HasCallStack => IO ()

And the function and its argumetnt do not agree about the placement of
the ``HasCallStack`` constraint. With deep skolemisation, GHC would
eta-expand the call to

::

      getBuildDoctests (\ a b c. withLibLBI a b c)  ...

but, as discussed, that is unsound in general.

Moreover, there is a *nested* use of ``CabalIO`` in ``withLibLBI``\ ’s
third argument, so GHC has to use contravariance and more eta expansion
to make that line up.

The solution is simple, and improves the code: just give
``getBuildDoctests`` a type signature!

Alternatives
------------

1. Status quo. But the the status quo is extremely unsatisfactory.
2. Change GHC’s internmediate language to base it on `System
   F-eta <https://www.sciencedirect.com/science/article/pii/0890540188900090>`__
   (Mitchell, 1988). In this language, everything is done modulo eta
   expansion/contraction. Apart from the huge engineering consequences,
   it’s not clear that F-eta as an intermediate language is compatible
   with Haskell, which distinguishes bottom from
   (:raw-latex:`\x`.bottom).

Unresolved Questions
--------------------

Implementation Plan
-------------------

Implementation is relatively easy. I can do it, or Richard, or
Alejandro.
