.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Separate `Constraint` from `Type`
=================================

`GitHub pull request for discussion <https://github.com/ghc-proposals/ghc-proposals/pull/32>`_

Since at least GHC 7.4, there has been an uneasy relationship between ``Constraint`` and ``Type`` (formerly known as ``*``). These
kinds were considered distinct in Haskell but indistinguishable in Core. This strange arrangement causes oddities in the
type system, as explained in `#11715 <https://ghc.haskell.org/trac/ghc/ticket/11715>`_.

This proposal adds a new plain-old-vanilla datatype ::

    data Visibility = Visible | Invisible
    
which is now the type of a parameter to ``TYPEV`` (renamed from ``TYPE`` of
levity-polymorphism fame). We now have ``type Constraint = TYPEV Invisible
LiftedRep``, making ``Constraint`` distinct from ``Type`` both in Haskell and
in Core.

Motivation
------------
Here are a few oddities caused by the arrangement in GHC 8.0:

1. Type families::

       type family F a where
         F Constraint = Int
         F Type       = Bool

   Such a definition is accepted. Yet it allows a proof that ``Int ~ Bool`` in Core. It's unclear to me whether this can be used to implement ``unsafeCoerce``, but it should scare us all.

2. Printing::

      main = do
        print $ typeRep (Proxy :: Proxy Eq)
        print $ typeOf (Proxy :: Proxy Eq)

   This prints ::

      Eq
      Proxy (* -> *) Eq

   But of course ``Eq`` doesn't have kind ``* -> *``. It has kind ``* -> Constraint``! Except that Core can't tell the difference.


3. Order sensitivity::

      main = do
        print $ typeOf (Proxy :: Proxy (Eq Int))
        print $ typeOf (Proxy :: Proxy Eq)

   prints ::

      Proxy Constraint (Eq Int)
      Proxy (Constraint -> Constraint) Eq

   but if you print them in the opposite order, you get ::

      Proxy (* -> *) Eq
      Proxy * (Eq Int)

   Ew.

4. The new ``Typeable`` plan (inspired by my `recent paper <http://cs.brynmawr.edu/~rae/papers/2016/dynamic/dynamic.pdf>`_
   and fully described on the `wiki page <https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari>`_)
   lays bare any and all shortcuts we have in the type system. It's unclear how to implement the plan without
   sorting this out.

5. As discussed on the `pull request <https://github.com/ghc-proposals/ghc-proposals/pull/32#issuecomment-271881898>`_, this
   paves the way to having proper type inference to distinguish constraint tuples from ordinary ones. However, this proposal
   does not specifically propose making this change.

Proposed Change Specification
-----------------------------

In ``GHC.Prim``::

    TYPEV :: Visibility -> RuntimeRep -> Type

In ``GHC.Types``::

    type TYPE = TYPEV Visible  -- convenient synonym
  
    data Visibility = Visible | Invisible
    data RuntimeRep = ...   -- as before
    
    type Constraint = TYPEV Invisible LiftedRep
    type Type       = TYPE LiftedRep

The Haskell ``(->)`` will be kind-checked to have kind ``TYPE r1 -> TYPE r2 -> Type`` (that is, both
argument and result must be visible), even though the Core ``(->)`` has kind ``TYPEV v1 r1 -> TYPEV v2 r2 -> Type``.
    
Effect and Interactions
-----------------------
The reason that ``Constraint`` and ``Type`` have been synonymous is that we need to be able to have
``Constraint``-kinded things to the left (and, more rarely, to the right) of arrows. But in our brave
new levity-polymorphic world, the types on either side of an arrow can have kind ``TYPEV v r`` for any ``v`` and ``r``.
Thus, the new ``Constraint`` fits in quite nicely.

Users who don't poke around the internals of ``RuntimeRep`` should not notice this change at all. GHC will be
taught to print ``Constraint`` whenever it is tempted to write ``TYPEV Invisible LiftedRep`` to the console.

One weird interaction is that we currently encode one-element classes as newtypes. Here is an example::

    class C a where
      def :: a

This yields a Core type defined like ``newtype C a = MkC { def :: a }``. The only problem is that ``C a :: Constraint``.
Thus the newtype axiom that relates ``C a`` to ``a`` is *heterogeneous*. Clever machinations using the coercion
forms as described `here <https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf>`_ could then prove
that ``Visible ~N Invisible``, which is a nominal equality between two distinct data constructors. Nightmares!

An earlier version of this proposal then proposed to change ``KindCo``
("Co_KindCo" in the linked specification, page 14) to avoid the nightmares
(specifically: require its input coercion to be nominal, instead of any role).
This change, however, is in deep irresolvable conflict with the recent change
to make ``(->)`` take its role arguments in Core. See `#11714
<https://ghc.haskell.org/trac/ghc/ticket/11714>`_ and its implementation
`D2038 <https://phabricator.haskell.org/D2038>`_. In particular, `this comment
<https://phabricator.haskell.org/D2038#inline-25457>`_ is where it all comes to a head.
There is no way to weaken ``KindCo`` in the presence of the (necessary) work to generalize the kind of ``(->)``.

Accordingly, this patch will indeed introduce the nightmares. That is, with this change, a motivated attacker may
be able to break the type system. I say "may" because the attack may require writing Core directly; it's unclear
if any Haskell code could tickle the type system infelicity. It will thus be imperative that we remove
newtype-classes (classes that desugar to newtype definitions, instead of data definitions) after this patch
is introduced. Of course, making this change is straightforward within GHC, but it has a nasty consequence: the
widely-used ``reflection`` package will no longer work, as it relies on GHC's treatement of one-element classes.
That package uses ``unsafeCoerce``, so GHC isn't techincally violating its contract with users with this change,
but it's poor form to do it nonetheless. We thus must come up with a new way to do reflection. `This wiki
page <https://ghc.haskell.org/trac/ghc/wiki/MagicalReflectionSupport>`_ suggests a way forward. I could incorporate
that proposal into this one, but it seems that better debate will ensue keeping the proposals separate. We
need to do *something* about ``reflection`` to keep type safety, but precisely *what* we do is orthogonal
to this proposal.

This proposal requires that we further generalize ``(->)`` to be ::

    (->) :: forall (v1 :: Visibility) (r1 :: RuntimeRep) (v2 :: Visibility) (r2 :: RuntimeRep).
            TYPEV v1 r1 -> TYPEV v2 r2 -> Type

This change would be user-facing via the ``Typeable`` mechanism, even though the Haskell ``(->)`` would
not be nearly so flexible. Perhaps an improvement would be to have different names for the Haskell ``(->)``
and the Core ``(->)``.

Note that this is orthogonal to proposal #29 and can be done with or without that change.

Costs and Drawbacks
-------------------
This is a simplification to the current implementation, which must be quite careful to choose between
``tcEqType :: Type -> Type -> Bool`` and ``eqType :: Type -> Type -> Bool``. Under the new scheme,
these functions become the same, allowing us to delete gobs of code. Similarly, we can delete abominations
like ``coreViewOneStarKind``.

The main drawback is that we are adding theoretical complication to an already-subtle aspect of GHC. This
complication faces users, if they choose to play in the levity-polymorphism playground.

Alternatives
------------

@int-index has argued very cogently and patiently for an alternative solution, whereby we allow ``Constraint ~ Type``
in Haskell code, resolving the discrepancy between Haskell and Core in the opposite direction. This idea
was originally proposed by Simon PJ `here <https://ghc.haskell.org/trac/ghc/ticket/11715#comment:9>`_, but he
has since changed his mind on the idea. It's hard to summarize @int-index's arguments here beyond Simon's original
proposal, but they are worthwhile reading if you're keen. The main drawbacks to the
alternative proposal might be written by Edward Kmett `here <https://ghc.haskell.org/trac/ghc/ticket/11715#comment:31>`_.
I confess I have not liked this idea much, but it's more from a language-design standpoint than from a type-safety
standpoint (the alternative proposal appears type-safe to me). (@int-index has since backed off this point of view,
as seen on the pull request)

The main alternative is a previous version of this proposal, where we would add a new constructor of ``RuntimeRep``
called ``ConstraintRep``. We would then distinguish ``Type`` from ``Constraint`` via the choice of ``RuntimeRep``.
However, this runs into a major problem: we have a hard time rejecting ``Eq a -> a -> Bool``. (Note the ``->`` instead
of ``=>``.) Seeing ``Eq a -> a -> Bool``, GHC would happily accept, because any thing of the form ``TYPE r`` to the left
of an arrow should be OK. We can only be sure something is wrong after zonking, and by then, we've lost the original
Haskell AST that the user wrote, so we can't tell whether they wrote ``=>`` or ``->``. Of course, this problem could
be avoided by engineering, but there is another wrinkle. Consider the type ``forall (r :: RuntimeRep) (a :: TYPE r). a -> a``.
Forget for a moment that this is unimplementable. The problem is that the type is sensible only for *most* choices of ``r``,
not *all* of them: choosing ``r`` to be ``ConstraintRep`` makes the type bogus. So something is really quite smelly
with this design.

Another axis for alternatives is in naming. Suggestions from the community have wanted ``Coherency`` where I have
written ``Visibility``, but I prefer the latter. For example, ``(?x :: Int)`` is a ``Constraint`` even though
it is not coherent. Also, the user might have specified ``-XIncoherentInstances``. On the other hand, visibility
is always a correct notion to apply here.

Yet another alternative is proposed in `this comment <https://github.com/ghc-proposals/ghc-proposals/pull/32#issuecomment-276102333>`_, where @sweirich proposes to have ``(->)`` and ``(=>)`` in Core. This would mean more arrows floating around
in Core that all get compiled to the same code later, but perhaps wouldn't cause further complication.

Regardless, the current proposal does not really bar the way to resolving the design challenges of the alternative
proposal in the future. Implementing what I've proposed here will be *deleting* code, so there's no sunk cost
to worry about if we decide to change course later.

Unresolved questions
--------------------
Is this idea type safe? I don't know for sure. The challenge has to do with the interaction between roles and
kind coercions, something yet to be studied in the literature. (My thesis cleverly avoids broaching the subject.)
When I hesitated on this point in a recent interaction with Simon, he rightly pointed out that we don't have
a proof for the status quo, so this new proposal doesn't make things any worse. My future hopefully holds
a mechanized proof of this all, but let's not wait for that future to arrive before making progress here.

Implementation Plan
-------------------
I volunteer to implement.
