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

This proposal adds a new constructor of ``RuntimeRep`` (of levity-polymorphism fame), ``ConstraintRep``, and defines
``type Constraint = TYPE 'ConstraintRep``. Doing so, ``Constraint`` and ``Type`` are fully different, all the way to Core.

Motivation
------------
Here are a few oddities caused by the current arrangement:

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

Proposed Change Specification
-----------------------------

Add a new constructor to ``GHC.Types.RuntimeRep``, named ``ConstraintRep`` and define ``type Constraint = TYPE 'ConstraintRep``.
That's it!

Effect and Interactions
-----------------------
The reason that ``Constraint`` and ``Type`` have been synonymous is that we need to be able to have
``Constraint``-kinded things to the left (and, more rarely, to the right) of arrows. But in our brave
new levity-polymorphic world, the types on either side of an arrow can have kind ``TYPE blah`` for any ``blah``.
Thus, the new ``Constraint`` fits in quite nicely.

Users who don't poke around the internals of ``RuntimeRep`` should not notice this change at all. GHC will be
taught to print ``Constraint`` whenever it is tempted to write ``TYPE 'ConstraintRep`` to the console.

One weird interaction is that we currently encode one-element classes as newtypes. Here is an example::

    class C a where
      def :: a

This yields a Core type defined like ``newtype C a = MkC { def :: a }``. The only problem is that ``C a :: Constraint``.
Thus the newtype axiom that relates ``C a`` to ``a`` is *heterogeneous*. Clever machinations using the coercion
forms as described `here <https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf>`_ could then prove
that ``ConstraintRep ~N LiftedRep``, which is a nominal equality between two distinct data constructors. Nightmares!
So this change will have to weaken the ``KindCo`` coercion ("Co_KindCo" in the linked specification, page 14) to
require a *nominal* input coercion instead of any old input coercion. This change weakens the coercion language
a tad, but I don't think anyone will notice. In order to see the lost expressiveness, you would need to have
a heterogeneous representational coercion. The user-accessible ``Coercible`` class is *homogeneous*, so creating
one seems impossible in user code. (GHC certainly could internally. But it doesn't.) We don't have to worry
about type safety with this change, because we are making equality weaker, which is always safe.

Note that this is orthogonal to proposal #29 and can be done with or without that change.

Costs and Drawbacks
-------------------
This is a simplification to the current implementation, which must be quite careful to choose between
``tcEqType :: Type -> Type -> Bool`` and ``eqType :: Type -> Type -> Bool``. Under the new scheme,
these functions become the same, allowing us to delete gobs of code. Similarly, we can delete abominations
like ``coreViewOneStarKind``.

The main drawback is that we are abusing ``RuntimeRep``. ``RuntimeRep`` is meant to represent the different
ways a Haskell value can be represented at runtime. However, ``ConstraintRep`` will have the same representation as
``LiftedRep``. This is a use ``RuntimeRep`` was not envisioned for, but it seems harmless. Are we starting
down a slippery slope? I do not believe we are.

Alternatives
------------

@int-index has argued very cogently and patiently for an alternative solution, whereby we allow ``Constraint ~ Type``
in Haskell code, resolving the discrepancy between Haskell and Core in the opposite direction. This idea
was originally proposed by Simon PJ `here <https://ghc.haskell.org/trac/ghc/ticket/11715#comment:9>`_, but he
has since changed his mind on the idea. It's hard to summarize @int-index's arguments here beyond Simon's original
proposal, but they are worthwhile reading if you're keen. The main drawbacks to the
alternative proposal might be written by Edward Kmett `here <https://ghc.haskell.org/trac/ghc/ticket/11715#comment:31>`_.
I confess I have not liked this idea much, but it's more from a language-design standpoint than from a type-safety
standpoint (the alternative proposal appears type-safe to me).

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
I volunteer to implement. In time for GHC 8.2 even!
