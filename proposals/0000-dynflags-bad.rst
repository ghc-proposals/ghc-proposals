Don't speak of ``DynFlags``
===========================

.. author:: John Ericson
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

``DynFlags`` hampers the modularity of GHC.
Instead of referring to it directly, refer to a type parameter constrained to have just the information necessary.
The benefits of this are somewhat uncertain, so merge the refactor on an experimental basis, and revert it if they aren't realized.

Motivation
----------

``DynFlags`` has no meaning.
It is just a roll up of every configuration option anything needs ever.
This is bad for modularity: it leaks information to components of the compiler that they shouldn't care about, and prevents adding additional components with more configuration options.

The easiest way to replace it today is to use type variables and ``Has*`` classes to project smaller records.
[Perhaps in ea future with row types we would do something different, but we can refactor to do that then.]
``DynFlags`` would still be substituted for those variables to satisfy those constraints for GHC the executable, but other consumers of the library could instantiate with something different instead.

With ``DynFlags`` not mentioned directly in most places, we should have opened up more opportunities to modularize the compiler further.
For example, we could break module ``hs-boot`` cycles, or even split the GHC library into multiple libraries.
Unfortunately, it's hard to predict exactly what opportunities will arise *a priori*.
But if we want modularity, we should be confident that referring to ``DynFlags`` indirectly is a necessary if not sufficient condition for these loftier goals.

Many attempts to modularize GHC have floundered in the past.
I think we have to be willing to stomach the risk of experiments to get us over the hump.
I wish this proposal was more certain, the best I can do is say we try this on preliminary basis, and if the benefits are not good enough we revert it.

Proposed Change Specification
-----------------------------

For the refactor itself.

1. Create smaller configuration records and ``Has*`` classes to project them.
   The exact break down can only be determined doing the work.
   TODO finish draft refactor and provide some examples.

2. Replace ``DynFlags`` with ``(HasFoo cfg, HasBar cfg) => cfg`` in functions.

3. ``Outputable`` is an extremely widely used class with a method which mentions ``DynFlags``.
   There is no one-size-fits-all constraint to replace it with, so add an associated ``Type -> Constraint`` instead::

     -- | Class designating that some type has an 'SDoc' representation
     class Outputable a where
        ppr :: a -> SDoc
        pprPrec :: Rational -> a -> SDoc
        type OutputableNeedsOfConfig a :: * -> Constraint
        -- Doing this breaks hs-boot files, which cannot have type family
        -- instances. TODO link issue
        -- type OutputableNeedsOfConfig a = NoConstraint

        ppr :: OutputableNeedsOfConfig a r => a -> SDoc' r
        -- ...

   TODO bikeshed ``OutputableNeedsOfConfig`` name.

   Constraints can be combined with a ``PairConstraint`` ::

     class (f a, g a) => PairConstraint f g a
     instance (f a, g a) => PairConstraint f g a

4. Try to replace ``unsafeGlobalDynFlags`` with a smaller configuration record, or ideally none at all.
   TODO exact plan, but need to finish ``Outputable`` refactor draft to know more.

If the larger modularity goals are not borne out in **x** months' time, the PR should be reverted.
TODO decide experiment length.

Examples
--------

See the WIP MR.

Effect and Interactions
-----------------------

See the WIP MR.

Costs and Drawbacks
-------------------

- ``PairConstraint`` is annoying,

- Lots of dictionary inlining is needed to maintain the same performance.

- Lots of types get bigger.
  Though note they don't get "fancier", e.g. pseudo-dependent GADT stuff.
  I hope that means they aren't harder to maintain, just merely take up more screen space.

I view these as an unavoidable cost of modularity, or at least tying the knot with the type classes instead of the module system.

Alternatives
------------

- Some other "poor man's row types" schemes.
  note that passing in projection functions does work for the freestanding functions, but not ``Outputable`` when multiple when multiple projections are needed.

- Create a big record for everything needed for each ``Outputable`` instance.
  It would perhaps break some cycles, and wouldn't incur the drawbacks of this.
  But, the larger modularity issues mentioned in the motivation remain unaddressed.

Unresolved Questions
--------------------

Exactly how big the types will get, and exactly what the benefits are.
The first step is to finish the refactor, but the only certain proof is in running the experiment.

Implementation Plan
-------------------

I've started in `GHC !1033`_, but I'd love a buddy to take turns pushing on this.
It's a massive refactor and accumulates conflicts fairly quickly, and so benefits from multiple people taking turns working on it.
I'll to do the revert if the experiment fails, and do not request assistance for that.

.. _`GHC !1033`: https://gitlab.haskell.org/ghc/ghc/merge_requests/1033
