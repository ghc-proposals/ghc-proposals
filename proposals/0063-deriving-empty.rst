Overhaul deriving instances for empty data types
================================================

.. author:: Ryan Scott
.. date-accepted:: 2017-08-28
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/7401, https://gitlab.haskell.org/ghc/ghc/issues/10577, https://gitlab.haskell.org/ghc/ghc/issues/13117
.. implemented:: 8.4
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/63>`_.
.. contents::

Currently, one can derive class instances for empty data types, i.e., data types that have no constructors, such as ``data Empty``. However, there are a number of warts in GHC's implementation of this feature that make it cumbersome to use.

There have been various discussions about changing GHC's behavior with respect to deriving instances for empty data types. These include several tickets (see `#7401 <https://gitlab.haskell.org/ghc/ghc/issues/7401>`_, `#10577 <https://gitlab.haskell.org/ghc/ghc/issues/10577>`_, and `#13117 <https://gitlab.haskell.org/ghc/ghc/issues/13117>`_) and mailing list discussions (see `here <https://mail.haskell.org/pipermail/libraries/2015-July/025959.html>`_ and `here <https://mail.haskell.org/pipermail/libraries/2017-January/027590.html>`_). However, none of these discussions ever reached a consensus for a new design. I'm putting this proposal together in hopes of coming to an agreement on a comprehensive design for this feature and to bring a conclusion to this discussion.


Motivation
------------
Currently, the behavior for deriving class instances for empty data types is unpredictable, and not as useful as it could be. Let's examine each of these three points in closer detail:

1. Unpredictable. If you try deriving certain instances for an empty data type using a ``deriving`` clause, it will simply fail. For instance: ::

       λ> data Empty deriving Eq

       <interactive>:1:21: error:
           • Can't make a derived instance of ‘Eq Empty’:
               ‘Empty’ must have at least one data constructor
               Possible fix: use a standalone deriving declaration instead
           • In the data declaration for ‘Empty’

   And yet, if one uses the ``StandaloneDeriving`` extension to derive ``Eq``, it will work: ::

       λ> :set -XStandaloneDeriving
       λ> data Empty
       λ> deriving instance Eq Empty

   Even more mysteriously, this distinction doesn't apply for all derivable type classes. For instance, one can use a ``deriving`` clause to derive ``Generic`` without issue: ::

       λ> :set -XDeriveGeneric
       λ> import GHC.Generics
       λ> data Empty deriving Generic

   Nor does it apply to all deriving strategies, since one can use ``DeriveAnyClass`` on empty data types as well: ::

       λ> :set -XDeriveAnyClass
       λ> class C a
       λ> data Empty deriving C

   Trying to remember all of these little rules and exceptions makes for an unpleasant GHC experience.

2. Not as useful as it could be. If one examines the code that is actually emitted from derived instances (using the ``-ddump-deriv`` GHC option), one will discover that the derived code is less than ideal. For example, consider the following GHCi session (using GHC 8.0.2): ::

       λ> :set -XStandaloneDeriving -ddump-deriv
       λ> data Empty
       λ> deriving instance Show Empty

       ==================== Derived instances ====================
       Derived instances:
         instance GHC.Show.Show Ghci1.Empty where
           GHC.Show.showsPrec = GHC.Err.error "Void showsPrec"

   This is a particularly bad way to implement ``Show`` for an empty data type. This implementation will _always_ ``error``, regardless of whether its input is a divergent computation or a computation which throws an exception. Moreover, it will ``error`` even if it is partially applied, making it especially cumbersome to use.

Proposed Change Specification
-----------------------------
To clean up this mess, I propose an overhaul of how GHC combines ``deriving``
with empty data types. Concretely, I propose:

1. Allow the use of ``deriving`` clauses for empty data types. For standard type classes mentioned in the Haskell Report (``Eq``, ``Ord``, ``Read``, ``Show``, ``Ix``, ``Bounded``, and ``Enum``), this would require the use of a new language pragma, ``-XEmptyDataDeriving``. For non-standard type classes, enabling ``-XEmptyDataDeriving`` would not be required, since we require enabling separate language extensions to derive the non-standard type classes anyways. (Similarly, standalone deriving declarations would not require ``-XEmptyDataDeriving``, since they separately require ``-XStandaloneDeriving``.)

2. Change the implementations of derived class instances for empty data types. For each stock derivable class, I will describe what currently gets derived for ``data Empty a``, and provide an example of how I want it to behave under this proposal:

* Deriving ``Eq``

  Currently, this gives: ::

      instance Eq (Empty a) where
        _ == _ = error "Void =="

  I propose: ::

      instance Eq (Empty a) where
        _ == _ = True

  Note that I am deliberately making this instance as "defined as possible" (to borrow an Edward Kmett phrase from `here <https://mail.haskell.org/pipermail/libraries/2015-July/025965.html>`_) by making it maximally lazy. For more on this, refer to the Alternatives section.

* Deriving ``Ord``

  Currently, this gives: ::

      instance Ord (Empty a) where
        compare _ _ = error "Void compare"

  I propose: ::

      instance Ord (Empty a) where
        compare _ _ = EQ

  This instance is as "defined as possible" (see the Alternatives section).

* Deriving ``Read``

  Currently, this gives: ::

      instance Read (Empty a) where
        readPrec = parens pfail

  I propose: ::

      instance Read (Empty a) where
        readPrec = pfail

  That is, reading an empty datatype should always just fail, without reading any input. Doing so makes this instance as "defined as possible" (see the Alternatives section), since it avoids forcing portions of the string that it doesn't need to.

* Deriving ``Show``

  Currently, this gives: ::

      instance Show (Empty a) where
        showsPrec = "Void showsPrec"

  I propose: ::

      instance Show (Empty a) where
        showsPrec _ x = case x of {}

  This uses the ``EmptyCase`` extension to inspect the argument ``x``. Essentially, if ``x`` diverges, then so will ``showsPrec``, and if ``x`` throws an exception, then ``showsPrec`` will throw the same exception. That is, it `"exchanges bottoms" <https://mail.haskell.org/pipermail/libraries/2017-January/027597.html>`_.

* Deriving ``Functor``

  Currently, this gives (in GHC HEAD): ::

      instance Functor Empty where
        fmap _ x = case x of {}

  This is one of the few derived instances that gets it right. I do not propose changing this behavior.

* Deriving ``Foldable``

  Currently, this gives (in GHC HEAD): ::

      instance Foldable Empty where
        foldMap _ _ = mempty

  This is one of the few derived instances that gets it right. I do not propose changing this behavior.

  This instance is as "defined as possible" (see the Alternatives section).

* Deriving ``Traversable``

  Currently, this gives (in GHC HEAD): ::

      instance Traversable Empty where
        traverse _ x = pure (case x of {})

  This is one of the few derived instances that gets it right. I do not propose changing this behavior.

  This instance is as "defined as possible" (see the Alternatives section).

* Deriving ``Lift``

  Currently, this gives: ::

      instance Lift (Empty a) where
        lift _ = error "Can't lift value of empty datatype Empty"

  I propose: ::

      instance Lift (Empty a) where
        lift x = pure (case x of {})

  This instance is as "defined as possible" (see the Alternatives section).

* Deriving ``Generic(1)``

  Currently, this gives (in GHC HEAD): ::

      instance Generic (Empty a) where
        from x = M1 (case x of {})
        to (M1 x) = case x of {}

      instance Generic1 Empty where
        from1 x = M1 (case x of {})
        to1 (M1 x) = case x of {}

  These are some of the few derived instances that get it right. I do not propose changing this behavior.

  These instances are as "defined as possible" (see the Alternatives section).

* Deriving ``Data``

  Current, this gives: ::

      instance Data a => Data (Empty a) where
        gfoldl _ _ _ = error "Void gfoldl"
        gunfold k z c = case constrIndex c of {}
        toConstr _ = error "Void toConstr"
        dataTypeOf _ = mkDataType "Empty" []
        dataCast1 f = gcast1 f

  I propose: ::

      instance Data a => Data (Empty a) where
        gfoldl _ x = case x of {}
        gunfold k z c = case constrIndex c of {}
        toConstr x = case x of {}
        dataTypeOf _ = mkDataType "Empty" []
        dataCast1 f = gcast1 f

Effect and Interactions
-----------------------
These changes would provide a consistent, predicatable, and useful design for derived instances for empty data types.

This proposed change wouldn't affect many other language features, as ``deriving`` is a somewhat isolated feature, being something which simply generates other code.


Costs and Drawbacks
-------------------
This would change the semantics of some current derived instances for empty data types, but in a very slight (and benign way). Current code that derives instances for empty data types might no longer crash at runtime (e.g., derived ``Eq`` instances would now return ``True`` instead of calling ``error``) or begin to diverge instead of calling ``error`` (e.g., derived ``Show`` instances). But this would be a very simple change to accommodate.


Alternatives
------------
When deciding how to implement derived code for empty data types, I deliberately adopted the principle of making the instances as "defined as possible". For instance, I chose to derive ``Eq`` for ``data Void`` like so: ::

    instance Eq Void where
      _ == _ = True

And not like this: ::

    instance Eq Void where
      x == !_ = case x of {}

While the latter implementation typechecks, I don't believe it is what we want for a derived instance. Edward Kmett puts his argument forth for the former behavior `here <https://mail.haskell.org/pipermail/libraries/2015-July/025965.html>`_:

    We rather deliberately made them [the ``Eq`` and ``Ord`` instances for ``Void``] as "defined as possible" back in 2012 after a very long discussion in which the pendulum swung the other way using a few examples where folks tied knots with fixed points to get inhabitants of ``Void`` and it was less consistent to rule them out than it was to define equality on ``⊥`` to be ``True``.

    I'd challenge that nothing is gained by making these combinators strict in their arguments.

Indeed, this is what led to Edward adopting the former convention in his ``void`` library. This allows for checking boolean equality on fixed-point terms which would otherwise diverge if scrutinized.

One consequence of the former design is that ``(error "foo" :: Void) == (error "bar" :: Void)`` always returns ``True``. This is by design, because in well behaved pure code, there is no way to observe the difference between any two inhabitants of ``Void``. Therefore, we can shortcut the implementation to return ``True`` (and similarly, return ``EQ`` for ``Ord.compare``).

Some might prefer the latter implementation as a matter of style, but the former implementation provides a reasonable default that should be suitable for 90% of uses (much like every other derived class).

I have adopted the same principle for other derived instances (for ``Ord``, ``Read``, ``Foldable``, ``Traversable``, ``Lift``, ``Generic``, and ``Generic1``).

Unresolved questions
--------------------
None at the moment.


Implementation Plan
-------------------
I volunteer to implement.
