Custom error messages for unsolved constraints
==============================================

.. author:: Sam Derbyshire
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/735>`_.
.. sectnum::
.. contents::


This proposal introduces a customisation mechanism for error messages that arise
from reporting unsolved Wanted constraints. This is particularly useful to avoid
exposing users of a library to internal implementation details of that library
(such as internal type families) in error messages.

Motivation
----------

Usage of TypeError
~~~~~~~~~~~~~~~~~~

`TypeError <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_errors.html>`__ and
`Unsatisfiable <https://github.com/ghc-proposals/ghc-proposals/pull/433>`__ allow
custom error messages to be reported to users. However, these mechanisms can
only be used for "hard errors": constraints of the form ``TypeError msg`` and
``Unsatisfiable msg`` are **insoluble**.

These mechanisms are useful to rule out definite errors in the code.
For example::

  type Insert :: k -> [k] -> [k]
  type family Insert k ks where
    Insert x '[] = '[x]
    Insert x (k ': ks) = InsertHelper (Compare x k) x k ks

  type InsertHelper :: Ordering -> k -> k -> [ks] -> [ks]
  type family InsertHelper cmp x k ks where
    InsertHelper LT x k ks = x ': k ': ks
    InsertHelper GT x k ks = x ': Insert x ks
    InsertHelper EQ x _ _ =
      TypeError (Text "Insert: duplicate key " :<>: ShowType x)

Users of this API attempting to insert duplicate keys will get civilised
error messages:

.. code-block:: text

  • Insert: duplicate key 3

Crucially, none of the internal implementation details of ``Insert`` leak out to
the user (in particular, no mention of ``InsertHelper``).

Example 1: generic programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When one isn't dealing with a definite error, it can be a lot trickier to present
meaningful error messages to the user. The blog post
`Detecting the undetectable <https://blog.csongor.co.uk/report-stuck-families/>`__
presents an interesting approach. When working with ``GHC.Generics``, users can
be presented with ungodly errors:

.. code-block:: text

    • Couldn't match representation of type:
        M1 D (MetaData "Tuple3" "GHC.Tuple" "ghc-prim" False)
          (C1 (MetaCons "(,,)" PrefixI False)
              (S1 (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                  (Rec0 Int)
                :*:
              S1 (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                  (Rec0 Int)
                :*:
              S1 (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                  (Rec0 Int))
      with that of: Rep Point3D ()

Here, the issue is that ``Point3D`` is missing a ``Generic`` instance, which
causes the ``Rep`` type family to be stuck. Unfortunately, a whole mess of
internal constructors from ``GHC.Generics`` is presented to the user, making
this fact hard to discern.

The solution presented in the blog post is to introduce a type family that can
be used to check whether another type family application is stuck, reporting
an error message if so::

  data family Dummy :: k
  type family Any :: k

  type ErrIfStuck :: comp -> err -> k -> k
  type family ErrIfStuck comp err a where
    ErrIfStuck Dummy _ _ = Any
    ErrIfStuck _     _ t = t

The point is that the behaviour of ``ErrIfStuck (TF args) (TypeError myErrMsg) t``
depends on whether the type family application ``TF args`` reduces:

* If ``TF args`` does not reduce, GHC cannot determine it to be apart from
  ``Dummy``, and the original type ``ErrIfStuck (TF args) (TypeError myErrMsg) t``
  remains as-is. Because this type contains ``TypeError myErrMsg``, the error
  message ``myErrMsg`` is reported to the user.
* Otherwise, GHC reduces ``ErrIfStuck (TF args) (TypeError myErrMsg) t`` to ``t``
  and there is no error message to report.

Example 2: type-level comparison operators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A similar mechanism is used since GHC 9.2 in the implementation of type-level
comparison operators (for kinds ``Nat``, ``Symbol`` and ``Char``)::

  type Assert :: Bool -> Constraint -> Constraint
  type family Assert check errMsg where
    Assert 'True _      = ()
    Assert _     errMsg = errMsg

  type x <= y = Assert (x <=? y) (LeErrMsg x y)
  type LeErrMsg x y =
    TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " <= " ':<>: 'ShowType y)

  type (<=?) :: k -> k -> Bool
  type m <=? n = OrdCond (Compare m n) 'True 'True 'False

  type OrdCond :: Ordering -> k -> k -> k -> k
  type family OrdCond o lt eq gt where
    OrdCond 'LT lt eq gt = lt
    OrdCond 'EQ lt eq gt = eq
    OrdCond 'GT lt eq gt = gt

The purpose of the ``Assert`` type family is again to prevent internal
implementation details to leak out in error messages. That is, if GHC cannot
solve the constraint ``x <= y``, it should not report completely unpenetrable
error messages such as:

.. code-block:: text

  • Couldn't match type ‘OrdCond (CmpNat x y) True True False’
                   with ‘True’

  • Couldn't match type ‘False’
                   with ‘True’

The desired behaviour is instead: if GHC fails to fully solve the constraint
``OrdCond (CmpNat x y) True True False``, it should instead report the error
message:

.. code-block:: text

  • Cannot satisfy x <= y

Looking at the definition of ``Assert``:

* If ``OrdCond (CmpNat x y) 'True 'True 'False`` remains stuck, the same
  mechanism as above will result in the error message being reported.
* If ``OrdCond (CmpNat x y) 'True 'True 'False`` evaluates to ``False``, the
  second type family equation of ``Assert`` will ensure the error gets thrown
  as well.
* If ``OrdCond (CmpNat x y) 'True 'True 'False`` evaluates to ``True``, the
  type error goes away and all is well.

One downside is that the use of this ``Assert`` type family makes it harder for
GHC to make deductions involving constraints of the form ``x <= y``, as reported
in `GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__.
Indeed, GHC is unable to deduce ``b ~ True`` from ``Assert b errMsg``, for
reasons that boil down to the fact that GHC Core does not contain proof terms
for type family injectivity, which in turn means that there is no way to use
a Given constraint of the form ``Assert b errMsg`` and produce a Given
constraint of the form ``b ~ True`` (more details in `GHC ticket #10833 <https://gitlab.haskell.org/ghc/ghc/-/issues/10833>`__).

Example 3: effect dispatch
~~~~~~~~~~~~~~~~~~~~~~~~~~

Another issue is that errors can become obfuscated as they become partially
solved through instance search.

Consider for example a common effect-handling setup, with a type-level list of
effects::

  type Eff :: [ Type ] -> Type -> Type

  data DbEff
  data LoggerEff
  data HttpEff

  class Has eff effs
  instance {-# OVERLAPPING #-} Has eff (eff : effs)
  instance Has eff effs => Has eff (other : effs)

  httpRequest :: Has HttpEff effs => String -> Eff effs ()

Calls to ``httpRequest`` within a monad that does not support the HTTP effect
will result in poor error messages, e.g.::

  bad :: Eff '[ LoggerEff, DbEff ] ()
  bad = httpRequest "ping"

results in the error:

.. code-block:: text

  • No instance for ‘Has HttpEff '[]’
      arising from a use of ‘sendHttpRequest’

The problem is that, instead of presenting the original unsolved constraint:

.. code-block:: text

  No instance for ‘Has HttpEff [LoggerEff, DbEff]’

the constraint solver has used top-level instances to make partial progress
on the constraint. This leaks internal implementation details (and, in
more realistic examples, internal-only types as well).

Using ``TypeError`` to solve this problem requires changing all the instances to
carry the original effect stack for error reporting, thus::

  class Has eff effs where
  instance HasHelper effs eff effs => Has eff effs

  type HasHelper :: [Type] -> Type -> [Type] -> Constraint
  class HasHelper origEffs eff effs where
  instance {-# OVERLAPPING #-} HasHelper origEffs eff (eff : effs) where
  instance HasHelper origEffs eff effs => HasHelper origEffs eff (other : effs) where
  instance TypeError ( ShowType origEffs :<>: Text " does not have effect: " :<>: ShowType eff )
        => HasHelper origEffs eff '[]

This introduces another problem, as this internal helper can leak into error
messages, e.g.::

  bad2 :: Eff effs ()
  bad2 = sendHttpRequest "hello"

results in:

.. code-block:: text

  • No instance for ‘HasHelper effs HttpEff effs’
    arising from a use of ‘sendHttpRequest’

Summary
~~~~~~~

This proposal suggests that GHC provide a magical constraint form which would
allow custom error messages to be reported when a particular constraint is not
fully solved.

In both Example 1 (generic programming) and Example 2 (type-level comparison operators),
this would allow producing civilised error messages that avoid exposing users
to internal implementation details. While both of these examples involve type
families, Example 3 (effect dispatch) shows the same mechanism can also be
useful when only class constraints are involved. We expect that many Haskell
DSLs could benefit from this mechanism in order to improve error messages.

Making this constraint built-in and known to GHC would allow improved reasoning
with these constraints, avoiding the issues of
`GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__.

Proposed Change Specification
-----------------------------

New constraint form
~~~~~~~~~~~~~~~~~~~

A new class, ``ErrIfUnsolved``, is added to and exported from ``GHC.TypeError``,
with the following definition::

  type ErrIfUnsolved :: CONSTRAINT r -> ErrorMessage -> CONSTRAINT r
  class ct => ErrIfUnsolved ct err
  instance ct => ErrIfUnsolved ct err

Custom solving rules
~~~~~~~~~~~~~~~~~~~~

GHC's solver would have special knowledge of this class:

* A Given constraint ``[G] ErrIfUnsolved ct err`` is immediately reduced to
  ``[G] ct``, discarding the error message entirely.
* A Wanted constraint ``[W] ErrIfUnsolved ct err`` is reduced to ``[W] ct``,
  but internally GHC also keeps track of the error message.
  Every time this constraint is simplified, we propagate the error message,
  in the same way that GHC today propagates ``CtOrigin``\ s during constraint solving.

  - Error messages override previous error messages. For example, in
    ``ErrIfUnsolved (ErrIfUnsolved ct err1) err2``, the error message ``err2``
    wins and will be used for error reporting.

    This allows library authors to customise error messages on top of another
    library which itself customises error messages.

  - When solving a (work item) Wanted constraint from an inert Wanted, if the
    work item has a custom error message and the inert does not, transfer the
    error message to the inert.

    This ensures we don't drop custom error messages just because we have
    already encountered the same constraint without a custom error message.

  - Constraints that get fully solved do not contribute any error messages,
    even in examples like

    ``[W] ErrIfUnsolved (alpha ~ beta) msg, [W] Maybe alpha ~ beta``

    in which solving ``alpha ~ beta`` unifies ``alpha := beta``, making
    ``Maybe alpha ~ beta`` insoluble.

    This choice is motivated by the implementation, as it seems undesirable to
    update the logic of ``Note [Wanteds rewrite Wanteds: rewriter-sets]`` in
    order to account for custom error messages.

Custom error reporting
~~~~~~~~~~~~~~~~~~~~~~

When reporting unsolved Wanteds, any unsolved Wanted with a custom error message
would be reported by using the custom error message instead of the error message
GHC would usually generate.

Moreover, GHC should prioritise reporting these errors, as the custom error
messages are expected to contain highly-relevant hand-crafted information. In
particular, the presence of unsolved equality constraints should not suppress
the reporting of these custom error messages.

Effect and Interactions
-----------------------

Changes in base
~~~~~~~~~~~~~~~

We propose to add ``ErrIfUnsolved`` to the export list of ``GHC.TypeError``,
and to replace uses of ``Assert`` in ``Data.Type.Ord`` by ``ErrIfUnsolved``.
For example, the definition of ``<=`` would become::

  type x <= y = ErrIfUnsolved ((x <=? y) ~ True) (LeErrMsg x y)

The special logic for the handling of Givens would solve
`GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__
while retaining the improved error message afforded by the use of ``Assert``.

Naturally, these changes will require a CLC proposal.

It would also be possible to only make ``ErrIfUnsolved`` available in the
``ghc-experimental`` package, without touching ``base``, but this would preclude
solving `GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__,
which is a central goal of this proposal.

Changes in other libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~

Libraries such as Csongor Kiss's `generic-lens <https://hackage.haskell.org/package/generic-lens>`__
would be able to use ``ErrIfUnsolved`` instead of a custom type family. For
example, the `current code <https://github.com/kcsongor/generic-lens/blob/93e921156783173762d91c1e398c089eecf7849f/generic-lens-core/src/Data/Generics/Product/Internal/Typed.hs#L32>`__::

  type Context a s =
    ( Generic s
    , Defined (Rep s) (NoGeneric s msg) (() :: Constraint)
    , ..
    )

in which ``Defined`` is implemented as follows::

  data Void1 a
  type Defined :: (Type -> Type) -> Constraint -> k -> k
  type family Defined break err a where
    Defined Void1 _ _ = Any
    Defined _     _ k = k

could be updated to the following::

  type Context a s =
    ( ErrIfUnsolved (Generic s) (NoGeneric s msg)
    , ..
    )

The fact that GHC would prioritise the reporting of these custom error messages
would ensure a helpful error would be displayed, instead of an error that
contains gobs of internal ``GHC.Generics`` constructors.

Interactions within GHC
~~~~~~~~~~~~~~~~~~~~~~~

The custom logic within GHC for this new constraint form is very self-contained,
so we do not foresee any significant interactions with other aspects of the
compiler.

Costs and Drawbacks
-------------------

Aside from the rather minor implementation cost, the only real concern is that
this mechanism might cause GHC to do more work on the happy-path when it ends
up not reporting any errors. That is because GHC would have to support a
mechanism for carrying around these error messages (including applying
substitutions to them and zonking them, as error messages may contain types)
in order for them to be reported at the end. The implementation would have to be
careful to avoid regressing compiler performance in situations where no error
messages are reported in the end.


Alternatives
------------

The alternative is to continue with user-defined classes and type families,
without special support in GHC. This has two downsides:

* Current approaches suffer from the deficiency detailed in
  `GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__.
  It is possible that other approaches might solve this problem, at the cost
  of yet more complexity. One such approach is suggested by L.S. Leary in
  `this comment <https://gitlab.haskell.org/ghc/ghc/-/issues/26190#note_646685>`__,
  using ``OverlappingInstances``.
* Lack of a centralised mechanism for customising the error reporting of
  unsolved Wanted constraints leads to library authors re-inventing the wheel,
  with possible unforeseen interactions.

There are also some similarities between this proposal and `GHC proposal #454 <https://github.com/ghc-proposals/ghc-proposals/pull/454>`__,
which proposes to introduce a ``Warning`` constraint. This too would provide a
mechanism for reporting custom error messages, but it fundamentally differs from
the current proposal in a few key aspects:

* warning messages are not attached to another constraint, and are instead
  free-standing,
* warning messages are not errors,
* warning messages do not suppress any other unsolved Wanted error messages.

Because of this, it does not seem possible to leverage the ``Warning`` constraint
form to achieve the aims of the current proposal.

Unresolved Questions
--------------------

Naming of the new constraint form
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The provisional name for the new class is ``ErrIfUnsolved``, but other names
might make sense too, such as ``Assert``, ``WithError``, ``WithMessage``, etc.

Reporting of unsolved Wanteds
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We currently propose that the custom error message completely override any
other error reporting for the constraints under consideration, as one of the
principal motivations is to avoid displaying gory internal details to the user
in error messages.

It would also be possible to report the custom error message alongside the
original unsolved Wanted error message generated by GHC. Ideally, we would have
interactive user messages, and the original error message would be hidden by
default but displayed when a toggle is actuated.

Implementation plan
-------------------

I volunteer to implement this proposal.
