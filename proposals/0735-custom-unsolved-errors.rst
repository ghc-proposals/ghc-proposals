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

Limitations of existing mechanisms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`TypeError <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_errors.html>`__ and
`Unsatisfiable <https://github.com/ghc-proposals/ghc-proposals/pull/433>`__ allow
custom error messages to be reported to users. These mechanisms are useful to
rule out definite errors in the code. For example::

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

However, these mechanisms can only be used for "hard errors": constraints of the
form ``TypeError msg`` and ``Unsatisfiable msg`` are **insoluble**.

In the following sections, we will see how library authors have tried to use
``TypeError`` in situations where there isn't a definite error, and what
limitations these approaches run into.

Example 1: generic programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When working with ``GHC.Generics``, users can be presented with ungodly errors:

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

The blog post `Detecting the undetectable <https://blog.csongor.co.uk/report-stuck-families/>`__
presents an interesting approach for using ``TypeError`` to improve error
message. The solution presented in the blog post is to introduce a type family
that can be used to check whether another type family application is stuck,
reporting an error message if so (naming updated from the blog post, which uses
``Break`` and ``Assert``)::

  data family Dummy :: k
  type family Any :: k

  type Defined :: comp -> err -> k -> k
  type family Defined comp err a where
    Defined Dummy _ _ = Any
    Defined _     _ t = t

The point is that the behaviour of ``Defined (TF args) (TypeError myErrMsg) t``
depends on whether the type family application ``TF args`` reduces:

* If ``TF args`` does not reduce, GHC cannot determine it to be apart from
  ``Dummy``, and the original type ``Defined (TF args) (TypeError myErrMsg) t``
  remains as-is. Because this type contains ``TypeError myErrMsg``, the error
  message ``myErrMsg`` is reported to the user.
* Otherwise, GHC reduces ``Defined (TF args) (TypeError myErrMsg) t`` to ``t``
  and there is no error message to report.

Adding a ``Defined (Rep ty) msg (() :: Constraint)`` context to appropriate
typeclass instances, as done in `generic-lens <https://hackage-content.haskell.org/package/generic-lens-core-2.3.0.0/docs/src/Data.Generics.Product.Internal.Fields.html#Context%27>`__,
allows the library to emit a much saner error message, of the form:

.. code-block:: text

  • No Generic instance for Point3D

While this mechanism works for its intended purpose, it is rather non-obvious,
requiring the delicately balanced ``Defined`` type family. The next section
also illustrates some limitations of the approach (although these are perhaps
not so relevant for ``generic-lens``).

Example 2: type-level comparison operators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A similar mechanism to the ``Defined`` type family is used since GHC 9.2 in the
implementation of type-level comparison operators (for kinds ``Nat``, ``Symbol``
and ``Char``)::

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
implementation details from leaking out in error messages. That is, if GHC
cannot solve the constraint ``x <= y``, it should not report completely
impenetrable error messages such as:

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

The previous examples concerned stuck type families, but a similar problem
arises with typeclasses: internal implementation details of how typeclass
instances are set up in a library can leak to the user, with GHC reporting
partially-solved constraints or confusing overlapping instance errors that make
little sense out of context.

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

  • No instance for ‘Has HttpEff ‘[]’
      arising from a use of ‘httpRequest’

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

This will allow GHC to report the following error message instead:

.. code-block:: text

  [LoggerEff, DbEff] does not have effect: HttpEff
      arising from a use of ‘httpRequest’

However, this approach would introduce another problem, as this internal helper
could then leak into error messages in other circumstances, e.g.::

  bad2 :: Eff effs ()
  bad2 = httpRequest "hello"

results in:

.. code-block:: text

  • No instance for ‘HasHelper effs HttpEff effs’
    arising from a use of ‘httpRequest’

The introduction of ``HasHelper`` has thus not solved the problem, but only
shifted it somewhat.

Another pain point for users, as reported in `effectful #300 <https://github.com/haskell-effectful/effectful/issues/300>`__,
involves overlapping instances and ambiguity during instance search. For example::

  data Reader a

  ask :: forall a effs. Has (Reader a) effs => Eff effs a

  bad3 :: Eff '[Reader Int] ()
  bad3 = void ask

Compiling this code produces the error message:

.. code-block:: text

  • Overlapping instances for Has (Reader a0) '[Reader Int]
      arising from a use of ‘ask’

The problem is the ambiguous type variable in the code
``void (ask @a0)``, which is nowhere constrained to be equal to ``Int``. To
accept this program or give a reasonable error message, one would have to use
incoherent instances such as::

  -- Option 1 (to accept the program)
  instance {-# INCOHERENT #-} (a ~ b) => Has (Reader a) (Reader b : effs)

  -- Option 2 (to give a nicer error message)
  instance {-# INCOHERENT #-} (TypeError errMsg) => Has (Reader a) (Reader b : effs)

This is entirely unsatisfactory, however, as it precludes effect stacks with
multiple different `Reader` effects, e.g.::

  shouldWork :: Eff [Reader Int, Reader Char] (Int, Char)
  shouldWork = (,) <$> ask <*> ask

Instead, we suggest that the library author should be able to annotate the
``Has`` constraint with their own error messages::

  -- User-facing constraint form
  type eff :< effs = ErrorIfUnsolved (Has eff effs) (CustomMessage eff effs)

With a suitable ``CustomMessage``, the library can present a more targeted
error message such as:

.. code-block:: text

  • Ambiguous usage of Reader effect:
    The effect stack provides 'Reader Int', but the use-site is unconstrained.
      arising from a use of 'ask'

Enforcing that these custom error messages crafted by the library author are
presented whenever the interior ``Has eff effs`` constraint goes unsolved would
address all the problems presented in this section: error messages would no
longer leak the fact that effect dispatch is internally implemented using
instance search, avoiding presenting partially solved goals or confusing
"overlapping instances" errors to the library user.

Summary
~~~~~~~

This proposal suggests that GHC provide a magical constraint form which would
allow custom error messages to be reported when a particular constraint is not
fully solved.

In both Example 1 (generic programming) and Example 2 (type-level comparison operators),
this would:

  - allow producing civilised error messages
  - avoid exposing users to internal implementation details
  - improve reasoning of constraints annotated with custom error messages,
    avoiding the issues of `GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__.

While both of these examples involve type families, Example 3 (effect dispatch)
shows the same mechanism can also be useful when only class constraints are
involved.

We expect that many Haskell DSLs could benefit from this mechanism in order to
improve error messages.

Proposed Change Specification
-----------------------------

New constraint form
~~~~~~~~~~~~~~~~~~~

A new class, ``ErrorIfUnsolved``, is added to and exported from ``GHC.TypeError``,
with the following definition::

  type ErrorIfUnsolved :: CONSTRAINT r -> ErrorMessage -> CONSTRAINT r
  class ct => ErrorIfUnsolved ct err
  instance ct => ErrorIfUnsolved ct err

Custom solving rules
~~~~~~~~~~~~~~~~~~~~

GHC's solver would have special knowledge of this class:

* A Given constraint ``[G] ErrorIfUnsolved ct err`` is immediately reduced to
  ``[G] ct``, discarding the error message entirely.
* A Wanted constraint ``[W] ErrorIfUnsolved ct err`` is reduced to ``[W] ct``,
  but internally GHC also keeps track of the error message.
  Every time this constraint is simplified, we propagate the error message,
  in the same way that GHC today propagates ``CtOrigin``\ s during constraint solving.

  - Error messages override previous error messages. For example, in
    ``ErrorIfUnsolved (ErrorIfUnsolved ct err1) err2``, the error message ``err2``
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

    ``[W] ErrorIfUnsolved (alpha ~ beta) msg, [W] Maybe alpha ~ beta``

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

We propose to add ``ErrorIfUnsolved`` to the export list of ``GHC.TypeError``,
and to replace uses of ``Assert`` in ``Data.Type.Ord`` by ``ErrorIfUnsolved``.
For example, the definition of ``<=`` would become::

  type x <= y = ErrorIfUnsolved ((x <=? y) ~ True) (LeErrMsg x y)

The special logic for the handling of Givens would solve
`GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__
while retaining the improved error message afforded by the use of ``Assert``.

Naturally, these changes will require a CLC proposal.

It would also be possible to only make ``ErrorIfUnsolved`` available in the
``ghc-experimental`` package, without touching ``base``, but this would preclude
solving `GHC ticket #26190 <https://gitlab.haskell.org/ghc/ghc/-/issues/26190>`__,
which is a central goal of this proposal.

Changes in other libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~

Libraries such as Csongor Kiss's `generic-lens <https://hackage.haskell.org/package/generic-lens>`__
would be able to use ``ErrorIfUnsolved`` instead of a custom type family. For
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
    ( ErrorIfUnsolved (Generic s) (NoGeneric s msg)
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

The provisional name for the new class is ``ErrorIfUnsolved``, but other names
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
