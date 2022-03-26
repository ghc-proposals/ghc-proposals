Non-punning list and tuple syntax
=================================

.. author:: Richard Eisenberg (with much influence from collaborators)
.. date-accepted:: 2022-03-24
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/21294
.. implemented::
.. highlight:: haskell
.. header::  This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/475>`_.
.. contents::
.. sectnum::

Introduce new names for the list type and tuple types to allow for users
to write pun-free code. In addition, add a new extension that disables
the built-in syntax for these types, thus requiring users to use the new
names.

Everything in this proposal is fully backward-compatible (except for the deprecation
on the data constructor ``Solo``, but that has a migration period).

.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/pull/281
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/pull/378
.. _`#473`: https://github.com/ghc-proposals/ghc-proposals/pull/473

Motivation
----------
Proposals `#270`_, `#281`_, `#378`_, and `#473`_ all struggle when faced
with *puns*, the use of an identifier that can be interpreted either in the
type-level namespace or the term-level namespace (but with different meanings).

Furthermore, puns can be hard for newcomers to Haskell. It is not hard to jump
from ::

  single :: [Int]
  single = [5]

to ::

  just :: Maybe Int
  just = Maybe 5

even though the latter is nonsense. With this proposal, users would have the
option of writing ::

  single :: List Int
  single = [5]

Puns also cause trouble when describing what we are talking about: conversations
around these features invariably require people to say "``[]``-the-type" or
"``[]``-the-empty-list" or similar.

This proposal thus allows for a common way to refer to tuples and lists without
punning. It is entirely opt-in, though we expect the new phrasing to become popular
over time.

Notation
--------
This proposal uses angle-bracket syntax ``<n>`` to denote the occurrence of a numeric
variable in a bit of code. This allows us to e.g. refer to ``Tuple2`` and ``Tuple3`` with
``Tuple<n>``.

Proposed Change Specification
-----------------------------

1. Create a new module in ``base`` called ``GHC.Prelude``. This module exports definitions
   peculiar to GHC that are safe to use in ordinary code.
   This module is *not* intended as a replacement to ``Prelude``,
   but instead a complement to it. This module is a counterpart to ``GHC.Exts``, which exports
   many *unsafe* internals.

   This proposal lists several definitions to be exported from ``GHC.Prelude``, but leaves other
   definitions to be added in separate proposal(s).

#. Export the following definitions from ``GHC.Tuple`` and ``GHC.Prelude``. (Implementation note:
   These would probably end up in a new module ``GHC.Tuple.Prim``, due to the dependency from
   the definitions below in ``GHC.Tuple`` on ``GHC.TypeLits``.) ::

     data Unit = ()
     data Solo a = MkSolo a    -- this is a change from today's `data Solo a = Solo a`

     {-# DEPRECATED Solo "The Solo constructor has been renamed to MkSolo to avoid punning." #-}
     pattern Solo :: a -> Solo a
     pattern Solo x = MkSolo x
     {-# COMPLETE Solo #-}

     getSolo :: Solo a -> a
     getSolo (Solo a) = a   -- as today

     type Tuple0 = Unit
     type Tuple1 = Solo
     data Tuple2 a b = (a, b)
     data Tuple3 a b c = (a, b, c)
     -- ...
     data Tuple64 ... = (...)

#. Export the following definitions from ``GHC.Exts`` and ``GHC.Prelude``. These replace
   the existing tuple definitions (in ``GHC.Classes``) today. (Note that ``(...) =>`` is special syntax, and does not
   construct tuples. See more on this point `below <#constraint-special-syntax>`_.)::

     class CUnit
     instance CUnit

     class a => CSolo a
     instance a => CSolo a

     type CTuple0 = CUnit
     type CTuple1 = CSolo

     class (a, b) => CTuple2 a b
     instance (a, b) => CTuple2 a b

     -- ...

     class (...) => CTuple64 ...
     instance (...) => CTuple64 ...

#. Export the following pseudo-definitions from ``GHC.Exts``. (Implementation note:
   These would likely be exported from ``GHC.Prim`` originally.) ::

     type Unit# :: TYPE (TupleRep [])
     data Unit# = (# #)

     type Solo# :: TYPE rep -> TYPE (TupleRep [rep])
     data Solo# a = (# a #)

     type Tuple0# = Unit#
     type Tuple1# = Solo#

     type Tuple2# :: TYPE r1 -> TYPE r2 -> TYPE (TupleRep [r1, r2])
     data Tuple2# a b = (# a, b #)

     -- ...

     type Tuple64# :: TYPE r1 -> ... -> TYPE r64 -> TYPE (TupleRep [r1, ..., r64])
     data Tuple64# ... = (# ... #)

     -- NB: There are no 0-sums or 1-sums in Haskell, today or tomorrow.

     type Sum2# :: TYPE r1 -> TYPE r2 -> TYPE (SumRep [r1, r2])
     data Sum2# a b = (# a | #) | (# | b #)

     type Sum3# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE (SumRep [r1, r2, r3])
     data Sum3# a b c = (# a | | #) | (# | b | #) | (# | | c #)

     -- ...

     type Sum64# :: TYPE r1 -> ... -> TYPE r64 -> TYPE (SumRep [r1, ..., r64])
     data Sum64# ... = ...

#. Remove existing tuple definitions from ``GHC.Tuple``.

#. Export the following definitions from ``GHC.Tuple``::

     type Length :: List a -> Nat   -- not exported
     type family Length xs where
       Length []     = 0
       Length (_:xs) = 1 + Length xs

     type TupleArgKind :: Type -> Nat -> Type
     type family TupleArgKind t_or_c n = r | r -> n where
       TupleArgKind _      0 = Unit
       TupleArgKind t_or_c 1 = t_or_c       -- *not* Solo t; see below
       TupleArgKind t_or_c 2 = Tuple2 t_or_c t_or_c
       TupleArgKind t_or_c 3 = Tuple3 t_or_c t_or_c t_or_c
       TupleArgKind t_or_c 4 = Tuple4 t_or_c t_or_c t_or_c t_or_c
       -- ...
       TupleArgKind t_or_c 64 = Tuple64 t_or_c ... t_or_c
       TupleArgKind _      n  = TypeError (ShowType n :<>: Text " is too large; the maximum size for a tuple is 64.")

     type Tuple :: forall (n :: Nat). TupleArgKind Type n -> Type
     type family Tuple ts where
       Tuple () = Unit
       Tuple a = a    -- see below
       Tuple (a, b) = Tuple2 a b
       Tuple (a, b, c) = Tuple3 a b c
       Tuple (a, b, c, d) = Tuple4 a b c d
       -- ...
       Tuple (a, b, ..., bk, bl) = Tuple64 a b ... bk bl
       Tuple @n _ = TypeError (ShowType n :<>: Text " is too large; the maximum size for a tuple is 64.")

     type Constraints :: forall (n :: Nat). TupleArgKind Constraint n -> Constraint
     type family Constraints ts where
       Constraints () = Unit
       Constraints a = a    -- see below
       Constraints (a, b) = CTuple2 a b
       Constraints (a, b, c) = CTuple3 a b c
       Constraints (a, b, c, d) = CTuple4 a b c d
       -- ...
       Constraints (a, b, ..., bk, bl) = CTuple64 a b ... bk bl
       Constraints @n _ = TypeError (ShowType n :<>: Text " is too large; the maximum size for a tuple is 64.")

     type TupleArgKind# :: List RuntimeRep -> Type
     type family TupleArgKind# reps where
       TupleArgKind# [] = Unit
       TupleArgKind# [r1] = TYPE r1     -- *not* Solo (TYPE r1); see below
       TupleArgKind# [r1, r2] = Tuple2 (TYPE r1) (TYPE r2)
       TupleArgKind# [r1, r2, r3] = Tuple3 (TYPE r1) (TYPE r2) (TYPE r3)
       -- ...
       TupleArgKind# [r1, ..., r64] = Tuple64 (TYPE r1) ... (TYPE r64)
       TupleArgKind# other = TypeError (ShowType (Length other) :<>: Text " is too large; the maximum size of a tuple is 64.")

     type Tuple# :: forall (reps :: List RuntimeRep). TupleArgKind# reps -> TYPE (TupleRep reps)
     type family Tuple# ts where
       Tuple# () = Unit#
       Tuple# (a :: TYPE r) = TypeError (Text "Tuple# does not work for 1-tuples; use Solo#.")  -- see below
       Tuple# (a, b) = Tuple2# a b
       Tuple# (a, b, c) = Tuple3# a b c
       -- ...
       Tuple# (a, b, ..., bk, bl) = Tuple64# a b ... bk bl
       Tuple# @reps _ = TypeError (ShowType (Length reps) :<>: Text " is too large; the maximum size of a tuple is 64.")

     type Sum# :: forall (reps :: List RuntimeRep). TupleArgKind# reps -> TYPE (SumRep reps)
     type family Sum# ts where
       Sum# () = TypeError (Text "GHC does not support empty unboxed sums. Consider Data.Void.Void instead.")
       Sum# (a :: TYPE r) = TypeError (Text "GHC does not support unary unboxed sums. Consider Data.Tuple.Solo# instead.")
       Sum# (a, b) = Sum2# a b
       Sum# (a, b, c) = Sum3# a b c
       -- ...
       Sum# (a, b, ..., bk, bl) = Sum64# a b ... bk bl
       Sum# @reps _ = TypeError (ShowType (Length reps) :<>: Text " is too large; the maximum size of a sum is 64.")

#. Change ``GHC.Types`` to have the following definition::

     data List a = [] | a : List a

#. Re-export ``List`` from ``GHC.List`` and ``GHC.Prelude``.

#. Introduce a new extension ``-XListTuplePuns``; this extension is part
   of ``-XHaskell98``, ``-XHaskell2010``, and ``-XGHC2021``. It is thus on by default.

#. With ``-XListTuplePuns``:

   1. An occurrence of ``[]`` in type-syntax (as defined in `#378`_) is a synonym
      for ``GHC.List.List``.

   #. An occurrence of ``[ty]`` in type-syntax is a synonym for ``GHC.List.List ty``.

   #. An occurrence of ``()`` in type-syntax, where the type is not expected to be of kind ``Constraint``,
      is a synonym for ``GHC.Tuple.Unit``.

   #. An occurrence of ``(,,...,,)`` where there are *n* commas (for *n* ≧ 1) in type-syntax
      is a synonym for ``GHC.Tuple.Tuple<n+1>``.

   #. An occurrence of ``(ty1,ty2,...,ty<n-1>,ty<n>)`` (for *n* ≧ 2) in type-syntax, where neither the type
      is expected to be of kind ``Constraint`` and either none of the ``ty<i>`` are inferred to have kind ``Constraint``
      or there exists a ``ty<i>`` inferred to kind ``Type`` and none of the ``ty<j>`` (with *j* < i) are inferred to have
      kind ``Constraint``, is
      a synonym for ``GHC.Tuple.Tuple<n>`` ``ty1 ty2 ... ty<n-1> ty<n>``. (This rule retains today's behavior.)

   #. With ``-XUnboxedTuples``, an occurrence of ``(# #)`` in type-syntax is a synonym for ``GHC.Exts.Unit#``.

   #. With ``-XUnboxedTuples``, an occurrence of ``(# ty #)`` in type-syntax is a synonym for ``GHC.Exts.Solo# ty``.

   #. With ``-XUnboxedTuples``, an occurrence of ``(#,,...,,#)`` where there are *n* commas (for *n* ≧ 1) in type-syntax
      is a synonym for ``GHC.Exts.Tuple<n+1>#``.

   #. With ``-XUnboxedTuples``, an occurrence of ``(# ty1, ty2, ... , ty<n-1>, ty<n> #)`` (for *n* ≧ 2) in type-syntax is a synonym
      for ``GHC.Exts.Tuple<n># ty1 ty2 ... ty<n-1> ty<n>``.

   #. With ``-XUnboxedSums``, an occurrence of ``(# | | ... | | #)`` where there are *n* pipes (for *n* ≧ 1) in type-syntax
      is a synonym for ``GHC.Exts.Sum<n+1>#``.

   #. With ``-XUnboxedSums``, an occurrence of ``(# ty1 | ty2 | ... | ty<n-1> | ty<n> #)`` (for *n* ≧ 2) in type-syntax is a
      synonym for ``GHC.Exts.Sum<n># ty1 ty2 ... ty<n-1> ty<n>``.

   #. An occurrence of ``()`` in type-syntax, where the type is expected to be of kind ``Constraint``,
      is a synonym for ``GHC.Tuple.CUnit``.

   #. An occurrence of ``(ty1, ty2, ..., ty<n-1>, ty<n>)`` (for *n* ≧ 2) in type-syntax, where the type is
      expected to be of kind ``Constraint``, is a synonym for ``GHC.Tuple.CTuple<n> ty1 ty2 ... ty<n-1> ty<n>``.

   #. An occurrence of ``(ty1, ty2, ..., ty<n-1>, ty<n>)`` (for *n* ≧ 2) in type-syntax, where the first
      ``ty<i>`` inferred to have kind ``Type`` or ``Constraint`` has kind ``Constraint``, is a synonym
      for ``GHC.Tuple.CTuple<n> ty1 ty2 ... ty<n-1> ty<n>``.

   #. An unapplied occurrence of ``GHC.List.List`` is pretty-printed as ``[]``.

   #. An occurrence of ``GHC.List.List ty`` is pretty-printed as ``[ty]``.

   #. An occurrence of ``GHC.Tuple.Unit`` is pretty-printed as ``()``.

   #. An occurrence of ``GHC.Tuple.Tuple<n> ty1 ty2 ... ty<n>`` is pretty-printed as ``(ty1, ty2, ..., ty<n>)``.

   #. An occurrence of ``GHC.Tuple.Tuple<n>``, but not applied to a full *n* arguments, is pretty-printed as ``(,,...,,)``,
      where there are *n-1* commas.

   #. An occurrence of ``GHC.Exts.Unit#`` is pretty-printed as ``(# #)``.

   #. An occurrence of ``GHC.Exts.Tuple<n># ty1 ty2 ... ty<n>`` is pretty-printed as ``(# ty1, ty2, ..., ty<n> #)``.

   #. An occurrence of ``GHC.Exts.Tuple<n>#``, but not applied to a full *n* arguments, is pretty-printed as ``(#,,...,,#)``,
      where there are *n-1* commas.

   #. An occurrence of ``GHC.Exts.Sum<n># ty1 ty2 ... ty<n>`` is pretty-printed as ``(# ty1 | ty2 | ... | ty<n> #)``.

   #. An occurrence of ``GHC.Exts.Sum<n>#``, but not applied to a full *n* arguments, is pretty-printed as ``(# | | ... | | #)``,
      where there are *n-1* pipes.

   #. An occurrence of ``GHC.Tuple.CUnit`` is pretty-printed as ``()``.

   #. An occurrence of ``GHC.Tuple.CTuple<n> ty1 ty2 ... ty<n>`` is pretty-printed as ``(ty1, ty2, ..., ty<n>)``.

#. With ``-XNoListTuplePuns``:

   1. Uses of ``[]``, ``[...]``, ``()``, ``(,,...,,)``, ``(...,...,...)``, ``(# #)``, ``(#,,...,,#)``, and ``(# ...,...,... #)``
      (among other arities) are now unambiguous. They always refer to data constructors,
      never types or type constructors. (Note that ``(...) =>`` is special syntax, not an occurrence of any of the types
      listed above. See `below <#constraints-special-syntax>`_.)

   #. A use of ``(# ... | ... | ... #)``, where each of the ``...`` is filled in, (among other arities) is now disallowed.

   #. An occurrence of ``GHC.Tuple.Tuple<n> ty1 ty2 ... ty<n>`` is pretty-printed as ``Tuple (ty1, ty2, ..., ty<n>)``.

   #. An occurrence of ``GHC.Tuple.CTuple<n> ty1 ty2 ... ty<n>`` is pretty-printed as ``Constraints (ty1, ty2, ..., ty<n>)``.

   #. An occurrence of ``GHC.Exts.Tuple<n># ty1 ty2 ... ty<n>`` is pretty-printed as ``Tuple# (ty1, ty2, ..., ty<n>)``.

   #. An occurrence of ``GHC.Exts.Sum<n># ty1 ty2 ... ty<n>`` is pretty-printed as ``Sum# (ty1, ty2, ..., ty<n>)``.

   #. A use of ``'[ty1, ..., ty<n>]`` (for *n* ≧ 0) is now disallowed.

   #. A use of ``'(,,,...,,,)`` where there are *n* commas (for *n* ≧ 0) is now disallowed.

   #. A use of ``'(ty1, ..., ty<n>)`` (for *n* ≧ 0) is now disallowed.

   #. Lists and tuples on the type-level are printed without any tick.

#. Three releases after this proposal is implemented, remove the ``Solo`` pattern synonym from ``GHC.Tuple``.

Effect and Interactions
-----------------------
1. With ``-XListTuplePuns`` (which is on by default), all programs that are accepted today continue
   to be accepted, and with the same meanings. Note that the peculiar dance around type tuples and constraint
   tuples exists today; I have tried to describe the current implementation faithfully, above.

#. With ``-XListTuplePuns`` (which is on by default), most pretty-printing will happen as it does
   today. The exception is around unsaturated ``CTuple<n>``, which is not handled above. It is hard to have
   an unsaturated constraint tuple, but possible by the use of a type family that decomposes one. Today's
   GHC prints out e.g. ``ghc-prim-0.6.1:GHC.Classes.(%,%)``. Switching to ``GHC.Classes.CTuple2`` (which is
   actually parseable) seems a positive improvement.

#. With the definitions above, users can avoid puns in their lists and tuples.

   .. _constraints-special-syntax:

#. Note that the type syntax ``(ty1, ty2, ..., ty<n>) => ...`` is already special syntax. The parser does *not*
   parse a type to the left of the ``=>``. This syntax thus remains completely unaffected by ``-XListTuplePuns``
   and will continue to work with ``-XNoListTuplePuns``. Furthermore, because a type like ``(ty1, ty2, ... ty<n>) => ...``
   does not contain any uses of ``CTuple<n>``, it will also continue to pretty-print just as today.

   On the other hand, collections of constraints occurring not to the left of a ``=>`` are affected by
   this proposal, for example in ``Dict (Eq a, Show b)`` (which would be written ``Dict (Constraints [Eq a, Show b])``
   under this proposal). Another example is ``(Eq a, (Show a, Read a)) => a -> a``, which would not
   be accepted under ``-XNoListTuplePuns``. Instead, the user should flatten the constraints or
   write ``(Eq a, Constraints [Show a, Read a]) => a -> a``.

#. An instance declaration like ``instance (C a, C b) => C (Tuple (a, b)) where ...`` would be
   rejected because it uses a type family in the instance head. We might choose to relax
   this restriction, by allowing type families in an instance head, as long as they can
   reduce to a ground (i.e. type-family-free) type. This proposal does *not* include such
   a lifting of the restriction, as the workaround is straightforward: just write
   ``instance (C a, C b) => C (Tuple2 a b) where ...``. Still, we may decide to revisit
   this in the future.

#. In due course, we may wish to consider re-exporting some of the definitions
   above from modules not in the ``GHC.`` namespace, perhaps even including the
   ``Prelude``. This proposal does *not* make any such suggestions, and it does *not*
   depend on any such ideas being adopted in the future. Any such idea would
   be evaluated by the Core Libraries Committee independently of this proposal.

#. This proposal changes the name of the constructor of the unary boxed tuple ``Solo``,
   from ``Solo`` to ``MkSolo``. The proposal includes a deprecated ``Solo`` pattern
   synonym to enable a migration period.

#. A tempting alternative to the design here is to have ::

     type Tuple :: [Type] -> Type
     data family Tuple ts
     data instance Tuple [a,b] = (a, b)

   and higher arities. The problem with this design is that we have no way to express
   what is today written as ::

     instance Functor ((,) a)

   and others.

#. The mixfix syntax for tuples is achieved by the e.g. ``Tuple`` type family. Note that
   ``Tuple (Int, Bool)`` use the term-level data constructor ``(,)`` to package up the
   type arguments ``Int`` and ``Bool``. Some have expressed discomfort at the loopiness
   of this definition, but I like it: it is easy to read, reminiscent of the old syntax
   for tuple types, and works both for boxed tuples and unboxed tuples.

   Handling singletons is a bit interesting:

   ``TupleArgKind t_or_c 1`` is defined to be ``t_or_c``, *not* ``Solo t_or_c``, as you would
   otherwise expect. This is because we expect to see e.g. ``Tuple (Int)``, not ``Tuple (MkSolo Int)``.
   (Actually, we would probably not expect these at all, but the current design allows us to be forgiving
   during refactoring.)

   In ``Tuple``, we see ``Tuple a = a``, which looks like a universally applicable equation. It is
   not. Because ``a :: Type`` (resp. ``a :: Constraint``) we learn the invisible argument to ``Tuple``
   (resp. ``Constraints``) must be ``1``, and thus this equation fires only when the argument to ``Tuple``
   is not a tuple of types (resp. constraints).

   A beautiful, unexpected consequence of this design is that it aids migration. Now, with or without
   ``-XListTuplePuns``, a user can write e.g. ``Tuple (Int, Bool, Double)``, and this will be the type
   of e.g. ``(1, True, 3.14)``. With ``-XListTuplePuns``, the argument to ``Tuple`` will be the tuple
   type, of kind ``Type``. That is, the type will really be understood as ``Tuple @1 (Tuple3 Int Bool Double)``.
   The "``1``" equation fires, reducing to ``Tuple3 Int Bool Double``. On the other hand, with
   ``-XNoListTuplePuns``, the user's type is understood as ``Tuple @3 (Int, Bool, Double)``, and the "``3``"
   equation fires, reducing to ``Tuple3 Int Bool Double`` -- the same answer! And so, all users
   can write ``Tuple`` before their tuples and not get hurt.

   For unboxed tuples (``Tuple#``), this non-uniformity is not as happy. For example, ``Tuple# (Int, Bool)``
   means either ``Tuple2# Int Bool`` (with ``-XNoListTuplePuns``) or ``Solo# (Tuple2 Int Bool)`` (with
   ``-XListTuplePuns``). Furthermore, ``Tuple# (Int#, Double#)`` would not kind-check with ``-XListTuplePuns``. So we avoid these problems by simply erroring in the 1-element case.

Costs and Drawbacks
-------------------
1. This is one more feature to maintain, but the code would be pretty local.

#. Having multiple ways of naming one thing may offer a boon to *writers* of code
   (they can choose whichever way to name a tuple that they like), but it imposes
   a burden on *readers* of code, who may need to be familiar with all possible
   ways of describing a tuple (and that they are interchangeable). Careful
   documentation of these ideas -- ideally, in the Haddock documentation for the
   names introduced above -- will help to mitigate this problem.

#. A particular class of code readers are beginners, and having multiple different
   ways to say the same thing is particularly challenging for beginners. We should
   thus think carefully about how to present these names to beginners, if
   ``-XNoListTuplePuns`` catches on.

Alternatives
------------
1. Instead of defining ``TupleN`` as a type family (as done here), it could be
   a data family, effectively replacing the ``Tuple2``, ``Tuple3``, ..., definitions.
   This design would seem to be too complicated to be the primitive definition
   of tuples, however, when a very vanilla datatype like ``data Tuple2 a b = (a, b)``
   suffices.

#. We could use ``CTuple`` instead of ``Constraints``. ``CTuple`` is more uniform,
   but I find ``Constraints`` easier to understand.

#. Instead of introducing new names, we could use more mixfix bits of punctuation,
   such as ``(~ ty1, ty2 ~)`` for normal tuples and ``(% ty1, ty2 %)`` for constraint
   tuples. This was not as popular in a recent `straw poll <https://github.com/ghc-proposals/ghc-proposals/pull/458#issuecomment-982230541>`_.

#. We could use a mixfix syntax for tuples, allowing something like ``a * b * c``,
   perhaps with a Unicode operator. Note that ``*`` there is *not* associative, because
   neither left-associative nor right-associative would work. This is tempting,
   but we cannot use ``*`` (it means multiplication and *is* associative), and no other
   operator naturally presents itself. Using Unicode as the primitive definition for
   tuples seems unwise.

   Note that a future proposal is welcome to include ideas for e.g. Unicode-based mixfix
   syntax for tuples. This proposal is more concerned about their primitive definition.

#. Controlling the ``(# ... | ... | ... #)`` syntax for unboxed sum types with
   ``-XNoListTuplePuns`` is not necessary to avoid punning, but is done only for
   consistency. We could skip this, but I prefer keeping it as proposed.

#. There was an objection in the commentary about the name ``GHC.Prelude``. I continue
   to like that name: the module exports basic definitions one will likely want when
   using the GHC compiler for Haskell. However, an alternative might be
   ``GHC.SafeExts`` or something similar. (I'd actually rather have the safe extensions
   be in ``GHC.Exts`` and the unsafe ones be in ``GHC.Exts.Unsafe``, but that ship has
   sailed and is not worth calling back to port.)

   Note that GHC itself already has a module named ``GHC.Prelude`` that would have to
   be renamed if we keep ``GHC.Prelude`` as the choice for the new module in ``base``.
   This is purely an implementation detail, though, and would not affect users (except
   via the GHC API).

#. The following definitions were included in an earlier version of this proposal,
   but the committee decided we were better without::

     type TupleNKind :: Nat -> Type     -- Nat is from GHC.TypeLits
     type family TupleNKind n = r | r -> n where
       TupleNKind 0 = Type
       TupleNKind n = Type -> TupleNKind (n-1)   -- this fails the injectivity check, but a little magic will allow this

     type TupleN :: forall (n :: Nat). TupleNKind n
     type family TupleN @n where     -- using syntax from #425
       TupleN @0 = Unit
       TupleN @1 = Solo
       TupleN @2 = Tuple2
       TupleN @3 = Tuple3
       TupleN @4 = Tuple4
       -- ...
       TupleN @64 = Tuple64
       TupleN @n  = TypeError (ShowType n :<>: Text " is too large; the maximum size for a tuple is 64.")

     type CTupleNKind :: Nat -> Type
     type family CTupleNKind n = r | r -> n where
       CTupleNKind 0 = Constraint
       CTupleNKind n = Constraint -> CTupleNKind (n-1)

     type CTupleN :: forall (n :: Nat). CTupleNKind n
     type family CTupleN @n where
       CTupleN @0  = CUnit
       CTupleN @1  = CSolo
       CTupleN @2  = CTuple2
       CTupleN @3  = CTuple3
       CTupleN @4  = CTuple4
       -- ...
       CTupleN @64 = CTuple64
       CTupleN @n  = TypeError (ShowType n :<>: Text " is too large; the maximum size for a tuple is 64.")

     type TupleNKind# :: List RuntimeRep -> List RuntimeRep -> Type
     type family TupleNKind# all_reps reps_to_go = r | r -> all_reps reps_to_go where
       TupleNKind# all_reps '[]                      = TYPE (TupleRep all_reps)
       TupleNKind# all_reps (first_rep : reps_to_go) = TYPE first_rep -> TupleNKind# all_reps reps_to_go

     type TupleN# :: forall (reps :: List RuntimeRep). TupleNKind# reps reps
     type family TupleN# where
       TupleN# @[]                 = Unit#
       TupleN# @[rep1]             = Solo#
       TupleN# @[rep1, rep2]       = Tuple2#
       TupleN# @[rep1, rep2, rep3] = Tuple3#
       -- ...
       TupleN# @[...]              = Tuple64#
       TupleN# @reps               = TypeError (ShowType (Length reps) :<>: Text " is too large; the maximum size for a tuple is 64.")

     type SumNKind# :: List RuntimeRep -> List RuntimeRep -> Type
     type family SumNKind# all_reps reps_to_go = r | r -> all_reps reps_to_go where
      SumNKind# all_reps '[]                      = TYPE (SumRep all_reps)
      SumNKind# all_reps (first_rep : reps_to_go) = TYPE first_rep -> SumNKind# all_reps reps_to_go

     type SumN# :: forall (reps :: List RuntimeRep). SumNKind# reps reps
     type family SumN# where
      SumN# @[]                 = TypeError (Text "GHC does not support empty unboxed sums. Consider Data.Void.Void instead.")
      SumN# @[rep1]             = TypeError (Text "GHC does not support unary unboxed sums. Consider Data.Tuple.Solo# instead.")
      SumN# @[rep1, rep2]       = Sum2#
      SumN# @[rep1, rep2, rep3] = Sum3#
      -- ...
      SumN# @[...]              = Sum64#
      SumN# @reps               = TypeError (ShowType (Length reps) :<>: Text " is too large; the maximum size for a Sum is 64.")

#. This proposal includes the syntax ``Tuple (Int, Bool)`` as an alternative for
   ``Tuple2 Int Bool``. Because this syntax uses the tuple data constructor in order
   to describe a tuple type, some have said it's loopy. An alternative would be to
   use lists of types instead of tuples of types, thus::

     type Length :: List a -> Nat   -- not exported
     type family Length xs where
       Length []     = 0
       Length (_:xs) = 1 + Length xs

     type Tuple :: List Type -> Type
     type family Tuple ts where
       Tuple []    = Unit
       Tuple [a]   = Solo a
       Tuple [a,b] = Tuple2 a b
       -- ...
       Tuple [...] = Tuple64 ...
       Tuple ts    = TypeError (ShowType (Length ts) :<>: Text " is too large; the maximum size for a tuple is 64.")

     type Constraints :: List Constraint -> Constraint
     type family Constraints cs where
       Constraints []    = CUnit
       Constraints [a]   = CSolo a
       Constraints [a,b] = CTuple2 a b
       -- ...
       Constraints [...] = CTuple64 ...
       Constraints ts    = TypeError (ShowType (Length ts) :<>: Text " is too large; the maximum size for a tuple is 64.")

   A drawback of this approach is that it does not extend to unboxed tuples or sums, because
   unboxed tuples and sums can contain unlifted types, which have a variety of kinds: the
   list would have to be heterogeneous.

Unresolved Questions
--------------------

None at this time.
