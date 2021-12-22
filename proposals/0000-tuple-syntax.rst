Non-punning list and tuple syntax
=================================

.. author:: Richard Eisenberg (with much influence from collaborators)
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header::  This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/475>`_.
.. contents::
.. sectnum::

Introduce new names for the list type and tuple types to allow for users
to write pun-free code. In addition, add a new extension that disables
the built-in syntax for these types, thus requiring users to use the new
names.

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

even though the latter is nonsense.

Puns also cause trouble when describing what we are talking about: conversations
around these features invariably require people to say "``[]``-the-type" or
"``[]``-the-empty-list" or similar.

This proposal thus allows for a common way to refer to tuples and lists without
punning. It is entirely opt-in, though we expect the new phrasing to become popular
over time.

Proposed Change Specification
-----------------------------

1. Add the following definitions to a new module ``GHC.Tuple.Prim``::

     data Unit = ()
     data Solo a = MkSolo a    -- this is a change from today's `data Solo a = Solo a`

     type Tuple0 = Unit
     type Tuple1 = Solo
     data Tuple2 a b = (a, b)
     data Tuple3 a b c = (a, b, c)
     -- ...
     data Tuple64 ... = (...)

#. Add the following definitions to ``GHC.Classes`` (re-exported from ``GHC.Exts``). These
   replace similar definitions today::

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

#. Remove existing tuple definitions from ``GHC.Tuple``.

#. Add the following definitions to ``GHC.Tuple``, which re-exports all of ``GHC.Tuple.Prim``::

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

     type Tuple :: List Type -> Type
     type family Tuple ts where
       Tuple []    = Unit
       Tuple [a]   = Solo a
       Tuple [a,b] = Tuple2 a b
       -- ...
       Tuple [...] = Tuple64 ...

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

     type Constraints :: List Constraint -> Constraint
     type family Constraints cs where
       Constraints []    = CUnit
       Constraints [a]   = CSolo a
       Constraints [a,b] = CTuple2 a b
       -- ...
       Constraints [...] = CTuple64 ...

#. Export the following pseudo-definitions from ``GHC.Prim``. Note that ``GHC.Prim`` defines
   types that cannot be defined in Haskell, so we say that we just export these
   from ``GHC.Prim``, not define them there. Note that ``GHC.Exts`` re-exports
   ``GHC.Prim``::

     type Unit# :: TYPE (TupleRep [])
     data Unit# = (# #)

     type Solo# :: TYPE rep -> TYPE (TupleRep [rep])
     data Solo# a = MkSolo# a

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

#. Change ``GHC.Types`` to have the following definition::

     data List a = [] | a : List a

#. Introduce a new extension ``-XListTupleTypeSyntax``; this extension is on by default.

#. With ``-XListTupleTypeSyntax``:

   1. An occurrence of ``[]`` in type-syntax (as defined in `#378`_) is a synonym
      for ``GHC.Types.List``.

   #. An occurrence of ``[ty]`` in type-syntax is a synonym for ``GHC.Types.List ty``.

   #. An occurrence of ``()`` in type-syntax, where the type is not expected to be of kind ``Constraint``,
      is a synonym for ``GHC.Tuple.Prim.Unit``.

   #. An occurrence of ``(,,...,,)`` where there are *n* commas (for *n* ≧ 1) in type-syntax
      is a synonym for ``GHC.Tuple.Prim.Tuple``\ *n+1*.

   #. An occurrence of ``(ty1,ty2,...,tyn-1,tyn)`` (for *n* ≧ 2) in type-syntax, where neither the type
      is expected to be of kind ``Constraint`` and either none of the ``tyi`` are inferred to have kind ``Constraint``
      or there exists a ``tyi`` inferred to kind ``Type`` and none of the ``tyj`` (with j < i) are inferred to have
      kind ``Constraint``, is
      a synonym for ``GHC.Tuple.Prim.Tuple``\ *n*\ `` ty1 ty2 ... tyn-1 tyn``.

   #. An occurrence of ``(# #)`` in type-syntax is a synonym for ``GHC.Prim.Unit#``.

   #. An occurrence of ``(#,,...,,#)`` where there are *n* commas (for *n* ≧ 1) in type-syntax
      is a synonym for ``GHC.Prim.Tuple``\ *n+1*\ ``#``.

   #. An occurrence of ``(# ty1, ty2, ... , tyn-1, tyn #)`` (for *n* ≧ 2) in type-syntax is a synonym
      for ``GHC.Prim.Tuple``\ *n*\ ``# ty1 ty2 ... tyn-1 tyn``.

   #. An occurrence of ``(# | | ... | | #)`` where there are *n* pipes (for *n* ≧ 1) in type-syntax
      is a synonym for ``GHC.Prim.Sum``\ *n+1*\ ``#``.

   #. An occurrence of ``(# ty1 | ty2 | ... | tyn-1 | tyn #)`` (for *n* ≧ 2) in type-syntax is a
      synonym for ``GHC.Prim.Sum``\ *n*\ ``# ty1 ty2 ... tyn-1 tyn``.

   #. An occurrence of ``()`` in type-syntax, where the type is expected to be of kind ``Constraint``,
      is a synonym for ``GHC.Classes.CUnit``.

   #. An occurrence of ``(ty1, ty2, ..., tyn-1, tyn)`` (for *n* ≧ 2) in type-syntax, where the type is
      expected to be of kind ``Constraint``, is a synonym for ``GHC.Classes.CTuple``\ *n*\ `` ty1 ty2 ... tyn-1 tyn``.

   #. An occurrence of ``(ty1, ty2, ..., tyn-1, tyn)`` (for *n* ≧ 2) in type-syntax, where the first
      ``tyi`` inferred to have kind ``Type`` or ``Constraint`` has kind ``Constraint``, is a synonym
      for ``GHC.Classes.CTuple``\ *n*\ `` ty1 ty2 ... tyn-1 tyn``.

   #. An unapplied occurrence of ``GHC.Types.List`` is pretty-printed as ``[]``.

   #. An occurrence of ``GHC.Types.List ty`` is pretty-printed as ``[ty]]`.

   #. An occurrence of ``GHC.Tuple.Prim.Unit`` is pretty-printed as ``()``.

   #. An occurrence of ``GHC.Tuple.Prim.Tuplen ty1 ty2 ... tyn`` is pretty-printed as ``(ty1, ty2, ..., tyn)``.

   #. An occurrence of ``GHC.Tuple.Prim.Tuplen``, but not applied to a full *n* arguments, is pretty-printed as ``(,,...,,)``,
      where there are *n-1* commas.

   #. An occurrence of ``GHC.Prim.Unit#`` is pretty-printed as ``(# #)``.

   #. An occurrence of ``GHC.Prim.Tuplen# ty1 ty2 ... tyn`` is pretty-printed as ``(# ty1, ty2, ..., tyn #)``.

   #. An occurrence of ``GHC.Prim.Tuplen#``, but not applied to a full *n* arguments, is pretty-printed as ``(#,,...,,#)``,
      where there are *n-1* commas.

   #. An occurrence of ``GHC.Prim.Sumn# ty1 ty2 ... tyn`` is pretty-printed as ``(# ty1 | ty2 | ... | tyn #)``.

   #. An occurrence of ``GHC.Prim.Sumn#``, but not applied to a full *n* arguments, is pretty-printed as ``(# | | ... | | #)``,
      where there are *n-1* pipes.

   #. An occurrence of ``GHC.Classes.CUnit`` is pretty-printed as ``()``.

   #. An occurrence of ``GHC.Classes.CTuplen ty1 ty2 ... tyn`` is pretty-printed as ``(ty1, ty2, ..., tyn)``.

#. With ``-XNoListTupleTypeSyntax``:

   1. Uses of ``[]``, ``[...]``, ``()``, ``(,,...,,)``, ``(...,...,...)``, ``(# #)``, ``(#,,...,,#)``, ``(# ...,...,... #)``,
      ``(# | | ... | | #)``, and ``(# ... | ... | ... #)`` are now unambiguous. They always refer to data constructors,
      never types or type constructors.

   #. An occurrence of ``GHC.Tuple.Prim.Tuplen ty1 ty2 ... tyn`` is pretty-printed as ``Tuple [ty1, ty2, ..., tyn]``.

   #. An occurrence of ``GHC.Classes.CTuplen ty1 ty2 ... tyn`` is pretty-printed as ``Constraints [ty1, ty2, ..., tyn]``.

Effect and Interactions
-----------------------
1. With ``-XListTupleTypeSyntax`` (which is on by default), all programs that are accepted today continue
   to be accepted, and with the same meanings. Note that the peculiar dance around type tuples and constraint
   tuples exists today; I have tried to describe the current implementation faithfully, above.

#. With ``-XListTupleTypeSyntax`` (which is on by default), most pretty-printing will happen as it does
   today. The exception is around unsaturated ``CTuplen``, which is not handled above. It is hard to have
   an unsaturated constraint tuple, but possible by the use of a type family that decomposes one. Today's
   GHC prints out e.g. ``ghc-prim-0.6.1:GHC.Classes.(%,%)``. Switching to ``GHC.Classes.CTuple2`` (which is
   actually parseable) seems a positive improvement.

#. With the definitions above, users can avoid puns in their lists and tuples.

#. Note that the type syntax ``(ty1, ty2, ..., tyn) => ...`` is special syntax. The parser does *not*
   parse a type to the left of the ``=>``. This syntax thus remains completely unaffected by ``-XListTupleTypeSyntax``
   and will continue to work with ``-XNoListTupleTypeSyntax``. Furthermore, because a type like ``(ty1, ty2, ... tyn) => ...``
   does not contain any uses of ``CTuplen``, it will also continue to pretty-print just as today.

   On the other hand, collections of constraints occurring not to the left of a ``=>`` are affected by
   this proposal, for example in ``Dict (Eq a, Show b)`` (which would be written ``Dict (Constraints [Eq a, Show b])``
   under this proposal).

#. An instance declaration like ``instance (C a, C b) => C (Tuple [a, b]) where ...`` would be
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
   be evaluated by the Core Library Committee independently of this proposal.

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
   ``-XNoListTupleTypeSyntax`` catches on.

Alternatives
------------
1. Instead of defining ``TupleN`` as a type family (as done here), it could be
   a data family, effectively replacing the ``Tuple2``, ``Tuple3``, ..., definitions.
   This design would seem to be too complicated to be the primitive definition
   of tuples, however, when a very vanilla datatype like ``data Tuple2 a b = (a, b)``
   suffices.

#. Instead of introducing new names, we could use more mixfix bits of punctuation,
   such as ``(~ ty1, ty2 ~)`` for normal tuples and ``(% ty1, ty2 %)`` for constraint
   tuples. This was not as popular in a recent `straw poll <https://github.com/ghc-proposals/ghc-proposals/pull/458#issuecomment-982230541>`_.

Unresolved Questions
--------------------

None at this time.
