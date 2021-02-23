A syntax for visible dependent quantification
=============================================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-09-30
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/16326
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/81>`_.
.. contents::

GHC 8.0 has support for visible dependent quantification in kinds. For example, GHC 8 will accept ::

  data T k (a :: k)

Note that any (fully-applied) use of ``T`` has to mention both a kind and a type. For example,
``T Nat 3`` and ``T Type Int`` are well-typed. If you ask GHCi for the kind of ``T``, it tells
you ``forall k -> k -> *`` (with ``-fprint-explicit-foralls``). Note that there is no ``.`` after
the ``forall``. Instead, this kind makes ``k`` a visible, dependent type variable. It's *visible*
because every (fully-applied) use of ``T`` must pass in a value for ``k`` visibly (explicitly).
It's *dependent* because ``k`` appears later in the kind of ``T``. The ability to have visible,
dependent quantifiees is a new feature in GHC 8, available only in kinds. (There's no way to do
this in terms.)

The syntax ``forall k -> k -> *`` that is printed in GHCi for these kinds is not currently parsed.
This proposal proposed to add this kind format to the language as a first-class kind.

This proposal is viable on its own, but it may be best considered in the context of other
quantifiers needed to support dependent types. See `#102 <https://github.com/goldfirere/ghc-proposals/blob/pi/proposals/0000-pi.rst>`_.

Motivation
------------

Currently, GHC allows the user to define a type with a visible, dependent quantifiers in its kind,
but it offers no way to write the kind directly. This is just plain silly.

(Historical note: the reason this happened is that I got early feedback claiming that ``forall k -> ...``
was poor syntax, and I was reticent to add it to the language with ``-XTypeInType``. However, now that
this proposals process is active, I'm happy to propose the new syntax where it can get debated in the
open and perhaps refined.)

A useful example of how this construct might be used is an alternative syntax for ``TypeRep``. We can
imagine ::

  TypeRepV :: forall k -> k -> Type   -- "V" for visible

as a companion to ::

  TypeRep :: forall k. k -> Type      -- we have this today

Now, if someone wants to operate on
a type representation for some type of kind ``Type -> Type``, they could say ::

  foo :: TypeRepV (Type -> Type) a -> ...

and GHC would easily infer that ``a`` should have kind ``Type -> Type``.

Proposed Change Specification
-----------------------------
Add a new bit of syntax for types (= kinds) that looks like this::

  'forall' tv_bndrs '->' ctype

where ``ctype`` is the point within GHC's grammar (as implemented in its
`parser <https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y>`_)
where the current ``'forall' tv_bndrs '.' ctype`` rule lives. The meaning will
be identical to that of the existing ``forall`` construct, except that the
``tv_bndrs`` will be visible.

(NB: The ``'forall'`` construct in the parser also accepts ``∀``.)

This new construct will be rejected in any context that is unambiguously a
type for a term. (For example, it will be rejected in type signatures of
terms, but allowed in type synonyms, which can be used in kinds.) No
term-level definition can have a type that has a visible dependent quantifier.

To wit, this new construct would be forbidden in the following places (with examples
of rejected constructs):

* The type ascription of a ``forall``\-bound term variable in a ``RULE``::

    {-# RULES "blah" forall a -> id a = a #-}

* The type of a foreign import/export::

    foreign export ccall freeStablePtr :: forall a -> StablePtr a -> IO ()

* A type signature for a term-level variable::

    id :: forall a -> a -> a

* The type in a ``SPECIALISE`` or ``SPECIALISE_INLINE`` or ``SPECIALISE instance`` pragma::

    {-# SPECIALISE foldM :: forall a b -> (a -> b -> IO a) -> a -> [b] -> IO a #-}

* An expression type ascription::

    zipWith ((<>) :: forall a -> Maybe a -> Maybe a -> Maybe a) xs ys

* A pattern synonym type signature::

    pattern Nil :: forall a -> [a]

* A type signature in a pattern::

    isJust (x :: forall a -> Maybe a) = ...

* A GADT data constructor type::

    data T where
      MkT :: forall a -> a -> T

Naturally, the new syntax is forbidden anywhere that ``forall`` is currently
forbidden (for example, in an argument position of a type family).

Effect and Interactions
-----------------------
Shouldn't be any untoward interactions. Template Haskell will have to be updated, and we'll have to
make sure no terms can get these strange new types.

Note that the new construct *can* be used in higher-rank scenarios::

  data S :: (forall k -> k -> Type) -> Type

will accept the ``T`` in the introduction as an argument, but it won't accept ``Data.Proxy``\'s
``Proxy``, as ``Proxy`` takes its argument invisibly. Perhaps one day we can devise a way
to coerce visibilities to allow ``S`` to take ``Proxy`` as an argument, but not today.

Costs and Drawbacks
-------------------
It's one more construct that has to be maintained, which is a non-negligible cost. But, I argue that
the language simply has a strange surface area without this feature, where a type exists that cannot
be written down.

A drawback of the design as proposed is that the signifier of the visible/invisible distinction can
be far away from individual variables. For example, consider ``forall a b c d.`` and ``forall a b c d ->``.
You have to scan for the ``.`` or the ``->`` before you know what kind of quantification is at hand.

Alternatives
------------

I don't have any good ones. Do you?

Unresolved questions
--------------------
None right now, other than bikeshedding this syntax.


Implementation Plan
-------------------
I or a close collaborator volunteer to implement.
