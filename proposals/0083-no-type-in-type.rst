Embrace ``Type :: Type``
==========================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-04-24
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15195
.. implemented:: 8.6
.. highlight:: haskell
.. header::
   This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/83>`_.
.. contents::

GHC 8.0 included a major change to GHC's type system: the ``Type :: Type`` axiom.
Though casual users were protected from this by hiding its features behind the
``-XTypeInType`` extension, all programs written in GHC 8+ have the axiom behind
the scenes. In order to preserve backward compatibility, various legacy features
were left unchanged. For example, with ``-XDataKinds`` but not ``-XTypeInType``,
GADTs could not be used in types.

This proposal suggests to remove the backward-compatibility features, embracing
``Type :: Type``. Specifically:

* Incorporate the features currently in ``-XTypeInType`` into the ``-XPolyKinds``
  and ``-XDataKinds`` extensions.

* Deprecate the ``-XTypeInType`` extension (as it would be a synonym for ``-XPolyKinds -XDataKinds``).

* Introduce a new extension ``-XStarIsType`` to control how to parse ``*`` in code
  and whether to print it in error messages.

Motivation
------------

* This is a simplification over the status quo, with two closely related
  extensions (``-XPolyKinds`` and ``-XTypeInType``)
  and an arbitrary, historical distinction between them.

* GHC's ability to parse ``*`` has a significant cost. It's the only symbolic
  identifier that's handled like an alphanumeric one. (Note that ``T * Int`` normally
  looks like an application of a binary operator ``*`` to ``T`` and ``Int``, but with
  the kind ``*``, it's ``T`` applied to ``*`` and ``Int``.) Because ``*`` is sometimes
  indeed a binary operator in types (see ``GHC.TypeLits``), we can disambiguate the
  infix from the prefix case only in the renamer. This means that the parser has to
  treat a type-level expression ``A B * C * E`` essentially as a list of types, only
  to be rejigged by the renamer. (This rejigging is independent of the fixity-rejigging
  the renamer also has to do, so there's no shared cost here.)
  GHC also has to handle both ``*`` and its unicode
  variant identically, adding to this cost.

  The new approach to handling ``*`` makes it obvious in the *parser* whether ``*`` is
  infix or not, vastly simplifying matters. Also, by using an extension to specify
  the handling of ``*``, it is straightforward also to have the same extension control
  output in error messages.

* If we plan to remove ``*`` from the language at some point, we should start updating
  error messages sooner than later.

* In truth, GHC always has ``Type :: Type``, whether you say ``-XTypeInType``
  or no. Thus, the real extension name should be ``-XPolyKinds``, because it's
  kind polymorphism that the user wants, not the always-true ``Type :: Type``.

* The reason for a distinction between the extensions was because
  ``-XTypeInType`` started out as rather buggy and experimental, whereas
  ``-XPolyKinds`` and ``-XDataKinds`` had settled down by GHC 8. There was the possibility that
  ``-XTypeInType`` would allow you to shoot the gorillas (my suggestion for an
  update of "launch the rockets"; the latter seems just a bit too poignant
  these days) while ``-XPolyKinds -XDataKinds`` wouldn't. That possibility has not come to
  fruition (happily), and so the distinction isn't really paying its way.
  Note that what we're doing here is very much like the merger between ``-XRankNTypes`` and ``-XRank2Types``.

  The one difference between ``-XPolyKinds`` and ``-XTypeInType`` that's worth preserving
  is that the former allows easy access to the kind ``*``. The ``-XStarIsType`` extension
  is meant to preserve this difference.

Proposed Change Specification
-----------------------------

1. Make ``-XPolyKinds`` incoporate the new features (other than expanded promotion of
   types) of ``-XTypeInType``. By scanning through GHC's source code, I was
   able to find the places where GHC currently distinguishes between these
   extensions (labeled for easy reference):

   a. The meaning of CUSK is slightly different between ``-XPolyKinds`` and ``-XTypeInType``.
      See the second bullet of `the CUSK section of the manual
      <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion>`_. The ``-XTypeInType`` behavior
      would be retained. Migrating from the ``-XPolyKinds`` behavior simply requires
      adding an explicit ``forall k`` in some cases.

   b. Type and kind vars can be freely mixed in ``-XTypeInType`` code. This change
      is fully backward compatible; no migration would be necessary.

   c. Various constructs can appear in kinds with ``-XTypeInType``, but not without.
      These include promoted lists, ``forall``, among others. This change is fully
      backward compatible; no migration would be necessary.

   e. With ``-XPolyKinds``, kind variables are assumed to have kind ``BOX`` (which
      has become ``Type``). ``-XTypeInType``, on the other hand, makes no assumption
      about the kind of a kind variable. This is a generalization over current
      behavior, which can potentially lead to trouble. I am unable to come up with
      an example, though.

   f. Kind-indexed GADTs are allowed with ``-XTypeInType``. These would now be allowed
      with ``-XGADTs -XPolyKinds`` only. This change is fully backward compatible;
      no migration would be necessary.

2. ``-XDataKinds`` would now promote GADTs and GADT constructors. This change is fully
   backward compatible; no migration would be necessary.

3. Two releases after this proposal is implemented, deprecate ``-XTypeInType``.

4. Introduce a new language extension ``-XStarIsType``, with the following behavior:

   a. ``-XStarIsType`` is on by default.

   b. When ``-XStarIsType`` is on, any occurrence of the symbol ``*`` in a type
      is treated as the kind of types with values. It is parsed similarly to alphanumeric
      identifiers, never as a binary operator.

   c. When ``-XStarIsType`` is on, a user can use a binary operator ``*`` only
      with a qualifying module name. For example, ``8 ~ (4 GHC.TypeLits.* 2)``, or
      ``8 ~ (4 L.* 2)`` if we have ``import GHC.TypeLits as L``.

   d. When ``-XStarIsType`` is not on, the pretty-printer will print ``Type``
      instead of ``*`` in error messages.

   e. Without ``-XStarIsType``, there is no way to use the symbol ``*`` to
      refer to the kind of types with values. Use ``Type`` (which can be
      imported from ``Data.Kind``) instead. The symbol ``*`` will refer to any
      type-level binary operator ``*`` in scope, according to the normal
      scoping rules. (If ``-XTypeOperators`` is not in effect, use of ``*`` in
      a type will be an error.)

   The ``-XStarIsType`` idea is due to David Feuer, @treeowl.

Effect and Interactions
-----------------------

* One way to understand the changes to ``*`` is this:

  Currently, GHC follows this process to determine what a ``*`` in a type-level
  context means:

  1. If ``-XTypeInType`` is in effect:

     a. If the use of ``*`` refers to ``Data.Kind.*``, then parse it as an
	alphanumeric identifier; it means ``Type``.
     b. If ``*`` refers to some other type, it is a binary operator.

  2. If ``-XTypeInType`` is not in effect:

     a. If the use of ``*`` is in a context that is syntactically understood
	to be a kind, ``*`` is parsed as an alphanumeric identifier and means
	``Type``.
     b. Otherwise, it is a binary operator.

  Under this proposal, this is all simplified to this:

  1. If ``-XStarIsType`` is in effect, ``*`` is parsed as an alphanumeric
     identifier and means ``Type``.
  2. Otherwise, ``*`` is a binary operator.

  Much simpler!

* Note that the design of this proposal conforms to the three-release policy,
  in that users will not need to use CPP to avoid warnings. (In particular,
  note that ``import Data.Kind`` is always a fine thing to do, even without
  ``-XTypeInType``.)

* This proposal paves the way for future proposals relating to type-level features.
  Specifically, implementing this will make it possible to treat kind-variable
  scoping the same way we do type-variable scoping, as proposed in `#103`_.

.. _`#103`: https://github.com/ghc-proposals/ghc-proposals/pull/103

* Migration path: For most users, no migration will be necessary.
  The exception will be those programs which define or use ``*`` as a type family.
  Currently working code like:

  ::

    {-# LANGUAGE KindSignatures, DataKinds, TypeOperators #-}
    import Data.Proxy
    import GHC.TypeLits

    mult :: forall (m :: Nat) (n :: Nat). Proxy m -> Proxy n -> Proxy (m * n)
    mult _ _ = Proxy

  will need to explicitly enable ``-XNoStarIsType``.
  See `Have TypeOperators imply NoStarIsType`_ for an alternative.

Costs and Drawbacks
-------------------

* This is a simplification to the implementation and description of GHC. Hooray!

* This will effectively create two different versions of ``-XPolyKinds`` and ``-XDataKinds``,
  which could be problematic for users who want tooling to choose compilers
  based on extension names. Is this a problem in practice? I don't know. Even
  without this change, ``-XPolyKinds`` evolved significantly during the GHC 7
  releases, as do various other extensions, so users already have to resort to
  measures other that just looking at extensions when choosing a compiler
  version.

* Modules that use ``*`` both as a binary operator and as the kind of types with
  values will have to be updated to use ``Type`` instead, as imported from ``Data.Kind``.
  This change is backward compatible to GHC 8.0. (Alternatively, they could
  use ``-XStarIsType`` and fully-qualify their uses of the binary operator ``*``.)

Alternatives
------------

1. Come up with a new extension name that encompasses both ``-XTypeInType`` and
   ``-XPolyKinds``. All three would be synonymous.

2. Live with the status quo, with quite a bit of code in GHC to support it.

3. Do not support fully-qualified uses of the binary operator ``*`` when ``-XStarIsType``
   is in effect. Under this alternative, users would have no workaround to access the
   binary operator ``*`` with ``-XStarIsType``.

4. Introduce a new extension ``-XTypeColonOperators``, which allows only
   those type-level operators that begin with a ``:``, conveniently working with
   ``Data.Type.Equality`` and ``GHC.Generics``. This new extension would not
   disable ``-XStarIsType``, as the two don't conflict.

   I personally do not think this addition is worth it, but it was suggested
   on the pull request.

5. Report ``Type`` in error messages, regardless of whether ``-XStarIsType`` is enabled.
   An advantage here is that I think ``Type`` is easier to understand than ``*``:
   just about everyone whom I've taught about kinds gets
   very confused about the name ``*``, thinking that ``*`` is some kind of universal
   kind that encompasses all other kinds. (Indeed, I thought this, too, once upon a
   time.)

   This alternative has two noteworthy drawbacks:

   * There are gobs of resources that use ``*``. These would all go out of date.

   * The Haskell Reports mention ``*`` by name. If error messages print ``Type`` instead
     of ``*``, we'll be further from the behavior that the Report authors intended at the
     time. However, as the Reports do not specify error message text, this change does
     not bring us further from formal compliance to the letter of the Report. It would bring
     us further from the spirit of the Report.

.. |star| unicode:: U+2605 .. unicode star

6. Currently, and in this proposal, both ``*`` and its unicode variant |star| are
   treated identically. One way to have our cake and eat it too is to follow the plan
   above for ``*`` but force |star| to always lex as an alphanumeric identifier
   (the way ``*``\-as-``Type`` lexes now). That way, folks who are really wedded
   to using a star can still do so. This would not be backwards compatible, because
   anyone who uses |star| as a type-level infix operator would have to change the
   name of their operator; there would be no way to use |star| infix (without
   backquotes, as usual).

7. Introduce a new way of writing ``Type``: ``type``. That is, the keyword ``type``
   would be the kind of types with values. We could say ``class Monad (m :: type -> type)``.
   This has the advantage that clients do not need to import anything, as we could
   make ``type`` always in scope (as it is a keyword). Furthermore, existing tools
   already apply syntax highlighting to ``type``, which I think is reasonable.
   Disadvantages include the fact that ``type`` will look like a type variable without
   syntax highlighting enabled, this is a new change to an area that has already undergone
   some disruptive changes, and it has been mentioned previously and rejected. But I
   still like it, so I'm mentioning it here.

   EDIT: After further discussion, I'm less enamored of this idea, for two reasons:

   1. This ``type`` would simply be a type synonym for ``TYPE LiftedRep``, as it is
      today. It's quite strange to have a keyword be an ordinary type synonym.

   2. @nomeata pointed out (offline discussion) that currently, ``type`` is used in
      export lists to denote a namespace. It's quite possible that its use as a
      namespace selector might grow in the future, and using ``type`` to mean ``Type``
      would preclude this.

   I find both arguments compelling independently, and so I withdraw support for this
   alternative. Nevertheless, I'm keeping it in the proposal in case someone wants to
   argue in support of it.

Have TypeOperators imply NoStarIsType
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One alternative is to have an additional 4.f step in the Proposed Change Specification section:

   f. For two releases, ``-XTypeOperators`` will imply ``-XNoStarIsType``, to
      provide a migration path for code that uses the binary operator ``*``. (After
      two releases, this code can include ``-XNoStarIsType`` explicitly without
      going against the three-release policy.) Users can re-enable ``-XStarIsType``
      after ``-XTypeOperators`` is enabled if they wish.

This alternative is problematic. It's intended to help migration, but
implementation evidence shows it causes more trouble.  If the code being
migrated only enables ``-XTypeOperators``, then 4.f will indeed be helpful, but
there is a lot of code which also enables ``-XKindSignatures`` in which
``-XNoStarInType`` changes the semantics. For example, this code from the
``hashable`` library

::

    newtype  Tagged  (s :: * -> *) = Tagged {unTagged :: Int}

would fail to compile with the following error:

::

  Data/Hashable/Generic.hs:116:22: error:
      Operator applied to too few arguments: *
      With NoStarIsType (implied by TypeOperators), ‘*’ is treated as a regular type operator.
      Did you mean to use ‘Type’ from Data.Kind instead?
      |
  116 | newtype Tagged (s :: * -> *) = Tagged {unTagged :: Int}
      |

Many other libraries in the wild simultaneously enable ``-XTypeOperators`` and use
``*`` as a kind, including servant, aeson, cereal, cassava, and others, so
including 4.f would break all of these libraries. While not having 4.f also
results in some breakage, far fewer libraries in the wild break without 4.f
than with 4.f, so this was ultimately decided against.

If ``-XTypeOperators`` were to imply ``-XNoStarIsType``, then any code which
uses ``*`` as kind instead of a binary operator would have to migrate somehow.
Two migration options are:

1. Declare ``-XStarIsType``. If they ever
   use ``*`` as a binary operator, those uses would have to be qualified
   with a module prefix.

2. Import ``Type`` from ``Data.Kind`` and change uses of ``*`` to ``Type``.
   If they already have a ``Type`` in scope, they may have to use qualified
   imports, etc.

Unresolved questions
--------------------

* Is this the right deprecation schedule? Is it moving too fast?

* What is the educational impact of this proposal? Specifically,
  ``-XPolyKinds`` is now bigger and harder to learn. On the other hand, the
  previous implementation of ``-XPolyKinds`` has some restrictions that may
  not have been obvious to users.

* What do we want the long-term future of ``*`` to be? I favor removing
  it after a long time (> 5 years). But deciding now what we want to have in the distant
  future can influence decisions made in the meantime. One particular decision: should
  ``-Wcompat`` warn on uses of ``*`` as ``Type``? Relatedly, should there be a plan
  to deprecate ``-XStarIsType``?

* Regardless of the long-term future of ``*``, is the migration path described around
  ``-XStarIsType`` the best possible path? Notably, the current migration path will cause
  breakage in ``-XTypeOperators`` code that uses ``*`` as a kind, requiring users to
  change all uses of ``*`` to ``Type`` when upgrading GHC. David Feuer has expressed unease
  at the migration path detailed here, but his counter-suggestion remains unclear to me.
  I am not without unease myself, but I don't see a better way.

Summary of discussion
---------------------
Much (just about all, really) of the discussion surrounds the future of ``*``. I've made
my case in the comments for eventually deprecating and removing it, though I've been
convinced by the ``-XStarIsType`` plan (which grew out of the discussion) that supporting
``*`` into perpetuity isn't so terrible, and that we should plan to keep it around for
years more. One vocal participant, @AntC2, has strenuously objected to any move toward
removing ``*``, but their points have not been echoed by anyone else in the discussion.
In particular, @AntC2 is worried about rotting of educational resources, something I was
perhaps too glib about in earlier versions of this proposal. I expect the committee will
carry on this debate, and I'm happy to submit to the view of the committee on this matter.

Other discussion concerns the details of the migration path and the ``-XStarIsType`` aspect
of this proposal, briefly summarized in the last unresolved question, above.

Implementation Plan
-------------------
I or a close collaborator volunteers to implement.
