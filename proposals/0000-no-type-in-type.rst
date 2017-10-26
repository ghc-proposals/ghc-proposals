.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/83>`_.

.. contents::

Embrace ``Type :: Type``
==========================

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

* Use ``Type`` instead of ``*`` when referring to the kind of types with values (e.g.,
  the kind of ``Int``) in error messages.

* Introduce a new extension ``-XStarIsType`` to control how to parse ``*`` in code.

Motivation
------------

* This is a simplification over the status quo, with two closely related
  extensions (``-XPolyKinds`` and ``-XTypeInType``)
  and an arbitrary, historical distinction between them.

* GHC's ability to handle ``*`` has a significant cost. It's the only symbolic
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
  infix or not, vastly simplifying matters.

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

2. ``-XDataKinds`` would now promote GADTs and GADT constructors.
      
3. Two releases after this proposal is implemented, deprecate ``-XTypeInType``.
      
4. The pretty-printer will print ``Type`` instead of ``*`` in error messages.

5. Introduce a new language extension ``-XStarIsType``, with the following behavior:

   a. ``-XStarIsType`` is on by default.

   b. For two releases, ``-XTypeOperators`` will imply ``-XNoStarIsType``, to
      provide a migration path for code that uses the binary operator ``*``. (After
      two releases, this code can include ``-XNoStarIsType`` explicitly without
      going against the three-release policy.) Users can re-enable ``-XStarIsType``
      after ``-XTypeOperators`` is enabled if they wish.

   c. When ``-XStarIsType`` is on, any occurrence of the symbol ``*`` in a type
      is treated as the kind of types with values. It is parsed similarly to alphanumeric
      identifiers, never as a binary operator.

   d. When ``-XStarIsType`` is on, a user can use a binary operator ``*`` only
      with a qualifying module name. For example, ``8 ~ (4 GHC.TypeLits.* 2)``, or
      ``8 ~ (4 L.* 2)`` if we have ``import GHC.TypeLits as L``.

   e. Without ``-XStarIsType``, there is no way to use the symbol ``*`` to refer
      to the kind of types with values. Use ``Type`` instead. The symbol ``*`` will
      refer to any type-level binary operator ``*`` in scope, according to the
      normal scoping rules. (If ``-XTypeOperators`` is not in effect, use of ``*``
      in a type will be an error.)

   The ``-XStarIsType`` idea is due to David Feuer, @treeowl.

Effect and Interactions
-----------------------

* Note that the design of this proposal conforms to the three-release policy,
  in that users will not need to use CPP to avoid warnings. (In particular,
  note that ``import Data.Kind`` is always a fine thing to do, even without
  ``-XTypeInType``.)
  
Costs and Drawbacks
-------------------

* This is a simplification to the implementation and description of GHC. Hooray!

* There are gobs of resources that use ``*``. These would all go out of date. This
  fact makes me sad. However, just about everyone whom I've taught about kinds gets
  very confused about the name ``*``, thinking that ``*`` is some kind of universal
  kind that encompasses all other kinds. (Indeed, I thought this, too, once upon a
  time.)

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

5. Continue to output ``*`` in error messages when ``-XStarIsType`` is enabled.

   This alternative has the very real benefit of conforming to existing educational
   materials. However, my own experience is that the name ``*`` is confusing (leading
   newer Haskellers to think it is some kind of wildcard). I would like to work toward
   a future where ``*`` is removed from the language, and changing error messages
   is one step in that direction.

Unresolved questions
--------------------

* Is this the right deprecation schedule? Is it moving too fast?

* What is the educational impact of this proposal? I see problems along at least two
  different dimensions:

  a. ``-XPolyKinds`` is now bigger and harder to learn. On the other hand, the previous
     implementation of ``-XPolyKinds`` has some restrictions that may not have been
     obvious to users.

  b. Moving away from ``*`` as the kind of types disagrees with educational literature.

Implementation Plan
-------------------
I or a close collaborator volunteers to implement.
