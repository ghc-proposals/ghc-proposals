.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/65>`_.

.. contents::

Require namespacing fixity declarations for type names
======================================================

GHC allows infix names at the type level using the ``TypeOperators`` extension. However, GHC's mechanism for providing fixity declarations for such infix type names is rather deficient. I propose to tighten up the implementation by allowing users to indicate that an fixity declaration should specifically be for an type name by typing out an explicit ``type`` namespace. I also propose deprecating the current, ambiguous mechanism for providing fixity declarations for type names and eventually requiring the use of the ``type`` namespace for all fixity declarations for type names in the future.


Motivation
------------
Currently, one can specify a fixity declaration for a name at the value level like so:

.. code-block:: haskell

    ($) :: (a -> b) -> a -> b
    f $ x = f x

    infixr 0 $

With the `TypeOperators` extension, one can also use the same syntax for specifying fixity declarations for names at the type level:

.. code-block:: haskell

    {-# LANGUAGE TypeOperators #-}

    type f $ x = f x

    infixr 0 $

However, what happens when a fixity declaration refers to a name that is used at both the value and the type level, as in the example below?

.. code-block:: haskell

    {-# LANGUAGE TypeOperators #-}

    ($) :: (a -> b) -> a -> b
    f $ x = f x

    type f $ x = f x

    infixr 0 $

It turns out that GHC applies to the same fixity (``infixr 0``) to both the value-level and type-level ``$`` names. It accomplishes this step in the renamer by effectively duplicating the fixity declaration. One can imagine this as the result of renaming, using some not-quite-legal Haskell syntax to denote the internal unique value of each name (``1`` and ``2``):

.. code-block:: haskell

    {-# LANGUAGE TypeOperators #-}

    ($_1) :: (a -> b) -> a -> b
    f $_1 x = f x

    type f $_2 x = f x

    infixr 0 $_1, $_2

This strategy is unsatisfying for a couple of reasons, however.

1. As long as the value-level and type-level ``$`` are defined in the same module, it is impossible to give them different fixities. (In this example that's perhaps not so bad, but one might imagine defining a type-level ``$`` that has no relationship to the value-level ``$``.)
2. It's buggy. What happens if you try to use the previous example in a Template Haskell quote?

   .. code-block:: haskell

       {-# LANGUAGE TemplateHaskell #-}
       {-# LANGUAGE TypeOperators #-}

       $([d| ($) :: (a -> b) -> a -> b
             f $ x = f x

             type f $ x = f x

             infixr 0 $
           |])

   During splicing, Template Haskell will rename the quoted declarations, convert them to a Template Haskell AST, turn that back into Haskell surface syntax, and pass it through to the renamer (and the rest of the compilation pipeline). But recall that when ``infixr 0 $`` is renamed, it is effectively turned into ``infixr 0 $_1, $_2``. After going through the Template Haskell AST, the renamer sees the declaration ``infixr 0 $_1, $_2`` and rejects it, because it believes that ``$_1`` and ``$_2`` are duplicate names! (Recall that ``$_1`` and ``$_2`` both refer to the name ``$``, but with different internal uniques.) This is the subject of `GHC Trac #14032 <https://ghc.haskell.org/trac/ghc/ticket/14032>`_.

Proposed Change Specification
-----------------------------
I propose two major changes: a modification to fixity declaration syntax that is specific to type-level names, and a plan to phase out the old way of specifying fixities for infix type-level names in favor of the new syntax.

I propose a new form of fixity declaration:

.. code-block:: haskell

    infixr 0 type $

The grammar for this declaration is exactly the same as normal ``infix`` declarations, except with the ``type`` keyword inserted between the precedence and the comma-separated list of infix names. The semantics of this declaration is to give ``$`` a fixity of ``infixr 0``, and moreover, it checks to see if ``$`` lives in the type namespace, giving an error if it does not. This declaration is only permitted if the ``TypeOperators`` extension is enabled.

``infix{l,r} n type`` would be applicable to type families, type classes, data types, and type synonyms. One type-level construct that ``infix{l,r} n type`` would not be applicable to is promoted data constructors, as promoted data constructors simply inherit the fixity of the original, unpromoted data constructor (at the value level).

The eventual goal is to make ``infix{l,r} n type`` the only means by which one can specify the fixity of type-level names, and to make ``infix{l,r} n`` declarations (without the ``type``) only applicable to value-level names. To this end, I propose following the `three-release policy <https://prime.haskell.org/wiki/Libraries/3-Release-Policy>`_:

* In the first version of GHC in which this proposal is implemented, introduce ``infix{l,r} n type``, but retain ``infix{l,r} n``'s ability to refer to both value-level and type-level names.
* In the subsequent major release of GHC, have ``infix{l,r} n`` emit a warning whenever it refers to type-level names.
* In the subsequent major release of GHC after that, have ``infix{l,r} n`` error whenever it refers to type-level names.

Once ``infix{l,r} n type`` is introduced, GHC will have an unambiguous way of specifying fixity declarations for names in both namespaces, and it will also work when quoted in Template Haskell, fixing Trac #14032.

Effect and Interactions
-----------------------
This proposal presents an opportunity to simplify code in the renamer, as there will no longer be a need to hackily rename, for instance, ``infixr 0 $`` to ``infixr 0 $_1, $_2``.

Costs and Drawbacks
-------------------
This will involve a deprecation/breakage cycle, so there will inevitably be some pain in having everyone transition their code over to the new style. My hope is that the proposed GHC warnings will help ease this transition.

These changes will mildly complicate the parser, as this give the ``type`` keyword meaning in more places. However, I don't anticipate the necessary changes being unreasonable.

Alternatives
------------
Instead of introducing a new ``infix{l,r} n type`` syntax, we could change the renamer to be smarter about ``infix{l,r} n`` declarations from Template Haskell quotes. But this only puts a band-aid over the wound, as there is no guarantee that the old ``infix{l,r} n`` semantics won't break somewhere else. (I certainly wouldn't be surprised if there were more lurking bugs because of this.) Moreover, there'd still be the problem that users cannot assign different fixities to names that live at the value level and the type level in the same module.

There is some amount of bikeshedding to be had concerning the new syntax. One could alternatively envision the ``type`` keyword being placed in front (i.e., ``type infix{l,r} n``). However, I slightly prefer putting ``infix{l,r}`` first, since it makes it clearer that we're dealing with a fixity declaration.

Instead of co-opting the ``TypeOperators`` keyword, we could invent a new ``LANGUAGE`` pragma for this purpose. I personally don't feel like this is necessary, since we're simply extending the capabilities of type-level operators (which is already a GHC extension), but others may feel differently.

Unresolved questions
--------------------

Implementation Plan
-------------------
I volunteer to implement.
