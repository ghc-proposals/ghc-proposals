Require namespacing fixity declarations for type names and ``WARNING``/``DEPRECATED`` pragmas
=============================================================================================

.. author:: Ryan Scott
.. date-accepted:: 2017-11-03
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/14032
.. implemented:: None yet
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/65>`_.
.. contents::

GHC allows infix names at the type level using the ``TypeOperators`` extension. However, GHC's mechanism for providing fixity declarations for such infix type names—or to deprecate infix type names—is rather deficient. The same syntax is used for declare fixities and to deprecate names for both value-level and type-level names. I propose to tighten up the implementation by allowing users to indicate that an fixity declaration or a ``WARNING``/``DEPRECATED`` pragma should specifically be for a value or type name by typing out an explicit ``value`` or ``type`` namespace. I also propose deprecating the current, ambiguous mechanism for providing fixity declarations/``WARNING`` pragmas/``DEPRECATED`` pragmas for type names and eventually requiring the use of the ``type`` namespace for all fixity declarations/``WARNING`` pragmas/``DEPRECATED`` pragmas for type names in the future.


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

   During splicing, Template Haskell will rename the quoted declarations, convert them to a Template Haskell AST, turn that back into Haskell surface syntax, and pass it through to the renamer (and the rest of the compilation pipeline). But recall that when ``infixr 0 $`` is renamed, it is effectively turned into ``infixr 0 $_1, $_2``. After going through the Template Haskell AST, the renamer sees the declaration ``infixr 0 $_1, $_2`` and rejects it, because it believes that ``$_1`` and ``$_2`` are duplicate names! (Recall that ``$_1`` and ``$_2`` both refer to the name ``$``, but with different internal uniques.) This is the subject of `GHC #14032 <https://gitlab.haskell.org/ghc/ghc/issues/14032>`_.

   The exact same problems that afflict fixity declarations also afflict ``WARNING`` pragmas (as well as ``DEPRECATED`` pragmas, which accomplish the same thing, so I'll refer to them henceforth as just ``WARNING`` pragmas), as they have a similarly ambiguous semantics surrounding infix type names.

Proposed Change Specification
-----------------------------
I propose two major changes: a modification to the syntax to allow optional ``value`` and ``type`` namespaces in fixity declarations and ``WARNING`` pragmas, and a plan to phase out the old way of deprecating and specifying fixities for infix type-level names (without the ``type`` namespace) in favor of the new syntax (where the ``type`` namespace would be required).

Syntax extension
~~~~~~~~~~~~~~~~

I propose an extension to the existing fixity declaration syntax:

.. code-block:: haskell

    -- Current syntax
    infixr 0 $, *, ^

    -- New syntax, for value-level names
    infixr 0 value $, *, ^

    -- New syntax, for type-level names
    infixr 0 type $, *, ^

The only difference from the current syntax is the presence of a namespace keyword (``value`` or ``type``) inserted between the precedence and the comma-separated list of infix names. The semantics of a ``infixr 0 value $`` declaration is to give the value-level ``$`` (and not the type-level ``$``, if one is also declared) a fixity of ``infixr 0``. If ``$`` is not declared in the value namespace, an error is thrown. (Similarly, ``infixr 0 type $`` applies only to the type-level ``$``, and errors if ``$`` is not declared in the type namespace.) This declaration is only permitted if the ``ExplicitNamespaces`` extension is enabled.

``infix{l,r} n value`` would be applicable to all value-level names (top-level functions, class methods, data constructors, and pattern synonyms).

``infix{l,r} n type`` would be applicable to most type-level names (type families, type classes, data types, and type synonyms).

I also propose a similar change to the existing ``WARNING`` pragma syntax:

.. code-block:: haskell

    -- Current syntax
    {-# WARNING ($) "Es muy peligroso" #-}

    -- New syntax, for value-level names
    {-# WARNING value ($) "Es muy peligroso" #-}

    -- New syntax, for type-level names
    {-# WARNING type ($) "Es muy peligroso" #-}

Aside: promoted data constructors (and other promoted things)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One type-level construct that ``infix{l,r} n type`` would not be applicable to is promoted data constructors. The reason is that promoted data constructor names shouldn't be thought of as separate from the original data constructor names, but rather the same names being used in a different context. For this reason, promoted data constructors simply inherit the fixity of the original, unpromoted data constructor (at the value level), so if a user specifies ``infixr 0 value Foo``, then both the constructor ``Foo`` and its promoted counterpart ``'Foo`` will be ``infixr 0``.

For the time being, data constructors are the only named construct in Haskell that can be used in multiple contexts like this. In the future (perhaps in work related to Dependent Haskell), it is conceivable that there will be other value-level constructs that can also be used at the type level. If this were to happen, I would advise following a similar principle of only allowing these constructs to have their fixity specified with ``infix{l,r} n value``, and to have uses of these constructs at the type level inherit their value-level fixities.

Migration plan
~~~~~~~~~~~~~~

The eventual goal is to make ``infix{l,r} n type`` the only means by which one can specify the fixity of type-level names, and to make ``infix{l,r} n`` declarations (without the ``type``) only applicable to value-level names. To this end, I propose following the plan (which adheres to the `three-release policy <https://prime.haskell.org/wiki/Libraries/3-Release-Policy>`_):

* Introduce ``infix{l,r} n value`` and ``infix{l,r} n type`` in an upcoming GHC version. (Call this GHC 8.X). Retain ``infix{l,r} n``'s ability to refer to both value-level and type-level names.
* In GHC 8.(X+4), have ``infix{l,r} n`` emit a warning whenever it refers to type-level names. Here is the plan for when to emit warnings:

  * If an ``infix{l,r} n`` declaration refers to exclusively to a type-level name (that is, either there is no value with the same name that is also declared, or there a value with the same name has its fixity declared separately with ``infix{l,r} n value``), warn that the user should change it to ``infix{l,r} n type``. This is a straightforward case, as this would become an error in GHC 8.(X+6).
  * If an ``infix{l,r} n`` declaration refers to both a value-level and type-level name (that is, there are no other ``infix{l,r} n value`` or ``infix{l,r} n type`` declarations referring to the same name), things are a bit trickier. There are two scenarios under which this could happen. One is when a user inadvertently assigned a fixity to a type-level name, such as in this example: ::

        {-# LANGUAGE TypeOperators #-}
        module A where

        infixr 0 $

        ($) :: (a -> b) -> a -> b
        f $ x = f x

        type f $ x = f x

    Here, the user only meant to assign a fixity to the value-level ``($)`` fixity, and doesn't care about the fixity of the type-level ``($)``. This situation could be addressed by converting the existing fixity declaration to ``infixr 0 value $``.

    It should be noted, however, that the code above is not wrong, and would compile in GHC 8.(X+6). However, we still should warn when we see code like this, because of the other scenario: it is possible that the user really did mean to assign the type-level ``($)`` a fixity. Even worse, the place where the fixity matters might be in an entirely different module: ::

        {-# LANGUAGE TypeOperators #-}
        module B where

        import A

        type MaybeMaybeInt = Maybe $ Maybe $ Int

    The code in module ``B`` will only compile if the type-level ``($)`` is right-associative. This means that the warning we emit when we see the code in module ``A`` should account for such a scenario.

    To encompass both use cases, I propose that the warning read approximately as follows: ::

      warning:
        * 'infixr 0 $' refers to both a value-level and a type-level name '$'
        * In GHC 8.(X+6), 'infixr 0 $' will only assign 'infixr 0' to the value-level '$'
        * If you intended this, use 'infixr 0 value $' instead
        * If you want the type-level '$' to also be 'infixr 0', add a 'infixr 0 type $' declaration

* In GHC 8.(X+6) have ``infix{l,r} n`` error whenever it refers exclusively to a type-level name.

Once ``infix{l,r} n type`` is introduced, GHC will have an unambiguous way of specifying fixity declarations for names in both namespaces, and it will also work when quoted in Template Haskell, fixing #14032.

A similar warning mechanism/migration plan would need to be put in place for ``WARNING`` pragmas as well (ironically enough, we'd have to put warnings on ``WARNING`` pragmas!)

Effect and Interactions
-----------------------
This proposal presents an opportunity to simplify code in the renamer, as there will no longer be a need to hackily rename, for instance, ``infixr 0 $`` to ``infixr 0 $_1, $_2``.

Costs and Drawbacks
-------------------
This will involve a deprecation/breakage cycle, so there will inevitably be some pain in having everyone transition their code over to the new style. My hope is that the proposed GHC warnings will help ease this transition.

These changes will mildly complicate the parser. However, I don't anticipate the necessary changes being unreasonable.

Alternatives
------------
Instead of introducing a new ``infix{l,r} n type`` syntax, we could change the renamer to be smarter about ``infix{l,r} n`` declarations from Template Haskell quotes. But this only puts a band-aid over the wound, as there is no guarantee that the old ``infix{l,r} n`` semantics won't break somewhere else. (I certainly wouldn't be surprised if there were more lurking bugs because of this.) Moreover, there'd still be the problem that users cannot assign different fixities to names that live at the value level and the type level in the same module.

There is some amount of bikeshedding to be had concerning the new syntax. One could alternatively envision the ``type`` keyword being placed in front (i.e., ``type infix{l,r} n``). However, I slightly prefer putting ``infix{l,r}`` first, since it makes it clearer that we're dealing with a fixity declaration.

Instead of co-opting the ``ExplicitNamespaces`` language extension, we could invent a new ``LANGUAGE`` pragma for this purpose. I personally don't feel like this is necessary, since we're simply extending the capabilities of namespace keywords (which is already a GHC extension), but others may feel differently.

Unresolved questions
--------------------

Implementation Plan
-------------------
