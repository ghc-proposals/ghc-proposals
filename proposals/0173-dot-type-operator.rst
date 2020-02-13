The dot type operator
=====================

.. author:: Vladislav Zavialov
.. date-accepted:: 2018-11-05
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/merge_requests/363
.. implemented:: 8.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/173>`_.
.. contents::

The ``TypeOperators`` language extension allows using symbols in type names,
similarly to operators in terms. However, some operator names that are valid in
terms are not valid in types, including the dot operator ``(.)`` commonly used
to denote function composition. We propose to lift this restriction.

Motivation
------------

At the moment, we cannot define function composition at type level::

  ghci> type (f . g) x = f (g x)

  <interactive>:1:9: error:
      Illegal symbol '.' in type
      Perhaps you intended to use RankNTypes or a similar language
      extension to enable explicit-forall syntax: forall <tvs>. <type>

There are two issues with this definition:

1. The dot is not a valid type operator.
2. The type synonyms and type families cannot be partially applied, severely
   limiting the usability of type-level function composition.

In this proposal, we seek to address the first issue. The second issue is out
of scope, but can be addressed with defunctionalization today or by allowing
unsaturated use type functions in the future.

With a small modification to the Haskell grammar, the following code is
accepted::

  ghci> :set -XTypeOperators
  ghci> type (f . g) x = f (g x)
  ghci> Nothing :: (Maybe . Either Int) Bool
  Nothing
  ghci> Just (Left 5) :: (Maybe . Either Int) Bool
  Just (Left 5)

We can achieve this by changing a single production in ``Parser.y``, namely
``tyapp``::

  -   | tyvarop          { sL1 $1 $ TyElOpr (unLoc $1) }
  +   | '`' tyvarid '`'  { sLL $1 $> $ TyElOpr (unLoc $2) }
  +   | '.'              { sL1 $1 $ TyElOpr (mkUnqual tcClsName (fsLit ".")) }

Note that the actual implementation will be more complex to provide decent
error messages, keep `annotations
<https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations>`_ working, etc.

We can use whitespace to disambiguate between uses of the dot as a type
operator and as a part of a qualified name. The situation is the same as in
terms, demonstrated by this table (courtesy of `@mstksg
<https://github.com/mstksg>`_)::

  foo . bar -- term level: (.) foo bar
            -- type level: (.) foo bar (currently disallowed)

  foo.bar   -- term level: (.) foo bar
            -- type level: (.) foo bar (currently disallowed)

  Foo . Bar -- term level: (.) Foo Bar
            -- type level: (.) Foo Bar (currently disallowed)

  Foo.Bar   -- term level: Bar imported from module Foo
            -- type level: Bar imported from module Foo

In the ``singletons`` library, there is a `special case
<https://github.com/goldfirere/singletons/blob/a9db6ff634d00a11a74595187e4ed935715f6626/src/Data/Singletons/Names.hs#L355-L361>`_
for promotion of ``(.)`` and ``(!)``::

    Note [Special cases for (.) and (!)]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Almost every infix value name can be promoted trivially. For example, (+)
    works both at the value- and type-level. The two exceptions to this rule
    are (.) and (!), which we promote to the special type names (:.) and (:!),
    respectively.  This is necessary since one cannot define or apply (.) or
    (!) at the type level -- they simply won't parse. Bummer.

The issue with ``(!)`` has been fixed in the upcoming GHC 8.8, but the problem
of ``(.)`` remains.

Proposed Change Specification
-----------------------------

Allow the use of the ``(.)`` type operator in all places where other type
operators are legal.

Effect and Interactions
-----------------------

Clasess, data types, data families, type synonyms, and type families can now be
given the name ``(.)``.

There are no known conflicts with other language features despite the use of
the dot in type-level constructs like ``forall <tvs>. <type>``. The parser has
enough information to disambiguate in all cases.

Costs and Drawbacks
-------------------

None.

Alternatives
------------

* Continue to disallow the use of the dot operator in types.
* Find a different use for the dot syntax inconsistent with terms.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

I (Vladislav Zavialov) will implement this change.

