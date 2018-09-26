The dot type operator
=====================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/173>`_.
.. sectnum::
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

