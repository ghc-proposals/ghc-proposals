====================
ForUnique Quantifier
====================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/684>`_.
.. sectnum::
.. contents::

.. _`#448`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst
.. _`#448rd`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0448-type-variable-scoping.html


This proposal introduces ``forunique`` quantifier existentials into GHC, which grab external type variables

Motivation
----------

To write some trivial functions ``ScopedTypeVariables`` extension is needed, which adds implicit rules how to read type signatures!

And this is a bit unhandy to use in language which has a huge system of types.

This Proposal suggest to add the simple **Forunique Quantifier** which allows to write explicitly type signatures, which depends from external type variables.

Explicitness is preferential in Haskell over implicitness. And this Proposal propose how to write ``forunique`` quantifier explicitly!

Just like ``ExplicitForAll`` allow explicitly say exactly what this specific type variable is ``forall`` quantified, this Proposal allow explicitly say exactly what this specific type variable is ``forunique`` quantified!
 
Main alternative is "Modern Scoped Type Variables" `#448`_ (rendered `#448rd`_ ) which was added into ``ScopedTypeVariables`` extension.

``ScopedTypeVariables`` is *de facto* **Implicit Forunique** : Implicit rules to add a forunique quantifier to type variables if they are not explicitly quantified.

Also Alternative is ``PartialTypeSignatures`` extension, with opposite philosophy: compiler infer type not for holes, but for ``forunique`` quantified type variables.


Rule (aka math-like proof)
~~~~~~~~~~~~~~~~~~~~~~~~~~

Forunique Quantifier is a special case of Exists Quantifier, which is known during compile time.

::

  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forunique b. b ]

  f :: forall a. [a] -> [a]
  f xs = ys ++ ys
     where
       ys :: forunique b. [b]
       ys = reverse xs


**Math-like Proof:**

*if* ∀a: f a *then* ∀a: ∃₌₁b, b ∈ a : f b

But since SameRanked Existential Quantifier ``forunique`` cannot use same symbol "∃" (which could be used by NotSameRanked Existential Quantifier ``exists`` ), we use a different one: "∋" (Latin [Capital] Contains as Member)

*if* ∀a: f a *then* ∀a: ∋b, b ∈ a : f b

This Proposal suggest to call this quantifier ``forunique``, but there are also good alternative names, such as ``fosome`` and ``forone``. It represents uniqueness quantification.


Proposed Change Specification
-----------------------------

Forunique Quantifier "grab" type variables external to this signature in one-to-one corresponded order
::

  f :: forall a b. [a] -> [b] -> [(a, b)]
  f xs ys  = zip (xs :: forunique aa. [aa]) yys
     where
       yys :: forunique _ bb. [bb]
       yys = reverse ys

By using ``forunique a`` we ask do not create a new type variable ``forall a``, but use already existed external type variable ``a``.

1. Forunique type variable "grabs" type variables only

2. Forunique type variable "grabs" type variables external to this signature only

3. Forunique type variables are always written to one-to-one corresponded order to ``forall`` order (regardless if it is written explicitly or implicitly)

4. Forunique type variables could have own names (regardless from depended ``forall`` type variable names)

5. If Forunique type variable is unused in this signature it could be wildcarded

6. If Forunique type variable is unused in this signature it could be omitted iff it is placed after all used type variables

7. For nested dependencies (inside several ``where`` and ``let`` definitions) ``forunique`` uses comma to separate ``forall`` type variables from nearest to farthest variables

8. Two different Forunique type variables in same signature cannot "grab" the same external type variable

9. In one signature Forunique type variables cannot have same names as Forall type variables is the same signature

10. All or nothing rule to be backward compatible with ``ScopedTypeVariables`` extension


Extension
~~~~~~~~~

Introduce a new extension ``-XForuniqueQuantifier`` .

With ``-XForuniqueQuantifier``, ``forunique`` is a keyword in both types and terms.

Syntax
~~~~~~

Syntax of ``forunique`` quantifier has a simple form.

::

  forunique a1 a2 a3. 

  forunique a1 a2 a3, a4 a5, a6 a7. 

It says that type variables ``a1, a2, a3`` are renamed type variables only external to this signature, not a new ones.

For nested dependencies ``forunique`` use comma to separate ``forall`` type variables from nearest to farthest variables.


Expansion to Core Language
~~~~~~~~~~~~~~~~~~~~~~~~~~

``forunique`` quantifier just renames "grabbed" external type variables to one-to-one corresponded order. 
	  
So, it is very easy for Compiler to calculates real external type variable and replace it.


Grammar
~~~~~~~

The grammar is modified as follows (baseline: GHC's parser)::

        ctype → quantifiers_telescope ctype   -- NEW!
              -- forall_telescope ctype       -- REMOVE!
              | ...

        quantifiers_telescope → forunique_telescope forall_telescope  -- NEW!
		
        -- just for comparison
        forall_telescope → 'forall' tv_bndrs '.'
                         | 'forall' tv_bndrs '->'
                         | {- empty -}

        -- NEW!
        forunique_telescope → 'forunique' tv_bndrs {',' tv_bndrs}* '.'
                          | {- empty -}


Examples
--------

Almost every example from  "Modern Scoped Type Variables" `#448`_ (rendered `#448rd`_ ) could be used with ``forunique``
::

  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forunique a. a ]   -- OK

  f = runST ( (op >>= \(x :: forunique s. STRef s Int) -> g x) :: forall s. ST s Bool )

  g (x:: forunique a. a) = x


  data T = forall a. MkT [a] (a -> Int)

  f :: T -> [Int]
  f (MkT (xs :: forunique a. [a]) f) = 
                        let mf :: forunique a. [a] -> [Int]
                            mf = map f
                        in mf xs


  class C a where
    op :: [a] -> a

    op xs = let ys:: forunique a. [a]
                ys = reverse xs
            in
            head ys
		  
  instance C b => C [b] where
    op xs = reverse (head (xs :: forunique b. [[b]]))


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

∋ symbol
^^^^^^^^^

Why "∋"?

1. ∋ ( ``∋`` , [Capital] Contains as Member, U+220B) is a "normal" symbol in Mathematical Operators section of Unicode, like ∀ and ∃.

2. Reason of supporting: ∋(U+220B) was added in 1.1 (June 1993) Unicode Version, same version were were added ∀(For All, U+2200) and ∃(There Exists, U+2203)

3. Reason of clearness: Symbol ∋ is clear and easy distinguishable from numbers and Latin letters (and from many non-Latin too)

Why not "∋"?

1. ∋ has name "Contains as Member" and it has meaning "such that"

2. Alternative to ``∋`` (∋, U+220B) is ``Ə`` (Ə, U+018F)

Example
::

  f1 :: ∀a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: ∋b. b ]

  f2 :: ∀a. [a] -> [a]
  f2 xs = ys ++ ys
     where
       ys :: ∋b. [b]
       ys = reverse xs


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``ForuniqueQuantifier`` is medium difficulty.


Alternatives
------------

Main alternative is "Modern Scoped Type Variables" `#448`_ (``ScopedTypeVariables`` extension), but also ``TypeAbstractions`` and ``PartialTypeSignatures``.


Backward Compatibility
----------------------

This proposal is fully backward compatible and is compatible with possible ``exists`` quantifier.

Even ``ScopedTypeVariables`` extension is an alternative to ``ForuniqueQuantifier`` extension, they both could coexist together in same file.

Even ``PartialTypeSignatures`` extension is an alternative to ``ForuniqueQuantifier`` extension, they both could coexist together in same file.


Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

It is unclear.
