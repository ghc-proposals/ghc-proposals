Improved syntax for Pattern Synonyms Signatures
=================================================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/767>`_.
.. sectnum::
.. contents::

We change the inner requested separator from ``=>`` to ``|-`` in pattern signatures of ``PatternSynonyms`` 
to avoid any conflicts with curried constraint syntax ::

    pattern P :: CReq |- CProv => t1 -> t2 -> ... -> tN -> t

We also allow writing more flexible Requested and Provided parts of signatures: ::

    pattern P :: SignReq |- SignProv

Furthermore, this proposal unblocks implementation of bi-signatures for Bidirectional patterns.
::

    pattern P :: t1
    pattern P ... <- some_expr_in
      where 
        pattern P :: t2
        P ...  = some_expr_out

Finally, we could unpin pattern signatures from pattern declarations.


Motivation
----------

Pattern-Synonyms Signatures are designed in such a way that 
they are not flexible enough to mark every complex relation.
This proposal suggests updating this design.

Curried Constrains and Context in Pattern-Synonyms Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several ideas, proposals, and feature requests appear to allow writing 
Curried Constraints and Context in different places: 

- Proposal: `Curried Class Contexts 
  <https://github.com/ghc-proposals/ghc-proposals/pull/666>`__

- Feature Request: `Curried contexts in prefix GADT constructors 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27424>`__

- Feature Request: `Refactor ConDeclGADT to allow nested foralls and contexts 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27425>`__

They suggest to see, for example that several constraints ``C1 a => C2 a b => (C3 a, C3 b)`` 
are the same as ``C1 a => (C2 a b, C3 a, C3 b)``
and are the same as ``(C1 a, C2 a b, C3 a, C3 b)``

This syntax is very universal and curried, and it looks promising.

Unfortunately, the syntax on ``PatternSynonyms`` has a conflict with the syntax 
with curried context/constraint.
And if we change the inner constraint separator 
in pattern signatures from ``=>`` to ``|-``,  
then we will avoid any future conflicts ::

    pattern P :: CReq |- CProv => t1 -> t2 -> ... -> tN -> t

Second, unfortunately, right now we have a strange situation: when
pattern signatures without a requested signature have different syntax 
if the provided signature has constraints or not: 
::

    pattern P1 :: t1 -> t2 -> ... -> tN -> t

    pattern P2 :: () => (C1 a, ...) => t1 -> t2 -> ... -> tN -> t

And vice versa: if we have requested constraints but no provided constraint, 
then we also have special syntax:
::

    pattern P3 :: (C1 a, ...) => () => t1 -> t2 -> ... -> tN -> t

Hopefully, we can write more elegant, simpler code with the new separator ``|-``.

Visible and Invisible Foralls in Requested part of Pattern-Synonym Signature
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Third, currently it is not supported visible forall in the Req/Prov parts 
of the pattern signature at all.
Invisible foralls also have limited support.
In general, the Req/Prov split shouldn't be solely about constraints.

- Feature Request: `Visible forall in pattern synonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/23704>`__

- Feature Request: `Support TypeAbstractions in PatternSynonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27642>`__

- Feature Request: `Unbound type variable causes panic with RequiredTypeArguments and PatternSynonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27583>`__

But the current requested separator is not flexible enough
to extend to support more requested/provided parameters.

Hopefully, we can distinguish requested explicit ``forall``
from provided explicit ``forall`` with new separator ``|-``. 
::

    pattern P :: forall aReq bReq. CReq |- forall aProv bProv. CProv => t1 -> t2 -> ... -> tN -> t

- Feature Request: `Generalise Pattern synonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27476>`__

For example, even though it is not part of this proposal, the described feature 
suggests having Function arguments ``T ->`` in the Requested part
to represent "failure continuation type from" (right now it is always ``Void#`` type).

This could be implemented later using the same format with the ``|-`` separator.

Bi-Signatures for Bidirectional Pattern-Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We also wish for the already Accepted 42nd Proposal `Pattern synonym construction function signatures
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-bidir-constr-sigs.rst>`__ 
unblocking for the implementation.
::

    pattern P4 ::  t1 -> t2 -> ... -> tN -> t
    pattern P4 ... <- some_expr_in
      where 
        pattern P4 :: t1 -> t2 -> ... -> tN -> t   -- optional
        P4 ...  = some_expr_out

Implementation of 42nd Proposal
was blocked mostly due to ... lack of volunteers/inactivity for more than 10 years!

But it has plenty of **unresolved questions**.

This proposal resolves them and makes it a bit easier to implement 
the 42nd Proposal more easily and clearly.

Unpinned Pattern-Synonyms Signatures from declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is not defined whether pattern **signatures** 
*are pinned or unpinned* to pattern **declarations** 
in the description of the ``PatternSynonyms`` extension.

Right now, pattern signatures *are de facto pinned* to pattern declarations.
But this relation is not uniform: most non-embedded item signatures 
are unpinned from item declarations.

We wish to clarify this by adding an expected **unpinning** rule.


Proposed Change Specification
-----------------------------

Unpinned Pattern-Synonyms Signatures from declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We clarify that in the ``PatternSynonyms`` extension, 
pattern-synonym signatures are **unpinned** from pattern-synonym declarations. 

Renaming names of Bidirectional Pattern Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal suggests the following renaming:

- from "Inferred Bidirectional Pattern" to "Symmetric Bidirectional Pattern",
  because Pattern Construction and Destruction have the same signature
- from "Explicit Bidirectional Pattern" to "Asymmetric Bidirectional Pattern",
  because Pattern Construction and Destruction could have (and often have) different signatures

Why? Because of wrong naming. Right now, by convention, the names of Bidirectional Patterns are:

- Inferred Bidirectional Pattern, but it is unclear what is inferred
- Explicit Bidirectional Pattern, but it is unclear what is non-explicit in the "Inferred" pattern

Naming Signatures for Pattern Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For Asymmetric Bidirectional Pattern, we call signatures:

- "Left-Handed Signature", same as "Obversed Signature" 
  for the first signature (which is before the ``where`` clause) 
- "Right-Handed Signature", same as "Reversed Signature" 
  for the second signature (which is after the ``where`` clause)

For the Symmetric Bidirectional Pattern, we call the signature "Both-Handed Signature" 
(because Left-Handed Signature is the same as Right-Handed Signature).

For the Unidirectional Pattern, we call the signature "Left-Handed Signature"
or "One-Handed Signature" (because it doesn't have a Right-Handed Signature).

Changing the Requested separator and allowing more parameters in Req/Prov parts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For much simpler and more flexible syntax

#. we replace ``=>`` requested separator to ``|-`` in pattern signatures

#. we make the operator ``|-`` a keyword in signatures. At least in a Constraint Namespace.

#. we start to distinguish requested explicit ``forall`` from provided one and have errors 
   if it is declared in the wrong part of the signature.

#. we make the requested part optional/omittable
   ::

    -- signature WITHOUT Requested parameters
    pattern P :: SignProv

    -- signature WITH Requested parameters
    pattern P :: SignReq |- SignProv

#. temporarily, we allow the use of the old requested separator ``=>`` 
   as a Deprecated one with a warning until ``CurriedContext`` 
   (or similar) extension is developed or earlier.

These changes allow to have any of the following both in the Requested and the Provided parts of a patsyn.:

- Invisible forall ``forall (a::k).`` or ``forall {k}.``
- Visible forall forall ``(a::k) ->``
- Constraints ``(C a, D b) =>``

**Note**: *Function arguments* ``T ->`` *in the Requested part are not a part of this proposal
(mainly because it is unclear what such FunArg in Req could represent), 
even though this could be extended later if it would be necessary.*

Changes for Reverse signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. we make the format for Reverse Signature the same as for Obverse Signature
   with a mandatory ``pattern`` keyword at the beginning of the signature.

   The accepted 42nd proposal specifies *different* formats for obverse and reverse signatures,
   so we change and simplify to just 1 unified format for signatures
   ::

    -- Asymmetric Bidirectional Pattern Synonym
    -- optional Obverse Signature
    pattern Double :: (Eq a) |- a -> (a, a)
    pattern Double a (fromUniTuple -> Just a)
      where
        -- optional Reverse Signature
        pattern Double :: a -> (a, a)
        --   ^
        -- mandatory keyword "pattern" here
        Double a = (a, a)

#. we make Reverse Signature for Asymmetric Bidirectional Pattern optionally explicit.

   Right now, Asymmetric Bidirectional Pattern has an optional obverse signature 
   and an inferred reverse signature, and by this proposal, 
   both obverse and reverse signatures are optional.
   That means we choose "Alternative 1" from the Accepted 42nd proposal:
   the compiler tries to infer Reverse Signature when it is omitted.

#. we add the following limitations for Reverse Signature relative to the Obverse one:

   - Obverse and Reverse signatures must have the same number of inputs (same amount of ``->`` )

   - Every input must have the same or equal type and kind

   - The output type for both signatures must also be the same or equal

   - Constraints could be different

Optional Explicitly directional Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Despite the 42nd Proposal, we add optional explicit direction marks 
to Signatures just after the ``pattern`` keyword :
::

    -- Unidirectional Pattern Synonym
    --    optional mark "<-" here
    --       v
    pattern <- Head :: a -> [a]
    pattern Head x <- x : _

    -- Symmetric Bidirectional Pattern Synonym
    --    optional mark "<->" here
    --       v
    pattern <-> Pair :: a -> b -> (a, b)
    pattern Pair x y = (x, y)

    -- Asymmetric Bidirectional Pattern Synonym
    --    optional mark "<-" here
    --       v
    pattern <- Double :: (Eq a) |- a -> (a, a)
    pattern Double a <- (fromUniTuple -> Just a)
      where
         --    optional mark "->" here
         --     v
        pattern -> Double :: a -> (a, a)
        Double a = (a, a)

where we use the following marks for directions in Signatures:

- ``<-`` mark for Left-Handed Signature
- ``->`` mark for Right-Handed Signature
- ``<->`` mark for Both-Handed Signature

These marks are not keywords.

Unpinning for Pattern-Synonyms Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. Unpinning for Unidirectional and Symmetric Bidirectional Pattern Signatures is trivial.

#. Unpinning for Asymmetrical Bidirectional Pattern Signatures could be **partial**
   (just like function signatures for function declarations, 
   which consist of  ``where`` clause, are partly unpinned).
   ::

    -- unpinned LH-signature 
    pattern Double :: (Eq a) |- a -> (a, a)

    -- LH-declaration
    pattern Double a <- (fromUniTuple -> Just a)
      where
        -- pinned RH-signature
        pattern Double :: a -> (a, a)
        -- RH-declaration
        Double a = (a, a)

#. Unpinning for Asymmetrical Bidirectional Pattern Signatures could be **total**
   when both Signatures are explicitly directional.
   ::

    -- unpinned LH-signature 
    pattern <- Double :: (Eq a) |- a -> (a, a)
    -- unpinned RH-signature 
    pattern -> Double :: a -> (a, a)

    -- LH-declaration
    pattern Double a <- (fromUniTuple -> Just a)
      where
        -- RH-declaration
        Double a = (a, a)

Bi-Signatures Representation for Patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. For unification purposes, **all patterns** has required 1 Signature, 
   which is One-Left-Handed Signature.

   And Bidirectional Patterns have a Second-Right-Handed Signature.

   So, Unidirectional Pattern has 1 Signature, and it is always a Left-Handed Signature.

#. commands in ``ghci`` - ``:type`` and ``:kind`` for patterns shows bi-signatures 
   with direction marks.

#. the compiler could describe types and kinds for patterns 
   with direction marks in Error messages.

Syntax changes for GHC
~~~~~~~~~~~~~~~~~~~~~~~~

The formal grammar changes for assigning pattern-synonym signatures:

.. code:: abnf

    pattern_synonym_sig ::= 'pattern' [ dirway ] sign1way      --; upd

    sign1way ::= patslist '::' [ signreq ] sigtype             --; upd

    dirway   ::= '<->'                                         --; new
               | '↔'
               | '<-'
               | '->'

    signreq  ::= context  '|-'                                 --; upd
               | foralls [ context ] '|-'                      --; new

    sigtype  ::= [ foralls ] [ context '=>' ] type             --; upd

Directional markers are not keywords and could be "overlapped" with 
operator-function or operator-type declarations.

To avoid Unicode renaming with ``UnicodeSyntax`` extension (since ``<->`` is not a keyword), 
we explicitly allow use ``↔`` (Left Right Arrow, U+2194) symbol to represent ``<->`` mark.

Syntax changes for GHCI
~~~~~~~~~~~~~~~~~~~~~~~~

#. For ``:info`` command for ``ghci`` we describe Signatures in next format

.. code:: none

    ghci> :info <PatName>
    pattern <PatName> <- :: <PatSignatureObverse> 
      where
        pattern <PatName> -> :: <PatSignatureReverse>

#. For ``:type`` and ``:kind`` command for ``ghci`` we describe Bi-Signatures in next 2 formats

.. code:: none

    ghci> :type <PatName>
      <PatName> <- :: <PatSignatureObverse>
    [ <PatName> -> :: <PatSignatureReverse> ]

    ghci> :type <PatName> <PatDirection>
    <PatName> <PatDirection> :: <PatSignatureDirection>

#. The compiler errors with Bi-Signatures uses same format as ``:type`` command


Examples
--------

Pattern signature with different Constraints in Req/Prov parts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An example with both requested and provided constraints in the pattern
::

    data T a where
        MkT :: (Show b) => a -> b -> T a

    f1 :: (Num a, Eq a) => T a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: (Num a, Eq a) |- (Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

    f2 :: (Eq a, Num a) => T a -> String
    f2 (ExNumPat x) = show x


An example without the requested Constraint, 
but with the provided Constraint
::

    data S a where
        S1 :: Bool -> S Bool

    pattern P1 :: Bool -> Maybe Bool
    pattern P1 b = Just b

    -- old separator
    -- pattern P2 :: () => (b ~ Bool) => Bool -> S b

    pattern P2 :: (b ~ Bool) => Bool -> S b
    pattern P2 b = S1 b

    f :: Maybe a -> String
    f (P1 x) = "no no no"     -- Type-incorrect

    g :: S a -> String
    g (P2 b) = "yes yes yes"  -- Fine

An example without the provided Constraint, 
but with the requested Constraint
::

    fromUniTuple :: Eq a => (a, a) -> Maybe a
    fromUniTuple (x, y)
        | x == y    = Just x
        | otherwise = Nothing

    -- old separator
    -- pattern Double ::  (Eq a) => () => a -> (a, a)

    pattern Double ::  (Eq a) |- a -> (a, a)
    pattern Double a <- (fromUniTuple -> Just a)
        where Double a = (a, a)

Theoretically, if the Curried Constraint becomes valid in Haskell, 
then the following can be written and fully distinguished:
::

    data T2 a where
        MkT :: Show b => Read b => a -> b -> T2 a

    f1 :: Num a => Eq a => T2 a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: Num a => Eq a |- Show b => Read b => b -> T2 a
    pattern ExNumPat x = MkT 42 x

Pattern signature with different Foralls in Req/Prov parts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With old syntax, there was no difference between foralls.
The compiler guesses, through Requested Constraint, which tyvar is which: Universal or Existential.
But the new syntax separates them.

With explicit ``forall`` we could rewrite signature for previous example
::

    pattern ExNumPat :: forall b. (Num a, Eq a) => forall a.(Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

An example without requested Constraint, but with requested ``forall``
::

    data T3 a where
        MkT3 :: Bool -> b -> (b -> Int) -> a -> T a

    pattern P ::  forall b. |- forall a. b -> (b -> Int) -> a -> T3 a
    pattern P x y v <- MkT3 True x y (v :: a)

Sure, we could use Visible forall ``forall b ->`` instead of invisible ``forall`` 
in both data declaration and the requested part.

With the new syntax, we could write Type Applications (and Type Arguments) in such patterns
::
    data T a where
      MkT :: forall a. a -> T a

    pattern P :: forall a. |- a -> T a     -- tyvar "a" is Existential
    pattern P @a x <- MkT @a x where
    --        ^           ^
    --     occurrence    binding
    --
        pattern P :: forall a. a -> T a     -- tyvar "a" is Universal
        P @a x = MkT @a x
    --    ^           ^
    --   binding  occurrence

Bi-Signatures for Bidirectional Patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several examples of Bi-Signatures for Bidirectional Patterns:
::

    pattern Zero :: (Num a, Eq a) => a
    pattern Zero <- ((== 0) -> True)
      where
        pattern Zero :: Num a => a
        Zero = 0


    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a 
      where
        pattern NF :: NFData a => a -> NF a
        NF a = a `deepseq` UnsafeNF a

Same examples, but with Unpinned signatures and Explicit direction in Signatures
::

    pattern <- Zero :: (Num a, Eq a) => a
    pattern -> Zero :: Num a => a

    pattern <- NF :: a -> NF a
    pattern -> NF :: NFData a => a -> NF a

    --

    pattern Zero <- ((== 0) -> True)
      where
        Zero = 0

    pattern NF a <- UnsafeNF a 
      where
        NF a = a `deepseq` UnsafeNF a

Bi-Signatures for ``:type`` and ``:kind`` commands in GHCI
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHCI shows in a slightly different format for ``:type`` and ``:kind`` commands 
for patterns that it was used before

.. code:: none

    ghci> :t Pair
    Pair <- :: a -> b -> (a, b)
    Pair -> :: a -> b -> (a, b)

    ////

    ghci> :t Head
    Head <- :: a -> [a]

    ghci> :t Head <-
    Head <- :: a -> [a]

    ghci> :t Head ->
    ERROR

    ////

    ghci> :t Double
    Double <- :: (Eq a) |- a -> (a, a)
    Double -> :: a -> (a, a)

    ghci> :t Double <-
    Double <- :: (Eq a) |- a -> (a, a)

    ghci> :t Double ->
    Double -> :: a -> (a, a)

Effect and Interactions
-----------------------

This change ``=>`` to ``|-`` keyword for inner constraint/requested 
separator for pattern signatures on the ``PatternSynonyms`` extension 
was made to avoid conflicts with Curried Context syntax
and to allow more liberal requested signatures.

UnicodeSyntax
~~~~~~~~~~~~~~~~

We wish to preserve the ``⊢`` (Right Tack, U+22A2) symbol to represent ``|-`` requested separator in pattern signatures.
::

    pattern ExNumPat :: (Num a, Eq a) ⊢ (Show b) => b -> T a


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of the separator change to be of average difficulty.


Backward Compatibility
----------------------

Some libs and code became Deprecated a bit and lately will cause a breakage.

Hopefully it affects very complex syntax, which is not widely used.

But this change will allow us to write code more universally - with curried constraints.


Alternatives
------------

The primary alternative is "status quo".

Alternative keywords for Requested separator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could choose a different requested separator in pattern signatures
instead of the proposed ``|-`` and Unicode ``⊢`` keywords.

Why ``|-`` is chosen? **Pro**: it looks mathematical. 
**Cons**: it is already used as an operator in several packages:
*uniplate*, *numhask*, *sbv*, *tidal*, *tidal-core*, *one-liner*.

- Use a *word* as an separator, for example ``then``. 
  **Pro**: it is already a keyword for terms, looks mathematical. 
  **Cons**: it would be a second word as keyword in types

- Use a different *operator* as separator, for example:

  #. Use ``|--`` and same Unicode ``⊢`` for separator. **Pro**: it looks mathematical. 
     **Cons**: 3 letter instead of 2.

  #. Use ``||-`` and Unicode ``⊩`` (Forces, U+22A9) for separator. 
     **Pro**: it looks mathematical. 
     **Cons**: 3 letter instead of 2. It is already used as operator in several packages:
     *tidal*, *tidal-core*.

  #. Use ``=>=>`` for separator. 
     **Pro**: Similar to already used one. 
     **Cons**: 4 letter instead of 2. 
     Unclear which Unicode could be used. Do not looks mathematical.

Alternative markers for Bi-signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could choose a different  markers for Bi-signatures in pattern signatures
instead of proposed ``->`` , ``<-`` and ``<->`` markers.

Why ``->`` and ``<-`` are chosen? **Pro**: they shows direction. They are already keywords.
**Cons**: both look like type application.

Use different *operators* as separators, for example:

#. Use ``=>`` and ``<-`` for separator. **Pro**: they show direction. They are already keywords.
   **Cons**: ``<-`` looks like type application and ``=>`` looks like context separator.

#. Use ``<=>`` insead of ``<->`` for marker. **Pro**: they show direction.
   **Cons**: ``<=>`` looks like context separator or equality.

Function arguments in Requested part
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Function arguments ``T ->`` in the requested part are not a part of this proposal,
because it is unclear what such FunArg in Req could represent.

But new syntax could easily be extended to support them if the meaning can be found.

Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Acknowledgments
---------------

- `int-index <https://github.com/int-index>`_, for proposing generalized syntax in the pull discussion.
- `Bodigrim <https://github.com/Bodigrim>`_, for proposing unblocking implementation bi-signature syntax in the pull discussion.


Endorsements
-------------
