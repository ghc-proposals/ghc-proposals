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

And we also allow to write more general Requested and Provided signatures: ::

    pattern P :: SignReq |- SignProv

This proposal also unblock implementation Bi-signatures for Bidirectional patterns.
::

    pattern P :: t1
    pattern P ... <- some_expr_in
      where 
        pattern P :: t2
        P ...  = some_expr_out

Motivation
----------

Pattern-Synonyms Signatures are designed in a such way, that they are not flexible enough.
This proposal suggest to update a bit their design.

Curried Constrains and Context in Pattern-Synonyms Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several ideas and proposals and feature requests appear to allow to write 
Curried Constrains and Context in different places: 

- Proposal: `Curried Class Contexts 
  <https://github.com/ghc-proposals/ghc-proposals/pull/666>`__

- Feature Request: `Curried contexts in prefix GADT constructors 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27424>`__

- Feature Request: `Refactor ConDeclGADT to allow nested foralls and contexts 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27425>`__

They suggest to see, for example that several constraints ``C1 a => C2 a b => (C3 a, C3 b)`` are the same 
as ``C1 a => (C2 a b, C3 a, C3 b)``
and are the same as ``(C1 a, C2 a b, C3 a, C3 b)``

This syntax is very universal and curried and it looks promising.

Unfortunately, the syntax on ``PatternSynonyms`` has a conflict syntax 
with curried context/constraint,
so if we change inner constraint separator in pattern signatures from ``=>`` to ``|-``,  
then we will avoid any future conflicts ::

    pattern P :: CReq |- CProv => t1 -> t2 -> ... -> tN -> t

Second, unfortunately right now we have strange situation, when
pattern signatures without requested signature has different syntax if provided signature
has constraint or not: 
::

    pattern P1 :: t1 -> t2 -> ... -> tN -> t

    pattern P2 :: () => (C1 a, ...) => t1 -> t2 -> ... -> tN -> t

And vice-versa if we have requested constraint, but without provided constraint, 
then we also have special syntax:
::

    pattern P3 :: (C1 a, ...) => () => t1 -> t2 -> ... -> tN -> t

Visible and Invisible Foralls in Requested part of Pattern-Synonym Signature
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Third, currently it is not supported visible forall in the Req/Prov parts 
of the pattern signature at all.
Invisible foralls also has limited support.
In general, the Req/Prov split shouldn't be solely about constraints.

- Feature Request: `Visible forall in pattern synonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/23704>`__

- Feature Request: `Support TypeAbstractions in PatternSynonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27642>`__

- Feature Request: `Unbound type variable causes panic with RequiredTypeArguments and PatternSynonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27583>`__

But the current requested separator is not flexible enough
to extend to support more requested/provided parameters.

- Feature Request: `Generalise Pattern synonyms 
  <https://gitlab.haskell.org/ghc/ghc/-/work_items/27476>`__

For example this feature suggests to have Function arguments ``T ->`` in Requested part
to represent "failure continuation type from" (right now it always ``Void#`` type).

Bi-Signatures for Bidirectional Pattern-Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We also change a bit already Accepted Proposal `Pattern synonym construction function signatures
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-bidir-constr-sigs.rst>`__ 
to unblock the implementation by making "constructor signature" optional:
::

    pattern P4 ::  t1 -> t2 -> ... -> tN -> t
    pattern P4 ... <- some_expr_in
      where 
        pattern P4 :: t1 -> t2 -> ... -> tN -> t   -- optional
        P4 ...  = some_expr_out

And we have special format for displaying pattern info, type and kind

.. code:: cmd

    ghci> :type P4
    P4 <- :: t1 -> t2 -> ... -> tN -> t
    P4 -> :: t1 -> t2 -> ... -> tN -> t

    ghci> :info P4
    pattern P4 :: t1 -> t2 -> ... -> tN -> t where
        pattern P4 :: t1 -> t2 -> ... -> tN -> t

Unpinned Pattern-Synonyms Signatures from declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the description of the ``PatternSynonyms`` extension, 
it is not defined whether pattern signatures 
are pinned or unpinned to pattern declarations.

Right now, pattern signatures are de facto pinned to pattern declarations.
But this is not uniform, because for most non-embedded signatures, 
they are unpinned from the declaration.

So, we clarify that in the ``PatternSynonyms`` extension, 
pattern signatures are unpinned from pattern declarations. 


Proposed Change Specification
-----------------------------

Changing the Requested separator and allowing more parameters in signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Main idea is: to have much simpler and more flexible syntax with ``|-`` requested separator in pattern signatures
::

    -- signature WITHOUT Requested parameters
    pattern P :: SignProv

    -- signature WITH Requested parameters
    pattern P :: SignReq |- SignProv

We should be able to have any of the following both in the Requested and the Provided parts of a patsyn:

- Invisible forall ``forall (a::k).`` or ``forall {k}.``
- Visible forall forall ``(a::k) ->``
- Constraints ``(C a, D b) =>``

**Note**: *Function arguments* ``T ->`` *in Requested part is not a part of this proposal
(mainly because it is unclear what such FunArg in Req could represent), 
even this could be extended lately if it would be necessary.*

Second, we make operator ``|-`` as keyword in signatures. At least as Constraint name.

Third, with the current syntax with ``=>`` separator, there is no difference 
between explicit foralls if they are in provided or required parts, 
but improved syntax distinguishes them.

Fourth, we allow to use old requested separator ``=>`` as Deprecated one with a Warning 
till ``CurriedContext`` (or similar) extension in not developed and is not active.

Optional Reverse Signature for Bidirectional Pattern Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Implementation of Accepted Proposal `Pattern synonym construction function signatures
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-bidir-constr-sigs.rst>`__ 
was blocked mostly due to ... lack of volunteers/inactivity for more than 10 years.

But that proposal still has several unresolved questions.

First, we are not "just" allowed to optionally write a signature to the "constructor part" we're fixing; 
that bidirectional patterns have 2 signatures: obverse one and reverse one.

What limitations do we have? 
Obverse and Reverse signatures have the same number of inputs (same amount of ``->`` )
and every input must have the same or equal type and kind. The output type for both
signatures must also be the same or equal.
Constraints could be different.

Second, to be backward compatible, we chose "Alternative 1" from the Accepted proposal: 
allow an optional reverse signature, and the compiler tries to infer its type if it is omitted.
::

    --  Obverse signature for asymmetric bidirectional patterns could be optionally added
    pattern Double :: (Eq a) |- a -> (a, a)
    pattern Double a <- (fromUniTuple -> Just a)
      where 
        --  Reverse signature for asymmetric bidirectional patterns could be optionally added
        pattern Double :: a -> (a, a)   -- optional, NEW
        Double a = (a, a)

    -- Both-handed signature for symmetric bidirectional patterns could be optionally added
    pattern Pair :: a -> b -> (a, b)
    pattern Pair x y = (x, y)

Third, even the accepted proposal specifies *different* formats for obverse and reverse signatures,
we simplify to just 1 unified format with a mandatory ``pattern`` keyword at the beginning of the signature.

Fourth unresolved question - is how many Signatures will have "Inferred"/Simple/Symmetric Bidirectional Pattern?
By this proposal, Simple/Symmetric pattern will also have 2 Signatures, but both Signatures are equal.

Fifth, we remain how ``:info`` command works with ``ghci`` (it is described in the Accepted proposal), 
but we change how ``:type`` and ``:kind`` commands works  with ``ghci`` and how 
the compiler describes types and kinds in errors:

.. code:: cmd

    ghci> :t Pair
    Pair <- :: a -> b -> (a, b)
    Pair -> :: a -> b -> (a, b)

    ////

    ghci> :t Double
    Double <- :: (Eq a) |- a -> (a, a)
    Double -> :: a -> (a, a)

    ghci> :t Double <-
    Double <- :: (Eq a) |- a -> (a, a)

    ghci> :t Double ->
    Double -> :: a -> (a, a)

    ghci> :info Double
    pattern Double :: (Eq a) |- a -> (a, a) 
      where
        pattern Double :: a -> (a, a)

Simple/Symmetric and Detailed/Asymmetric Bidirectional Patterns have now 2 Signatures:

- ``<-`` mark for "Left-Handed Signature", same as "Obversed Signature" 
- ``->`` mark for "Right-Handed Signature", same as "Reversed Signature" 
- ``<->`` mark for "Both-Handed Signature" (or without such mark, but when Left-Handed Signature is the same as Right-Handed Signature)

Left-Handed Signature for Unidirectional Patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sixth unresolved question is which "variation" of Signatures will have Unidirectional Pattern?
By this proposal, for unification purposes, Unidirectional Pattern has 1 Signature, and it is always a Left-Handed Signature:
::

    pattern Head :: a -> [a]
    pattern Head x <- x : _

.. code:: cmd

    ghci> :t Head
    Head <- :: a -> [a]

    ghci> :t Head <-
    Head <- :: a -> [a]

    ghci> :t Head ->
    ERROR

Explicitly directional Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is not mandatory, but optionally we could add explicit direction to Signatures
just after ``pattern`` keyword :
::

    pattern <- Head :: a -> [a]
    pattern Head x <- x : _

    pattern <-> Pair :: a -> b -> (a, b)
    pattern Pair x y = (x, y)

    pattern <- Double :: (Eq a) |- a -> (a, a)
    pattern Double a <- (fromUniTuple -> Just a)
      where
        pattern -> Double :: a -> (a, a)
        Double a = (a, a)

Bidirectional Pattern renaming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Right now, by convention, the names of Bidirectional Patterns are:

- Inferred Bidirectional Pattern, but it is unclear what is inferred
- Explicit Bidirectional Pattern, but it is unclear what is non-explicit in "Inferred" pattern

So, this proposal suggests the following renaming:

- "Inferred Bidirectional Pattern" into "Symmetric Bidirectional Pattern",
  because Pattern Construction and Destruction have the same signature
- "Explicit Bidirectional Pattern" into "Asymmetric Bidirectional Pattern",
  because Pattern Construction and Destruction could have (and often have) different signatures.

Right now "Explicit Bidirectional Pattern" has an optional obverse signature and an inferred reverse signature, 
and by this proposal, both obverse and reverse signatures are optional.

Unpinned Pattern-Synonyms Signatures Syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Syntax for unpinning Unidirectional and Symmetric Bidirectional Pattern Signatures is trivial.

But Detailed/Asymmetrical Bidirectional Pattern Signatures are only partly unpinned 
just like unpinned signatures in function declarations, which
consists ``where`` clause.
::

    -- unpinned LH-signature 
    pattern Double :: (Eq a) |- a -> (a, a)

    -- LH-declaration
    pattern Double a <- (fromUniTuple -> Just a)
      where
        -- RH-signature
        pattern Double :: a -> (a, a)
        -- RH-declaration
        Double a = (a, a)

But together with Explicitly directional Signatures we could fully unpinn
Asymmetrical Bidirectional Pattern Signatures from Asymmetrical Bidirectional Pattern Declaration.
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


Syntax
~~~~~~~~~~~~

The formal grammar changes for assigned a pattern-synonym signatures:

.. code:: abnf

    pattern_synonym_sig ::= 'pattern' [ dirway ] sign1way      --; upd

    sign1way ::= patslist '::' [ signreq ] sigtype             --; upd

    dirway   ::= '<->'                                         --; new
               | '<-'
               | '->'

    signreq  ::= context  '|-'                                 --; upd
               | foralls [ context ] '|-'                      --; new

    sigtype  ::= [ foralls ] [ context '=>' ] type

Directional markers are not keywords and could be "overlapped" with 
operator-function or operator-type declarations.

Omittable Request part in the Signature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We make the Requested part in the Signature fully omittable if there is no Request part.

Optional Foralls in Request part in the Signature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We allow using foralls in the Requested and Provided parts of the Signature when needed.

Syntax of Bi-Signatures
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The formal grammar changes for Bi-Signatures are described in the Accepted Proposal.

Syntax changes for GHCI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. For ``:info`` command for ``ghci`` we describe Bi-Signatures in next format

.. code:: none

    ghci> :info <PatName>
    pattern <PatName> :: <PatSignatureObverse> where
        <PatName> :: <PatSignatureReverse>

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

An example with both requested and provided constraint in pattern
::

    data T a where
        MkT :: (Show b) => a -> b -> T a

    f1 :: (Num a, Eq a) => T a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: (Num a, Eq a) |- (Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

    f2 :: (Eq a, Num a) => T a -> String
    f2 (ExNumPat x) = show x


An example without requested Constraint, but with provided Constraint
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

An example without provided Constraint, but with requested Constraint
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

Theoretically if Curried Constraint become valid in Haskell, 
then next will be possible to write and fully distinguishable:
::

    data T2 a where
        MkT :: Show b => Read b => a -> b -> T2 a

    f1 :: Num a => Eq a => T2 a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: Num a => Eq a |- Show b => Read b => b -> T2 a
    pattern ExNumPat x = MkT 42 x

Pattern signature with different Foralls in Req/Prov parts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With old syntax, there were no difference between foralls.
The compiler guess through Requested Constraint which tyvar is which: Universal or Existential.
New syntax see the clear difference.

An example without requested Constraint, but with requested ``forall``
::

    data T3 a where
        MkT3 :: Bool -> b -> (b -> Int) -> a -> T a

    pattern P ::  forall b. |- forall a. b -> (b -> Int) -> a -> T3 a
    pattern P x y v <- MkT3 True x y (v :: a)

Sure, we could use Visible forall ``forall b ->`` instead of invisible ``forall`` 
in both data declaration and requested part.

With new syntax we could write Type Applications (and Type Arguments) in such patterns
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

Several examples for Bi-Signatures for Bidirectional Patterns:
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


Effect and Interactions
-----------------------

This change ``=>`` to ``|-`` keyword for inner constraint/requested separator for pattern signatures
on ``PatternSynonyms`` extension was made to avoid conflicts with Curried Context syntax
and to allow more liberal requested signatures.

UnicodeSyntax
~~~~~~~~~~~~~~~~

We wish to preserve ``⊢`` (Right Tack, U+22A2) symbol to represent ``|-`` requested separator in pattern signatures.
::

    pattern ExNumPat :: (Num a, Eq a) ⊢ (Show b) => b -> T a


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of separator change has average difficulty.


Backward Compatibility
----------------------

Some libs and code became Deprecated a bit and lately will cause a breakage.

Hopefully it affects very complex syntax, which is not widely uses.

But this change will allow to write a code in a more universal way - with curried constraint.


Alternatives
------------

The primary alternative is "status quo".

Alternative keywords for Requested separator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could choose different requested separator in pattern signatures
instead of proposed ``|-`` and Unicode ``⊢`` keywords.

Why ``|-`` is chosen? **Pro**: it looks mathematical. 
**Cons**: it is already used as operator in several packages:
*uniplate*, *numhask*, *sbv*, *tidal*, *tidal-core*, *one-liner*.

- Use a *word* as separator, for example ``then``. 
  **Pro**: it is already a keyword for terms, looks mathematical. 
  **Cons**: it would be a second word as keyword in types

- Use different *operator* as separator, for example:

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

We could choose different  markers for Bi-signatures in pattern signatures
instead of proposed ``->`` , ``<-`` and ``<->`` markers.

Why ``->`` and ``<-`` are chosen? **Pro**: they shows direction. They are already keywords.
**Cons**: both looks like type application.

Use different *operators* as separators, for example:

#. Use ``=>`` and ``<-`` for separator. **Pro**: they shows direction. They are already keywords.
   **Cons**: ``<-`` looks like type application and ``=>`` looks like context separator.

#. Use ``<=>`` insead of ``<->`` for marker. **Pro**: they shows direction.
   **Cons**: ``<=>`` looks like context separator or equality.

Function arguments in Requested part
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Function arguments ``T ->`` in Requested part is not a part of this proposal,
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
