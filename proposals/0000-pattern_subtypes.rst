=========================
Pattern-Matching SubTypes
=========================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/649>`_.
.. sectnum::
.. contents::

This proposal introduces Pattern-Matching SubTypes into GHC

.. _`#639`: https://github.com/ghc-proposals/ghc-proposals/pull/639


Motivation
----------

Haskell and GHC extensions have very poor control of values for Multi-Constructor Types (including Sum-Types).

The main tool is pattern-matching. Also helps a bit data promotion and type promotion.

This Proposal suggests to control values for Multi-Constructor Types by adding Sub-Types and "fix" the subtype by value in type.

And Sub-Types of specific type are known during pattern-matching.

So, it is expected that subtypes have zero-cost binary for statical (non-dynamical) Sub-Types.


*Note: This Proposal is not about reform of field-selectors*

Right now we could write Sum-Records like next
::

  -- no field-selectors
  data Color = RGB Int Int Int
             | HSL Int Int Int

  -- with field-selectors
  data Color = RGB { red :: Int, green :: Int, blue :: Int } 
             | HSL { hue :: Int, saturation :: Int, lightness :: Int }

Partial selectors could be unsafe in use and the compiler doesn't catch many such errors.
::

  justRed :: Color -> Maybe Int
  justRed c = Just $ red c

  redUpdate :: Int -> Color -> Maybe Color
  redUpdate red c = Just $ c { red }


But nothing prevents us for creating next functions (for type without field-selectors). Are no-field-selectors safer then with partial selectors?
::

  maybeGreen :: Color -> Maybe Int
  maybeGreen (RGB green _ _) = Just green
  maybeGreen _ = Nothing

  maybeBlue :: Color -> Maybe Int
  maybeBlue (HSL _ _ blue) = Just blue
  maybeBlue _ = Nothing

So, our goal is to have subtypes for Sum-Types to have both advantages from field-selectors and no-field-selectors and also have disadvantages neither from field-selectors nor no-field-selectors.
::

  -- we have now
  red :: Color -> Int

  -- our goal is to have subtypes
  red :: Color of "RGB" -> Int

In brief, patterns (including ones from ``PatternSynonyms`` ) "fix" the "property" on Left Side of sub-definition / guard / case , but not on Right Side.

This Proposal is a way how to "fix" the "property" on Left Side, which could be also used at Right Side of sub-definition / guard / case.
::

  -- Wrong, but OK now: 
  -- red :: Color -> Int
  --
  -- Wrong and ERROR in future: 
  -- red :: Color of "HSL" -> Int
  -- Err: Expected type `Color of "RGB"`, but found `Color of "HSL"`
  redMaybe :: Color -> Maybe Int
  redMaybe c@(HSL {..}) = Just $ red c
  redMaybe _            = Nothing

If ``red :: Color -> Int`` , nothing prevent us to write ``redMaybe`` function, even after we check the "property" (Constructor) on the Left Side of sub-definition.
But if ``red :: Color of "RGB" -> Int`` we could control subtype on Right Side and compiler could find an Error.

And we could create absolutely safe and clear functions with subytpes
::

  redMaybe :: Color -> Maybe Int
  redMaybe c@(RGB {..}) = Just $ red c
  redMaybe _            = Nothing

  redUpdate :: Int -> Color -> Either Color (Color of "RGB")
  redUpdate red c@(RGB {..}) = Right $ c { red }
  redUpdate _   c            = Left c


Proposed Change Specification
-----------------------------

This Proposal suggests to add Sub-Types (for Sum-Types and Multi-Constructor Types), which are known during pattern-matching.

Technically Pattern-Matching SubTypes are Depended types, which are known during pattern-matching. But this dependency is weak and easy un-subtypeble.

Extension
~~~~~~~~~

Introduce a new extension -XPatternMatchingSubTypes.

1. Introduce a new extension ``-XPatternMatchingSubTypes``.

#. With ``-XPatternMatchingSubTypes``, ``subtype`` is a keyword for import / export / declaration.

#. With ``-XPatternMatchingSubTypes``, ``of`` is a also keyword for subtypes.

Syntax
~~~~~~

Syntax use ``of`` keyword as subtype marker.
::

  val :: SomeType a1 a2 a3 of b1 b2 b3 

It says that type ``SomeType a1 a2 a3`` has next subtype elements ``b1, b2, b3`` which means that we describe next subtypes simultaneously: ``SomeType a1 a2 a3 of b1`` and ``SomeType a1 a2 a3 of b2`` and ``SomeType a1 a2 a3 of b3``.

It is expected, that all subtypes has kind ``Symbol`` : ``SomeType a1 a2 a3 of (b1 :: Symbol) (b2 :: Symbol) (b3 :: Symbol)``

Syntax use ``subtype`` keyword as subtype declaration.
::

  subtype pat_cnstr :: type

It is Ok to have several subtype declarations with same pattern constructor, but different subtype element.


Syntax use ``subtype`` keyword import / export subtype.

Subtype is "glued" implicitly with constructor and patterns is import / export. But we could hide subtype explicitly.

We also allow inner "hiding subtype" for types, constructors and patterns.

Examples
::

  module Example (pattern Zero hiding subtype) where 
  
    import Data.Maybe( pattern Just hiding subtype
                     , pattern Nothing
                     , type Either hiding subtype) 
               hiding (subtype Nothing)

Grammar
~~~~~~~

1. The grammar is modified as follows (baseline: GHC's parser)::

    -- NEW!
    stype :=
          type 'of' subtype
        | type 

    -- NEW!
    subtype :=
          type subtype
        | type 

2. The grammar for declaration ::

    -- NEW!
    subtype_synonym_sig :=
        'subtype' con_list '::' sigtype


3. The grammar for import / export ::

    -- was
    import :=
          qcname_ext export_subspec
        | 'module' modid
        | 'pattern' qcon
        | 'subtype' qcon  -- NEW!

    -- was
    export :=
          maybe_warning_pragma qcname_ext export_subspec
        | maybe_warning_pragma 'module' modid
        | maybe_warning_pragma 'pattern' qcon
        | maybe_warning_pragma 'subtype' qcon  -- NEW!

4. The grammar for nameless hiding subtypes inside import / export ::

    -- was
    qcname_ext_w_wildcard :=
          qcname_ext 'hiding' 'subtype'  -- NEW!
        | qcname_ext
        | '..'


Rules
~~~~~

Pattern-Matching SubTypes are additional none, one or finite several subtypes, which is definitely known during pattern-matching.

- *Symbol rule*: all subtypes elements have kind ``Symbol`` : ``SomeType a1 a2 a3 of (b1 :: Symbol) (b2 :: Symbol) (b3 :: Symbol)``

- *Empty rule*: ``SomeType of <nothing>  ~  SomeType`` (forbidden syntax)

- *Unique rule*: ``SomeType of a a  ~  SomeType of a``

- *Commutativity rule*: ``SomeType of a b  ~  SomeType of b a``

- *Include rule*: ``SomeType of a b c d  ⊆  SomeType of a`` and ``SomeType of a  ⊆  SomeType``

- *Branch rule*: if expression has branches of subtypes ``SomeType of as`` and ``SomeType of bs`` then united subtype after branches is ``SomeType of (as ∩ bs)``

- *Combining rule*: if different declarations say that ``SomeConstructor`` has subtype ``SomeType of a`` and ``SomeType of b``, then during pattern-matching ``SomeConstructor`` has type ``SomeType of a b``

- *Argument rule*: if function requires argument with type ``SomeType of as``, but argument itself has type ``SomeType of bs``, then elimination is Ok if ``SomeType of bs  ⊆  SomeType of as``

- *Return rule*: if function requires return type ``SomeType of as``, but return type itself is calculated as ``SomeType of bs``, then elimination is Ok if ``SomeType of bs  ⊆  SomeType of as``


Subtype elements of specific type have kind ``Symbol`` and behave like a type with (Set of subtypes elements) subtype.

For non-dynamical types Sub-Types are needed for compiler for checking types purpose only, and it is erased lately. So, it is expected that subtypes have zero-cost binary.


Examples
--------

Safe in use ``head'`` function
::

  data [a] = [] | a : [a]

  subtype (:) :: a -> [a] of "(:)"
  subtype []  ::      [a] of "[]"

  head' :: [a] of "(:)" -> a
  head' (x : _) = x

Several subtypes
::

  data Either a b = Left a | Right b

  subtype Left  :: a -> Either a b of "Left"
  subtype Right :: b -> Either a b of "Right"

  pattern LeftMono :: a -> Either a a of "LeftMono"
  pattern LeftMono x  = Left x
 
  pattern RightMono :: a -> Either a a of "RightMono"
  pattern RightMono x = Right x
  
  foo :: a -> Either a a of "RightMono" "Right"
  foo x = RightMono x


Telescoped subtypes
::

  data Maybe a = Nothing | Just a

  subtype Nothing ::      Maybe a of "Nothing"
  subtype Just    :: a -> Maybe a of "Just"

  foo :: Maybe (Maybe Int of "Just") of "Just"
  foo x = Just (Just x)


Sum-Records
::

  data Color = RGB { red :: Int, green :: Int, blue :: Int } 
             | HSL { hue :: Int, saturation :: Int, lightness :: Int }

  subtype RGB :: Int -> Int -> Int -> Color of "RGB"
  subtype HSL :: Int -> Int -> Int -> Color of "HSL"

  red' ::   Color of "RGB" -> Int
  red' = red

  green' :: Color of "RGB" -> Int
  green' = green

  blue' ::  Color of "RGB" -> Int
  blue' = blue


  hue' :: Color of "HSL" -> Int
  hue' = hue

  saturation' :: Color of "HSL" -> Int
  saturation' = saturation

  lightness' ::  Color of "HSL" -> Int
  lightness' = lightness

Several subtypes for non-unique field-selectors
::

  data T = A { fa :: Int, f1 :: Int }
         | B { fb :: Int, f1 :: Int, f2 :: Int }
         | C { fc :: Int, f2 :: Int }

  subtype A :: Int -> Int        -> T of "A"
  subtype B :: Int -> Int -> Int -> T of "B"
  subtype C :: Int -> Int        -> T of "C"

  subtype A :: Int -> Int        -> T of "A|B"
  subtype B :: Int -> Int -> Int -> T of "A|B"

  subtype B :: Int -> Int -> Int -> T of "B|C"
  subtype C :: Int -> Int        -> T of "B|C"

  fa' :: T of "A" -> Int
  fa' = fa
  
  fb' :: T of "B" -> Int
  fb' = fb
  
  fc' :: T of "C" -> Int
  fc' = fc


  f1' :: T of "A|B" -> Int
  f1' = f1

  f2' :: T of "B|C" -> Int
  f2' = f2


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``⊇`` (⊇, Superset of or Equal To, U+2287) is added to ``UnicodeSyntax`` as synonym for suffix ``of`` keyword as subtype. Like ``SomeType x y ⊇ a b c``

PatternSynonyms
~~~~~~~~~~~~~~~

We could declare subtype explicitly by using ``PatternSynonyms`` extension.
::

  pattern RightMono :: a -> Either a a of "RightMono"
  pattern RightMono x = Right x

Additional rule for pure patterns

- *Combining rule for patterns*: if ``SomeConstructor`` has subtype ``SomeType of a`` and depended transformed pattern ``SomePatternConstructor`` has subtype  ``SomeType of b``, 
then during pattern-matching ``SomePatternConstructor`` has united subtype ``SomeType of a b``

GADTs
~~~~~

We could declare subtype explicitly by using ``GADTs`` extension.
::

  data Maybe a where
    Nothing ::      Maybe a of "Nothing"
    Just    :: a -> Maybe a of "Just"


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``PatternSubTypes`` has medium difficulty.

Backward Compatibility
----------------------

This proposal is backward compatible.


Alternatives
------------

Status quo: to remain as is.

Different marker
~~~~~~~~~~~~~~~~

- Suffix place: ``SomeType x y of a b c``

- Prefix place: ``forsub a b c. SomeType x y``

- Use already keyword or pseudo-keyword. Like ``of`` , ``using`` , ``via`` , ``as``

- Add new pseudo-quantifier. Like ``forsub`` , ``forpartly``

- Use ``subtype`` for both pseudo-quantifier and marker: ``subtype a b c. SomeType x y``

Why suffix ``of`` keyword?
~~~~~~~~~~~~~~~~~~~~~~~~~~

Using of ``of`` keyword and in suffix position is inspired by OCaml and F# languages, which use it with type declaration.
::

  --OCaml, 
  type 'a option = Some of 'a | None

  --F#
  type Option<'a> =
    | Some of 'a
    | None

Why ``subtype`` keyword?
~~~~~~~~~~~~~~~~~~~~~~~~

Word "subtype" has clear meaning.

Word ``subtype`` has same length as ``pattern`` keyword.


Prior Art
---------

Some languages allow to have "enum subtypes" or "partial enums". Most advanced is TypeScript, which has huge support **Utility Types**.

.. code:: typescript

  export enum CarBrands {
    Toyota = "TOYOTA",
    Ford = "FORD"
  }

  type JapaneseCars = CarBrands.Toyota;

  const car: JapaneseCars = CarBrands.Toyota;


Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

It is unclear.


Endorsements
------------

This proposal is inspired by "Extra MaybeField Selectors" `#639`_ Proposal.
