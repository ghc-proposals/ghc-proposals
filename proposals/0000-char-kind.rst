The ``Char`` kind
==================

.. author:: Daniel Rogozin
.. date-accepted:: ""
.. ticket-url:: `#11342 <https://gitlab.haskell.org/ghc/ghc/-/issues/11342>`_
.. implemented:: `!4351 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4351>`_
.. highlight:: haskell
.. contents::

At the moment, we have built-in type-level strings and natural numbers in GHC.TypeLits and
GHC.TypeNats respectively. We also would like to introduce the ``Char`` kind.
This proposal describes our updates that introduce the build-in character kind.
This proposal also provides a solution for the issue `#11342 <https://gitlab.haskell.org/ghc/ghc/-/issues/11342>`_.
Here we only provide changes making the character kind built-in as `Nat` and `Symbol`.
This affects the type-checker, the parser, GHC core, and several libraries such as Template Haskell,
Typeable, and TypeLits.


Motivation
----------

The purpose of the proposed changes is to provide a possibility of analysing type-level strings
as we can make it with term-level ones. This feature allows users to implement such programs as
type-level parsers. See examples above.

At the moment, we have no instruments allowing one to process type-level strings
addressing to their characters. ``Symbol`` is a promoted ``FastString`` data type, not a usual
list of chars for efficiency reasons. However, we still have no direct access to characters of a
type-level string since there is no ability for pattern matching as in the term-level case. For that,
one needs to have the full-fledged support for type-level characters similarly to strings and natural numbers.
We are going to introduce the built-in ``Char`` kind. Secondly, we are going to provide several type families
for the built-in ``Char`` kind. At this stage, we decided to include ``CmpChar`` for
type-level comparison of type-level characters. We also introduce the type families called
``ConsSymbol`` and ``UnconsSymbol`` that connect `Char` and `Symbol` with each other.

Proposed Change Specification
-----------------------------

Proposed changes are the following:

1. That's how the grammar of type literals looks like with our changes::

    tylit ::= nat | symbol | char


2. We extend the GHC.TypeLits module with the following built-in type families::

    type family CmpChar (a :: Char) (b :: Char) :: Ordering

    type family ConsSymbol (a :: Char) (b :: Symbol) :: Symbol

    type family UnconsSymbol (a :: Symbol) :: Maybe (Char, Symbol)


3. Introduce the class ``KnownChar`` with such as additional helpers as ``charVal``::

    class KnownChar (n :: Char) where
      charSing :: SChar n


4. Introduce the data type ``SomeChar`` with a converting function called
``someCharVal``. This data type also has ``Ord``, ``Eq``, ``Show``, and ``Read`` instances::

    data SomeChar = forall n. KnownChar n => SomeChar (Proxy n)

    someCharVal :: Char -> SomeChar
    someCharVal n   = withSChar SomeChar (SChar n) Proxy


Examples
--------

We show how one can implement type-safe formatters using the character kind.
The example is a type-safe version of ``format`` from the ``formatting`` library.
There are several extensions and imports that we omit for brevity. See the full version in Appendix::

    data FmtPart = Lit Symbol | PctS | PctD

    type ParseFormat :: Symbol -> [FmtPart]
    type ParseFormat s = ParseFormat1 '[] (UnconsSymbol s)

    type ParseFormat1 :: [Char] -> Maybe (Char, Symbol) -> [FmtPart]
    type family ParseFormat1 acc s where
      ParseFormat1 acc Nothing = AddLit acc '[]
      ParseFormat1 acc (Just '( '%', s)) = AddLit acc (ParseFormat2 (UnconsSymbol s))
      ParseFormat1 acc (Just '(c, s)) = ParseFormat1 (c : acc) (UnconsSymbol s)

    type ParseFormat2 :: Maybe (Char, Symbol) -> [FmtPart]
    type family ParseFormat2 s where
      ParseFormat2 Nothing = TypeError ('Text "Expected a formatter after '%'")
      ParseFormat2 (Just '( 'd', s)) = PctD : ParseFormat s
      ParseFormat2 (Just '( 's', s)) = PctS : ParseFormat s
      ParseFormat2 (Just '(c, _)) = TypeError ('Text "Not a valid formatter: " :<>: ShowType c)

    -- AddLit and FromReversedString are intermediate type families. The signatures are the following:
    type AddLit :: [Char] -> [FmtPart] -> [FmtPart]

    type FromReversedString :: [Char] -> Symbol -> Symbol


    class ToFmtElem (x :: FmtPart) where
      type FmtElemFn x r
      transformElem :: Proxy x -> Format r (FmtElemFn x r)

    formatSafe
      :: forall symb. (KnownSymbol symb, ToFmt (ParseFormat symb))
      => FmtFn (ParseFormat symb) Text
    formatSafe = runFormat (transform (Proxy :: Proxy (ParseFormat symb))) toLazyText
    -- Here we assume that we have all required instances

    {-
    > formatSafe @"Person's name is %s and age is %d" "Danya" 26
    "Person's name is Danya and age is 26"
    -}

Effect and Interactions
-----------------------

1. The example above demonstrate that our changes make type-level text processing more convenient.
In particular, we may type-level parsers more simply and efficiently.

2. Moreover, types containing ``Char`` become promotable. A simple example:

Before::

    ghci> :kind! [ 'a', 'b']
    <interactive>:1:3: error: parse error on input ‘'’

Now::

    ghci> :kind! [ 'a', 'b']
    [ 'a', 'b'] :: [Char]
    = '['a', 'b']

3. GHC would accept type declarations like the following one::

    type A = 'a' :: Char

4. Declaration such as the following one also become well-typed::

    t :: 'x' :~: 'x'
    t = Refl


5. This feature also works with ``Template Haskell`` and ``Typeable``. A couple of simple examples::

    ghci> type X = $( [t| 'x' :: Char |] )
    ghci> :kind! X
    X :: Char
    = 'x'

    ghci> typeRep (Proxy :: Proxy 'c')
    'c'

Costs and Drawbacks
-------------------
Proposed changes increase the API surface, but this increasing doesn't look critical.
Our type families will be deprecated in the presence of full dependent types.

If you notice any other drawbacks, we'll mention them as well.


Alternatives
------------
1. Previously, there was a quite similar patch by Vieth, see `here <https://gitlab.haskell.org/ghc/ghc/-/issues/11342#note_173991>`_.
In contrast to this approach, we use the same ``Char`` and don't introduce the different `Character` kind.

2. The `symbols <https://hackage.haskell.org/package/symbols>`_
library provides a slightly different approach for parsing type-level strings.
See also `this blog post <https://kcsongor.github.io/symbol-parsing-haskell/>`_ to read more.
The difference between our approach and Csongor's one is decomposition.
In ``symbols``, there is a type class ``Uncons`` that uses ``Proxy``::

    class Uncons (sym :: Symbol) (h :: Symbol) (t :: Symbol) where
      uncons :: Proxy '(h, t)

One needs to have an instance of ``Uncons`` with several constraints
to decompose a non-empty type-level string::

    instance ( h ~ Head sym , AppendSymbol h t ~ sym ) => Uncons sym h t where
      uncons = Proxy

where ``Head`` is a type family that maps every non-empty ``Symbol`` to its first character
(which is also ``Symbol``). This type family is implemented using binary search in a binary tree.

In our approach, we do not use any constraints to split a type-level string into
its head and tail. That makes decomposition of symbols with ``UnconsSymbol`` more straightforward.
Our changes would also simplify the functionality from the ``symbols`` package and
make it less intricate and more efficient.


Unresolved Questions
--------------------

We suppose that the issue is done.
The possible direction for further development is the implementation of type-level Unicode classifiers as a plugin.

Implementation plan
-------------------
See `this merge request <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4351>`_.

Appendix
--------

The full version of the example with formatters::

  {-# LANGUAGE AllowAmbiguousTypes #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE MultiParamTypeClasses #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE RankNTypes #-}
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE StandaloneKindSignatures #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE TypeOperators #-}
  {-# LANGUAGE UndecidableInstances #-}

  module FormatSafe where

  import Data.String ( IsString(..) )
  import Data.Text.Lazy
  import Data.Text.Lazy.Builder hiding ( fromString )
  import Data.Proxy
  import GHC.TypeLits

  import Formatting

  data FmtPart = Lit Symbol | PctS | PctD

  type ParseFormat :: Symbol -> [FmtPart]
  type ParseFormat s = ParseFormat1 '[] (UnconsSymbol s)

  type ParseFormat1 :: [Char] -> Maybe (Char, Symbol) -> [FmtPart]
  type family ParseFormat1 acc s where
    ParseFormat1 acc Nothing = AddLit acc '[]
    ParseFormat1 acc (Just '( '%', s)) = AddLit acc (ParseFormat2 (UnconsSymbol s))
    ParseFormat1 acc (Just '(c, s)) = ParseFormat1 (c : acc) (UnconsSymbol s)

  type ParseFormat2 :: Maybe (Char, Symbol) -> [FmtPart]
  type family ParseFormat2 s where
    ParseFormat2 Nothing = TypeError ('Text "Expected a formatter after '%'")
    ParseFormat2 (Just '( 'd', s)) = PctD : ParseFormat s
    ParseFormat2 (Just '( 's', s)) = PctS : ParseFormat s
    ParseFormat2 (Just '(c, _)) = TypeError ('Text "Not a valid formatter: " :<>: ShowType c)

  type AddLit :: [Char] -> [FmtPart] -> [FmtPart]
  type family AddLit acc s where
    AddLit '[] ps = ps
    AddLit acc ps = Lit (FromReversedString acc "") : ps

  type FromReversedString :: [Char] -> Symbol -> Symbol
  type family FromReversedString cs s where
    FromReversedString '[] acc = acc
    FromReversedString (c:cs) acc = FromReversedString cs (ConsSymbol c acc)

  type ParseFormat :: Symbol -> [FmtPart]
  type family ParseFormat symb where
    ParseFormat symb = Foldr '[] (Foo symb)

  class ToFmtElem (x :: FmtPart) where
    type FmtElemFn x r
    transformElem :: Proxy x -> Format r (FmtElemFn x r)

  instance KnownSymbol s => ToFmtElem (Lit s) where
    type FmtElemFn (Lit s) r = r
    transformElem _ = fromString (symbolVal (Proxy :: Proxy s))

  instance ToFmtElem PctS where
    type FmtElemFn PctS r = Text -> r
    transformElem _ = text

  instance ToFmtElem PctD where
    type FmtElemFn PctD r = Int -> r
    transformElem _ = later decimal

  class ToFmt (xs :: [FmtPart]) where
    type FmtFn xs r
    transform :: Proxy xs -> Format r (FmtFn xs r)

  instance ToFmt '[] where
    type FmtFn '[] r = r
    transform _ = ""

  instance (ToFmtElem x, ToFmt xs) => ToFmt (x : xs) where
    type FmtFn (x : xs) r = FmtElemFn x (FmtFn xs r)
    transform (Proxy :: Proxy (x : xs)) = transformElem (Proxy :: Proxy x) % transform (Proxy :: Proxy xs)

  formatSafe :: forall symb. (KnownSymbol symb, ToFmt (ParseFormat symb)) => FmtFn (ParseFormat symb) Text
  formatSafe = runFormat (transform (Proxy :: Proxy (ParseFormat symb))) toLazyText

  example :: Text
  example = formatSafe @"Person's name is %s and age is %d" "Danya" 26
  -- "Person's name is Danya and age is 26"
