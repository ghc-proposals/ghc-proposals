The ``Char`` kind
==================

.. author:: Daniel Rogozin (with thanks to Vladislav Zavialov)
.. date-accepted:: 2021-01-22
.. ticket-url:: `#11342 <https://gitlab.haskell.org/ghc/ghc/-/issues/11342>`_
.. implemented:: 9.2
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/387>`_.
.. contents::

Haskell has support for type-level data in the form of the ``DataKinds``
extension, which promotes algebraic data types such as ``Bool``, ``Maybe``, or
even user-defined ADTs. It also has special support for two non-algebraic
data types: ``Symbol`` and ``Natural``, representing type-level strings and
type-level numbers respectively.

We propose to add support for one more non-algebraic data type, namely
``Char``, thereby resolving the long-standing issue `#11342
<https://gitlab.haskell.org/ghc/ghc/-/issues/11342>`_.

Motivation
----------

The existing support for type-level strings is limited to:

1. String literals::

     ghci> :kind "myString"
     "myString" :: Symbol

2. String concatenation::

     ghci> :kind! AppendSymbol "hello" "world"
     AppendSymbol "hello" "world" :: Symbol
     = "helloworld"

3. String comparison::

     ghci> :kind! CmpSymbol "hello" "world"
     CmpSymbol "hello" "world" :: Ordering
     = 'LT

There are no ways to decompose or analyse a type-level string in terms of its
constituent characters. Constrast that with the API of `Text
<https://hackage.haskell.org/package/text/docs/Data-Text.html>`_, which offers
a multitude of functions such as ``uncons``, ``map``, ``splitAt``, and so on.

We could try to extend the API of ``Symbol`` accordingly. For example, the
type-level counterpart of ``uncons :: Text -> Maybe (Char, Text)`` could be a
built-in type family such as ``UnconsSymbol :: Symbol -> Maybe (Char,
Symbol)``.

Notice that the return type of the proposed ``UnconsSymbol`` mentions ``Char``.
However, there's currently no support for type-level characters::

  ghci> :kind! 'x'
  <interactive>:1:1: error: parse error on input ‘'’

We propose to fix this omission. The ``Char`` kind and the accompanying
built-in type families will make it possible to implement type-level parsers
(see the "Examples" section below).

Proposed Change Specification
-----------------------------

1. Extend the grammar of type-level literals
   with character literals::

     tylit ::=
         INTEGER
       | STRING
       | CHAR       (NEW)

   The lexical syntax matches that of term-level character literals: the
   character enclosed in single quotes, e.g. ``'X'`` or ``'\n'``.

2. Extend the ``GHC.TypeLits`` module
   with the following built-in type families::

     type family CmpChar (a :: Char) (b :: Char) :: Ordering
     type family ConsSymbol (a :: Char) (b :: Symbol) :: Symbol
     type family UnconsSymbol (a :: Symbol) :: Maybe (Char, Symbol)

   * The semantics of ``CmpChar`` match that of ``compare @Char``.
   * The semantics of ``ConsSymbol`` and ``UnconsSymbol`` match that of
     ``(:)`` and ``Data.List.uncons`` respectively (via ``Symbol`` ≅ ``String``).
     Unlike ``Data.Text.cons``, we do *not* map UTF-16 surrogate code points to
     ``U+FFFD``.

3. Introduce the class ``KnownChar`` that allows the user to get hold of the
   type-level character in a term-level context by means of the ``charVal``
   function::

     class KnownChar (n :: Char) where
       ...

     charVal :: forall n proxy. KnownChar n => proxy n -> Char
     charVal' :: forall n. KnownChar n => Proxy# n -> Char

   Cf. ``KnownSymbol`` and ``KnownNat``

4. Introduce the data type ``SomeChar`` with a conversion function called
   ``someCharVal``. This data type also has ``Ord``, ``Eq``, ``Show``, and
   ``Read`` instances::

     data SomeChar = forall n. KnownChar n => SomeChar (Proxy n)
     someCharVal :: Char -> SomeChar

     instance Eq SomeChar
     instance Ord SomeChar
     instance Show SomeChar
     instance Read SomeChar

   Cf. ``SomeSymbol`` and ``SomeNat``

5. Extend Template Haskell as follows::

     data TyLit =
         NumTyLit Integer
       | StrTyLit String
       | CharTyLit Char     (NEW)

Examples
--------

The ``formatting`` library is a type-safe implementation of ``printf``.
However, instead of a formatting string, it introduces special combinators to
construct a formatter::

  > format ("Person's name is " % text % " and age is " % int) "Dave" 54
  "Person's name is Dave and age is 54"

In Appendix I we offer a proof-of-concept implementation of a type-safe
``printf`` that builds upon the ``formatting`` library but adds support for
formatting strings by parsing it at compile-time::

    > formatS @"Person's name is %s and age is %d" "Danya" 26
    "Person's name is Danya and age is 26"

A crucial part of the implementation is the use of the proposed
``UnconsSymbol`` type family::

    type ParseFormat :: Symbol -> [FmtPart]
    type ParseFormat s = ParseFormat1 '[] (UnconsSymbol s)

    type ParseFormat1 :: [Char] -> Maybe (Char, Symbol) -> [FmtPart]
    type family ParseFormat1 acc s where
      ParseFormat1 acc Nothing = AddLit acc '[]
      ParseFormat1 acc (Just '( '%', s)) = AddLit acc (ParseFormat2 (UnconsSymbol s))
      ParseFormat1 acc (Just '(c, s)) = ParseFormat1 (c : acc) (UnconsSymbol s)

Effect and Interactions
-----------------------

1. Type-level text processing becomes more convenient. The users can do
   compile-time parsing without the use of Template Haskell.

2. Types containing ``Char`` become promotable. A simple example:

   Before::

       ghci> :kind! [ 'a', 'b' ]
       <interactive>:1:3: error: parse error on input ‘'’

   Now::

       ghci> :kind! [ 'a', 'b' ]
       [ 'a', 'b' ] :: [Char]
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

The API surface of ``GHC.TypeLits`` is increased. The added type families will
become redundant with full-fledged support for dependent types.

Alternatives
------------
1. Previously, there was a quite similar patch by Alexander Vieth, see `here
   <https://gitlab.haskell.org/ghc/ghc/-/issues/11342#note_173991>`_.  In
   contrast to this approach, we use the same ``Char`` and don't introduce a
   distinct ``Character`` kind.

2. The `symbols <https://hackage.haskell.org/package/symbols>`_
   library offers a different approach to parsing type-level strings.
   See `"Parsing type-level strings in Haskell" <https://kcsongor.github.io/symbol-parsing-haskell/>`_ by Csongor Kiss.

   ``symbols`` is based on a clever use of ``AppendSymbol`` and ``CmpSymbol``
   to work around the lack of ``UnconsSymbol``. Our approach offers better
   compile-time performance and scales beyond the ASCII character range.

3. We may also define `Symbol` as a synonym for ``[Char]`` since ``Char`` becomes promotable with our patch.
   This way we wouldn't need any built-in type families since ``UnconsSymbol`` and ``ConsSymbol`` could be defined by the user.
   We reject this alternative for several reasons. First of all, we keep ``Symbol`` for type-checking efficiency.
   Moreover, we would also handle type families inside cons cells when solving ``HasField`` constraints.
   For example, ``HasField T ('x' : F y : G z) ty``.

4. We can include a different set of built-in type families.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

See `Merge Request !4351 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4351>`_.

Appendix I
----------

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

  module FormatS where

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

  formatS :: forall symb. (KnownSymbol symb, ToFmt (ParseFormat symb)) => FmtFn (ParseFormat symb) Text
  formatS = runFormat (transform (Proxy :: Proxy (ParseFormat symb))) toLazyText

  example :: Text
  example = formatS @"Person's name is %s and age is %d" "Danya" 26
  -- "Person's name is Danya and age is 26"
