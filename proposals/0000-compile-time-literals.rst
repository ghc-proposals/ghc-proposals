Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Compile Time Literals
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Currently, overloaded literal values are handled at run-time.
This proposal creates an extension `CompileTimeLiterals` that shifts the conversion to compile-time, along with a new set of classes for parsing these values.

Motivation
------------

Haskell's support for overloaded literals provides a dramatic advantage for concise and readable code.
There are two overloaded literals by default, ``Num`` and ``Fractional``, and extensions allow for overloaded strings and lists.
The behavior of numeric literals is given in `section 6.4.1 of the Haskell Report<https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350006.4>_`, and reproduced here:

    The syntax of numeric literals is given in Section 2.5. An integer literal represents the application of the function fromInteger to the appropriate value of type Integer. Similarly, a floating literal stands for an application of fromRational to a value of type Rational (that is, Ratio Integer). 

In other words, a value ``0`` is *really* a value ``fromInteger (0 :: Integer) :: Num a => a``.
The other overloadings follow suit: using ``fromString :: IsString s => String -> s``, ``fromRational :: Fractional a => Rational -> a``, and ``fromList :: IsList xs => [Item xs] -> xs``.
These function applications occur at run-time, and do not permit failure.
By not permitting a means of failure, partial instances must resort to ``error`` calls or surprising differences if a value fails to parse.

In addition, to acquire overloaded numeric literals for a type, you must make it an instance of the ``Num`` type class.
This requires implementing many functions which don't make sense for all types that might be represented as numeric literals.
As an example, consider the ``Time`` type from `the o-clock library <https://github.com/serokell/o-clock/blob/master/src/Time/Units.hs#L257>_`: it uses ``error`` instead of providing a nonsensical implementation of multiplication (as ``3hr * 3hr = 9 hr^2``, not ``9hr``).

This proposal introduces a new set of type classes to allow for overloading, and the parsing of these values will occur at compile-time to guarantee validity.

Proposed Change Specification
-----------------------------

Introduce a new extension: ``CompileTimeLiterals``.
When this extension is enabled, the resolution of literals will be accomplished at compile-time by the following type-classes instead of the current classes::

 class CompileInteger a where
     compileInteger :: Integer -> Either String a

 class CompileFractional a where
     compileFractional :: Fractional -> Either String a

 class CompileString a where
     compileString :: String -> Either String a

 class CompileList a where
     type Item a
     compileList :: [Item a] -> a

To preserve backwards compatibility, a compiler-provided default instance will be provided for each class that falls back to the old behavior::

 instance {-# OVERLAPPABLE #-} (Num a) => CompileInteger a where
     compileInteger = Right . fromInteger

Effect and Interactions
-----------------------

Moving these checks to compile-time will provide a much safer experience working with overloaded literal values.
As these literals will be safer, they can be used with confidence in more places.

Consider ``Num`` and the ``Natural`` data type.
A negative literal ``-5 :: Natural`` fails whenever it is demanded with the exception: ``Exception: arithmetic underflow``.
This occurs at runtime, and the error may be triggered far from the definition site.

The ``Time`` type mentioned above can use ``AdditiveGroup`` and ``VectorSpace`` (as in ``Data.Thyme``) classes to provide more meanginful addition/subtraction/scaling operators, and still allow you to use the overloaded numeric syntax.

``ByteString`` has an ``IsString`` instance that silently truncates Unicode characters (as `documented here <https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Char8.html>_`).
Using ``CompileTimeLiterals``, we could provide a new implementation::

 instance CompileString ByteString where
    compileString str
        | any ((> 255) . fromEnum) str = 
            Left "Unicode characters present"
        | otherwise = 
            Right (packChars str)

This would fail at compile-time if any non-ASCII characters were present in the literal.

A regular expression library could expose a ``CompileString`` instance for a ``Regex`` type.
If a user provided a ``String`` that was an invalid regular expression, then the library could fail at compile-time for this convenience.

``OverloadedLists`` could be safely and easily extended to more exotic list-like types.
A simple example is ``NonEmpty`` ::
 data NonEmpty a = a :| [a]

 instance CompileList (NonEmpty a) where
     type Item (NonEmpty a) = a
     compileList [] = 
        Left "A non-empty list must have at least one element."
     compileList (x:xs) = 
        Right (x :| xs)

This moves the failure from a run-time error when the list is demanded to a compile-time error.

A more interesting overloaded list is that of length-indexed vectors.

::
 {-# LANGUAGE DataKinds          #-}
 {-# LANGUAGE FlexibleContexts   #-}
 {-# LANGUAGE FlexibleInstances  #-}
 {-# LANGUAGE GADTs              #-}
 {-# LANGUAGE OverloadedLists    #-}
 {-# LANGUAGE StandaloneDeriving #-}
 {-# LANGUAGE TypeFamilies       #-}
 
 import           GHC.Exts
 
 data Nat = Z | S Nat
 
 data Vec i a where
     Nil :: Vec Z a
     Cons :: a -> Vec n a -> Vec (S n) a
 
 deriving instance (Show a) => Show (Vec i a)
 
 vecToList :: Vec i a -> [a]
 vecToList Nil         = []
 vecToList (Cons a as) = a : vecToList as
 
 vtail :: Vec (S n) a -> Vec n a
 vtail (Cons _ xs) = xs
 
 instance IsList (Vec Z a) where
     type Item (Vec Z a) = a
 
     fromList [] = Nil
     fromList _  = error "nope"
 
     toList = vecToList
 
 instance (IsList (Vec n a), Item (Vec n a) ~ a) => IsList (Vec (S n) a) where
     type Item (Vec (S n) a) = a
 
     fromList (x:xs) = Cons x (fromList xs)
     fromList _      = error "nope"
 
     toList = vecToList

But this instance is dangerous to use in practice, as the exception is buried lazily in the list.

::
 λ> ['a', 'b', 'c'] :: Vec (S (S (S Z))) Char
 Cons 'a' (Cons 'b' (Cons 'c' Nil))
 
 λ> vtail ['a', 'b', 'c'] :: Vec (S (S (S Z))) Char
 Cons 'b' (Cons 'c' *** Exception: nope
 CallStack (from HasCallStack):
   error, called at /home/matt/olist.hs:33:18 in main:Main

With compile-time literals, this becomes a compile-time error, and ``OverloadedLists`` becomes safe to use with length-indexed vectors.

Costs and Drawbacks
-------------------

This introduces an entirely different way of handling literal values in Haskell.
If an implementation cannot be defined that translates cleanly to the current way, then it is likely untenable.

Beginners to the language (if they choose to enable this extension) will see errors about misformed literals closer to the source, and at compile-time.
I expect that this will help people learning the language -- in terms of "spooky action at a distance," this trades a bit of spooky for a large reduction in distance.


Alternatives
------------

Instead of introducing new type classes for this, ``CompileTimeLiterals`` could evaluate the ``from{Integer,String,Rational,List}`` function application at compile-time to normal form.
If they ``error``, then the error will at least be visible at compile-time and exactly where the malformed literal is written.

Unresolved questions
--------------------

1. I am sure y'all will think of many :)


Implementation Plan
-------------------

One possibility is to follow the approach in the `qq-literals <https://hackage.haskell.org/package/qq-literals-0.1.0.0/docs/src/QQLiterals.html#qqLiteral>_` package.
A desugaring pass can translate numeric literals into an application of a quasiquoter when the types are known ahead of time.

::
 x :: Vec (S (S (S Z))) Int
 x = [1, 2, 3]

 -- becomes:
 x :: Vec (S (S (S Z))) Int
 x = [compileLiteral| [1, 2, 3] |]

 -- but this can't be desugared further
 y :: (CompileList xs, Item xs ~ Int) => xs
 y = [1, 2, 3]
