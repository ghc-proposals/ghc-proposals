.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/45>`_.

Improve ieee float decimal processing
=====================================
Recently i'm trying to `optimzing IEEE float decimal formatting <https://www.reddit.com/r/haskell/comments/5uf060/faster_dtoa_for_haskell_using_grisu3_review_needed>`_, during that process i started to realize it's better to directly optimze current base's implementation instead of just bytestring. Alongaside formatting, there're some other things to be optimzed too. This proposal described my plan.

Motivation
------------
IEEE float(``Double``, ``Float``)s are represented in a way that it's not possible precisely converted into a floating number in decimal and back, and it can't be processed precisely in decimal either. There're many efforts on how we can do things with IEEE float as precisely as we can, and many mature algorithms have been invented, but haskell lacks serious support for these decimal operations, we often have to do inefficient things.

Another problems is that we mixed IEEE float with some fixed point/rational representation types in `RealFrac` typeclass, for example we have ``round :: (Integral b, RealFrac a) => a -> b`` which force us return result as an Integral, but this makes all IEEE Float algorithms using round became inefficient because we have to convert these ``Integral`` s back and forth.

Proposed Change Specification
-----------------------------
I propose adding following methods to ``RealFloat`` typeclass::

    class  (RealFrac a, Floating a) => RealFloat a  where
        ...
        -- | This function is just like 'decodeFloat', but in decimal, and return significand
        -- as a list of decimal digits, It should be able to use 'encodeFloatDecimal' to convert
        -- these digits back into an IEEE float that's /equal/ to the original one, and this 
        -- digit list should be as short as possible. Here /equal/ means IEEE approximate equal,
        -- since we can not always encode an IEEE float precisely as a decimal one.
        -- 
        decodeFloatDecimal :: a -> ([Word8], Int)

        -- | 'encodeFloatDecimal' performs the inverse of 'decodeFloatDecimal' in the
        -- sense that for finite @x@ with the exception of @-0.0@.
        --
        encodeFloatDecimal :: [Word8] -> Int -> a

        -- | 'roundFloat' return the integral value nearest to x rounding half-way 
        -- cases to even following the rounding rules of 'round', but return result
        -- as an IEEE float.
        --
        roundFloat :: a -> a
        
        -- | These functions follow the same idea with `round`, that is to provide
        -- specific decimal processing methods to IEEE float to speed up computations.
        --
        ceilingFloat :: a -> a
        floorFloat :: a -> a
        truncateFloat :: a -> a
        ...

Effect and Interactions
-----------------------
These functions can be used to improve the performance of ``Double``, ``Float`` 's ``Show``, ``Read`` instance,
for some packages that rely on ``GHC.Float.floatToDigits`` (scientific for example), ``decodeFloatDecimal`` will also
be a better choice.

Some algorithms using IEEE float rounding or truncating will be faster.

Costs and Drawbacks
-------------------
A implementation of ``decodeFloatDecimal`` and ``encodeFloatDecimal`` using Grisu3 in C can be found `here <https://github.com/dvidelabs/flatcc/tree/master/external/grisu3>`_. I have port part of it in `this patch <https://github.com/haskell/bytestring/pull/115>`_. It should be straightforward to port the rest of it.

Current ``round`` implementation already provide the primitives that ``roundFloat`` need, we can easily add them.

The only drawback i can think of is name collision, if an user have defined these functions, we may break his/her code.


Alternatives
------------
Just don't do it ; )


Unresolved questions
--------------------
We use ``[Word8]`` in ``decodeFloatDecimal`` and ``encodeFloatDecimal`` for efficiency, but current ``floatToDigits`` use
``[Int]``. I also want change ``floatToDigits`` to use ``[Word8]``, but it's unclear if that's possible.


Implementation Plan
-------------------
I will implement it. In time for 8.2.
