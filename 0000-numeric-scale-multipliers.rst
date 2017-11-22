.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::


Numeric scale multipliers
========================

This proposal adds some syntactic sugar to numeric literals, to make it slightly more convenient to use common multipliers, 
from the scientific and computing fields, when writing literal numbers. This extension to the language's syntax would be 
activated by a new ``language`` pragma (whose name is yet to be decided).


Motivation
------------
This extension works, essentially, in the spirit of ``-XNumDecimals``, in that it reduces avenues for user error, caused by 
an inadequately-expressive syntax for numeric literals. As a matter of convenience, this extension will imply ``-XNumDecimals``.

We wish to enrich the syntax of decimal numeric literals to allow the use of common scientific prefixes, as suffixes, to denote
common multipliers.

The syntax presented below already is already available in various other languages, including `Ceylon's decimal suffixes <https://ceylon-lang.org/documentation/1.0/reference/literal/integer/#decimal_suffixes>`_
and `Habit's literal suffixes <http://hasp.cs.pdx.edu/habit-report-Nov2010.pdf>`_ (PDF link).


Proposed Change Specification
-----------------------------
When writing a numeric decimal literal, the user may use any of the case-sensitive scale multipliers below, immediately concatenated at 
the end of the literal:

================ ========= =============== =========
 Suffix           Mnemonic  Multiplier     Example    
================ ========= =============== =========
``Yi``            yobi      2\ :sup:`80`   ``2.5Yi``
``Zi``            zebi      2\ :sup:`70`   ``2.5Zi``
``Ei``            exbi      2\ :sup:`60`   ``2.5Ei``
``Pi``            pebi      2\ :sup:`50`   ``2.5Pi``
``Ti``            tebi      2\ :sup:`40`   ``2.5Ti``
``Gi``            gibi      2\ :sup:`30`   ``2.5Gi``
``Mi``            mebi      2\ :sup:`20`   ``2.5Mi``
``ki`` or ``Ki``  kibi      2\ :sup:`10`   ``2.5ki``
``E``             exa       10\ :sup:`18`  ``2.5E``
``P``             peta      10\ :sup:`15`  ``2.5P``
``T``             tera      10\ :sup:`12`  ``2.5T``
``G``             giga      10\ :sup:`9`   ``2.5G``
``M``             mega      10\ :sup:`6`   ``2.5M``
``k``             kilo      10\ :sup:`3`   ``2.5k``
``h``             hecto     10\ :sup:`2`   ``2.5h``
``da``            deca      10\ :sup:`1`   ``2.5da``
``d``             deci      10\ :sup:`-1`  ``2.5d``
``c``             centi     10\ :sup:`-2`  ``2.5c``
``m``             milli     10\ :sup:`-3`  ``2.5m``
``μ`` or ``u``    micro     10\ :sup:`-6`  ``2.5μ``
``n``             nano      10\ :sup:`-9`  ``2.5n``
``p``             pico      10\ :sup:`-12` ``2.5p``
``f``             femto     10\ :sup:`-15` ``2.5f``
``a``             atto      10\ :sup:`-18` ``2.5a``
================ ========= =============== =========

The extension will imply ``-XNumDecimals``, such that the parser will attempt to parse numeric literals as ``Num a => a``, unless
a ``Fractional`` interpretation is inevitable. A numeric literal using the extension will, in essence, be evaluated at compile-time,
and the choice made as to whether it should be intepreted as ``Num a => a`` or ``Fractional a => a``.

::
  
  j = 5k   -- interpreted as 5000 :: (Num a => a)
  s = 5e4m -- interpreted as 50 :: (Num a => a)
  x = 3.5M -- interpreted as 3500000 :: (Num a => a)
  z = 5m   -- interpreted as 0.005 :: (Fractional a => a)
  y = 4ki  -- interpreted as 4096 :: (Num a => a)
  t = 32Mi -- interpreted as 33554432 :: (Num a => a)
  p = 250c -- interpreted as 2.5 :: (Fractional a => a)
 

Note that the above table includes common variations on ``ki`` and ``μ``. Moreover, it uses the "new" binary prefixes to
disambiguate them from the decimal suffixes (e.g. 256 kilobytes is represented by ``256ki`` instead of ``256k``).

Effect and Interactions
-----------------------
This extension will imply ``-XNumDecimals``. As with ``-XNumDecimals``, this affects a very narrow part of Haskell (the syntax of
numeric literals), and thus should have minimal interactions, except as noted in the section below.

Semantically, a usage such as ``2M`` is semantically equivalent to a product ``2 * 1000000``, if one ignores the tyepclass 
promotion (from ``Integral`` to ``Num``, in this example). As these are multipliers, they have to appear after the exponent
specifier (``e`` or ``E``).

::

  2e5 -- valid; intepreted as 200000 :: (Num a => a) due to NumDecimals
  2E5 -- variation of the above
  2e5Ki -- valid; interpreted as 204800000 :: (Num a => a)
  2kie5 -- invalid
  2e -- invalid; simple syntax error
  2E -- valid; equivalent to 2e18

Formal syntax specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~
<todo>

Costs and Drawbacks
-------------------
This is a very narrow syntax change, and will have minimal interaction with the rest of the language. In essence, once one places
a space after a numeric literal, then the effects of this extension are out of scope. The effect on the type system is exactly
the same as that introduced by ``-XNumDecimals``.

I believe that the cognitive burden of this extension is minimal. 

Exponent specifier syntax ambiguity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that we can currently specify powers of ``10`` using the non-case-sensitive letter ``E``. That is, to specify one thousand, 
both ``1e3`` and ``1E3`` are valid. Note, moreover, that ``E`` is also a metric multiplier, denoting exa- (10\ :sup:`18`). The
syntax here will not clash, as the multipliers have to be followed by a space, ending the literal, whereas the exponent specifier
must immediately be followed by a decimal (possibly ``+`` or ``-``, followed by a string of digits).

::
  
  -- in the absense of -XNumDecimals 
  decimal   ->  digit{digit}
  float     ->  decimal . decimal [exponent]
	            |  decimal exponent
  exponent  ->  (e | E) [+ | -] decimal 


``Num (a -> b)`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that in Haskell, the space-as-function-application syntax is valid even if the "function" looks like a numeric literal:

::
  
  j :: Int
  j = 3
  
  b :: Num (Int -> t) => t
  b = 5a
  
If we enable ``-XFlexibleContexts``, not only will this snippet be accepted and compile perfectly fine, but if we turn off the 
monomorphism restriction then GHC will even infer the type of ``b``.

This can only be "useful" if an instance of ``Num (Int -> t)`` is in scope. I suggest that, at least when this extension is
enabled, then this "edge case" in the syntax of Haskell be commandeered. That is, without the explicit type annotation, the
definition of ``b`` should be a syntax error as ``j`` is not a valid scale-multiplier. In the current syntax, ``b = 5 a`` is 
equivalent to the ``b`` defined above, but, due to the space, the extension will not try to interpret it as a usage of a scale 
multiplier, and will instead revert to the old behaviour of assuming a ``Num (Int -> t)`` instance.

Admittedly, this is a wrinkle in this specification, but is certainly more reasonable than believing a user would have good reason
to implement a ``Num`` instance for function arrows.


Alternatives
------------
At the moment, with ``-XNumDecimals`` enabled, a user may use expinent notation to specify metric multipliers. The binary scale
multipliers can be simulated via simple multiplication. The extension is strictly not necessary.

However, as with ``-XNumDecimals`` and the proposed ``-XNumericUnderscores`` extension, this extension serves to enrich the syntax
of literals, prevent common errors, and improve code readability and self-documentation.


Unresolved questions
--------------------
<todo>

Implementation Plan
-------------------
I (Tebello Thejane) will, with the help and patience of the community, attempt to implement this relatively-simple change.
