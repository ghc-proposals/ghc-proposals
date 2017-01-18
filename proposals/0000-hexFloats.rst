.. proposal-number::

.. trac-ticket::

.. implemented::

.. highlight:: haskell

This proposal is `under discussion <https://github.com/ghc-proposals/ghc-proposals/pull/37>`_.

Hexadecimal Floats in Haskell
=============================

Currently, Haskell only allows writing floating-point numbers in the decimal format. Unfortunately,
writing floats in decimal/scientific format is not always the best option: To write finite floats precisely
one might need an extraordinary number of decimal digits, for instance.

As an alternative, there's the so called "hexadecimal floating point" format, described in
p57-58 of: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

The format is rather simple, unambiguous, and relatively easy to implement. And it's been around for
about 10 years by now. Some examples are:

     * `0x1p+1`
     * `0x1p+8`
     * `0x1.b7p-1`
     * `0x1.fffffffffffffp+1023`
     * `0X1.921FB4D12D84AP-1`

It would be nice if the Haskell standard was changed to include such literals. But in the meantime,
perhaps GHC can support such literals via a pragma, such as ``LANGUAGE HexadecimalFloats`` or similar.

Eventually, the change should make it into the Haskell report.

Note that there's already a feature request filed for GHC: http://ghc.haskell.org/trac/ghc/ticket/13126

Motivation
------------
Floating-point is always tricky, due to loss of precision during computation. This starts from
the reading of such values, as decimal notation is not sufficient for expressing floats precisely anc concisely.
The hexadecimal notation provides a concise notation without losing precision, and conforms to the
new standards as implemented by gcc.

Proposed Change Specification
-----------------------------
The changes are rather simple.

  * Introduce a new pragma ``LANGUAGE HexadecimalFloats`` or similar.
  * Follow the grammar given in p57-58 of http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
  * Provide the corresponding pretty-printer (`showHFloat`) in the `Numeric` package.
  * `Read` instance for floats-doubles should be changed to support the new format.
  
Types
-----
(Thanks to Reid Barton for pointing this out!)

Ordinarily, a literal such as ``2.5`` is overloaded in Haskell, and inhabits all ``Fractional`` values.
It desugars to ``fromRational (25 % 10)``.

Due to the special nature of this notation, my recommendation is to have numbers written in the hexadecimal notation
to have the type ``RealFloat a => a`` instead. This simplifies the story.

The issue here is how to deal with overflow. Consider a literal such as ``0x1p5000``. The correct value to
translate this to is ``Infinity``, but ``Fractional`` class really does not have any notion of ``Infinity``.
Currently, this is already an issue in Haskell: An expression like ``toRational (1/0)`` returns a fractional value that
would be converted to ``Infinity`` when interpreted as the underlying machines ``Float`` or ``Double`` type. (It is a
``Rational`` with a huge numerator.) This is rather unfortunate. That is why I'm thinking the ``RealFloat a => a``
type might be more appropriate for literals written in this notation, as they are really intended for floats, not
fractionals.

However, if this proves to be problematic from an implementation point of view, we can also discuss
the type ``Fractional a => a``. I don't think the implications will be too drastic in that case either.
(However floating-point is always tricky; I'd appreciate further feedback on this matter.)

Effect and Interactions
-----------------------
None. The addition is orthogonal, and the changes to the grammar is unambiguous by design. No significant
complexity to any part of the compiler anticipated.

Costs and Drawbacks
-------------------
This proposal should be fairly simple to implement. Perhaps half a day of coding and test cases. Also,
some code reuse is possible as the idea is already implemented as a library. See below.

Drawbacks: It was pointed out that the ``Read`` instance would break backwards compatibility. Consider::

     Prelude> reads "0x1p3" :: [(Double, String)]
     [(1.0,"p3")]
     
With the new implementation, this would return: ``[(8.0, "")]`` instead. While this is a change in behavior, I think
it's an acceptable one given the new syntax for floats. The drawback here is that we cannot guard against this using
a language pragma.

Alternatives
------------
The obvious alternative is to use quasi-quoting to implement this in a library. Indeed, there is
already a hackage package that implements this as a quasi-quoter, together with the pretty
printer: http://hackage.haskell.org/package/FloatingHex

Unfortunately, the "library" solution is really not ideal:
    
   * It relies on the rather heavy mechanism for quasi-quotes
   * Usage requires importing a new module
   * Usage requires a pragma (``QuasiQuotes``)
   * Most imporantly: Usage requires dependency on a hackage package

This is indeed a lot of requirements and heavy machinery to be able to write literals! With this proposal, we will
reduce the dependency to one pragma (``HexadecimalFloats``); and when the Haskell standard catches up, even that 
will become unnecessary.

Unresolved questions
--------------------
The format allows for specifying numbers that are larger than what the underlying type can represent. For instance
a number like ``0x1p5000`` would not fit in a ``Double`` and thus would have the special value ``Infinity``. 
(Similar to ``1/0``).

I think the right thing to do when the literal is too large is to print a warning, similar to what we already have for
other literals::

    Prelude Data.Word> 200000::Word16

    <interactive>:3:1: warning: [-Woverflowed-literals]
         Literal 200000 is out of the Word16 range 0..65535
    3392
    
However, I'll note that GHC currently doesn't provide a similar warning for decimal floats (such as ``2E20000``), so perhaps
the hexadecimal floats should do the same. The warning would be useful, but this can be resolved at implementation time
based on how the other floats behave.

Implementation Plan
-------------------
TBD
