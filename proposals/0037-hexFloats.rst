Hexadecimal Floats in Haskell
=============================

.. author:: Joachim Breitner
.. date-accepted:: 2017-04-02
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/13126
.. implemented:: 8.4.1
.. highlight:: haskell
.. header:: This proposal was `under discussion <https://github.com/ghc-proposals/ghc-proposals/pull/37>`_.
.. contents::

Currently, Haskell only allows writing floating-point numbers in the decimal format. Unfortunately,
writing floats in decimal/scientific format is not always the best option: To write finite floats precisely
one might need an extraordinary number of decimal digits, for instance.

As an alternative, there's the so called "hexadecimal floating point" format, described in
p57-58 of http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

Another source for the same format is the IEEE-754 spec itself: Section 5.12.3 of
https://www.csee.umbc.edu/~tsimo1/CMSC455/IEEE-754-2008.pdf

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

Note that there's already a feature request filed for GHC: https://gitlab.haskell.org/ghc/ghc/issues/13126

Motivation
------------
Floating-point is always tricky, due to loss of precision during computation. This starts from
the reading of such values, as decimal notation is not sufficient for expressing floats precisely anc concisely.
The hexadecimal notation provides a concise notation without losing precision, and conforms to the
new standards as implemented by gcc.

Proposed Change Specification
-----------------------------
The changes are rather simple, and follows that of the other languages with some Haskell specific deviations:

  * Introduce a new pragma ``LANGUAGE HexadecimalFloats`` or similar.
  * Follow the grammar given in p57-58 of http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
       * Exception: We do not need the suffix ``F`` or ``L`` as types would be enough to do the appropriate conversion.
       * Exception: We will make the exponent (the ``p...`` part) optional, just like the regular `e` is optional.
       * Exception: If a literal contains a dot, then it must have some hex digits before and after it
  * Provide the corresponding pretty-printer (`showHFloat`) in the `Numeric` package.
  * `Read` instance for floats-doubles should be changed to support the new format.

Note that the original allows ``0x.Ap4`` as a literal, which we disallow in Haskell. This is similar to the case
for ``.5`` or ``5.`` that is allowed by other languages, but not by Haskell.

Desugaring
----------
Ordinarily, a literal such as ``2.5`` is overloaded in Haskell, and inhabits all ``Fractional`` values.
It desugars to ``fromRational (25 % 10)``.

The new notation makes no changes to this semantics. One thing to note, however, is that since precision is
the primary motive for this notation, we should be more vigilant in issuing warnings for underflow/overflow cases.
(See below.)

Note that the ``.`` and ``p`` are both optional in the notation. If either exist, we desugar through ``fromRational``.
If neither exists, then it's already a hexadecimal literal that desugars as usual via ``fromInteger``. Various
cases to consider:

   * ``0xAB``: No dots, no exponents: Regular literal. Desugars via ``fromInteger``.
   * ``0x1a.3``: Dot, no exponent. Floating point literal: Desugars via ``fromRational``.
   * ``0x1p-4``: No dot, exponent. Floating point literal: Desugars via ``fromRational``.
   * ``0x1.2p3``: Both dot and exponent. Floating point literal. Desugars via ``fromRational``.

So, the rule is simple: If ``.`` or ``p`` is present: Desugar through ``fromRational``. Otherwise use ``fromInteger``.

Effect and Interactions
-----------------------
None. The addition is orthogonal, and the changes to the grammar is unambiguous by design. No significant
complexity to any part of the compiler anticipated.

Costs and Drawbacks
-------------------
This proposal should be fairly simple to implement. Perhaps about a day of coding and test cases for someone familar
with the code base. Even if it's tackled as an intern/summer-of-code idea, it should not take more than a few days
to flesh it out at the worst case. Also, some code reuse is possible as the idea is already implemented
as a library. See below.

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

Overflow/Underflow
------------------
The format allows for specifying numbers that are larger or smaller than what the underlying type can represent. For instance
a number like ``0x1p5000`` would not fit in a ``Double`` and thus would have the special value ``Infinity``.
(Similar to ``1/0``). In the other direction, a number like ``0x1p-5000`` is too small to be represented, and would round to
the correct value based on the rounding-mode, which is by default round-to-nearest-ties-to-even in Haskell. This is really
no different than how decimal floats are treated in Haskell today.

I think the right thing to do when the literal is too large/small is to print a warning, similar to what we already have for
other literals::

    Prelude Data.Word> 200000::Word16

    <interactive>:3:1: warning: [-Woverflowed-literals]
         Literal 200000 is out of the Word16 range 0..65535
    3392

However, I'll note that GHC **currently doesn't** provide a similar warning for decimal floats (such as ``2E20000``).
Indeed, the recommended practice section of
http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf on page 58 says:

     The implementation should produce a diagnostic message if a hexadecimal constant
     cannot be represented exactly in its evaluation format; the implementation should then
     proceed with the translation of the program.

I think GHC should follow the same practice, and issue warnings for all float values when the coversion
would cause undeflow/overflow,
controlled by the ``-Woverflowed-literals`` flag.

Unresolved Questions
--------------------
None

Implementation Plan
-------------------
Iavor Diatchki (@yav) has a Phabricator patch that implements the proposal (https://phabricator.haskell.org/D3066). which
requires minimal amount of work to be complete. (Essentially the ``read`` instance and the pretty-printer are missing;
as of Feb 20 2017.)
