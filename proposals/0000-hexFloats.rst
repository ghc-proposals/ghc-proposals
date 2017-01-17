.. proposal-number::

.. trac-ticket::

.. implemented::

.. highlight:: haskell

Hexadecimal Floats in Haskell
=============================

Currently, Haskell only allows writing floating-point numbers in the decimal format. Unfortunately,
writing floats in decimal/scientific format is not always the best option, due to loss of precision.
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
perhaps GHC can support such literals via a pragma, such as `LANGUAGE HexadecimalFloats` or similar.

Eventually, the change should make it into the Haskell report.

Note that there's already a feature request filed for GHC: http://ghc.haskell.org/trac/ghc/ticket/13126

Motivation
------------
Floating-point is always tricky, due to loss of precision during computation. This starts from
the reading of such values, as decimal notation is not sufficient for expressing floats precisely.
The hexadecimal notation makes it precise, and conforms to the new standards as implemented by gcc.

Proposed Change Specification
-----------------------------
The changes are rather simple.

  * Follow the grammar given in http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
  * Allow both upper-lower case hexadecimals
  * Provide the corresponding pretty-printer (`showHFloat`) in the `Numeric` package.

Effect and Interactions
-----------------------
None. The addition is orthogonal, and the changes to the grammar is unambiguous by design. No significant
complexity to any part of the compiler anticipated.

Costs and Drawbacks
-------------------
This proposal should be fairly simple to implement. Perhaps half a day of coding and test cases. Also,
some code reuse is possible as the idea is already implemented as a library. See below.

No drawbacks.

Alternatives
------------
The obvious alternative is to use quasi-quoting to implement this in a library. Indeed, there is
already a hackage package that implements this as a quasi-quoter, together with the pretty
printer: http://hackage.haskell.org/package/FloatingHex

Unfortunately, the "library" solution is really not ideal:
    
   * It relies on the rather heavy mechanism for quasi-quotes
   * It requires an extra library dependency
   * Usage requires a pragma (QuasiQuotes)
   * Usage requires import and dependency of a hackage package
   * The library suffers from a bug in TH, as TH does not support double's natively; and insists
     on storing them as Rationals. This is bad since there is no way to represent negative literals
     this way. (See bug: http://ghc.haskell.org/trac/ghc/ticket/13124) This isn't a huge issue, but
     would be nice not to have it!

By this proposal, we will reduce the dependency to one pragma (`HexadecimalFloats`); and when the Haskell
standeard catches up, even that will disappear.

Unresolved questions
--------------------
None.

Implementation Plan
-------------------
TBD
