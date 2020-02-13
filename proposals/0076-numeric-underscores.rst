Underscores in Numeric Literals
===============================

.. author:: Takenobu Tani
.. date-accepted:: 2017-11-14
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/14473
.. implemented:: 8.6.1
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/76>`_.
.. contents::

GHC supports various numeric literals such as decimal, octal, hexadecimal, binary, and floating point numbers.
However, large numeric literals are hard to read.
This proposal improves the readability, quality, expressiveness of numeric literals.

Motivation
------------
Large numeric literals are hard to read.
This easily causes a bug.
For example, the following numbers are confusing:

.. code-block:: none

    x = 10000000            -- Is this one million or ten million?
    y = 0x3ffffff           -- Is this 4M or 64M?
    z = 0b000100000000      -- Which position is a 1-bit digit?

Readability can be improved by separating numbers with underscores (``_``).
This feature improves readability, quality and expressiveness as follows:

.. code-block:: none

    x = 10_000_000          -- ten million
    y = 0x3ff_ffff          -- 64M
    z = 0b0001_0000_0000    -- 8th bit


Similar feature is introduced in some languages such as `Verilog-HDL <https://inst.eecs.berkeley.edu/~cs150/fa06/Labs/verilog-ieee.pdf#page=20>`_ and `Rust <https://doc.rust-lang.org/reference/tokens.html#number-literals>`_ .

Proposed Change Specification
-----------------------------

I propose an extension to the existing syntax of numeric literals.

New syntax (this proposal)
~~~~~~~~~~~~~~~~~~~~~~~~~~
When the ``NumericUnderscores`` language extension is enabled, syntax is changed as follows:

.. code-block:: none

    -- `numSpacer` is enabled with NumericUnderscores extension
    numSpacer = {_}

    decimal     →  digit{numSpacer digit}
    octal       →  octit{numSpacer octit}
    hexadecimal →  hexit{numSpacer hexit}
    binary      →  binit{numSpacer binit}

    integer →  decimal
             | 0 (o | O) numSpacer octal
             | 0 (x | X) numSpacer hexadecimal
             | 0 (b | B) numSpacer binary

    float →  decimal . decimal [exponent]
           | decimal exponent
           | 0 (x | X) numSpacer hexadecimal . hexadecimal [bin_exponent]
           | 0 (x | X) numSpacer hexadecimal bin_exponent

    exponent     →  numSpacer (e | E) [+ | -] decimal
    bin_exponent →  numSpacer (p | P) [+ | -] decimal

    -- Underscores (_) in numeric literals are simply ignored.

Current syntax
~~~~~~~~~~~~~~
Current specification in `Haskell 2010 Language Report, chapter 2 <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-190002.5>`_ , `BinaryLiterals <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=binaryliterals#ghc-flag--XBinaryLiterals>`_ , and `HexFloatLiterals <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0004-hexFloats.rst>`_ language extension:

.. code-block:: none

    decimal     →  digit{digit}
    octal       →  octit{octit}
    hexadecimal →  hexit{hexit}
    binary      →  binit{binit}                                  -- BinaryLiterals

    integer →  decimal
             | 0 (o | O) octal
             | 0 (x | X) hexadecimal
             | 0 (b | B) binary                                  -- BinaryLiterals

    float →  decimal . decimal [exponent]
           | decimal exponent
           | 0 (x | X) hexadecimal . hexadecimal [bin_exponent]  -- HexFloatLiterals
           | 0 (x | X) hexadecimal bin_exponent                  -- HexFloatLiterals

    exponent     →  (e | E) [+ | -] decimal
    bin_exponent →  (p | P) [+ | -] decimal                      -- HexFloatLiterals

    digit    →  ascDigit | uniDigit
    ascDigit →  0 | 1 | … | 9
    uniDigit →  any Unicode decimal digit
    octit    →  0 | 1 | … | 7
    hexit    →  digit | A | … | F | a | … | f
    binit    →  0 | 1                                            -- BinaryLiterals

Examples
--------
The followings are examples of this proposal:

use case examples
~~~~~~~~~~~~~~~~~

.. code-block:: none

    -- decimal
    million    = 1_000_000
    billion    = 1_000_000_000
    lightspeed = 299_792_458
    version    = 8_04_1
    date       = 2017_12_31

    -- hexadecimal
    red_mask = 0xff_00_00
    size1G   = 0x3fff_ffff

    -- binary
    bit8th   = 0b01_0000_0000
    packbits = 0b1_11_01_0000_0_111
    bigbits  = 0b1100_1011__1110_1111__0101_0011

    -- float
    pi       = 3.141_592_653_589_793
    faraday  = 96_485.332_89
    avogadro = 6.022_140_857e+23

    -- function
    isUnderMillion = (< 1_000_000)

    clip64M x
        | x > 0x3ff_ffff = 0x3ff_ffff
        | otherwise = x

    test8bit x = (0b01_0000_0000 .&. x) /= 0

validity examples
~~~~~~~~~~~~~~~~~

.. code-block:: none

    x0 = 1_000_000   -- valid
    x1 = 1__000000   -- valid
    x2 = 1000000_    -- invalid
    x3 = _1000000    -- invalid

    e0 = 0.0001      -- valid
    e1 = 0.000_1     -- valid
    e2 = 0_.0001     -- invalid
    e3 = _0.0001     -- invalid
    e4 = 0._0001     -- invalid
    e5 = 0.0001_     -- invalid

    f0 = 1e+23       -- valid
    f1 = 1_e+23      -- valid
    f2 = 1__e+23     -- valid
    f3 = 1e_+23      -- invalid

    g0 = 1e+23       -- valid
    g1 = 1e+_23      -- invalid
    g2 = 1e+23_      -- invalid

    h0 = 0xffff      -- valid
    h1 = 0xff_ff     -- valid
    h2 = 0x_ffff     -- valid
    h3 = 0x__ffff    -- valid
    h4 = _0xffff     -- invalid

Effect and Interactions
-----------------------
I believe that this proposal will improve the readability, quality and expressiveness of native numeric literals without degrading performance.

Costs and Drawbacks
-------------------
* Implementation costs are mostly related to lexers.
* Maintenance costs are related to compatibility. Compatibility can be handled with language extension of ``NumericUnderscores``.
* I think the user's learning curve is not a problem. They will soon get used to it.
* Syntax highlighting for text editors and code browsers is affected.

Alternatives
------------
For example, these expressions are current alternatives:

.. code-block:: none

    x = 10 * 1000 * 1000 :: Int
    y = [0x3ff, 0xffff] :: [Int]
    z = "0001 0000 0000" :: String
    t = 5000000   -- five sec (inline comment)

However, they cause increased description cost or performance degradation.

Unresolved questions
--------------------
None
