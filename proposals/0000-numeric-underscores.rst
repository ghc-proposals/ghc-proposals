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

Underscores in Numeric Literals
===============================

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

This proposal
~~~~~~~~~~~~~
I propose an extension to the existing syntax of numeric literals.

Current syntax:

.. code-block:: none

    decimal     →  digit{digit}
    octal       →  octit{octit}
    hexadecimal →  hexit{hexit}
    binary      →  binit{binit}

New syntax (this proposal):

.. code-block:: none

    decimal     →  digit{[_ | digit]}
    octal       →  octit{[_ | octit]}
    hexadecimal →  hexit{[_ | hexit]}
    binary      →  binit{[_ | binit]}

    Underscores (_) in numeric literals are simply ignored.

Current specification of numeric literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Detail of current specification in `Haskell 2010 Language Report <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-190002.5>`_, chapter 2:

.. code-block:: none

    decimal     →  digit{digit}
    octal       →  octit{octit}
    hexadecimal →  hexit{hexit}

    integer → decimal
             | 0o octal | 0O octal
             | 0x hexadecimal | 0X hexadecimal

    float → decimal . decimal [exponent]
           | decimal exponent

    exponent → (e | E) [+ | -] decimal

    digit    →  ascDigit | uniDigit
    ascDigit →  0 | 1 | … | 9
    uniDigit →  any Unicode decimal digit
    octit    →  0 | 1 | … | 7
    hexit    →  digit | A | … | F | a | … | f

Detail of current specification in `BinaryLiterals <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=binaryliterals#ghc-flag--XBinaryLiterals>`_ language extension in my understanding:

.. code-block:: none

    binary      →  binit{binit}
    binit       →  0 | 1

    integer → decimal
             | 0o octal | 0O octal
             | 0x hexadecimal | 0X hexadecimal
             | 0b binary | 0B binary

Examples
--------
The followings are examples of this proposal:

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

Effect and Interactions
-----------------------
I believe that this proposal will improve the readability, quality and expressiveness of native numeric literals without degrading performance.

Costs and Drawbacks
-------------------
* Implementation costs are mostly related to lexers.
* Maintenance costs are related to compatibility. Compatibility can be handled with language extension like ``NumericUnderscores``.
* I think the user's learning curve is not a problem. They will soon get used to it.
* Syntax highlighting for text editors and code browsers is affected.

Alternatives
------------
For example, these expressions are current alternatives:

.. code-block:: none

    x = 10 * 000 * 000 :: Int
    y = [0x3ff, 0xffff] :: [Int]
    z = "0001 0000 0000" :: String

However, they cause increased description cost and performance degradation.

Unresolved questions
--------------------
None
