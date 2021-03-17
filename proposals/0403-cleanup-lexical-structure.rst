Cleanup lexical structure of numbers and identifiers
====================================================

.. author:: Oleg Grenrus
.. date-accepted:: 2021-03-17
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/403>`_.
.. contents::


This proposal cleanups and clarifies lexical structure of numbers and
identifiers.

Motivation
----------

`The Haskell2010
report <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4>`__
specifies lexical structure of numbers and identifiers as follows.

First character classes:

::

   small       →   ascSmall | uniSmall | _
   ascSmall    →   a | b | … | z
   uniSmall    →   any Unicode lowercase letter

   large       →   ascLarge | uniLarge
   ascLarge    →   A | B | … | Z
   uniLarge    →   any uppercase or titlecase Unicode letter

   digit       →   ascDigit | uniDigit
   ascDigit    →   0 | 1 | … | 9
   uniDigit    →   any Unicode decimal digit
   octit       →   0 | 1 | … | 7
   hexit       →   digit | A | … | F | a | … | f

then numeric literals as

::

   decimal      →   digit{digit}
   octal        →   octit{octit}
   hexadecimal  →   hexit{hexit}

and identifiers as

::

   varid   →   (small {small | large | digit | ' })⟨reservedid⟩
   conid   →   large {small | large | digit | ' }

There are two problems:

-  There are plenty of `Decimal
   Numbers <https://www.compart.com/en/unicode/category/Nd>`__, not only
   0..9. Also there are plenty of non-decimal numbers, e.g. `Other
   Numbers <https://www.compart.com/en/unicode/category/No>`__ and
   `Letter Numbers <https://www.compart.com/en/unicode/category/Nl>`__
-  There are plenty of letters which are not upper, lower or titlecase.
   For example Asian scripts, see `Other
   Letter <https://www.compart.com/en/unicode/category/Lo>`__

These issues are already partially fixed but not fully nor documented.

Some people read the report, try out things in GHC, and it doesn’t work,
`as this StackOverflow question
shows <https://stackoverflow.com/questions/59923193/should-a-haskell-parser-allow-unicode-digits-in-numeric-literals>`__.
The answer should be in the manual, not only in the source code.

Proposed Change Specification
-----------------------------

A short summary: the collection of alphanumerical characters is divided
into four groups:

1. Large (upper) letters
2. Small letters
3. 0-9 digits
4. The rest

First three start ``conid``, ``varid`` and ``decimal`` tokens
respectively. *The rest* cannot appear as “first” character of any
token. All four can be used as trailing characters in the identifiers.

More precisely:

Extend the ``small`` character class to allow scripts without
small/large character distinction (see `Other
Letter <https://www.compart.com/en/unicode/category/Lo>`__)

::

   uniSmall    →   any Unicode lowercase letter or Other Letter

Introduce two new character groups, ``uniIdchar`` and ``idchar``

::

   uniIdchar  →  any Unicode Modifier Letter or NonSpacingMark
   idchar     →  small | large | digit | uniIdchar | '

Change identifiers to

::

   varid   →   small {idchar} ⟨reservedid⟩
   conid   →   large {idchar}

and numbers

::

   digit       →   ascDigit | uniDigit
   ascDigit    →   0 | 1 | … | 9
   uniDigit    →   any Unicode Decimal Number, Letter Number or Other Number -- change
   octit       →   0 | 1 | … | 7
   hexit       →   ascDigit | A | … | F | a | … | f  -- digit to ascDigit

   decimal     →   ascDigit{ascDigit}  -- digit to ascDigit
   octal       →   octit{octit}
   hexadecimal →   hexit{hexit}

Additionally, the ``graphic`` token (which is used in rules for
character and string literals) is extended with the new ``uniIdchar``:

::

   graphic →   small | large | symbol | digit | uniIdchar | special | " | '

And the GHCs ``$pragmachar``, which doesn’t appear in the report:

::

   $pragmachar = [$small $large $digit $uniidchar ]

The two truly new changes are abandoning the idea of “decimal digit”
commented with a ToDo in GHC’s ``Lexer.x`` (there would be just ascii
digits and all others number characaters) and adding the *Letter Number*
category to the ``uniDigit`` class (Other Number is already there). In
the ``graphic`` token GHC already allows Letter Numbers, as that token
is parsed manually and not by its Alex rule (this is performance
optimization).

With these change all Unicode general categories are assigned in GHC
Haskell lexical structure; from (edited) ``Lexer.x``:

.. code:: haskell

   case generalCategory c of
     UppercaseLetter       -> upper
     LowercaseLetter       -> lower
     TitlecaseLetter       -> upper
     ModifierLetter        -> uniidchar -- see #10196
     OtherLetter           -> lower -- see #1103
     NonSpacingMark        -> uniidchar -- see #7650
     SpacingCombiningMark  -> other_graphic
     EnclosingMark         -> other_graphic
     DecimalNumber         -> digit
     LetterNumber          -> digit -- this proposal, previously other_graphic
     OtherNumber           -> digit -- see #4373
     ConnectorPunctuation  -> symbol
     DashPunctuation       -> symbol
     OpenPunctuation       -> other_graphic
     ClosePunctuation      -> other_graphic
     InitialQuote          -> other_graphic
     FinalQuote            -> other_graphic
     OtherPunctuation      -> symbol
     MathSymbol            -> symbol
     CurrencySymbol        -> symbol
     ModifierSymbol        -> symbol
     OtherSymbol           -> symbol
     Space                 -> space
     _other                -> non_graphic

Examples
--------

The

.. code:: haskell

   Prelude> yearⅯⅯ= 2000

   <interactive>:3:5: error: lexical error at character '\8559'

doesn’t work in current GHC. With proposed change it will:

.. code:: haskell

   ghci> yearⅯⅯ= 2000
   ghci> yearⅯⅯ
   2000

Using Letter Number as an identifier will continue to be disallowed:

.. code:: haskell

   ghci> ⅯⅯ = 2000

   <interactive>:6:1: error: lexical error at character '\8559'

Also Decimal Numbers cannot be used in numeric literals

::

   ghci> ٥

   <interactive>:10:1: error: lexical error at character '\1637'

This is the current, undocumented GHC behaviour which deviates from the
language report. There *any Unicode decimal digit* is valid character in
``integer`` token (for example).

Effect and Interactions
-----------------------

This proposal documents changes from

-  `#10196: Regression regarding Unicode subscript characters in
   identifiers <https://gitlab.haskell.org/ghc/ghc/-/issues/10196>`__
-  `#7650: Can’t use combining characters in
   identifiers <https://gitlab.haskell.org/ghc/ghc/-/issues/7650>`__
-  `#4373: Lexer does not handle unicode numeric
   subscripts <https://gitlab.haskell.org/ghc/ghc/-/issues/4373>`__
-  `#1103: Japanese
   Unicode <https://gitlab.haskell.org/ghc/ghc/-/issues/1103>`__

and fixes

-  `#18158: Lexer is confused by suzhou
   numeral <https://gitlab.haskell.org/ghc/ghc/-/issues/18158>`__

Numeric Underscores
~~~~~~~~~~~~~~~~~~~

This proposal doesn’t interfere with numeric underscores. While `the
corresponding
proposal <https://github.com/ghc-proposals/ghc-proposals/pull/76>`__
specifies the change as

.. code:: diff

   -decimal     →  digit{digit}
   +decimal     →  digit{numSpacer digit}

it is in practice:

.. code:: diff

   -decimal     →  ascDigit{ascDigit}
   +decimal     →  ascDigit{numSpacer ascDigit}

so there is no conflict.

Costs and Drawbacks
-------------------

The development costs are minimal, the code patch is inline Obviously we
need to add tests and update the documentation too. The ``$decdigit``
token can be completely removed in favour of ``$ascdigit``, but that
results in slightly bigger diff.

.. code:: diff

   --- a/compiler/GHC/Parser/Lexer.x
   +++ b/compiler/GHC/Parser/Lexer.x
   @@ -128,7 +128,7 @@ $tab         = \t

    $ascdigit  = 0-9
    $unidigit  = \x03 -- Trick Alex into handling Unicode. See [Unicode in Alex].
   -$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
   +$decdigit  = $ascdigit -- exactly $ascdigit, no more no less.
    $digit     = [$ascdigit $unidigit]

    $special   = [\(\)\,\;\[\]\`\{\}]
   @@ -144,17 +144,17 @@ $unismall  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
    $ascsmall  = [a-z]
    $small     = [$ascsmall $unismall \_]

   +$uniidchar = \x07 -- Trick Alex into handling Unicode. See [Unicode in Alex].
   +$idchar    = [$small $large $digit $uniidchar \']
   +
    $unigraphic = \x06 -- Trick Alex into handling Unicode. See [Unicode in Alex].
   -$graphic   = [$small $large $symbol $digit $special $unigraphic \"\']
   +$graphic   = [$small $large $symbol $digit $idchar $special $unigraphic \"\']

    $binit     = 0-1
    $octit     = 0-7
    $hexit     = [$decdigit A-F a-f]

   -$uniidchar = \x07 -- Trick Alex into handling Unicode. See [Unicode in Alex].
   -$idchar    = [$small $large $digit $uniidchar \']
   -
   -$pragmachar = [$small $large $digit]
   +$pragmachar = [$small $large $digit $uniidchar ]

    $docsym    = [\| \^ \* \$]

   @@ -2434,7 +2434,7 @@ adjustChar c = fromIntegral $ ord adj_c
                      SpacingCombiningMark  -> other_graphic
                      EnclosingMark         -> other_graphic
                      DecimalNumber         -> digit
   -                  LetterNumber          -> other_graphic
   +                  LetterNumber          -> digit
                      OtherNumber           -> digit -- see #4373
                      ConnectorPunctuation  -> symbol
                      DashPunctuation       -> symbol

None of GHC own tests failed with this change.

Alternatives
------------

Should *LetterNumber* be ``small``? Then it could start an ``varid``,
for example

.. code:: haskell

   Ⅻ  :: Int
   Ⅻ  = 12

`Letter Numbers <https://www.compart.com/en/unicode/category/Nl>`__ are
letter like. We can then also argue that `Other
Numbers <https://www.compart.com/en/unicode/category/No>`__ should also
be able to appear as a leading ``varid`` character.

Having `Decimal
Numbers <https://www.compart.com/en/unicode/category/Nd>`__ sans 0-9
parsed as ``small`` is yet another option. Agda goes that far, but it is
a very lexically liberal language.

Alternatively Decimal Numbers should be allowed in numeric literals, as
report specifies. Maybe only with ``UnicodeSyntax`` extension enabled
though. If Decimal Numbers cannot lead identifier tokens, this wont
cause language fork.

Relatedly, we may ask why `Other
Letter <https://www.compart.com/en/unicode/category/Lo>`__ are
considered ``small``, and not just ``idchar`` (i.e caseless character).
This was an arbitrary choice made 14 years ago, `see GHC issue
#1103 <https://gitlab.haskell.org/ghc/ghc/-/issues/1103>`__.

Again, this proposal makes conservative choice and doesn’t propose any
change there.

There are also ideas more comprehensive lexical overhaul of the language
(e.g. https://github.com/blamario/rfcs/blob/unicode-identifers/0000-unicode-identifers.rst)
but they are a lot more controversial.
