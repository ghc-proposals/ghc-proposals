Multiline strings
=================

.. author:: Brandon Chinn
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/569>`_.
.. sectnum::
.. contents::

Most languages have native support for writing strings with newlines. Rust supports it out of the box with the normal ``"`` syntax, Python and Scala have ``"""`` syntax, and Javascript/Typescript has backticks. This proposal recommends adding ``"""`` as a delimiter which starts and ends a multiline string that automatically ignores whitespace up to the same column as the initial double quote.

Motivation
----------

Currently you could do this with ``unlines`` (more commonly) or line continuations:

::

  s1 = unlines
    [ "line 1"
    , "line 2"
    , "line 3"
    ]

  s1_2 = "line 1\n\
         \line 2\n\
         \line 3\n"

Using ``unlines`` is decent, but it's a bit verbose. The verbosity becomes more visible when printing out Haskell code, especially when there are double quotes involved:

::

  s2 = unlines
    [ "x :: String"
    , "x = a ++ b"
    , "  where"
    , "    a = \"Hello \""
    , "    b = \"world!\""
    ]

Compare with multiline strings:

::

  s1' =
    """
    line 1
    line 2
    line 3
    """

  s2' =
    """
    x :: String
    x = a ++ b
      where
        a = "Hello "
        b = "world!"
    """

Third party libraries also provide this functionality with quasiquoters, e.g. ``heredoc`` or libraries that also do interpolation like ``neat-interpolation``. But a lot of people try to avoid Template Haskell in general, and it's a bit overkill anyway.

Proposed Change Specification
-----------------------------

#. Add ``"""`` as an `additional string delimiter <https://gitlab.haskell.org/ghc/ghc/-/blob/8c0ea25fb4a27d4729aabf73f4c00b912bb0c58d/compiler/GHC/Parser/Lexer.x#L577>`_

#. Store the column that the ``"""`` start-delimiter starts on

#. After parsing everything up to the ``"""`` end-delimiter, remove at most ``$COLUMN`` space characters. If using tabs, remove all leading tab characters (assuming people use the tabs-for-indentation, spaces-for-alignment rule).

   * Escaping characters with ``\`` is still valid

   * Line continuations are still respected

#. In parsing, it should be converted to the equivalent single-quoted string (with appropriate annotations for the new exact-printing framework)

I don't have enough knowledge to know if (2) is possible. If it's not, remove common whitespace prefix between lines, e.g.

::

  x =
    """
      a
        b
       c
    """

  -- equivalent to:
  x' = "a\n  b\n c\n"

Examples
--------

Escaped characters
~~~~~~~~~~~~~~~~~~

::

  x =
    """
    name\tage
    Alice\t20
    Bob\t30
    """

Trailing newline
~~~~~~~~~~~~~~~~

A trailing newline is implied by the above specification. This is the most straightforward implementation of the spec, and there's no obvious reason to deviate. It's also what ``unlines`` does, which is a nice symmetry. To avoid a trailing newline, put the closing ``"""`` immediately after the last line, or use a line continuation:

::

  x =
    """
    a
    b
    c"""

  x2 =
    """
    a
    b
    c\
    \"""

Characters before start of delimiter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Characters before the starting ``"""`` will be treated the same as characters on the same column as ``"""``.

::

  x =
    """
    a
  b
    c
      d
    """

  -- equivalent to:
  x' = "a\nb\nc\n  d\n"

Escaping triple quotes
~~~~~~~~~~~~~~~~~~~~~~

Only three literal ``"""`` characters in a row will end the multiline string, so escaping any or all of the quote characters will not terminate the string:

::

  x =
    """
    This is a literal multiline string:
    \"\"\"
    Hello
      world!
    \"""
    """

Real world code
~~~~~~~~~~~~~~~

Example from Fourmolu (`link <https://github.com/fourmolu/fourmolu/blob/0b228e12872be8f8e97daf24e82632321fff947f/config/ConfigData.hs#L230-L242>`_):

::

  adtParseJSON =
    unlines
      [ "\\v -> case v of",
        "  Aeson.Null -> pure PrintStyleInherit",
        "  Aeson.String \"\" -> pure PrintStyleInherit",
        "  _ -> PrintStyleOverride <$> Aeson.parseJSON v"
      ],

  adtParsePrinterOptType =
    unlines
      [ "\\s -> case s of",
        "  \"\" -> pure PrintStyleInherit",
        "  _ -> PrintStyleOverride <$> parsePrinterOptType s"
      ]

With multiline strings:

::

  adtParseJSON =
    """
    \\v -> case v of
      Aeson.Null -> pure PrintStyleInherit
      Aeson.String "" -> pure PrintStyleInherit
      _ -> PrintStyleOverride <$> Aeson.parseJSON v
    """

  adtParsePrinterOptType =
    """
    \\s -> case s of
      "" -> pure PrintStyleInherit
      _ -> PrintStyleOverride <$> parsePrinterOptType s
    """

While the double backslash is still required, I think the overall style is much better.

Another example using ``printf`` (`link <https://github.com/fourmolu/fourmolu/blob/0b228e12872be8f8e97daf24e82632321fff947f/config/Generate.hs#L146-L165>`_):

::

  [ printf "instance Aeson.FromJSON %s where" fieldTypeName,
    printf "  parseJSON =",
    printf "    Aeson.withText \"%s\" $ \\s ->" fieldTypeName,
    printf "      either Aeson.parseFail pure $",
    printf "        parsePrinterOptType (Text.unpack s)",
    printf "",
    printf "instance PrinterOptsFieldType %s where" fieldTypeName,
    printf "  parsePrinterOptType s =",
    printf "    case s of",
    unlines_
      [ printf "      \"%s\" -> Right %s" val con
        | (con, val) <- enumOptions
      ],
    printf "      _ ->",
    printf "        Left . unlines $",
    printf "          [ \"unknown value: \" <> show s",
    printf "          , \"Valid values are: %s\"" (renderEnumOptions enumOptions),
    printf "          ]",
    printf ""
  ]

With multiline strings:

::

  printf
    """
    instance Aeson.FromJSON %s where
      parseJSON =
        Aeson.withText "%s" $ \\s ->
          either Aeson.parseFail pure $
            parsePrinterOptType (Text.unpack s)

    instance PrinterOptsFieldType %s where
      parsePrinterOptType s =
        case s of
    %s
          _ ->
            Left . unlines $
              [ "unknown value: " <> show s
              , "Valid values are: %s"
              ]
    """
    fieldTypeName
    fieldTypeName
    fieldTypeName
    ( unlines_
        [ printf "      "%s" -> Right %s" val con
          | (con, val) <- enumOptions
        ]
    )
    (renderEnumOptions enumOptions)

Effect and Interactions
-----------------------

A multiline string should be the same as a normal string after parsing, so ``OverloadedStrings`` and any other language features should work as usual.

Should not break existing code, unless someone is actually using ``"""a"""`` to mean ``"" "a" ""``. Since it doesn't break existing code, I am not recommending to hide behind an extension.

Costs and Drawbacks
-------------------

Since this only affects lexing and parsing, I expect development and maintenance costs to be low. This feature is common in other languages, so there shouldn't be any learning curve for new developers coming from another language. If anything, the auto-stripping of leading whitespace might be a source of confusion, but a one-line explanation should be sufficient.

Alternatives
------------

* Status quo, e.g. using ``unlines``

  * As mentioned in the Motivation, it's not great ergonomics, but it works.

* Third party libraries, using quasiquoters

  * Template Haskell is overkill for this

* No stripping of leading whitespace

  * This probably comes from one of two concerns: more complex implementation, conceptually adds automagic. It does make the implementation a bit harder, but this is a small enough change that I don't think it makes the overall proposal much harder to implement. While it does add a bit more magic behind the scenes, I think the rule is simple enough (no more complex than do-block indentation rules) and the use-case common enough (I can't think of a single use-case that would want the indentation to be part of the string) that it warrants the bump in ergonomics.

* Hide behind a ``MultilineStrings`` extension

* Enable any number of ``"""+`` quotes to delimit strings

* Reuse single-quoted ``"`` for multiline syntax

  * Would require escaping double quotes in the multiline string, which, while not a major part of the proposal, is a nice bonus

Unresolved Questions
--------------------

* Is it possible to store the column the starting ``"""`` delimiter is on?

Implementation Plan
-------------------

I can implement

Endorsements
-------------
