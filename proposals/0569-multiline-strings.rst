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

Most languages have native support for writing strings with newlines. Rust supports it out of the box with the normal ``"`` syntax, Python and Scala have ``"""`` syntax, and Javascript/Typescript has backticks. This proposal recommends adding ``"""`` as a delimiter which starts and ends a multiline string that automatically strips the common whitespace prefix from lines.

This proposal is heavily influenced by Java's text blocks in `JEP 378 <https://openjdk.org/jeps/378>`_.

Motivation
----------

Currently you could do this with ``unlines`` (more commonly) or line continuations (`"gaps" <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6>`_):

::

  s1 = unlines
    [ "line 1"
    , "line 2"
    , "line 3"
    ]

  s1_2 = "line 1\n\
         \line 2\n\
         \line 3\n"

Using ``unlines`` is decent, but it's a bit verbose. The verbosity becomes more apparent when building multiline strings with double quotes, e.g. rendering Haskell code or SQL queries.

::

  s2 = unlines
    [ "x :: String"
    , "x = a ++ b"
    , "  where"
    , "    a = \"Hello \""
    , "    b = \"world!\""
    ]

  s3 = unlines
    [ "SELECT \"id\", \"name\""
    , "FROM \"user\""
    , "WHERE \"name\" ILIKE 'Alice%'"
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

  s3' =
    """
    SELECT "id", "name"
    FROM "user"
    WHERE "name" ILIKE 'Alice%'
    """

Third party libraries also provide this functionality with quasiquoters, e.g. ``heredoc`` or libraries that also do interpolation like ``string-interpolate``. But Template Haskell is not great:

* It makes compilation difficult in certain environments (e.g. GHCJS)
* It can slow down compilation
* Some people avoid it as much as possible out of principle
* It's a rather heavyweight tool for a seemingly lightweight syntactic feature

Proposed Change Specification
-----------------------------

A working prototype is available at `brandonchinn178/string-syntax <https://github.com/brandonchinn178/string-syntax>`_.

#. Lex ``"""`` as an `additional string delimiter <https://gitlab.haskell.org/ghc/ghc/-/blob/8c0ea25fb4a27d4729aabf73f4c00b912bb0c58d/compiler/GHC/Parser/Lexer.x#L577>`_ when the ``MultilineStrings`` language extension is enabled

#. Post-process the string in the following steps:

   #. Collapse line continuations

   #. Convert leading tabs into spaces (See "Mixing tabs and spaces" example)

   #. Remove common whitespace prefix in every line

      * Ignore any characters preceding the first newline
      * Blank lines and lines with only whitespace should not be included in this calculation

   #. Remove exactly one newline from the beginning of the string (if one exists)

   #. Interpret escape sequences (occurs after removing whitespace prefix so that literal ``\n`` characters are not included)

#. After parsing, it becomes indistinguishable to the equivalent single-quoted string (modulo annotations for exact-printing)

Line terminators will match whatever line terminators the user is using in the file.

Examples
--------

Line continuations
~~~~~~~~~~~~~~~~~~

Line continuations are collapsed first and not included in the whitespace calculation

::

  s =
      """
        a b\
    \ c d e
        f g
      """

  -- equivalent to
  s' = "a b c d e\nf g\n"

Remove common whitespace
~~~~~~~~~~~~~~~~~~~~~~~~

In the below examples, leading spaces will be marked as ``.`` for visibility.

::

  s =
        """
  ......a b c

  ......d e f
  ..
  ....g h i
  ..."""

  -- equivalent to
  s' = "..a b c\n\n..d e f\n\ng h i\n.."

After lexing, the initial multiline above is parsed as

::

  [ "......a.b.c"
  , ""
  , "......d.e.f"
  , ".."
  , "....g.h.i"
  , "..."
  ]

The blank line + the whitespace-only line are excluded from the calculation, and we calculate 4 spaces as the shared whitespace prefix, which are removed from every line.

Note that the whitespace preceding the closing ``"""`` is included. This implies that there will be a trailing newline (see the "Trailing newline" example for more information).

Ignore leading characters
~~~~~~~~~~~~~~~~~~~~~~~~~

The common prefix calculation ignores all characters preceding the first newline. This means that characters immediately after the ``"""`` delimiter will be included verbatim. The same would occur with a line continuation (since line continuations are collapsed before the prefix calculation).

::

  s =
    """Line 1
       Line 2
    Line 3
    """

  s_2 =
    """\
   \Line 1
       Line 2
    Line 3
    """

  -- equivalent to
  s' = "Line 1\n   Line 2\nLine 3"

This implies that normal strings could also be written using ``"""``

::

  -- the following are equivalent
  s = """hello world"""
  s' = "hello world"

Mixing tabs and spaces
~~~~~~~~~~~~~~~~~~~~~~

Leading tabs will be immediately converted into spaces per the `Haskell report <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3>`_: "A tab character causes the insertion of enough spaces to align the current position with the next tab stop."

::

  s =
  ⇥"""
  ⇥␣␣␣␣␣␣␣␣a
  ⇥␣⇥b
  ⇥␣␣␣␣⇥c
  ⇥"""

  -- equivalent to
  s' = "a\nb\nc"

Each line will be considered to have 16 leading spaces which will all be stripped.

Like normal strings, any tabs in the middle of a multiline string will be a lexical error.

Leading newline
~~~~~~~~~~~~~~~

The specification strips exactly one leading newline, which is the behavior of least surprise for most devs used to multiline strings. To keep the initial newline, add a blank line before the first line:

::

  s =
    """

    a
    b
    c
    """

  -- equivalent to
  s' = "\na\nb\nc\n"

Trailing newline
~~~~~~~~~~~~~~~~

As mentioned in the "Remove common whitespace" example, trailing newlines are naturally included without any explicit rules. As a bonus, it does the same thing that ``unlines`` does. To avoid a trailing newline, put the closing ``"""`` immediately after the last line, or use a line continuation:

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

Indent every line
~~~~~~~~~~~~~~~~~

To indent every line, use the ``\&`` escape character

::

  s1 =
    """
      a
      b
      c
    """

  s1' = "a\nb\nc"

  s2 =
    """
    \&  a
      b
      c
    """

  s2_2 =
    """
    \&  a
    \&  b
    \&  c
    """

  s2' = "  a\n  b\n  c"

In this example, ``s2`` and ``s2_2`` are equivalent, both resulting in ``s2'``. One noteworthy aspect of this technique is that it comes for free with the current rules, since ``\&`` is already an escape character meaning "empty string" (https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6).

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

Escaped characters
~~~~~~~~~~~~~~~~~~

::

  s =
    """
     name\tage
     Alice\t20
     Bob\t30
    \t40
    """

Since escaped characters are resolved *after* calculating the common whitespace prefix, the leading ``\t`` in the last line is not included in the prefix.

::

  s' = " name\tage\n Alice\t20\n Bob\t30\n\t40"

Example from Fourmolu
~~~~~~~~~~~~~~~~~~~~~

(`link <https://github.com/fourmolu/fourmolu/blob/0b228e12872be8f8e97daf24e82632321fff947f/config/ConfigData.hs#L230-L242>`_)

With ``unlines``:

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

With ``string-interpolate``:

::

  adtParseJSON =
    [__i|
    \v -> case v of
      Aeson.Null -> pure PrintStyleInherit
      Aeson.String "" -> pure PrintStyleInherit
      _ -> PrintStyleOverride <$> Aeson.parseJSON v
    |]

  adtParsePrinterOptType =
    [__i|
    \s -> case s of
      "" -> pure PrintStyleInherit
      _ -> PrintStyleOverride <$> parsePrinterOptType s
    |]

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

While the double backslash is still required, I think the overall style is much better (could be resolved in a later proposal adding raw strings).

Example from Fourmolu (printf)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(`link <https://github.com/fourmolu/fourmolu/blob/0b228e12872be8f8e97daf24e82632321fff947f/config/Generate.hs#L146-L165>`_)

With ``unlines``:

::

  unlines
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

With ``string-interpolate`` (without interpolation, for a fair comparison):

::

  printf
    [__i|
    instance Aeson.FromJSON %s where
      parseJSON =
        Aeson.withText "%s" $ \s ->
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
    |]
    fieldTypeName
    fieldTypeName
    fieldTypeName
    ( unlines_
        [ printf "      \"%s\" -> Right %s" val con
        | (con, val) <- enumOptions
        ]
    )
    (renderEnumOptions enumOptions)

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
        [ printf "      \"%s\" -> Right %s" val con
        | (con, val) <- enumOptions
        ]
    )
    (renderEnumOptions enumOptions)

Effect and Interactions
-----------------------

A multiline string should be the same as a normal string after parsing, so ``OverloadedStrings`` and any other language features should work as usual.

Should not break existing code, unless someone is actually using ``"""a"""`` to mean ``"" "a" ""``.

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

  * The downside of doing this is that generally speaking, developers will want to keep the multiline string at the same indentation level as surrounding code. Not doing any post processing means that reindenting code would change the string content. I would also posit that the common case is wanting leading whitespace stripped, which would lead to devs putting multiline strings at the 0th column or implementing their own deindenter.

* Only strip leading whitespace with delimiter

  * This alternative can be done in one of two ways:

    #. Special case the delimiter and resolve it at compile-time
    #. Add new ``trimMargin`` / ``trimMarginWith`` functions that trim the delimiter (or some custom delimiter) at runtime

  * The first option involves hardcoding the delimiter in the compiler, which is Not Great. Plus, wanting to actually use the delimiter to start a line in the string would require escaping it.

  * The second option requires adding new functions to ``Prelude`` and would trim the margins at run-time, instead of compile-time. This would also not work with ``OverloadedStrings``

  * Furthermore, any use case that doesn't want to strip leading whitespace either:
    #. Is agnostic to the whitespace (e.g. HTML) so it doesn't matter if it's stripped or not, or
    #. Explicitly needs leading whitespace on every line. In this case, the developer would not be able to reindent their code without changing behavior, so IMO, the developer *should* explicitly opt-in to specify exactly where the indentation should start. This is possible with the "indent every lines" technique listed above. Since it's possible to do this, and I believe a priori that stripping is more common than not-stripping, it doesn't make sense to make this use-case the default.

* Use ``''`` to delimit multiline strings, which has the benefit of being a parse error without ``MultilineStrings``

* Enable any number of ``"""+`` quotes to delimit multiline strings

* Reuse single-quoted ``"`` for multiline syntax

  * Would require escaping double quotes in the multiline string, which, while not a major part of the proposal, is a nice bonus

* New ``[s|foo|]`` construct that embeds a multiline literal string with no TH (so *not* a quasiquoter, but reusing the same syntax)

* Support arbitrary terminators, like Bash's heredocs

  ::

    x = <<EOF
    line 1
    line 2
    line 3
    EOF

  * Everyone will use a different terminator, which I think would contribute to a reduction in overall readability
  * I think ``"""`` is an uncommon enough delimiter, and it can be escaped, that I don't think this is necessary

* Use some delimiter to start the string, but use layout indentation rules to dictate when the string ends

* Strip trailing whitespace in post-processing

  * Nice to have, but not necessary. I think it would be better to keep post-processing as minimal as possible, and it doesn't seem as common as removing leading whitespace.

Out of scope
~~~~~~~~~~~~

* String interpolation
  * See https://github.com/ghc-proposals/ghc-proposals/pull/570
  * One way this proposal can work with raw strings is by allowing both ``s"..."`` and ``s"""..."""`` syntaxes. In general, any raw strings proposal that works with the current double quoted string syntax should be able to work with a triple-quoted string syntax as well, since the proposed triple-quoted string syntax desugars to a single-quoted string.

* "Raw" strings (without escaping)
  * To an extent, this proposal already helps this a little bit, since double quotes no longer need to be escaped within a triple-quoted string. But this proposal doesn't address needing to escape backslashes.
  * This is particularly useful for regexes or any other situation where the backslash character is useful as an actual character.
  * One way this proposal can work with raw strings is by allowing both ``r"..."`` and ``r"""..."""`` syntaxes. See comment in "String interpolation".

Comparisons with other languages
--------------------------------

* Java

  * As mentioned in the beginning, this proposal draws a lot from Java.

  * Java strips trailing whitespace. See "Strip trailing whitespace" under "Alternatives".

  * Java defines the content to start after the first newline after the opening ``"""``, and disallows any non-whitespace characters after the opening delimiter. Instead of adding this restriction, we added the rule to remove exactly one newline from the beginning of the string, if one exists. This allows people to start the multiline string on the same line, enabling one-line strings to use the syntax, e.g. ``"""A string using "unescaped" quotes"""``.

  * Java includes the line that the closing ``"""`` delimiter is on, so that the position of the closing delimiter is included in the common-prefix calculation. One motivation for this was to enable indenting every line. However, discussion on this proposal indicated that this was too magical and would be confusing behavior. Instead of this, we can reuse Haskell's existing ``\&`` escape character to add indentation to every line. See the "Indent every line" example and the "Only strip leading whitespaces with delimiter" alternative.

  * This proposal also adds the addition of collapsing line continuations before any post-processing, which is a Haskell-specific syntax.

* Python, Groovy, Kotlin, Scala, Swift

  * All of these languages use ``"""`` to delimit multi-line strings.

  * Most of them keep the multiline string verbatim; to strip indentation, each language provides functions: Python = ``textwrap.dedent``, Kotlin/Groovy/Scala provide some version of ``stripIndent`` or ``stripMargin``.

  * Swift uses Java's method of using the closing delimiter to determine the leading whitespace to strip

* Go, Javascript

  * These languages use a single backtick to delimit multi-line strings.

  * None of them strip indentation automatically. Go has the ``dedent`` library, Javascript can do ``s.replace(/^\s{4}/g, '')``.

* C#

  * Allows opening with at least 3 ``"`` characters

  * Strips newline after opening delimiter and before closing delimiter.

  * Uses Java's method of using the closing delimiter to determine the leading whitespace to strip

  * Also allows ``@"..."`` syntax, which won't work for us, as ``@`` is used for type applications, in this case, a type application for a ``Symbol``.

* Ruby

  * Normal double quoted strings can be on multiple lines, does not strip whitespace
  * ``<<-EOF``: heredoc, does not strip whitespace
  * ``<<~EOF``: heredoc, strips whitespace
  * ``%q(...)``: does not strip whitespace

* C, C++

  * Raw string literals with ``R"..."``.

Unresolved Questions
--------------------

Implementation Plan
-------------------

I can implement

Endorsements
-------------
