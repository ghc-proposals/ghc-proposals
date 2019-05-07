Record ``with`` syntax
=======================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

We propose a layout syntax for records, triggered by a new keyword ``with``.
This means that record fields in type declarations, expressions and patterns can be formatted using indentation instead of punctuation.
The feature removes syntactic noise and makes large records easier to read and modify.
It is consistent with the use of layout for other constructs that span several lines (e.g. ``do`` blocks, branches of ``case`` expressions).


Motivation
----------
Record syntax in Haskell uses curly braces and commas. For example, you can declare a record type as follows:
::

 data Rec = Rec { field1 :: Int, field2 :: Bool }

You can construct and pattern match on expressions of this type using the syntax
::

 Rec { field1 = f1, field2 = f2 }


When records have a large number of fields, programmers typically spread declarations and expressions over several lines, e.g.
::

 data BigRec = BigRec
   { field1 :: Int
   , field2 :: Bool
   , field3 :: String
   }

Similarly when constructing a big record:
::

 BigRec
   { field1 = 1
   , field2 = True
   , field3 = "hello"
   }

Indentation of the fields inside curly braces is not required, but is usually maintained by the programmer (with varying styles of formatting).

Let's compare this situation to ``do`` blocks in monadic code.
You could write
::

  echo = do {
    line <- getLine;
    putStrLn line;
  }

But the much more standard style is to use indentation and write
::

  echo = do
    line <- getLine
    putStrLn line

The latter version avoids meaningless characters, and alignment of statements in the ``do`` block is required by the parser.

The goal of this proposal is to use the same technique and introduce indentation-based record syntax to Haskell.

We propose a keyword ``with`` that induces layout syntax for record fields.
Using ``with``, our big record declaration from earlier can be written as
::

  data BigRec = BigRec with
    field1 :: Int
    field2 :: Bool
    field3 :: String

We can use indentation for record construction
::

  Rec with
    field1 = 1
    field2 = True

as well as record field updates
::

  r with
    field1 = field1 r + 1
    field2 = True

Note that when using indentation, the lines corresponding to fields are formatted identically.
This means that reordering or deleting fields does not require adjusting braces or commas.
Not only is this less hassle to write, but it also leads to simpler patches and code review.

On the whole, record ``with`` syntax removes syntactic noise and makes large records easier to read and modify.
The feature is well precedented by layout syntax in other Haskell constructs.


Proposed Change Specification
-----------------------------
We propose a language extension called ``RecordWith`` offering layout syntax for records.
With the extension enabled, there is an additional keyword ``with`` which induces layout syntax for record fields.

The structure and formatting options are very similar to monadic ``do`` blocks (and other layout-inducing constructs).
Specifically, ``with`` can be followed either by indentation and newline delimiters or by curly braces ``{ }`` and semicolon ``;`` delimiters.
For example, the following 3 declarations are all valid, equivalent ways to define a record type ``Rec``:
::

  data Rec = Rec with
    field1 :: T1
    field2 :: T2
    field3 :: T3

  data Rec = Rec with
    { field1 :: T1
    ; field2 :: T2
    ; field3 :: T3 }
  
  data Rec = Rec with field1 :: T1; field2 :: T2; field3 :: T3

These are all equivalent to the traditional syntax
::

  data Rec = Rec { field1 :: T1, field2 :: T2, field3 :: T3 }

(which can also be spread across multiple lines).

The ``with`` syntax can occur in any language construct where traditional record syntax usually appears.
Namely, we need to handle the following features:

1. Data type declarations
2. Record construction
3. Record field updates
4. Record patterns

We showed an example of the first case above.
Note that ``with`` should work in data types with several constructors, as well as constructors with zero arguments.
So the following declaration is valid:
::

  data ComplexRec = Con1 with f1 :: Int; f2 :: Bool | Con2 with | Con3 String

(``Con2 with`` is permitted for consistency with ``Con2 {}``.)

We can also use ``with`` to construct record expressions.
We can use newlines and indentation
::

  Rec with
    field1 = expr1
    field2 = expr2
    field3 = expr3

or we can use delimiting punctuation
::

  Rec with
    { field1 = expr1
    ; field2 = expr2
    ; field3 = expr3 }

Record update expressions look similar:
::

  r with
    field2 = newExpr2
    field3 = newExpr3

(where ``r`` is a record of type ``Rec``.)

The last construct in which ``with`` can appear is a record pattern.
For example, we can pattern match on a record ``r`` of type ``Rec`` as follows:
::

  case r of
    Rec with
      field1 = f1
      field2 = f2
      field3 = f3
        -> someExpression

Or more concisely using the ``NamedFieldPuns`` extension:
::

  case r of
    Rec with field1; field2; field3 -> someExpression

As this last example suggests, ``with`` should work seamlessly with syntax introduced by other language extensions.
The major ones to consider are:

1. ``NamedFieldPuns``
2. ``RecordWildCards``
3. ``GADTs``

Working with ``RecordWildCards`` means we can construct and pattern match on records using the syntax ``Rec with ..``.
We can also write variations on this such as constructing a record with some fields explicit and some taken from scope:
::

  let field2 = expr2
      field3 = expr3
  in
  Rec with
    field1 = expr1
    ..

Record ``with`` syntax can also work with GADTs in a fairly predictable way.
The following example shows a GADT declaration using ``with`` syntax.
::

  data G a where
    Con1 :: with
        i :: Int
        j :: Int
      -> G Int
    Con2 :: with { b :: Bool } -> G Bool

Note that to write ``Con2`` on a single line, the curly braces are required to separate the field type from the result type.


Effect and Interactions
-----------------------
The proposed change directly addresses a lack of language support for formatting records over several lines.

The change has very little interaction with other language features.
It introduces a new keyword but otherwise reuses established layout syntax.
Moreover, it does not clash with but rather complements existing extensions like ``NamedFieldPuns`` and ``RecordWildCards``.

Note that we have implemented record ``with`` syntax in a fork of GHC used for the `DAML <https://github.com/digital-asset/daml>`_ language.
We have found the user experience to be intuitive and did not encounter unexpected interactions with other features.


Costs and Drawbacks
-------------------
The main maintenance cost I can forsee is due to additional productions in the parser and extra information (a Boolean flag) in the AST nodes for records.
For example, future changes to record parsing or pretty printing would bear a slightly higher complexity.
However we do not consider this a significant cost.

On the user side, the proposed change could in fact be *more* intuitive for novice users.
The ``with`` syntax is consistent with other occurrences of layout syntax and promotes the use of indentation across the board.
It avoids questions around the placement of braces and commas as well as the need for alignment conventions.

The possible drawback is that there would be two different syntax styles to be aware of.
The mixture of semicolons and commas may be confusing, as
``Rec with { x :: Int; y :: Bool }`` looks quite similar to ``Rec { x :: Int, y :: Bool }``,
but the field delimiters are different.
This probably won't be much of an issue as ``with`` syntax will mostly be used with indentation instead of punctuation.


Alternatives
------------
There is no alternative syntax for records that I am aware of.
However, one could avoid large record expressions and patterns by using existing extensions such as ``NamedFieldPuns`` or ``RecordWildCards``.
For example, you can bind field names using a ``let`` or ``where`` block outside of a record expression:
::

  let field1 = expr1
      field2 = expr2
      field3 = expr3
  in
  Rec {..}

Another example is using field punning for an update expression (assuming a variable ``r`` of type ``Rec``):
::

  let field1 = newExpr1
      field2 = newExpr2
  in
  r { field1, field2 }

This is consistent with the Haskell idiom of using identation for blocks of name bindings.

These techniques are fairly limited, however.
For one, they do not apply to record type declarations.
Second, they rely on bound variables being exactly the same as the field names of the record in question.
Therefore the syntactic convenience cannot be used for multiple record expressions of the same type in the same scope.
(The same is true if you need to pattern match on multiple records of the same type in a single definition, as the field names would clash.)
Moreover, using ``RecordWildCards`` to construct records can be error prone as it is easy to use a variable from scope when you should have set the value of the field explicitly.


Unresolved Questions
--------------------

With the ``RecordWith`` extension enabled, how flexible should we be in mixing traditional and layout syntax?
For example, should we allow commas when using ``with`` (as in the following)?
::

  data Rec = Rec with field1 :: T1, field2 :: T2, field3 :: T3

If so, should we also allow the reverse scenario?
::

  data Rec = Rec { field1 :: T1; field2 :: T2; field3 :: T3 }

Allowing these cases may be more forgiving for users switching between the two syntaxes on different projects.

We do propose that declaring multiple fields of the same type should only use commas regardless of the syntax choice, e.g.
::

  data Rec = Rec with field1, field2 :: T12; field3 :: T3

Should we allow mixing commas and semicolons more generally (as in the following)?
::

  data Rec = Rec with field1 :: T1, field2 :: T2; field3 :: T3


Implementation Plan
-------------------
I volunteer to implement this feature with my collaborators.
As mentioned, we have a prototype implementation for the `DAML <https://github.com/digital-asset/daml>`_ language that we can use as reference.
We do not require any changes to GHC in advance of the feature.

The implementation entails the following changes to the compiler frontend:

1. Extra ``with`` token in the lexer (conditional on the ``RecordWith`` extension being set), enabling layout syntax
2. Extra productions in the parser for ``with`` followed by record fields within virtual braces, storing source locations as usual
3. Boolean flags in AST nodes involving records, indicating if ``with`` syntax was used
4. Embellishment of pretty printers to respect layout syntax (e.g. when reporting error messages)
