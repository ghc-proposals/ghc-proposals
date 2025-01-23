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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/231>`_.
.. sectnum::
.. contents::

We propose a layout syntax for records, triggered by a new keyword ``with``.
This means that record fields in type declarations, expressions and patterns can be formatted using indentation instead of punctuation.
The feature removes syntactic noise and makes large records easier to read and modify.
It is consistent with the use of layout for other constructs that span several lines, e.g. ``do`` blocks and ``case`` expressions.


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

Informal Specification
~~~~~~~~~~~~~~~~~~~~~~
We propose a language extension called ``RecordWith`` offering layout syntax for records.
With the extension enabled, there is an additional keyword ``with`` inducing layout syntax for record fields.

The ``with`` keyword is a so-called *layout herald*: it enables the layout mechanism in the same way as ``where``, ``let``, ``do`` and ``of``.
In the formal grammar, ``with`` should be followed by curly braces ``{ }`` and semicolons ``;`` delimiting fields.
In the source program, the user can instead use indentation and newlines as delimiters, and the layout mechanism will automatically insert the missing braces and semicolons.
A general description of the layout mechanism is available `here <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7>`_.

To achieve a uniform record syntax, we propose to allow semicolons as field delimiters even without the ``with`` keyword.
(However you cannot mix commas and semicolons in a single field list.)
We allow optional leading and trailing semicolons, as well as extraneous repeated semicolons, for consistency with other layout syntax.
The more flexible semicolon delimiters could be useful for easier record editing, even without layout syntax.

The syntax proposal is easiest to understand through examples.
Firstly, we can declare a record ``Rec`` using the layout herald ``with`` and newline delimiters.
::

  data Rec = Rec with
    field1 :: T1
    field2 :: T2
    field3 :: T3

The layout mechanism inserts the necessary punctuation to get the following equivalent declaration.
::

  data Rec = Rec with
    { field1 :: T1
    ; field2 :: T2
    ; field3 :: T3 }

Alternatively, you can write a hybrid version all on one line as follows.
::

  data Rec = Rec with field1 :: T1; field2 :: T2; field3 :: T3

Here the layout mechanism inserts the curly braces for us.

We can also use semicolon delimiters without the ``with`` layout herald:
::

  data Rec = Rec { field1 :: T1; field2 :: T2; field3 :: T3 }

These declarations are all equivalent to the traditional record syntax:
::

  data Rec = Rec { field1 :: T1, field2 :: T2, field3 :: T3 }

The ``with`` syntax can occur in any language construct where traditional record syntax usually appears.
Specifically, we consider the following features:

1. Data type declarations
2. Record construction
3. Record field updates
4. Record patterns

We showed examples of the first case above.
Note that ``with`` should work in data types with several constructors, as well as constructors with zero arguments.
So the following declaration is valid:
::

  data ComplexRec = Con1 with f1 :: Int; f2 :: Bool | Con2 with | Con3 String

(``Con2 with`` has the same meaning as ``Con2 {}``.)

We can also use ``with`` to construct record expressions.
We can use newlines and indentation
::

  Rec with
    field1 = expr1
    field2 = expr2
    field3 = expr3

or we can write the punctuation explicitly
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

Record ``with`` syntax also works with GADTs in a fairly predictable way.
The following example shows a GADT declaration using ``with`` syntax.
::

  data G a where
    Con1 :: with
        i :: Int
        j :: Int
      -> G Int
    Con2 :: with { b :: Bool } -> G Bool

Note that to write ``Con2`` on a single line, the curly braces are required to separate the field type from the result type.

Extension to BNF Grammar
~~~~~~~~~~~~~~~~~~~~~~~~

Here we show the proposed extension to the BNF grammar of the Haskell 2010 `base language <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_.
Recall the four affected record constructs: data constructor declarations, record construction and update expressions, and record patterns.
These correspond to four new lines in the ``constr``, ``aexp`` and ``apat`` productions.

Note that the grammar specification assumes that the layout mechanism has inserted all missing curly braces and semicolons to the user-written layout syntax.
The optional ``[with]`` syntax in the new rules reflects our proposal that records can use semicolon delimiters even without the ``with`` keyword.
For clarity, we only include the modified productions with some surrounding ones for context.
The new lines in the grammar are marked with a ``-- NEW`` comment.

::

  ...
  
  topdecls	→	topdecl1 ; … ; topdecln	    (n ≥ 0)
  topdecl	→	type simpletype = type
  		|	data [context =>] simpletype [= constrs] [deriving]
  		|	newtype [context =>] simpletype = newconstr [deriving]
  		|	class [scontext =>] tycls tyvar [where cdecls]
  		|	instance [scontext =>] qtycls inst [where idecls]
  		|	default (type1 , … , typen)	    (n ≥ 0)
  		|	foreign fdecl
  		|	decl
  
  ...

  constrs	→	constr1 | … | constrn	    (n ≥ 1)
  constr	→	con [!] atype1 … [!] atypek	    (arity con  =  k, k ≥ 0)
  		|	(btype | ! atype) conop (btype | ! atype)	    (infix conop)
  		|	con { fielddecl1 , … , fielddecln }		(n ≥ 0)
  		|	con [with] { fielddecl1 ; … ; fielddecln }	(n ≥ 0) -- NEW
  newconstr	→	con atype
  		|	con { var :: type }
  fielddecl	→	vars :: (type | ! atype)
  deriving	→	deriving (dclass | (dclass1, … , dclassn))	    (n ≥ 0)
  dclass	→	qtycls
  
  ...
  
  aexp		→	qvar	    (variable)
  		|	gcon	    (general constructor)
  		|	literal
  		|	( exp )	    (parenthesized expression)
  		|	( exp1 , … , expk )	    (tuple, k ≥ 2)
  		|	[ exp1 , … , expk ]	    (list, k ≥ 1)
  		|	[ exp1 [, exp2] .. [exp3] ]	    (arithmetic sequence)
  		|	[ exp | qual1 , … , qualn ]	    (list comprehension, n ≥ 1)
  		|	( infixexp qop )	    (left section)
  		|	( qop⟨-⟩ infixexp )	    (right section)
  		|	qcon { fbind1 , … , fbindn }		(labeled construction, n ≥ 0)
  		|	qcon [with] { fbind1 ; … ; fbindn }	(labeled construction, n ≥ 0) -- NEW
  		|	aexp⟨qcon⟩ { fbind1 , … , fbindn }	(labeled update, n  ≥  1)
  		|	aexp⟨qcon⟩ [with] { fbind1 ; … ; fbindn }	(labeled update, n  ≥  1) -- NEW
  
  ...

  fbind		→	qvar = exp
  
  ...
  
  apat		→	var [ @ apat]	    (as pattern)
  		|	gcon	    (arity gcon  =  0)
  		|	qcon { fpat1 , … , fpatk }	(labeled pattern, k ≥ 0)
  		|	qcon [with] { fpat1 ; … ; fpatk }	(labeled pattern, k ≥ 0) -- NEW
  		|	literal
  		|	_	    (wildcard)
  		|	( pat )	    (parenthesized pattern)
  		|	( pat1 , … , patk )	    (tuple pattern, k ≥ 2)
  		|	[ pat1 , … , patk ]	    (list pattern, k ≥ 1)
  		|	~ apat	    (irrefutable pattern)
  
  fpat		→	qvar = pat
  
  ...


Effect and Interactions
-----------------------
The proposed change directly addresses a lack of language support for formatting records over several lines.

The change has very little interaction with other language features.
It introduces a new keyword but otherwise reuses established layout syntax.
Moreover, it does not clash with but rather complements existing extensions like ``NamedFieldPuns`` and ``RecordWildCards``.

Note that we have implemented ``with`` syntax in a fork of GHC used for the `DAML <https://github.com/digital-asset/daml>`_ language.
We have found the user experience to be intuitive and did not encounter unexpected interactions with other features.


Costs and Drawbacks
-------------------
From a development perspective, the main maintenance costs I forsee are due to:

1. Additional productions in the parser and
2. Extra information in the record AST nodes indicating the user's choice of syntax.

Future changes to record parsing or pretty printing would need to take these extra cases into account and be slightly more complex.

On the user side, the proposed change could in fact be *more* intuitive for novice users.
The ``with`` syntax is consistent with other occurrences of layout syntax and promotes the use of indentation across the board.
It avoids questions around the placement of braces and commas as well as the need for alignment conventions.
However, it does require users to be familiar with both old and new styles of syntax, especially for moving between codebases.

One obvious drawback is the introduction of a new keyword.
This means that ``with`` cannot be used as an identifier in any module or project using the extension.
We have been informed of uses of ``with`` for naming resource allocation functions.
Such names would need to be changed to be compatible with the extension.
We explore the option of reusing an existing keyword in the Alternatives section.


Alternatives
------------

Workarounds Using Existing Extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is currently no alternative syntax for records.
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

Reusing an Existing Keyword
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We explored whether we could use an existing layout keyword instead of ``with``.
The ones which read sensibly are ``where`` and ``of``.

``where`` reads quite naturally for record constructs.
For example, you could declare a record like
::

  data R = R where
    f1 :: Int
    f2 :: Int

However, there are serious ambiguities when it comes to record construction and update.
Using the previous declaration of ``R``, the following term declaration has two type-correct meanings:
::

  e = R where f1 = 2; f2 = 2

(one being a record of type ``R`` and the other being the constructor of type ``Int -> Int -> R`` ignoring the local ``f1`` and ``f2`` bindings).

We get a similar ambiguity for record updates:
::

  r = R 1 2
  e = r where f1 = 11; f2 = 22

These issues seem to be dealbreakers for reusing ``where`` for record fields.

``of`` on the other hand may be more plausible.
A declaration looks like
::

  data R = R of
    f1 :: Int
    f2 :: Int

However there may be confusion (both for users and for the GHC parser) when trying to parse a record expression within a ``case`` expression.
::

  c = case R of f1 = 1; f2 = 2 of
        R of f1; f2 -> f1

There is only one successful parse of this example, but it may cause an error in Happy parser generation or parsing itself.

You can see further issues for a record update within a ``case`` expression.
::

  r = R of f1 = 1; f2 = 2
  c = let f1 = 11 in
      case r of f1 of f1 -> f1

The first ``of f1`` in ``c`` is a field update using ``NamedFieldPuns`` whereas the second ``of f1`` starts a pattern binding a variable ``f1``.
There is only one successful parse of this example too, but again I expect this to cause trouble for the parser.


Unresolved Questions
--------------------

No unresolved questions.


Implementation Plan
-------------------
I volunteer to implement this feature with my collaborators at Digital Asset.
As mentioned, we have a prototype implementation for the `DAML <https://github.com/digital-asset/daml>`_ language that we can use as reference.
We do not require any changes to GHC in advance of the feature.

The implementation entails the following changes to the compiler frontend:

1. Extra ``with`` token in the lexer, conditional on the ``RecordWith`` extension being set, enabling layout syntax
2. Additional parser rules reflecting the new grammar, i.e. record fields delimited by semicolons following the optional ``with`` keyword
3. Additional data in record-related AST nodes, indicating the user's choice of syntax
4. Embellishment of pretty printers to preserve the user's choice of record syntax, e.g. when reporting error messages
