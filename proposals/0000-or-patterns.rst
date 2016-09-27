.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Or patterns
===========

We propose a new syntax extension for "or-patterns". An or pattern is
essentially a list of patterns, where patterns match exactly the same set of
variables of same types. The right hand side is shared by all these patterns,
and can refer to the variables matched by the patterns.

Main advantages of this extension are:

1. With or patterns we can avoid `_` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. It allows more code reuse as right hand sides can be shared by many
   patterns.

Motivation
----------

There are two motivations as summarised in the abstract.

**First,** `_` patterns make code harder to maintain. They essentially mean "match
every other pattern", which also includes "patterns that may be enabled in the
future" e.g. when a new constructor is added to a type.

In my experience this is rarely the intention. Usually, when a new constructor
is added, the programmer needs to revisit functions on the type and update them
accordingly. But if functions use `_` patterns this is not easy as she won't be
getting any compile time warnings about functions she needs to update.

This is also against the Haskell way of refactoring programs. Haskell is well
known for its features that make refactoring easier than most other languages
[1]_, but `_` patterns actually make refactoring harder.

As an example, GHC developers would know that adding a new constructor to an
existing type means many compile-run-edit cycles, with no compile-time help,
because of `_` patterns. Given that by default we don't get stack traces in
Haskell, and also GHC takes a lot of time to build, this wastes GHC developers'
time.

Or patterns solve this problem by allowing programmers to explicitly match a
list of constructors in a concise way. As an example, suppose we had this type:

::

    data T = T1 String | T2 | T3

We might want to write a function on this like

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT _      = Nothing

Now suppose that some time later we add a new constructor:

::

    data T = T1 String | T2 | T3 | T4 String

We need to update `stringOfT` but unfortunately we don't get a warning because
we used a `_` pattern.

Or patterns solve the problem by allowing us to do this:

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s)        = Just s
    stringOfT (T2{} | T3{}) = Nothing

This function doesn't match `T4`, so we get our warning.

**Second,** or patterns allow more code reuse. In our previous example, we might
extend `stringOfT` to something like

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT (T4 s) = Just s
    stringOfT _      = Nothing

While this is not too bad (and we can always introduce new functions for similar
right hand sides), as the number of constructors increase this becomes
repetitive.

Or patterns can solve this problem like this

::

    stringOfT :: T -> Maybe String
    stringOfT (T1 s | T4 s) = Just s
    stringOfT (T2{} | T3{}) = Nothing

Now we have code reuse, and we will get nice warnings next time a new
constructor is added.

Details
-------

An or pattern can appear in anywhere that an ordinary pattern can appear.

We extend `Haskell 2010 pattern grammar
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-590003.17.1>`_
with a new non-terminal:

::

    [new]
    pats ->  pat `|` pats
         |   pat

    pat	->	lpat qconop pat	    (infix constructor)
        |	lpat

Type checking
~~~~~~~~~~~~~

A restriction is that patterns in `pats` need to bind exactly same identifiers.
Since type checking happens before desugaring, this is checked by type checker.

Type checker then type checks the RHS using one of the patterns. Using types
inferred for the binders, rest of the patterns are then checked. If binders
with same names in different patterns have different types, a type error is
shown:

::

    * Binders in patterns have different types.

        `x` in pattern `T1 x` has type `String`
        `x` in pattern `T2 _ x` has type `Int`

One improvement that would give us a better error messages would be to type
check RHS using different patterns, until a pattern that makes RHS well-typed is
found. Then rest of the patterns are compared against this pattern, considering
binder types in this pattern as "expected" types.

Desugaring
~~~~~~~~~~

RHS of a or pattern is abstracted as a function that takes variables bound by
the patterns as arguments, and floated out to the same level with the case
expression. Patterns are then desugared as if they're ordinary patterns.
Example:

::

    f :: Either (Either Int Int) (Maybe Int) -> IO ()
    f (Left (Right i) | Right (Just i)) = <expr_1>
    f _ = <expr_2>

First `<expr_1>` is abstracted as a function:

::

    f_rhs_1 i = <expr_1>

Then the or pattern is expanded to ordinary patterns:

    f (Left (Right i)) = f_rhs_1 i
    f (Right (Just i)) = f_rhs_1 i
    f _ = <expr_2>

This function is then desugared as usual.

When patterns have multiple binders, function for RHS takes an unboxed tuple,
instead of taking bound variables as multiple arguments. Example:

::

    f (X1 a b | X2 b a) = ...

This is first desugared to:

::

    f_rhs_1 (# a, b #) = ...

    f (X1 a b) = r_rhs_1 (# a, b #)
    f (X2 b a) = r_rhs_1 (# a, b #)

Proposed Change
---------------

TODO

Drawbacks
---------

TODO

Alternatives
------------

TODO

Unresolved Questions
--------------------

- `As far as I can see
  <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4>`_,
  `|` is a reserved operator. So I think we can use it here, but need to make
  sure.

- Need to figure how this interacts with

  - GADTs
  - Pattern synonyms
  - Existentials
  - ViewPatterns
  - BangPatterns
  - Irrefutable patterns

.. [1] For a recent talk on this topic, see https://www.youtube.com/watch?v=_K6UAq4hjAs
