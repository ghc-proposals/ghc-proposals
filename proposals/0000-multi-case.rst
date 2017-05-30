.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `under discussion <https://github.com/ghc-proposals/ghc-proposals/pull/38>`_.

Multi-scrutinee case
====================

It is often convenient to `case` match on several expressions at once. Existing
mechanisms for doing so are syntactically awkard and/or limited.

Motivation
----------

Suppose I have

.. code-block:: haskell

    f (A a) y = e1
    f x (B b) = e2
    f x y = e3

and I want to change the top-level patterns to a `case` so I can add a `where`
clause scoping over the entire definition. One option is to use a nested
`case`:

.. code-block:: haskell

    f x y = case x of
              A a -> e1
              _ -> case y of
                     B b -> e2
                     _ -> e3

but this becomes rather unwieldy when there are many patterns. Another option
is to bundle the values together in a tuple:

.. code-block:: haskell

   f x y = case (x,y) of
             (A a, y) -> e1
             (x, B b) -> e2
             (x, y) -> e3

This is relatively reasonable, but it has two problems. It relies on the
optimizer to erase the tuples, and it does not work if any of the scrutinees
has an unlifted type. We can solve this using `UnboxedTuples`:

.. code-block:: haskell

   f x y = case (# x,y #) of
             (# A a, y #) -> e1
             (# x, B b #) -> e2
             (# x, y #) -> e3

but this seems like awfully heavy syntax for something so simple!

Proposed Change
---------------

Inspired by Icelandjack's proposal for optional tuple parenthesization, I
propose that we allow case matching on multiple scrutinees separated by commas:

.. code-block:: haskell

    f x y = case x, y of
              A a, y -> e1
              x, B b -> e2
              x, y -> e3

This would be interpreted just like all other pattern matching in Haskell: left
to right and top to bottom.

Drawbacks
---------

The main drawback is that the syntax looks somewhat different from the syntax
for defining functions in multpile clauses. One cannot simply change a bit of
indentation to switch from one to the other.

Alternatives
------------

One possible variant would be to bring the syntax closer to multiple-clause
function definition syntax:

.. code-block:: haskell

    f x y = case x, y of
      (A a) y -> e1
      x (B b) -> e2
      x y -> e3

The trouble with this alternative is that it feels less *internally*
consistent, with the scrutinees separated one way and the patterns another.
    

Unresolved Questions
--------------------
