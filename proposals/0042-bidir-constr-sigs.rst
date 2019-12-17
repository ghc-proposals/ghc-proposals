Pattern synonym construction function signatures
================================================

.. author:: David Feuer
.. date-accepted:: 2017-01-25
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/14602
.. implemented::
.. highlight:: haskell
.. header:: This proposal was discussed at `this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/42>`_.
.. contents::

I propose to allow users to add type signatures to the constructor functions
defined in bidirectional pattern synonyms.

Motivation
----------

This proposal grows out of an idea Simon Peyton Jones expressed in
`Pattern synonym used in an expression context could have different constraints to pattern
used in a pattern context <https://gitlab.haskell.org/ghc/ghc/issues/8581#note_87372>`_.

    The two directions of an explicitly-bidirectional pattern might have utterly
    different class constraints. After all, the two directions are specified by
    quite different code. Suppose that

    - Pattern `P` (used in a pattern) requires constraints `CR`, and provides
      constraints `CP`

    - Constructor `P` (used in an expression) requires constraints `CE`

    Then I think the only required relationship is this: `CP` must be provable
    from `CE` (since `CP` is packaged up in a `P`-object).

Pattern synonyms can have type signatures expressing the constraints *required*
to match and the constraints *provided* by matching. These signatures seem to
express the types involved in pattern matching quite well.

*Bidirectional* patterns add another feature: they allow the pattern synonym to
be used to *construct* values using a definition provided in a `where` clause.
Unfortunately, it seems somewhat less attention was paid to the typing need of
this element. Today, the "smart constructor" defined in the `where` clause is
given the same type signature as the pattern, and the *union* of the required
and provided constraints. This turns out to be wrong in both directions.

The documentation on pattern synonyms offers one direction this is wrong:
something like numeric literal pattern matching. To use a numeric literal
as a *value*, the desired type must be an instance of `Num`. To use it in
a *pattern*, the desired type must be an instance of both `Num` and `Eq`.
It is impossible to express such requirements with pattern synonyms. Suppose
we wanted to name numbers, like `Zero`.

.. code-block:: haskell

    pattern Zero :: (Num a, Eq a) => a
    pattern Zero <- ((== 0) -> True)
      where
        Zero = 0

The trouble in this case is that the `Eq` constraint from the pattern
"infects" the constructor. So if I have a number type I can't test for
equality, I can't use `Zero` to construct it.

One good example in the other direction is provided by Edward Yang's `nf`
package, where we have

.. code-block:: haskell

    newtype NF a = UnsafeNF a

    makeNF :: NFData a => a -> NF a
    makeNF a = a `deepseq` UnsafeNF a

    getNF :: NF a -> a
    getNF (MkNF a) = a

We have a pattern synonym `NF` to work with this type conveniently.

.. code-block:: haskell

    pattern NF :: NFData a => a -> NF a
    pattern NF a <- UnsafeNF a where
      NF a = a `deepseq` UnsafeNF a

The smart constructor `NF` has exactly the right type (the same as `makeNF`).
But the pattern synonym has an overly stringent "requires" constraint.
We have absolutely no need for any constraints when matching, but we are
forced to include an `NFData` constraint there to allow the smart constructor
to get it.

Proposed Change Specification
-----------------------------

Allow a type signature for the construction function in a bidirectional
pattern synonym to appear within the same `where` clause:

.. code-block:: haskell

    pattern Zero :: (Num a, Eq a) => a
    pattern Zero <- ((== 0) -> True)
      where
        Zero :: Num a => a
        Zero = 0

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a where
      NF :: NFData a => a -> NF a
      NF a = a `deepseq` UnsafeNF a

The type signature for the construction function must be the same as the
pattern signature, except for its constraints. Whereas Peyton Jones suggested
that the constraints provided by the pattern should be implied by the
constraints on the constructor, we do not make such a demand; in rare cases it
can be invaluable to violate it. For example, we could write

.. code-block:: haskell

    pattern TR :: () => Typeable a => TypeRep a -> SomeTypeRep
    pattern TR t <- ... where
      TR :: TypeRep a -> SomeTypeRep
      TR t = ...

using the ``withTypeable`` function to obtain the necessary ``Typeable``
dictionary.

When the construction function has no signature, there are several possible
options, none of which is perfect. For the sake of compatibility with GHC 8.0
and 8.2, the default will continue to be the pattern signature with the union
of the provided and required pattern constraints.


Possible Extension: Offer special syntax to refer to the pattern signature
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could imagine offering special syntax that can be used in the
constructor signature to splice in (textually) one or more pieces of the
pattern signature, allowing the user to offer a complete signature without
copying and pasting. It's not entirely clear what this would look like,
and stealing more syntax is expensive, so I don't know that it's really
worth the trouble.

Effect and Interactions
-----------------------
I do not anticipate any particularly notable effects on or interactions
with other language features.

The GHCi `:info` command will report both types, unless they are the same, by
abbreviating the pattern declaration:

.. code-block:: haskell

    pattern NF :: a -> NF a where
      NF :: NFData a => a -> NF a

Based on the plan outlined in
`#14478 <https://gitlab.haskell.org/ghc/ghc/issues/14478>`_, we would allow
similarly abbreviated signatures in `.hs-boot` and `.hsig` files:

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- .. where
      NF :: NFData a => a -> NF a

Indeed, we could use the more informative form for `:info` as well; that is
largely orthogonal to this proposal.

Costs and Drawbacks
-------------------
The main costs will be modifying the parser and simplifying the way the type
checker handles the construction functions. I don't anticipate
that these costs will be very high. I believe this change has minimal impact on
learnability of the language, as new users are relatively unlikely to define
pattern synonyms.

Alternatives
------------

There are several options for what to do in the case of a missing constructor
signature.

Option 1: Plain inference
~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest option is just to treat the construction functon like any other
top-level binding and try to infer its type.

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a where
      NF :: NFData a => a -> NF a -- optional
      NF a = a `deepseq` UnsafeNF a

    pattern Zero :: (Eq a, Num a) => a
    pattern Zero <- ((== 0) -> True) where
      Zero :: Num a => a -- optional
      Zero = 0

Option 2: Assign a signature based on *provided* constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another simple option would be to implicitly give the construction
function the same signature as the pattern, but using only *provides*
constraints and ignoring *requires* ones. A construction synonym for
`NF` would be mandatory in this case. It would be possible to avoid one
for `Zero` by adding a redundant *provides* constraint, but that does not
look like good style to me.

.. code-block:: haskell

    pattern Zero :: (Eq a, Num a) => Num a => a
    pattern Zero <- ((== 0) -> True) where
      Zero = 0

Option 3: Assign a partial signature based on the *provided* constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A third approach would be to give the construction function a partial signature
based on *provides* constraints. That is, given

.. code-block:: haskell

    pattern P :: Req => Prov => E

it would assign the constructor the type

.. code-block:: haskell

    P :: (Prov, _) => E

The type system gurus, unfortunately, have concluded that this option would
be difficult and fragile.


A useful generalization
~~~~~~~~~~~~~~~~~~~~~~~

The requirement that the construction function signature be the same as the
pattern signature with the exception of constraints has several downsides.
An alternative would be to allow it to be completely unrelated, and a weaker
alternative would be to force it to have the same final result type, but
otherwise to be unrelated.

In many cases, the restriction can be worked around using ugly constraints.
However, this is not always so in the presence of `RankNTypes`. For example,
one might wish to write a pattern that looks like this:

.. code-block:: haskell

    pattern P :: T S -> V
    ....
      where
        P :: (forall s. T s) -> V
        ...

This would allow the author of a pattern synonym to force the user of the
construction function to provide a *more polymorphic* term than the pattern
user can get back out. The restriction in the current proposal forbids
such signatures.

Unresolved questions
--------------------

Implementation Plan
-------------------
