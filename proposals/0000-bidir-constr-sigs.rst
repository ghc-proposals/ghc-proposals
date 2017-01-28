.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

.. role:: haskell(code)
   :language: haskell

.. default-role:: haskell

This proposal is discussed at `this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/42>`_.

Pattern synonym construction function signatures
================================================

I propose to allow users to add type signatures to the constructor functions
defined in bidirectional pattern synonyms.

Motivation
----------

This proposal grows out of an idea Dr. Érdi Gergő expressed in
https://ghc.haskell.org/trac/ghc/ticket/8581. Pattern synonyms can have type
signatures expressing the constraints *required* to match and the constraints
*provided* by matching. These signatures seem to express the types involved
in pattern matching quite well.

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

Considering such cases led Dr. Érdi to conclude that we should be able to
specify separate constraints for the pattern and the constructor. I believe
a simpler approach, both for users and implementers, is to allow the
constructor to have an entirely independent type signature, and to use
inference to determine its type in the absence of such a signature.

Proposed Change Specification
-----------------------------

Allow a type signature for the construction function in a bidirectional
pattern synonym to appear within the same `where` clause.

When the construction function has no signature, there are several possible
options, none of which is perfect.

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

A third approach, which I am beginning to think might be the best of all,
would be to give the construction function a partial signature
based on *provides* constraints. That is, given

.. code-block:: haskell

    pattern P :: Req => Prov => E

it would assign the constructor the type

.. code-block:: haskell

    P :: (Prov, _) => E

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
with other language features, except the compatibility issue noted below.

Costs and Drawbacks
-------------------
The main costs will be modifying the parser and simplifying the way the type
checker handles the construction functions. I don't anticipate
that these costs will be very high. I believe this change has minimal impact on
learnability of the language, as new users are relatively unlikely to define
pattern synonyms.

The biggest drawback is that the proposal handles construction functions
without type signatures in a new and potentially incompatible way. I believe
it is worth doing so for the simple reason that the current arrangement is
fundamentally wrong and inconsistent with the rest of the language. Nowhere
else is a potentially incorrect signature assigned in such a fashion. I
predict that most current code will continue to work, albeit perhaps with
warnings about missing type signatures.

Alternatives
------------

I don't like any of the below ideas at all.

Alternative 1
~~~~~~~~~~~~~

Allow the user give a third set of constraints in the pattern type signature,
to be used for the construction function. Since having two sets of constraints
is already quite confusing enough, I think a third has very little to recommend
it.

Alternative 2
~~~~~~~~~~~~~

Continue to use the current arrangement when a type signature is missing. This
would maintain full backwards compatibility, but only by maintaining what I
believe is a linguistic wart.

Alternative 3
~~~~~~~~~~~~~

Require the pattern signature to be equivalent to the construction function
signature with the exception of constraints. While it would not be a bad idea
to *warn* about violations of such a rule, it has several downsides:

a. It is not required for type safety, and I firmly believe that it's not the
   type checker's place to enforce good taste.

b. If a user *wants* to work around such a rule, I believe they can always
   do so by writing horrifyingly ugly type signatures. We don't want to
   encourage that!

c. There may be reasonable signatures that such a rule would complicate
   unnecessarily, forcing users to use explicit equality constraints.

Unresolved questions
--------------------

Implementation Plan
-------------------
