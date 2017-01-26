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

When the construction function has no signature, infer its type as usual
for a top-level binding.

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a where
      NF :: NFData a => a -> NF a -- optional
      NF a = a `deepseq` UnsafeNF a

    pattern Zero :: (Eq a, Num a) => a
    pattern Zero <- ((== 0) -> True) where
      Zero :: Num a => a -- optional
      Zero = 0

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
predict that most current code will continue to work, albeit with warnings
about missing type signatures.

Alternatives
------------

I can think of four alternatives. The first three I greatly oppose. The
fourth I think has significant merit, and the fifth might not be a
terrible idea either.

Bad
~~~

An alternative I'd like to dismiss altogether would be to allow the user to
give a third set of constraints in the pattern type signature, to be used for
the construction function. Since having two sets of constraints is already
quite confusing enough, I think a third has very little to recommend it.

Another alternative I dislike would be to continue to use the current
arrangement when a type signature is missing. This would maintain full
backwards compatibility, but only by maintaining what I believe is a linguistic
wart.

A third alternative I greatly dislike would be to require the pattern signature
to be equivalent to the construction function signature with the exception of
constraints. While it would not be a bad idea to *warn* about violations of
such a rule, it has several downsides:

a. It is not required for type safety, and I firmly believe that it's not the
   type checker's place to enforce good taste.

b. If a user *wants* to work around such a rule, I believe they can always
   do so by writing horrifyingly ugly type signatures. We don't want to
   encourage that!

c. There may be reasonable signatures that such a rule would complicate
   unnecessarily, forcing users to use explicit equality constraints.

Good?
~~~~~

I think imposing something like my despised third alternative would be quite
reasonable, and perhaps helpful, in the case of a missing construction
signature. That is, if we had

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a where
      NF a = a `deepseq` UnsafeNF a

then it would implicitly insert a partial signature:

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a where
      NF :: _ => a -> NF a
      NF a = a `deepseq` UnsafeNF a

This would probably help prevent mistakes, and perhaps improve inference, while
allowing users to get whatever they want by just writing a signature.

Another idea that might not be terrible would be to add syntax to refer to
the post-constraint portion of the pattern signature from the constructor
function signature. So we could write, e.g.,

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a where
      NF :: NFData a => ...
      NF a = a `deepseq` UnsafeNF a

and have the `...` splice in `a -> NF a`, yielding a signature of `NF :: NFData
a => a -> NF a`. In case a pattern has a long and complicated signature, this
would be much more readable than naming the type involved:

.. code-block:: haskell

    type NFType a = a -> NF a
    pattern NF :: NFType a
    pattern NF a <- UnsafeNF a where
      NF :: NFData a => NFType a
      NF a = a `deepseq` UnsafeNF a

Unresolved questions
--------------------

Implementation Plan
-------------------
