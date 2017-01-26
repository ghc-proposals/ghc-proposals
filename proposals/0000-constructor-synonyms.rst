.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is discussed at `this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/41>`_.

Proposal title
==============

I propose to add *constructor synonyms* to complement pattern synonyms. These
would allow users to define variables with capitalized names and operators
that begin with colons.

The basic motivation is to make bidirectional pattern synonyms more useful
and conceptually cleaner.


Motivation
------------

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
specify separate constraints for the pattern and the constructor. When I
thought about it some more, I realized that aside from import/export control,
there wasn't really any connection between a pattern synonym and a constructor
synonym at all, and no inherent reason for their types to even be related.

Proposed Change Specification
-----------------------------

Offer a new keyword for defining constructor synonyms. A constructor synonym
is a value binding whose name starts with either a capital letter or a colon.
For example,

.. code-block:: haskell

    constructor Foo :: String
    constructor Foo = "Hello "

    let constructor Bar :: String
        constructor Bar = "World"
    in Foo ++ Bar

    constructor (:|>) :: Seq a -> a -> Seq a
    constructor as :|> a = as |> a

Constructor synonyms and pattern synonyms would be allowed to share names,
so we could write

.. code-block:: haskell

    pattern NF :: a -> NF a
    pattern NF a <- UnsafeNF a

    constructor NF :: NFData a => a -> NF a
    constructor NF a = a `deepseq` UnsafeNF a

    pattern Zero :: (Eq a, Num a) => a
    pattern Zero <- ((== 0) -> True)

    constructor Zero :: Num a => a
    constructor Zero = 0

The current full bidirectional pattern syntax would become a shorthand for a
pattern synonym definition and a constructor synonym definition. The constructor
synonym would be given its current estimated type by default, but could also be
annotated with a type signature:

.. code-block:: haskell

    pattern Zero :: (Eq a, Num a) => a
    pattern Zero <- ((== 0) -> True) where
      Zero :: Num a
      Zero = 0

As Richard Eisenberg notes, it would be very difficult to specify or understand
syntax for defining constructor synonyms using pattern bindings, so the following
would be prohibited:

.. code-block:: haskell

    constructor (X, a) = y

Effect and Interactions
-----------------------
I do not anticipate any particularly notable effects on or interactions
with other language features, except regarding import and export syntax
as detailed in the "Unresolved questions" section.

Costs and Drawbacks
-------------------
The main cost, I believe, will be modifying the parser. I don't anticipate
that this cost will be very high. I believe this change has minimal impact on
learnability of the language, as new users are relatively unlikely to define
pattern synonyms. That said, I think making this split reduces the number of
things that have to be learned at once by separating two fundamentally
orthogonal features.


Alternatives
------------
One possible modification is to allow constructor synonyms to be defined only
at the top level. I don't care much one way or the other.

One alternative is to support only bidirectional pattern syntax with
an additional signature, or yet another context. Personally, I believe
this is unwieldy. Furthermore, it prevents users from writing constructor
synonyms without corresponding pattern synonyms, when there is no apparent
reason to prohibit such. And, worst, it continues to tie together language
features that have almost nothing to do with each other.


Unresolved questions
--------------------
Import and export syntax are still open. I believe we should allow a
`constructor` clause as we currently allow a `pattern` one. I further
believe that the pattern bundling syntax should capture both the pattern
and the constructor synonym if both have the same name. The bigger
question is what should happen to `pattern` imports and exports. I think
probably the best thing for backwards compatibility would be to make
`pattern` catch the constructor if and only if the constructor was defined
in a `where` clause. It's ugly, but I can't think of a better way.


Implementation Plan
-------------------
