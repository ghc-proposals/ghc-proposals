Unify type-level literals with their equivalent types
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/154>`_.
.. sectnum::
.. contents::

The kinds of type-level literals not correspond to their term-level equivalents' types. This makes promoting many common data structures useless, increasing code duplication. In addition, the current type-level literal types leave out trivially-addable functionality that would make them more broadly useful.

Motivation
----------
It is currently impossible to inspect the contents of a Symbol, because it is an opaque kind that only exposes equality, ordering, and appending. This means that it is impossible to use type-level strings to add compile-time validation of string-like values without using Template Haskell or a compiler plugin, meaning there is no way to write code that looks like the following:

::

    safePackBS :: forall str. (ValidBS str) => BS.ByteString
    safePackBS = BS.pack $ symbolVal (Proxy :: Proxy str)

    validBS, invalidBS :: BS.ByteString
    validBS = safePackBS @"foo"
    -- works!
    invalidBS = safePackBS @"中国語"
    -- fails with error: '\20013' is not a single-byte character.
    -- Gives clean, helpful error message for invalid input at compile time.

Turning compile-time problems into compile-time errors is one of the main reasons that I use Haskell, and this value-transparency would allow that to be done cleanly in more cases, and the opacity of ``Symbol``\s is not the only restriction that is unfortunately limiting.

This is largely motivated by the discussion on ghc-proposals/ghc-proposals#124 into a way to use type-level verification to make it safe to implement overloaded literals for the many types for which it is currently not possible, but I feel that this is both a substantial enough change and a potentially useful enough feature to be its own proposal rather than something added to a continuation of that work.

Proposed Change Specification
-----------------------------
The following is relative to the current behavior of ``DataKinds``.

Grammar changes
^^^^^^^^^^^^^^^
The lexical syntax would gain the following new case:

::

    signedint -> ['+'|'-'] integer

The ``atype`` non-terminal would gain new cases as follows:

::

    atype -> ...
           | string
           | char
           | integer
           | signedint
           | float

This is relative to the specification, not the current effects of ``DataKinds``.

Semantics
^^^^^^^^^
The kinds of type-level literals will be as follows:

::

    "foo" :: String
    'f' :: Char
    -123 :: Integer
    +123 :: Integer
    123 :: Natural
    3.14 :: Rational

While the ``Rational`` literals are the most dubious, they require very minimal changes (since ``Ratio Integer`` will now work correctly "for free") and are needed to satisfy the original impetus for this change.

The case of positive ``Integer`` literals is challenging. The syntax above is not great, but the obvious alternatives are a subkinding relation or an explicit conversion and I'd consider both of those to be worse.

To handle the legacy case, ``Symbol`` and ``Nat`` will become aliases for ``String`` and ``Natural``, respectively.

All of the mathematical type families in ``GHC.TypeNats`` will be generalized to be poly-kinded so that they work for these new numeric kinds, with the exception of a ``Rational`` implementation for  ``Div``, ``Mod``, and ``Log2``. ``/`` and ``%`` will be added for ``Rational``. Put more explicitly, their interfaces will be as listed in the block below, where ``a`` means any of ``Integer``, ``Natural``, or ``Rational`` and ``b`` means either ``Integer`` or ``Rational``.

::

    type family Add a (m :: a) (n :: a) :: a
    type family Mul a (m :: a) (n :: a) :: a
    type family Exp a b (m :: a) (n :: b) :: a
    type family Sub a (m :: a) (n :: a) :: a
    type family DivI a (m :: a) (n :: a) :: a
    type family ModI a (m :: a) (n :: a) :: a
    type family Log2I b (m :: b) :: b

    type (m :: a) + (n :: a) = Add a m n
    type (m :: a) * (n :: a) = Mul a m n
    type (m :: a) ^ (n :: b) = Exp a b m n
    type (m :: a) - (n :: a) = Sub a m n
    type (m :: a) / (n :: a) = DivI a m n
    type (m :: a) % (n :: a) = ModI a m n
    type Div (m :: b) (n :: b) = DivI b m n
    type Mod (m :: b) (n :: b) = ModI b m n
    type Log2 (m :: b) = Log2I b m

A new type family will be added, ``Data.Type.Equality.Cmp``, using a similar alias-wrapper trick to provide a uniform interface for comparisons.

::

    type family CmpI a (m :: a) (n :: a) :: Ordering

    type Cmp (m :: a) (n :: a) = CmpI a m n

Here, ``a`` is at least the five literal types, but there are many more implementations that could be added.

Effect and Interactions
-----------------------
For the specific example I used to motivate this change, the implementation of the ``ValidBS`` constraint is as follows:

::

    type ValidBS str = (KnownSymbol str, AllValidChars str) 

    type family AllValidChars (xs :: String) :: Constraint where
        AllValidChars (x:xs) = If (IsValidChar x) (AllValidChars xs) (TypeError (InvalidCharError x))
        AllValidChars '[]    = True ~ True

    type IsValidChar c = CmpChar c '\256' == LT
    type InvalidCharError c = ShowType c :<>: Text " is not a single-byte character."

This is a simple example, but it is a clear example of a program that is not possible to write as it stands and that would have practical applications.

In general, this makes this already well-loved feature of GHC even better, allowing more advanced uses of type-level strings and more flexible uses of type-level numerics for cases that require more than just the natural numbers. 


Costs and Drawbacks
-------------------
The development time will be fairly minimal, because the "new" functionality represents no novel codepaths or design challenges, merely adding additional parallel constructors and cases to code that already handles the existing type-level literals.

One potential drawback is that type-level strings are currently efficiently represented as ``FastString``\s, but these changes require a change to keeping them always as large, troublesome type-level lists of type-level ``Char``\s. The alternatives are either to (as I have implemented currently) keep ``Symbol``\s as they are but add a type family to convert them into promoted ``String``\s or to hack type-level pattern matching so that they extensively seem to be ``[Char]`` but are actually still efficient.

Alternatives
------------
Use of Template Haskell
^^^^^^^^^^^^^^^^^^^^^^^
Template Haskell is very flexible, but it carries both performance and readability drawbacks. There is no way to make a splice look "natural" in normal code, rather than adding what is likely unfamiliar syntax for beginning Haskellers. While the implementation of something like ``ValidBS`` is not entirely trivial, once written it looks perfectly natural.

Use of type-checker plugins
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Type-checker plugins are hard to write, intimidating for those who are not familiar with the GHC API, and require an explicit (and non-trivial) pragma in every file where they are used. While this will not replace every case where they are required (not even close!), it does increase the utility of type-level strings without them substantially.

Unresolved questions
--------------------
- Should this be a modification of ``DataKinds``, since it is (appears to be?) a strict superset of the previous behavior? Should it be a new extension?

Implementation Plan
-------------------
I have already written a patch that provides a basic implementation of much of the above, and I would be more than happy to implement the final state of this proposal myself.
