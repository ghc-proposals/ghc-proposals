Type functions
==============

.. author:: Rinat Striungis
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/19634
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/509>`_.
.. contents::

Type-level functions are currently modeled using type families. This approach
has a few drawbacks:

1. Lack of expressive power to define complex computations (no ``let``, ``where``, ``case of``, and so on).
2. Inefficient and slow reduction.
3. Inability to use FFI or low-level primitives.

We propose to make it possible to call term-level functions at the type level
by reusing Template Haskell infrastructure.

Motivation
----------
There are two major motivations:

1. To allow users to define primitive functions over promoted built-in types,
   such as ``Char`` or ``Natural``. Currently such functions must be built into
   the compiler, leading to excessive amounts of code. A great example is
   `!3598 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3598>`_ that
   adds over 1500 lines of code, mostly due to built-in type families.

2. To make it practical to perform complex computations at the type level, for
   example parsing. Although it is already possible to encode such computations
   as type families using existing language features, the resulting type families
   are slow to reduce, and their code is hard to read and understand.

We propose to allow the use of term-level functions at the type level to
address these issues. For example::

  p1 :: Data.Char.isDigit '0' :~: True
  p1 = Refl

This declaration checks that ``isDigit '0'`` holds during type checking.

A few restrictions are necessary to make this work:

* The name of the function must be module-qualified to disambiguate it from an
  implicitly-quantified type variable. That is, the programmer must write
  ``Data.Char.toUpper`` or ``C.toUpper`` rather than simply ``toUpper``.

  To clarify why this is necessary, recall that GHC treats the following
  declarations equivalently::

    id ::           a -> a
    id :: forall a. a -> a

  Therefore, it would also treat the following equivalently::

    p1 ::                 isDigit '0' :~: True
    p1 :: forall isDigit. isDigit '0' :~: True

  And this is not what we want. When we write ``isDigit``, we mean the
  term-level function from ``Data.Char``, not an implicitly quantified type
  variable. This restriction could be addressed by changing implicit
  quantification rules, which we leave out of scope of this proposal.

* The function cannot be used in the module in which it is defined because we
  need an object file (``.o``) to link to. This is the same limitation as we
  have in Template Haskell, since we are reusing the underlying mechanism.

* The function must be fully instantiated and applied to all inputs.
  Furthermore, inputs and the output must be of **promotable** types. The set
  of promotable types is implementation-defined, but it's going to include at
  least ``Char``, ``Bool``, ``Natural``, ``String``, ``Maybe a``,
  ``Either a b``, ``[a]``, ``(a, b)``, and so on.

Even with those limitations in place, we expect the feature to be of great
utility. Consider this excerpt from the documentation of the ``servant``
library used to derive webservices from a declarative specification::

  >>> -- GET /books/:isbn
  >>> type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book

You'll notice that the example is first given using the intuitive notation
``GET /books/:isbn``, and then using the ``servant``-specific syntax ``:>``,
``Capture``, and so on.

Instead, we could imagine defining the endpoint using the intuitive notation
only::

  type MyApi = Endpoint "GET /books/:isbn" (Text -> Book) '[JSON]

The only problem is that we'd need a type family ``Endpoint`` that could parse
the intuitive specification, but type-level parsing using type families is
difficult and slows down the compiler.

However, if we could write the parser at the term level, i.e. introduce a
helper function ``parseEndpoint :: String -> [Either String String]``, and then
use it to define ``Endpoint``, then this approach would be viable.

We could define all sorts of type-level parsers, from SQL queries to CSS color
notation, opening up new possibilities for type-level programming.

Proposed Change Specification
-----------------------------

* Extend the type syntax with qualified lowercase names ``M.fn``.

* Kind checking the application of a term to zero or more type arguments,
  ``M.fn t0 t1 ... tn``, is done as follows:

  1. Check that ``M.fn`` is defined in another module that can be dynamically
     loaded, otherwise report a type error.
  2. Check that the types of arguments ``t0 t1 ... tn`` and the result type are
     monomorphic and **promotable** (defined below), otherwise report a type
     error.
  3. Instantiate and solve constraints, e.g. ``Prelude.sum [2,3,5]`` is
     desugared into ``Prelude.sum @Natural $dNumNatural [2,3,5]``.
     If there's no valid instantiation or constraints cannot be solved, report
     a type error.

  At least the following types are **promotable**:

  * ``Char``, ``Natural``, ``Bool``, ``()``, ``Void``
  * ``Maybe a`` if ``a`` is promotable
  * ``[a]`` if ``a`` is promotable
  * ``Either a b`` if ``a`` and ``b`` are promotable
  * ``(a, b)`` if ``a`` and ``b`` are promotable
  * ``(a, b, c)`` if ``a``, ``b`` and ``c`` are promotable
   
  The set of promotable types can be extended in the future and must be
  specified in the User's Guide.

* The application of a term to type arguments ``M.fn t0 t1 ... tn`` is reduced
  as follows:

  1. Reduce the arguments to their normal forms. If any of them contain stuck
     type families or skolems, the function application is also stuck.
  2. Evaluate the term-level function.
  3. Force the result of evaluation to normal form.
     Synchronous exceptions are caught and reported as type errors.
     Asynchronous exceptions crash the compiler.

Examples
--------

1. Basic functions over built-in types::

    p1 :: Data.Char.toUpper 'x' :~: 'X'
    p1 = Refl

    p2 :: Data.Char.isDigit 'x' :~: False
    p2 = Refl

2. Stuck on skolems, reduced when instantiated:
   ::

     p3 :: forall c uc. (Data.Char.toUpper c ~ uc, KnownChar uc) => Proxy c -> Char
     p3 Proxy = charVal (Proxy @uc)

     ghci> p3 (Proxy @'x')
     'X'

   In the definition of ``p3``, ``Data.Char.toUpper c`` is stuck. However, in the use of
   ``p3``, the ``c`` is instantiated to ``'x'`` and ``Data.Char.toUpper 'x'`` gets
   reduced to ``'X'``.

3. Stuck on skolems, never reduced:
   ::

     g :: forall (a :: Nat). a -> Prelude.id a
     g = id   -- rejected

   In the type signature of ``g``, ``Prelude.id a`` is stuck. In the definition
   of ``g``, we get a type error due to a type mismatch between ``a`` and
   ``Prelude.id a``.

   This example demonstrates a limitation of the proposed approach. The example
   would compile if we defined ``Id`` as follows::

     type Id :: a -> a
     type family Id x where
       Id x = x

Compile-time performance
------------------------

Our proof-of-concept implementation shows a two orders of magnitude improvement
in reduction time compared to defunctionalised type families generated by the
``singletons`` library.

1. One of our case studies is a type-level parser for filepaths, which does
   compile-time analysis of a string to split it into a list of path segments,
   and uses heuristics to detect whether it's a Windows or POSIX path, absolute
   or relative, and whether it points to a file or a directory.

   If the parser is implemented as a term-level function and then promoted to
   defunctionalised type families using ``singletons``, it takes about 30s
   to parse a filepath::

     ghci> runParserFinal @PathParser @"D:\\test\\stack\\test\\rrrrr\\gggg\\bbb\\ddd\\vvvv\\nnnn\\nnnn\\yyyyy\\ddddd\\jjjjj\\mmmm\\eeee\\mmmm\\yyyyyy\\aaaaaaaaaa"
     Path (PathParam Windows Dir Absolute) ("D" :| ["test","stack","test","rrrrr","gggg","bbb","ddd","vvvv","nnnn","nnnn","yyyyy","ddddd","jjjjj","mmmm","eeee","mmmm","yyyyyy","aaaaaaaaaa"])
     (31.15 secs, 243,096 bytes)

   If the parser is run directly using our proposed mechanism, it takes about
   0.3s::

     ghci> path @(ShowPathResult (MkPathFst (SymbolToString ("D:\\test\\stack\\test\\rrrrr\\gggg\\bbb\\ddd\\vvvv\\nnnn\\nnnn\\yyyyy\\ddddd\\jjjjj\\mmmm\\eeee\\mmmm\\yyyyyy\\aaaaaaaaaa"))))
     "((Windowss,Dir,Absolute),[\"D\",\"test\",\"stack\",\"test\",\"rrrrr\",\"gggg\",\"bbb\",\"ddd\",\"vvvv\",\"nnnn\",\"nnnn\",\"yyyyy\",\"ddddd\",\"jjjjj\",\"mmmm\",\"eeee\",\"mmmm\",\"yyyyyy\",\"aaaaaaaaaa\"])"
     (0.31 secs, 247,984 bytes)

2. Another case study is a type-safe ``printf``. Using type families defined by
   equations, a simple example takes about 11s to be compiled and run::

     ghci> formatS @"Person's name is %s and age is %d and height is %f" "Danya" 26 8.1
     "Person's name is Danya and age is 26 and height is 8.100000381469727"
     (11.71 secs, 1,331,464 bytes)

   With the proposed mechanism, this can be type checked and executed in
   0.25s::

     ghci> formatS @"Person's name is %s and age is %d and height is %f" "Danya" 26 8.1
     "Person's name is Danya and age is 26 and height is 8.100000381469727"
     (0.25 secs, 1,166,296 bytes)

Effect and Interactions
-----------------------

The proposed feature makes it feasible to perform complex type-level
computations in type families.

Costs and Drawbacks
-------------------

The feature relies on dynamic linking to execute term-level code at compile
time, so it has the same drawbacks as Template Haskell, e.g. it creates more
module dependencies, leading to more frequent recompilation.

Alternatives
------------

1. We could permit using term-level function
   unqualified::

     p1 :: toUpper 'x' :~: 'X'

   The problem is that we couldn't tell the compiler that ``toUpper`` here is a
   term-level function and not a type variable and GHC would insert an unwanted
   implicit ``forall`` here::

     p1 :: forall toUpper. toUpper 'x' :~: 'X'   -- Not what we want!

   This could be solved by changing implicit quantification rules under
   ``TypeFunctions``. There's a precedent for this in `#287
   <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst#implicit-quantification>`_.

2. We could promote term-level functions into type families by syntactically
   translating the equations. Given an ``f`` defined as::

      f True = 1
      f False = 2

   GHC could generate an accompanying type family::

     type family F a where
       F True = 1
       F False = 2

   This is roughly equivalent to what ``singletons`` can do today. It addresses
   the issues of ergonomics of defining type-level computations, but reduction
   is extremely slow.

3. We could use new syntax to introduce a special variant
   of closed type families::

      type FstMod :: (Nat, Bool) -> Nat
      type family FstMod a | FstMod = fstMod

   In this case we could also introduce a possiblility of
   defining custom axioms::

      type FstMod :: (Nat, Bool) -> Nat
      type family FstMod a | FstMod = fstMod
        where
          FstMod (a, True) = a -- These equations should be tried if at least one of the arguments is
                               -- a skolem variable or stuck type family.

   This alternative is less appealing because the new syntax would probably be
   deprecated with proper support for function promotion.

Unresolved Questions
--------------------

1. Could we support lazy evaluation?
   It is easy to implement it partially, but such an implementation would not
   support returning unsubstituted type variables or unreduced type family
   applications. For example ``Fst (1,a)`` will be reduced to ``1`` but ``Fst
   (a,1)`` will not be reduced at all. Such a behavior looks unclear and
   fragile, but proper implementation of laziness will probably require
   support for neutral terms in the RTS.

Implementation Plan
-------------------
Proposed changes are partially implemented in a prototype.

Currently the supported types for inputs/outputs are ``Nat``, ``Char``,
``[a]``, ``Either a b``, ``()``, and tuples. The conversion between term-level
Haskell values and the promoted values in the type checker is hard-coded for
each type. We hope to add support for arbitrary ADTs in the future.
