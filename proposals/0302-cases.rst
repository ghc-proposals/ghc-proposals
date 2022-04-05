``\cases`` - Multi-way lambda expressions
=========================================

.. author:: Jakob Brünker
.. date-accepted:: 2021-09-21
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/20768
.. implemented:: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7873
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/302>`_.
.. contents::


This proposal introduces a syntactic construct that provides
functionality to case split on the arguments of multi-parameter lambda
expressions.

Motivation
----------

In Haskell 2010, there are two syntaxes to define a function: via the
function definition syntax, or via lambda expressions. The most obvious
difference between these is that the former assigns a name to the
function, whereas the latter can be used to create anonymous functions.
However, the differences go significantly beyond that:

-  Lambda expressions can only have a single clause, function
   declarations can have an arbitrary non-zero number of equations
-  Lambda expressions cannot have guards
-  Lambda expressions must have at least one parameter

There have been multiple attempts in the past to bring the capabilities
of lambda expressions closer to those of function declarations:

1. The extension ``-XLambdaCase`` introduces a ``\case`` construct which
   allows lambda expression to have multiple clauses, however, only one
   pattern can be matched on. Like a regular ``case``-expression, this
   can also have guards. `During its
   implementation <https://gitlab.haskell.org/ghc/ghc/issues/4359#note_44819>`__
   as well as `after
   it <https://github.com/ghc-proposals/ghc-proposals/pull/18>`__, there
   were attempts to make it possible to match on multiple patterns. No
   solution was found, in part because this would make it different from
   regular case-expressions.

   -  If there were an expression that had pattern matching syntax more
      similar to lambda expressions but which could also have guards and
      multiple clauses, it could be used instead of ``-XLambdaCase`` and
      would be able to match on multiple patterns.

2. The extension ``-XMultiWayIf`` essentially introduces standalone
   guards, simplifying the use of guards that aren’t at the outermost
   level of a function declaration or case-expression. Among other
   things, this made it easier to use guards inside of lambda
   expressions.

   -  If there were an expression similar to lambda expressions that
      could have guards and wasn’t required to have at least one
      parameter, it could be used instead of ``-XMultiWayIf``. This
      includes all uses of ``-XMultiWayIf``, not just those inside of
      lambdas (see Example section).

3. During the implementation of ``-XLambdaCase``, `some
   suggested <https://gitlab.haskell.org/ghc/ghc/issues/4359#note_51110>`__
   allowing lambda expressions to have multiple clauses. This was not
   implemented: The most obvious approach of turning ``\`` into a layout
   herald had the disadvantage of making some common idioms invalid.

   -  This could be circumvented by introducing a new expression that
      isn’t required to be backwards compatible with existing idioms.

Proposed Change Specification
-----------------------------

BNF Changes
^^^^^^^^^^^

**Bold** indicates changes to the existing BNF.

.. raw:: html

   <table>

.. raw:: html

   <tr>

.. raw:: html

   <td>

lexp

.. raw:: html

   </td>

.. raw:: html

   <td>

→

.. raw:: html

   </td>

.. raw:: html

   <td>

…

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

\|

.. raw:: html

   </td>

.. raw:: html

   <td>

``\case`` { alts }

.. raw:: html

   </td>

.. raw:: html

   <td>

(``\case`` expression)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

\|

.. raw:: html

   </td>

.. raw:: html

   <td>

``\cases`` { nalts }

.. raw:: html

   </td>

.. raw:: html

   <td>

(``\cases`` expression)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

alts

.. raw:: html

   </td>

.. raw:: html

   <td>

→

.. raw:: html

   </td>

.. raw:: html

   <td>

alt1 ; … ; altm

.. raw:: html

   </td>

.. raw:: html

   <td>

(m ≥ 0)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

alt

.. raw:: html

   </td>

.. raw:: html

   <td>

→

.. raw:: html

   </td>

.. raw:: html

   <td>

pat -> exp [ where decls ]

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

\|

.. raw:: html

   </td>

.. raw:: html

   <td>

pat gdpat [ where decls ]

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

\|

.. raw:: html

   </td>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

(empty alternative)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

nalts

.. raw:: html

   </td>

.. raw:: html

   <td>

→

.. raw:: html

   </td>

.. raw:: html

   <td>

nalt1 ; … ; naltm

.. raw:: html

   </td>

.. raw:: html

   <td>

(m ≥ 1)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

nalt

.. raw:: html

   </td>

.. raw:: html

   <td>

→

.. raw:: html

   </td>

.. raw:: html

   <td>

[ apat1 … apatn ] -> exp [ where decls ]

.. raw:: html

   </td>

.. raw:: html

   <td>

(n ≥ 0)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

\|

.. raw:: html

   </td>

.. raw:: html

   <td>

[ apat1 … apatn ] gdpat [ where decls ]

.. raw:: html

   </td>

.. raw:: html

   <td>

(n ≥ 0)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <tr>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

\|

.. raw:: html

   </td>

.. raw:: html

   <td>

.. raw:: html

   </td>

.. raw:: html

   <td>

(empty alternative)

.. raw:: html

   </td>

.. raw:: html

   </tr>

.. raw:: html

   <table>

Aside from the explicit layout using ``{``, ``}``, and ``;``, implicit
layout as described in the Haskell report can also be used.

Note the differences in the BNF to ``\case``: - ``\case`` always has
arity 1, whereas ``\cases`` can have any arity, including zero - The
patterns in ``\case`` do not need to be parenthesized (as in ``case``,
with *pat*), whereas the patterns in ``\cases`` must be atomic or
parenthesized (as in function definitions, with *apat*) - ``\case`` can
have zero clauses, whereas ``\cases`` must have at least one clause

In expressions that have zero scrutinees and multiple guards, there is
an ambiguity as to whether the expression has multiple alternatives with
one guard each or one alternative with multiple guards (or any
combination thereof). However, the semantics for these are equivalent,
so this ambiguity can be resolved in an arbitrary way.

Example
^^^^^^^

.. code:: haskell

   filter = \cases _ []                 -> []
                   p (x:xs) | p x       -> x : filter p xs
                            | otherwise ->     filter p xs

Changes
^^^^^^^

When the ``-XLambdaCase`` extension is enabled, a new expression is
added, introduced by the token sequence \\ cases. The whitespace between
``\`` and ``cases`` is optional and may contain an arbitrary sequence of
whitespace characters. ``\cases`` behaves in a way largely similar to
``\``, but it is a layout herald.

As the BNF shows - There can be any number of *patterns* including zero
(n ≥ 0) - There must be at least one clause (m ≥ 1). If there were zero
clauses, the arity of the ``\cases``-expression would be ambiguous (see
``Alternatives`` section for details). - As with ``\case``-expressions,
it is possible to use ``where`` blocks within each clause - Unlike
``case``, ``cases`` is *not* a keyword. Only the token sequence ``\``
``cases`` is treated specially (more precisely, lexically, ``cases`` is
a *varid* rather than a *reservedid*, and it may be used as a variable).

As with function declaration equations, all clauses must have the same
number of patterns.

Given a ``\cases``-expression ``csexp`` with one or more scrutinees and
a function ``f`` declared with function declaration syntax, and with the
same alternatives and same guards for each alternative as ``csexp``, the
semantics of the expression ``mcexp`` are the same as those of the
expression ``f``. If ``csexp`` has no scrutinees, the semantics are the
same as those of an expression ``p`` declared with a pattern binding
with the same guards as ``csexp``.

The new expression matches function declaration syntax very closely,
making refactoring easy.

Further Examples
----------------

Guards can be used instead of ``-XMultiWayIf`` inside lambda
expressions:

.. code:: haskell

   {-# LANGUAGE MultiWayIf, BlockArguments #-}
   take' :: Int -> [a] -> [a]
   take' = flip $ flip foldr (const [])
     \x more n -> if | n > 0 -> x : more (n - 1)
                     | otherwise -> []

   -- becomes

   take' :: Int -> [a] -> [a]
   take' = flip $ flip foldr (const [])
     \cases x more n | n > 0 -> x : more (n - 1)
                     | otherwise -> []

The new syntax could be used instead of ``-XMultiWayIf`` elsewhere as
well:

.. code:: haskell

   foo = bar baz if | g1 -> a
                    | g2 -> b

   -- with -XBlockArguments becomes

   foo = bar baz \cases | g1 -> a
                        | g2 -> b

``\case`` can be replaced by the new syntax:

.. code:: haskell

   \case Bar baz -> Just baz
         Quux -> Nothing

   -- becomes

   \cases (Bar baz) -> Just baz
          Quux -> Nothing

Unlike current ``\case``, multiple patterns can be matched:

.. code:: haskell

   -- \case can't be used here!
   -- At least not easily
   \foo bar baz -> case (foo, bar, baz) of
     (Just 4, 3, False) -> 42
     _ -> 0

   -- becomes

   \cases
     (Just 4) 3 False -> 42
     _ _ _ -> 0

The new syntax can be used instead of regular function declaration
syntax, potentially resulting in more concise definitions:

.. code:: haskell

   extremelyLengthyFunctionIdentifier (Just a) False = Just 42
   extremelyLengthyFunctionIdentifier (Just a) True  = Just (a / 2)
   extremelyLengthyFunctionIdentifier _        _     = Nothing

   -- becomes

   extremelyLengthyFunctionIdentifier = \cases
     (Just a) False -> Just 42
     (Just a) True  -> Just (a / 2)
     _        _     -> Nothing

This also makes it possible to have ``where`` bindings that scope over
multiple equations

.. code:: haskell

   -- have to repeat the definition of `magicNumber` or place it outside the definition of
   -- foo
   foo (Just x) p | x < 0 = ...
                  | let y = blah + 1 = ...
     where blah = x + magicNumber
           magicNumber = 5
   foo Nothing _ = magicNumber
     where magicNumber = 5

   -- becomes

   -- note that the first `where` clause belongs to the first clause, rather than the
   -- function declaration, because it is indented further

   foo = \cases
     (Just x) p | x < 0 -> ...
                | let y = blah + 1 -> ...
       where blah = x + magicNumber
     Nothing _ -> magicNumber
     where
       magicNumber = 5

To illustrate with some real-world examples, this section shows how some
snippets found on hackage would look if they used this new syntax:

red-black-record-2.1.0.3/lib/Data/RBR/Internal.hs

.. code:: haskell

   _prefixNS = \case
       Left l -> S l
       Right x -> case x of Here fv -> Z @_ @v @start fv

   _prefixNS = \cases
       (Left l) -> S l
       (Right x) -> case x of Here fv -> Z @_ @v @start fv

roc-id-0.1.0.0/library/ROC/ID/Gender.hs

.. code:: haskell

   printGender :: Language -> Gender -> Text
   printGender = \case
     English -> printGenderEnglish
     Chinese -> printGenderChinese

   printGenderEnglish :: Gender -> Text
   printGenderEnglish = \case
     Male   -> "Male"
     Female -> "Female"

   printGenderChinese :: Gender -> Text
   printGenderChinese = \case
     Male   -> "男性"
     Female -> "女性"

   -- becomes

   printGender :: Language -> Gender -> Text
   printGender = \cases
     English Male   -> "Male"
     English Female -> "Female"
     Chinese Male   -> "男性"
     Chinese Female -> "女性"

Effect and Interactions
-----------------------

The new expression subsumes the functionality that ``-XLambdaCase``
provides. (See “Alternatives” section for a discussion on deprecation.)

The proposed syntax can also be used instead of ``-XMultiWayIf``,
however in a slightly more verbose manner, as shown in the “Further
Examples” section.

(1) would introduce a new keyword and thus make it impossible to use the
    chosen word for other purposes, though this would be gated behind an
    extension.

Since the proposal changes the existing ``-XLambdaCase`` extension, it
is not entirely backwards-compatible: Expressions like
``\cases -> cases + 1`` are now interpreted differently when
``-XLambdaCase`` is active.

Costs and Drawbacks
-------------------

It is one additional syntactic construct to maintain, however the
maintenance cost should be fairly low due to the similarity to already
existing constructs.

While this also means one additional construct to learn for beginners,
the syntax is largely consistent with similar constructs in the existing
language, and as such users might in fact be surprised that a construct
with similar capabilities doesn’t yet exist.

Alternatives
------------

-  Instead of adding functionality to ``-XLambdaCase``, a new extension,
   e.g. ``-XLambdaCases`` could be added. It might be desirable to have
   this new extension imply ``-XLambdaCase``.

-  This proposal does not permit zero clauses, as with
   ``case``-expressions and ``-XEmptyCase``. This could be permitted. In
   this case, however, a way would have to be found to indicate how many
   arguments a given expression matches on, as otherwise, it would be
   ambiguous. The number of arguments an expression matches on becomes
   obvious from the clauses, e.g. ``\cases a b -> ...`` clearly matches
   on two arguments. Without clauses, this remains unclear. This means
   it would also be unclear whether the patterns are non-exhaustive:
   Consider the expression ``f = \cases {} :: Bool -> Void -> a``. If
   the expression is supposed to match on both arguments, the patterns
   are exhaustive. If it is only supposed to match on the first argument
   and evaluate to a function of type ``Void -> a``, it is not
   exhaustive. Moreover, in the former case,
   :literal:`f undefined `seq` ()` evaluates to ``()``, whereas in the
   latter case, it evaluates to bottom. Currently, with ``\case {}``,
   this problem doesn’t arise, since it always matches on exactly one
   argument, and similarly for ``case x of {}``, which only matches on
   ``x``. A syntax to resolve this has been proposed in the discussion:
   ``(\cases)`` for matching on no arguments, ``(\cases _)`` for one,
   ``(\cases _ _)`` for two, and so on. Alternatively, `absurd
   patterns <https://github.com/ghc-proposals/ghc-proposals/discussions/423>`__
   could be introduced, which would provide a more general solution that
   could also be used in function definition syntax.

-  Regular lambda expressions could be extended to use layout and
   guards, however, this necessitates some potentially controversial
   decisions on when exactly to herald layout, since always doing so
   would disallow existing idioms; these would not be legal when the
   extension is enabled:

   .. code:: haskell

      do
        f a >>= \b ->
        g b >>= \c ->
        h c

      foo = \x -> do
        a x
        b

   Two alternatives would be to only herald layout

   -  if a newline immediately follows the ``\`` or
   -  if, given that token ``t`` is the token after ``\``, the line
      below the one with ``t`` has the same indentation as or greater
      than ``t``

   Both of these would avoid the problem, but both rules are dissimilar
   from how layout heralding is handled in other Haskell constructs.

-  Expressions with zero patterns could be allowed only if the
   expression contains guards, rather than always being allowed. This
   would make them somewhat less consistent, but it is how lambda
   expressions work (i.e. ``\ -> ...`` is illegal) and only disallows
   expressions that are needlessly verbose (e.g. ``\cases -> exp`` can
   always be replaced by ``exp``).

-  ``\case`` could be deprecated, since all its use cases (except for
   those involving ``-XEmptyCase``) would be subsumed by ``\cases``.
   However, the discussion of this proposal has shown that such a
   deprecation would be a controversial change in its own right, and
   that some working out has to be done as to the exact details of it,
   thus, this might be better suited to being its own, separate
   proposal.

-  The possibility to have a construct similar to ``-XMultiWayIf`` but
   without the keyword, i.e. using guards directly as an expression, was
   also raised in the discussion. If this were to be used, any pattern
   matching would have to be done with pattern guards.

-  A few alternative designs were discussed in greater detail. Denoting
   ``\cases`` as (1), these are

   -  **(2): Comma-separated ``\case``**

      Example:

      .. code:: haskell

         filter = \case _, []               -> []
                        p, x:xs | p x       -> x : filter p xs
                                | otherwise ->     filter p xs

      This alternative does not introduce a new construct. It instead
      consists of a straightforward extension to an existing one: Allow
      separating multiple patterns in ``\case`` by commas. This makes it
      the least disruptive of the presented alternatives.

      A clause would only match if all of its patterns match their
      respective scrutinee.

      Rather than introducing a new extension, this behavior would be
      enabled by ``-XLambdaCase``.

      Additionally, an analogous extension could be introduced for
      ``case of``:

      .. code:: haskell

         case numerator, denominator of
           _          , 0 -> Nothing
           Whole n    , d -> Whole (n `div` d)
           Complex a b, d -> Complex (a `div` d) (b `div` d)

      This can be used instead of using tuples to achieve something
      similar:

      .. code:: haskell

         case (numerator, denominator) of
           (_          , 0) -> Nothing
           (Whole n    , d) -> Whole (n `div` d)
           (Complex a b, d) -> Complex (a `div` d) (b `div` d)

      With the advantage that users don’t have to be worried or learn
      about whether using tuples in such cases incurs a performance
      penalty, and it would mean that the ``\case`` syntax stays
      consistent with ``case of`` syntax.

      This extension to ``case of`` would be enabled regardless of
      whether or not ``-XLambdaCase`` is turned on.

      If no clauses are given, i.e. the expression in question if
      ``\case {}``, how many arguments this expression should take is
      ambiguous. However, currently, with ``-XEmptyCase``, this
      expression is already valid and takes a single argument. Thus, to
      maintain backwards compatibility and for lack of a better option,
      this proposal does not alter the behavior of this expression.

      In general, the lack of parentheses makes this alternative
      slightly more concise than the others, especially in cases with
      only a single pattern.

      One potential concern is that this breaks the pattern of symmetry
      between expressions and patterns that match them. For example, if
      a function is defined as ``f (Just a) (Right b) = a + b``, it can
      be called as ``f (Just a) (Right b)``, but when using ``\case``
      (i.e. ``f = \case Just a, Right b -> a + b``), the patterns are
      separated by commas, whereas the expression calling ``f`` still
      uses parentheses.

   -  **(3): One lambda per clause, ``case of``**

      Example:

      .. code:: haskell

         filter = case of \_ []                 -> []
                          \p (x:xs) | p x       -> x : filter p xs
                                    | otherwise ->     filter p xs

      The functionality of ``-XLambdaCase`` is extended, according to
      the following schema:

      .. code:: haskell

         case [ scrutinee ] of
           [ Pattern_0a ] \ Pattern_1a ... Pattern_na -> Expression_a
           [ Pattern_0b ] \ Pattern_1b ... Pattern_nb -> Expression_b
           ...

      Semantically, this would be equivalent to

      .. code:: haskell

         \var_1 ... var_n -> case ([ scrutinee, ] var_1, ..., var_n) of
           ([ Pattern_0a, ] Pattern_1a, ..., Pattern_na) -> Expression_a
           ([ Pattern_0b, ] Pattern_1b, ..., Pattern_nb) -> Expression_b

      A new extension ``-XExtendedCase`` is introduced. With this new
      extension enabled, the ``case`` expression is able to define
      anonymous functions. The scrutinee may be omitted, in which case
      the corresponding pattern in each clause must also be omitted.
      Furthermore, in each clause, between the usual pattern (if it is
      present) and the arrow, a ``\`` and a number of patterns may be
      written. The number of patterns must be consistent across all
      clauses, and the types of corresponding patterns must match (e.g.,
      the first pattern after the backslash must have the same type for
      all clauses). As usual, ``case`` clauses can contain guards as
      well.

      The number of patterns after each ``\`` determines the arity of
      the function that a ``case`` expression produces. The *n*\ th
      pattern after the ``\`` is matched against the *n*\ th argument
      given to the function.

      Note that the patterns after the ``\`` must be enclosed by
      parentheses if they consist of more than one token, just like
      patterns in a lambda expression, but unlike the pattern that can
      come before the ``\``.

      If there is no scrutinee, it is not immediately clear what the
      meaning of an expression without clauses, i.e. the expression
      ``case of {}``, should be, since the number of arguments to the
      anonymous function is not specified. Users might expect this to
      compile if the ``-XEmptyCase`` extension is enabled. However, due
      to the inherent ambiguity, this proposal does not allow a ``case``
      expression that lacks both a scrutinee and clauses. Other
      approaches are possible, see Alternatives section.

      Like the existing behavior for alternatives in ``case``
      expressions, and equations in function declaration syntax, it is
      possible to use ``where`` clauses within each clause of the
      extended ``case`` expression. Furthermore, each clause can have
      guards, which appear after all patterns.

      This alternative has some desirable properties, in that it extends
      an existing syntactic construct rather than introducing a new one
      and is syntactically similar to lambda expressions. On the other
      hand, it does not relate to existing syntax as directly as the
      alternatives (e.g., it produces an anonymous function but doesn’t
      start with ``\``, as opposed to lambda expressions and ``\case``),
      and its functionality overlaps with that of ``\case``.

      Since the introductory example only demonstrates the case without
      scrutinee, here is a different example:

      .. code:: haskell

         sendEmail :: Text -> Text -> Text -> Maybe Attachment -> IO ()
         sendEmail address = case validate address of
           Just emailAddress \subject content (Just attachment) -> sendWithAttachment emailAddress subject content attachment
           Just emailAddress \subject content Nothing           -> sendWithoutAttachment emailAddress subject content
           Nothing           \_       _       _                 -> error "invalid address"

   -  **(4): Multi-pattern ``\case`` with parentheses**

      Example:

      .. code:: haskell

         filter = \case _ []                 -> []
                        p (x:xs) | p x       -> x : filter p xs
                                 | otherwise ->     filter p xs

      Regular function definition syntax requires parentheses around
      patterns that consist of more than one token. The same could be
      done with ``\case``. This would make the two syntaxes more
      consistent, and allow easy refactoring from one to the other. It
      also doesn’t introduce any new syntactic constructs that have to
      be maintained.

      However, it behaves differently from the current ``-XLambdaCase``
      extension, which doesn’t require parentheses around patterns. This
      would seem to make it non-backwards-compatible, especially if it
      still uses the same ``-XLambdaCase`` extension name. This can be
      mitigated in various ways.

      First, a refactoring tool could be provided to update existing
      code and introduce parentheses where necessary, which would
      massively lower the effort required to update old code to be
      compatible with the new extension. Note that while the current
      ``-XLambdaCase`` extension doesn’t *require* parentheses, it
      doesn’t prohibit them, either. Thus, code updated with such a tool
      would work with both versions of the extension.

      Second, a special case could be introduced to have the compiler
      handle ``\case`` differently if there is only one pattern. More
      specifically, this means that the type checker detects when the
      first pattern in a clause is a solitary, non-nullary constructor.
      If this is the case, the AST is reconstructed such that the
      remaining patterns in the clause become arguments of this
      constructor.

      When this special case is triggered, the compiler would produce a
      warning (``-Wdeprecated-lambda-case``), which would be on by
      default, and warn the user that they’re using syntax which will be
      deprecated at some future point. This would make it possible to
      remove the special casing and warning after a few releases have
      passed.

   -  **Summary:**

      .. raw:: html

         <table>

      .. raw:: html

         <tr>

      .. raw:: html

         <th>

      Approach

      .. raw:: html

         </th>

      .. raw:: html

         <th>

      Example

      .. raw:: html

         </th>

      .. raw:: html

         <th>

      Pros

      .. raw:: html

         </th>

      .. raw:: html

         <th>

      Cons

      .. raw:: html

         </th>

      .. raw:: html

         </tr>

      .. raw:: html

         <tr>

      .. raw:: html

         <td>

      (1) Keyword (``\cases``, ``\cases``, etc.)

          .. raw:: html

             </td>

          .. raw:: html

             <td>

          .. raw:: html

             <pre style="display: inline">
                <code> \cases (Just a) (Left b) -> ...
               _        _        -> ...</code>
             </pre>

          .. raw:: html

             </td>

          .. raw:: html

             <td>

          .. raw:: html

             <ul>

          .. raw:: html

             <li>

          Parity with function equation syntax

          .. raw:: html

             </li>

          .. raw:: html

             </ul>

          .. raw:: html

             </td>

          .. raw:: html

             <td>

          .. raw:: html

             <ul>

          .. raw:: html

             <li>

          Disagreement over which keyword to use

          .. raw:: html

             </li>

          .. raw:: html

             <li>

          Adds yet another similar construct and hence disagreements
          about deprecations

          .. raw:: html

             </li>

          .. raw:: html

             </ul>

          .. raw:: html

             </td>

          .. raw:: html

             </tr>

          .. raw:: html

             <tr>

          .. raw:: html

             <td>

          (2) Comma-separated ``\case``

              .. raw:: html

                 </td>

              .. raw:: html

                 <td>

              .. raw:: html

                 <pre style="display: inline">
                    <code> \case Just a, Left b -> ...
                  _     , _      -> ...</code>
                 </pre>

              .. raw:: html

                 <pre style="display: inline">
                    <code> case Just 34, Right [] of
                 Just a, Left b -> ...
                 _     , _      -> ...</code>
                 </pre>

              .. raw:: html

                 </td>

              .. raw:: html

                 <td>

              .. raw:: html

                 <ul>

              .. raw:: html

                 <li>

              Conceptually, the smallest change that achieves the goal:
              Just a minor extension to one or two existing constructs

              .. raw:: html

                 </li>

              .. raw:: html

                 <li>

              That means no demand for or concerns about potential
              deprecation

              .. raw:: html

                 </li>

              .. raw:: html

                 <li>

              Parity with extended case … of syntax

              .. raw:: html

                 </li>

              .. raw:: html

                 <li>

              Like current ``\case``, single pattern uses are
              concise due to lack of parentheses

              .. raw:: html

                 </li>

              .. raw:: html

                 </ul>

              .. raw:: html

                 </td>

              .. raw:: html

                 <td>

              .. raw:: html

                 <ul>

              .. raw:: html

                 <li>

              Different from function equation syntax and from function
              application syntax: you apply as f (Just 34) (Right [])
              but pattern match with ``\case ``Just 23, Right
              []

              .. raw:: html

                 </li>

              .. raw:: html

                 </ul>

              .. raw:: html

                 </td>

              .. raw:: html

                 </tr>

              .. raw:: html

                 <tr>

              .. raw:: html

                 <td>

              (3) One lambda per clause case of

                  .. raw:: html

                     </td>

                  .. raw:: html

                     <td>

                  .. raw:: html

                     <pre style="display: inline">
                        <code> case of
                     \(Just a) (Left b) -> ...
                     \_        _        -> ...</code>
                     </pre>

                  .. raw:: html

                     <pre style="display: inline">
                        <code> case [1,2,3] of
                     (x:xs) \(Just a) (Left b) -> ...
                     _      \_        _        -> ...</code>
                     </pre>

                  .. raw:: html

                     </td>

                  .. raw:: html

                     <td>

                  .. raw:: html

                     <ul>

                  .. raw:: html

                     <li>

                  Allows combining pattern matching on scrutinees and
                  function arguments in one expression

                  .. raw:: html

                     </li>

                  .. raw:: html

                     <li>

                  Parity with function equation syntax

                  .. raw:: html

                     </li>

                  .. raw:: html

                     </ul>

                  .. raw:: html

                     </td>

                  .. raw:: html

                     <td>

                  .. raw:: html

                     <ul>

                  .. raw:: html

                     <li>

                  While it extends an existing construct, this extension
                  makes it overlap with ``\case`` functionality

                  .. raw:: html

                     </li>

                  .. raw:: html

                     <li>

                  Not as obvious an extension from existing syntax as
                  the other options (i.e. starts with case, not </tt>,
                  even though it takes arguments)

                  .. raw:: html

                     </li>

                  .. raw:: html

                     </ul>

                  .. raw:: html

                     </td>

                  .. raw:: html

                     </tr>

                  .. raw:: html

                     <tr>

                  .. raw:: html

                     <td>

                  (4) Multi-pattern ``\case`` with parentheses

                      .. raw:: html

                         </td>

                      .. raw:: html

                         <td>

                      .. raw:: html

                         <pre style="display: inline">
                            <code> \case
                         (Just a) (Left b) -> ...
                         _        _        -> ...</code>
                         </pre>

                      .. raw:: html

                         </td>

                      .. raw:: html

                         <td>

                      .. raw:: html

                         <ul>

                      .. raw:: html

                         <li>

                      Doesn’t introduce a new construct, and doesn’t
                      introduce any overlap with others

                      .. raw:: html

                         </li>

                      .. raw:: html

                         <li>

                      Parity with function equation syntax

                      .. raw:: html

                         </li>

                      .. raw:: html

                         </ul>

                      .. raw:: html

                         </td>

                      .. raw:: html

                         <td>

                      .. raw:: html

                         <ul>

                      .. raw:: html

                         <li>

                      Not backwards compatible - can be mitigated by
                      using (possibly temporary) compiler magic to allow
                      single-scrutinee ``\case`` without
                      parentheses, as well as providing an automatic
                      refactoring tool to update existing code

                      .. raw:: html

                         </li>

                      .. raw:: html

                         </ul>

                      .. raw:: html

                         </td>

                      .. raw:: html

                         </tr>

                      .. raw:: html

                         </table>

Implementation Plan
-------------------

I (Jakob Brünker) will implement this proposal.
