.. proposal-number::

.. trac-ticket::

.. implemented::

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/64>`_.

.. contents::

Inline Bindings
===============

This proposal describes a small extension to ``do`` notation called *inline bindings*. It allows monadic bindings of the form ``<- expression`` to appear in expressions within a ``do`` block, as well as in record field bindings. This lets the programmer:

- Avoid needlessly naming intermediate results
- Stay in the domain of legible syntactic sugar
- Avoid falling back to monadic or applicative combinators

The extension is intended to make Haskell syntax both simpler for beginners and more familiar to the many programmers who come to Haskell from an imperative-programming background. It subsumes several common patterns in real-world Haskell code, does not clash with other language features or extensions, and interoperates nicely with the applicative-``do`` extension.

Code with inline bindings (Example A) desugars to ordinary binds (Example B), or applicative combinators (Example C) when combined with applicative ``do``.

::

  -- Example A: Using inline bindings

  do
    f (<- a) b (<- c) d

::

  -- Example B: Inline bindings desugared

  do
    _a_ <- a
    _c_ <- c
    f _a_ b _c_ d

::

  -- Example C: Applicative-do desugared

  join (f <$> a <*> pure b <*> c <*> pure d)

(Here, ``_a_`` and ``_c_`` are assumed to be fresh variable names, a convention used for the rest of this proposal.)


Motivation
------------

``do`` notation provides a readable, imperative look and feel for writing monadic code in Haskell. It allows beginners and programmers coming to Haskell from imperative languages to quickly get up to speed writing effectful code, before understanding low-level combinators for mapping ``<$>``, application ``<*>``, binding ``>>=``, and composition ``>=>``.

Unfortunately, the current design of ``do`` notation often leads to a choice between two undesirable alternatives when writing monadic code. On one hand, a programmer can stick to ``do`` notation entirely, at the cost of naming many intermediate results and obscuring the structure of their code. On the other hand, they can preserve the structure and avoid spurious names, at the cost of manually desugaring to monadic & applicative combinators. (In practice, Haskell programmers tend to use a mix of these two styles, according to preference.)

Inline bindings let the programmer stick to ``do`` notation while avoiding the syntactic noise of combinators and unnecessary names.

Consider an expression in an impure imperative programming language (Example D), where all of the functions involved are impure.

::

  -- Example D: An impure expression

  frob(quux(foo(bar(), baz())))

We can’t translate this directly to ``frob (quux (foo bar baz))``, because Haskell requires us to distinguish *evaluation* of pure expressions from *execution* of monadic actions. So there are two basic ways to translate it: ``do`` notation (Example E) and combinators (Example F).

::

  -- Example E: Using do notation

  do
    r <- bar
    z <- baz
    f <- foo r z
    q <- quux f
    frob q

::

  -- Example F: Using monadic & applicative combinators

  frob =<< quux =<< join (foo <$> bar <*> baz)

Neither of these is ideal. Using strictly ``do`` notation, we need to bind a new name for the result of each action, and we obscure the structure of an otherwise simple expression. Using only combinators, we keep the structure of the original code and avoid unnecessary names, but add several “special” operators that are largely unknown outside of Haskell.

With inline bindings, we can write monadic bindings directly in expressions within the context of a ``do`` block (Example G).

::

  -- Example G: Using inline bindings

  example = do
    frob (<- quux (<- foo (<- bar) (<- baz)))

  -- or:

  example = do
    frob $ <- quux $ <- foo (<- bar) (<- baz)

These bindings still explicitly indicate where actions are executed, while preserving the shape of the code, avoiding intermediate names, and using only a single special symbol ``<-``, which can be read as “bind” or “execute”. With inline bindings enabled, the desugaring of Example G is equivalent to Example E; with applicative-``do`` enabled as well, it’s equivalent to Example F.

----

Monadic parser combinator libraries such as Parsec are among the first examples of nontrivial monadic code that people encounter when learning Haskell. So for a more concrete example, consider this parser for C-style identifiers (Example H) written in ``do`` notation.

::

  -- Example H: A parser in do notation

  identifier = do
    first <- choice [letter, char '_']
    rest <- many (choice [letter, char '_', digit])
    return (Text.pack (first : rest))

We can avoid naming the intermediate values using applicative notation (Example I).

::

  -- Example I: A parser in applicative notation

  identifier = Text.pack
    <$> ((:)
      <$> choice [letter, char '_']
      <*> many (choice [letter, char '_', digit])

But we lose the ability to use the cons operator ``:`` infix, and clutter the code with the applicative combinators ``<$>`` and ``<*>``. With inline bindings, we can avoid both names and combinators (Example J).

::

  -- Example J: A parser using inline bindings

  identifier = do
    return $ Text.pack
      $ (<- choice [letter, char '_'])
         ----------------------------
      : (<- many (choice [letter, char '_', digit]))
         ------------------------------------------

The underlined portions are exactly the same as the the bindings for ``first`` and ``rest`` in the original code (Example H), only moved inline. Moreover, this desugars to the same code we wrote originally (Example K).

::

  -- Example K: Inline bindings desugared

  identifier = do
    _a_ <- choice [letter, char '_']
        ----------------------------
    _b_ <- many (choice [letter, char '_', digit])
        ------------------------------------------
    return (Text.pack (_a_ : _b_))
                       ---   ---

----

Finally, consider asynchronous code written with inline bindings and applicative-``do`` (Example L).

::

  -- Example L: Async code with inline bindings

  do
    let (async1, async2) = (<- async (getURL url1), <- async (getURL url2))
    let (page1, page2) = (<- wait async1, <- wait async2)
    ...

  =>

  do
    _get1_ <- async (getURL url1)
    _get2_ <- async (getURL url2)
    let (async1, async2) = (_get1_, _get2_)

    _wait1_ <- wait async1
    _wait2_ <- wait async2
    let (page1, page2) = (_wait1_, _wait2_)

    ...

  =>

  do
    _gets_ <- (,) <$> async (getURL url1) <*> async (getURL url2)
    let (async1, async2) = _gets_

    _waits_ <- (,) <$> wait async1 <*> wait async2
    let (page1, page2) = _waits_

    ...

the ``<- async``/``<- wait`` notation is close in both syntax and semantics to the ``async``/``await`` notation found in other languages, but consists solely of library functions and syntactic sugar for standard Haskell abstractions.


Proposed Change Specification
-----------------------------

Add a language pragma, ``{-# LANGUAGE InlineBindings #-}``, which enables desugaring of inline bindings. The desugaring adds a new level-10 precedence expression (on par with lambda, ``let``, ``if``, ``case``, and ``do``) consisting of a unary prefix leftward-arrow operator (``<-``, or U+2190 ``←`` when Unicode syntax is enabled).

::

  exp^10 -> ...
          | <- exp  -- (inline monadic binding)
          | ...

In addition, it introduces a new type of field binding for record construction and update, also using a leftward arrow.

::

  fbind -> ...
         | qvar <- exp  -- (monadic field binding)

An inline binding expression may only appear within the scope of a ``do`` block, since it participates in ``do`` desugaring. The desugaring relates inline bindings to their innermost enclosing ``do``. Semantically, it occurs before the rest of ``do`` desugaring, although an implementation is free to work in any manner that produces an equivalent result.

Desugaring operates on each statement in a ``do`` block, and each expression within a statement, in source order (left to right, depth first). Upon encountering a statement containing inline bindings, the bindings are first replaced with fresh variable names (Example M).

::

  -- Example M: Step 1 of desugaring

  do
    f (g (<- x))
          ----
    a (<- b) (<- c)
       ----   ----

  =>

  do
    f (g _x_)
         ---
    a _b_ _c_
      --- ---

Next, a monadic binding statement for each name is inserted immediately before the statement containing the inline bindings (Example N). The right-hand side of each binding is taken from the corresponding inline binding site. When there are multiple bindings, they are bound in source order.

::

  -- Example N: Step 2 of desugaring

  do
    _x_ <- x
        ----
    f (g _x_)

    _b_ <- b
        ----
    _c_ <- c
        ----
    a _b_ _c_

The desugaring is similar for monadic field bindings in records. The bindings are replaced with ordinary field bindings using fresh variables, and those variables are bound immediately before the statement containing the record expression (Example O).

::

  -- Example O: Desugaring monadic field bindings

  do
    return R { m <- x, n <- y }

  =>

  do
    _x_ <- x
    _y_ <- y
    return R { m = _x_, n = _y_ }

The equivalent solution using ``RecordWildCards`` (Example P) suffers from problems with legibility and refactoring, while inline bindings do not.

::

  -- Example P: Implicit binding with record wildcards

  do
    m <- x
    n <- y
    return R{..}

When ``do`` blocks are nested, the inner ``do`` is desugared first (Example Q). Note that this breaks the invariant that ``do { expression }`` is equivalent to just ``expression``: the presence of a ``do`` can change how an expression is desugared. See Unresolved Questions for more discussion of the consequences of this.

::

  -- Example Q: Nested do desugaring

  do
    f $ do
      g (<- x)

  =>

  do
    f $ do
      _x_ <- x
      g _x_

Nested bindings are also desugared in source order (Example R).

::

  -- Example R: Nested binding desugaring

  do
    process (<- (<- getAction) (<- getArgument)) (<- getConfig)
             ----------------------------------   ------------

  =>

  do
    _result_ <- (<- getAction) (<- getArgument)
             ----------------------------------
    _config_ <- getConfig
             ------------
    process _result_ _config_
            -------- --------

::

  do
    _result_ <- (<- getAction) (<- getArgument)
                 ------------   --------------
    _config_ <- getConfig
    process _result_ _config_

  =>

  do
    _action_ <- getAction
             ------------
    _argument_ <- getArgument
               --------------
    _result_ <- _action_ _argument_
                -------- ----------
    _config_ <- getConfig
    process _result_ _config_

  ==

  join
    (process
      <$> (join (($) <$> getAction <*> getArgument))
      <*> getConfig)

Note that since the variables generated by the desugaring are inaccessible to the programmer, each one is guaranteed to have only a single use site. This makes inline bindings work very nicely with the applicative-``do`` extension: it’s impossible to introduce a data dependency on an inline binding.

This notation is syntactically a superset of existing Haskell, including extensions. It replaces some patterns in monadic code (``do { x <- a; y <- b; return (x + y) }``) and applicative code (``(+) <$> a <*> b``) with a more concise & legible alternative (``(<- a) + (<- b)``), and makes ``do`` notation more closely resemble imperative programming, while retaining the distinction between pure and impure code.


Effect and Interactions
-----------------------

Inline bindings allow programmers to enjoy the syntactic sugar of ``do`` notation without being forced to name intermediate results or use monadic & applicative combinators.

This desugaring doesn’t directly interfere with any existing language feature or extension. Currently, a leftward arrow may appear in:

- Binding statements in ``do`` notation
- Generators in list comprehensions
- Pattern guards with the ``PatternGuards`` extension
- Unidirectional pattern synonyms with the ``PatternSynonyms`` extension

In these contexts, leftward arrows will retain their current meanings; inline bindings will only be considered in expression context. A binding statement in a ``do`` block such as ``do { x <- y; ... }`` is not parsed as ``do { x (<- y); ... }`` unless the user explicitly indicates this with ``x (<- y)`` or ``x $ <- y``.


Costs and Drawbacks
-------------------

This is a modest syntactic extension that should have a minimally invasive implementation, since it primarily affects ``do`` notation desugaring. Care should be taken that inline bindings don’t interfere with applicative-``do`` desugaring or other uses of the leftward arrow operator.

It makes the language larger and adds syntax, claiming the ``<-`` operator in expression context, which is currently unused. However, I believe it will improve the ability of novice users to learn the language by making certain code patterns simpler and more similar to other languages.

At this time, the primary drawback seems to be that it’s not entirely clear how certain edge cases should be handled; see Unresolved Questions. This may mean that some users will inevitably find these edge cases surprising.


Alternatives
------------

In existing Haskell code, ordinary ``do`` notation and applicative combinators are both available as alternatives. They are both lacking in terms of legibility and preservation of simple code structure.

Alternatives to inline bindings have been proposed for Haskell and implemented in other languages. The two major examples are *idiom brackets* and ``!``-notation.

Idiom brackets provide a desugaring from a new syntactic form ``(| ... |)`` to applicative operators. However, they are a poor candidate because there are several open questions about basic parts of their design, such as:

- Whether ``(| f a b c |)`` desugars to ``pure f <*> a <*> b <*> c`` or ``f <$> a <*> b <*> c``
- Whether ``(| (f x) |)`` means ``pure (f x)`` (because ``(f x)`` occurs first), ``pure f <*> x`` (because ``(f x)`` is the same as ``f x``), or ``f <$> x``
- Whether ``(| a, b |)`` is valid syntax for ``(| (a, b) |)``
- How to desugar forms such as ``(| if … |)``, ``(| case … of … |)``, and ``(| let … in … |)``

The Idris programming language has a feature called ``!``-notation (“bang notation”), which is more closely related: an expression marked with ``!`` is lifted “as high as possible within its current scope” (see ``Idris.DSL.debind``), then bound to a name and substituted in the same manner as this proposal.

Inline bindings differ from ``!``-notation both superficially (using ``<-`` instead of ``!`` because the latter is already in use as an operator) and in how bindings are lifted (but see Unresolved Questions).


Unresolved Questions
--------------------

Should inline bindings be allowed in ``let`` statements in ``do`` blocks?

::

  let oneLine = <- getLine
  let twoLines = (<- getLine) ++ (<- getLine)
  let _ = <- putStrLn twoLines
  let threeLines = [<- getLine, <- getLine, <- getLine]
  mapM_ putStrLn threeLines

  =>

  oneLine <- getLine

  _a_ <- getLine
  _b_ <- getLine
  twoLines <- _a_ ++ _b_

  _ <- putStrLn twoLines

  _c_ <- getLine
  _d_ <- getLine
  _e_ <- getLine
  let threeLines = [_c_, _d_, _e_]
  mapM_ putStrLn threeLines

My impression is that they should be, because this makes the extension considerably more useful, although there are some gotchas. Reordering ``let`` bindings statements may now change the order of their effects, and the variables bound by the ``let`` aren’t in scope in inline bindings, because semantically they occur *before* the ``let``. It would be possible to bring these variables into scope by desugaring to *recursive* ``do`` notation, but I feel this is undesirable.

----

Should the desugaring of inline bindings be allowed to cross other structures such as lambdas, ``if``, and ``case``? If so, how so? Essentially, how should this code be desugared?

::

  f = putStrLn (<- getLine)

  g = do
    let x = (<- a)
    case x of
      0 -> print (<- b)
      _ -> if x < 0
        then print (<- c)
        else (\y -> print (<- d)) ()

Broadly, there are three possible choices:

1. Raise an error if an inline binding would be lifted outside a control structure, or occurs outside a ``do``

2. Insert an implicit ``do`` in certain control structures for scoping of inline bindings

3. Allow desugaring to cross control structures

Option 1 (just bail out) is the most conservative, but makes it easy to offer a mechanical workaround for the error: the programmer must either add a ``do`` to make the binding locally scoped, or move the binding outside the control structure.

::

  f = putStrLn (<- getLine)           -- error: inline binding outside ‘do’

  g = do
    let x = (<- a)                    -- OK?
    case x of
      0 -> print (<- b)               -- error: inline binding crosses ‘case’
      _ -> if x < 0
        then print (<- c)             -- error: inline binding crosses ‘if’
        else (\y -> print (<- d)) ()  -- error: inline binding crosses lambda

  -- Workaround: add ‘do’ or binding statement.

  f = do
    putStrLn (<- getLine)             -- add ‘do’

  g = do
    let x = (<- a)                    -- OK?
    case x of
      0 -> do
        print (<- b)                  -- add ‘do’
      _ -> if x < 0
        then do
          print (<- c)                -- add ‘do’
        else do
          _d_ <- d                    -- add ‘do’ and binding
          (\y -> print _d_) ()

Option 2 (implicit ``do``) would be a more complex change. It would consist of inserting an implicit ``do`` around top-level definitions, lambda bodies, and branches of ``case`` and ``if``, causing inline bindings to be scoped to those structures.

::

  f = putStrLn (<- getLine)           -- OK: equivalent to ‘do { putStrLn (<- getLine) }’

  g = do
    let x = (<- a)                    -- OK?
    case x of
      0 -> print (<- b)               -- OK: equivalent to ‘do { print (<- b) }’
      _ -> if x < 0
        then print (<- c)             -- OK: equivalent to ‘do { print (<- c) }’
        else (\y -> print (<- d)) ()  -- OK: equivalent to ‘\y -> do { print (<- d) }’

  -- Desugars to:

  f = do
    _l_ <- getLine
    putStrLn _l_

  g = do
    _a_ <- a
    let x = _a_
    case x of
      0 -> do
        _b_ <- b
        print _b_
      _ -> do
        if x < 0
          then do
            _c_ <- c
            print _c_
          else do
            (\y -> do
              _d_ <- d
              print _d_) ()

My worry with option 2 is that it wouldn’t be consistent with control structures defined in user code, such as ``when``, suffering from the same problems as option 3.

::

  do
    when (x == 0) (print (<- b))

  =>

  do
    _b_ <- b                          -- bad: executed even if x /= 0
    when (x == 0) (print _b_)

Option 3 (what you write is what you get) is probably undesirable, because it may cause effects to occur earlier than expected if a ``do`` is accidentally omitted.

::

  f = do
    _l_ <- getLine
    putStrLn _l_

  g = do
    _a_ <- a
    let x = _a_

    _b_ <- b                          -- bad: executed regardless of the value of x
    _c_ <- c
    _d_ <- d
    case x of
      0 -> print _b_
      _ -> if x < 0
        then print _c_
        else (\y -> print _d_) ()

I believe that option 1, raising an error for such potentially confusing code, is the most reasonable. This still suffers from problems with user-defined control structures such as ``when``, but the arguments to these are typically wrapped in ``do`` blocks (especially by beginners) so it may not be a significant issue in practice.


Implementation Plan
-------------------

If this proposal is accepted, I (Jon Purdy) will implement the change. I’ll probably need some help from someone more familiar with the internals of GHC.
