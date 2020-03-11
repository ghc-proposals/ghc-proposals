Qualified do
============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/216>`_.
.. sectnum::
.. contents::

This proposal introduces a new extension ``-XQualifiedDo`` which makes it possible to overload the meaning of a do-notation expression *on a case-by-case basis* (as opposed to the global effect of ``-XRebindableSyntax``), by writing ``modid.do``.

Motivation
------------

There are many kinds of monad-like things out there:

* monads
* indexed monads
* graded monads
* relative monads
* linear variants of all the above once `linear types
  <https://github.com/ghc-proposals/ghc-proposals/pull/111>`_ are
  implemented
  * in particular, the linear IO monad from the `Linear Haskell paper
    <https://arxiv.org/abs/1710.09756>`_ is an example of a linear
    graded monad
* …

All of these are theoretically compatible with the do-notation. And, in fact, really want the do-notation to work for them. After all, the entire existence of the do-notation can be ascribed to: “it's really annoying to program with ``(>>=)`` all the time”.

The prescribed solution to use ``-XRebindableSyntax``. But ``-XRebindableSyntax`` is a very blunt instrument:

* It affects many syntactic constructs (numerical literals, the ``if then else`` syntax, … (see the `full list <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax>`_)).
* It implies ``-XNoImplicitPrelude``
* It uses the same rebinding for all the do-expression in an entire file.

You may not want all this. For instance, with linear types ``if then else`` syntax cannot be meaningfully rebound to a function. And there is no reason why a file wouldn't have a piece code referring to a monad, one to a graded monad, and one to a linear relative monad.

Proposed Change Specification
-----------------------------

This proposal creates a new language extension ``-XQualifiedDo``.

When ``-XQualifiedDo`` is activated, the syntax of the ``do`` notation is changed to

::

  <lexp> ⟶ [modid.]do

where ``modid`` stands for some module name.

The additional module name is called the qualifier of the do-expression.

The semantics of ``do`` notation statements is given as follows (using
``-XLambdaCase`` notation):

* The ``x <- u`` statement uses ``(modid.>>=)``

  ::

    M.do { x <- u; stmts }  =  (M.>>=) u $ \x -> M.do { stmts }
* The ``u`` statement uses ``modid.(>>)``

  ::

    M.do { u; stmts }  =  (M.>>) u $ M.do { stmts }

* The a ``pat <- u`` statement uses ``M.fail`` for the failing case,
  if such a case is needed

  ::

    M.do { pat <- u; stmts }  =  (M.>>=) u $ \case
      { pat -> M.do { stmts }
      ; _ -> M.fail "…"
      }

  If the pattern cannot fail, then we don't need to use ``M.fail``.

  ::

    M.do { pat <- u; stmts }  =  (M.>>=) u $ \case pat -> M.do { stmts }

* ``-XApplicativeDo`` uses ``(M.<$>)``, ``(M.<*>)`` and ``M.join`` (this
  assumes that the applicative-do grouping has been performed)

  ::

    M.do { (x1 <- u1 | … | xn <- un); return e }  =
      (\x1 … xn -> e) M.<$> u1 M.<*> … M.<*> un

    M.do { (x1 <- u1 | … | xn <- un); stmts }  =
      M.join (\x1 … xn -> M.do { stmts }) M.<$> u1 M.<*> … M.<*> un


  Note that ``M.join`` is only needed if the final expression is
  not identifiably a ``return``.

  When the applicative statements contain nested statements (see the
  `wiki page
  <https://gitlab.haskell.org/ghc/ghc/wikis/applicative-do>`_ for a
  complete description of applicative-do statements), we also need a
  ``M.return``. *e.g.*

  ::

    M.do { ({stmt1; …; stmtn} {x1; …; xn} | y <- u) ; return e }  =
      (\(x1,…,xn) y -> e) <$> (M.do { stmt1; …; stmtn; M.return (x1, …, xn)}) <*> u

*  With ``-XRecursiveDo``, ``rec`` blocks use ``M.mfix`` and ``M.return``:

   ::

     M.do { rec { x1 <- u1; … ; xn <- un }; stmts }  =
       M.do
       { (x1, …, xn) <- M.mfix (\~(x1, …, xn) -> M.do { x1 <- u1; …; xn <- un; M.return (x1, …, xn)})
       ; stmts
       }

If a name ``M.op`` is required by the desugaring process (and only if it's required!) but the name is not in scope, an error message is produced:

* “Desugaring statement <stmt> requires <M.op> which is not in scope”

The qualified operations are subject to the same type restrictions as their counterparts with ``-XRebindableSyntax``.

When the qualifier ``modid.`` is omitted, the meaning of ``do { … }`` is the
same as if ``-XQualifiedDo`` is *not* in effect.

Examples
--------

``-XQualifiedDo`` does not affect ``return`` in the monadic ``do`` notation.

::

  import qualified Some.Monad.M as M

  boolM :: (a -> M.M Bool) -> b -> b -> a -> M.M b
  boolM p a b x = M.do
      px <- p x     -- M.>>=
      if px then
        return b    -- Prelude.return
      else
        M.return a  -- M.return

``-XQualifiedDo`` does not affect explicit ``(>>=)`` in the monadic ``do`` notation.

::

  import qualified Some.Monad.M as M
  import Data.Bool (bool)

  boolMM :: (a -> M.M Bool) -> M b -> M b -> a -> M.M b
  boolMM p ma mb x = M.do
      p x >>= bool ma mb   -- Prelude.>>=

Linear ``do`` blocks would look as follows.

::

  import qualified Control.Monad.Linear as Linear

  f :: Linear.Monad m => a #-> m b
  f a = Linear.do
    b <- someLinearFunction a         -- Linear.>>=
    anotherLinearFunction b

  g :: Linear.Monad m => a #-> m b
  g a = Linear.do
    b <- someLinearFunction a         -- Linear.>>=
    c <- anotherLinearFunction b      -- Linear.>>=
    Linear.return c


Effect and Interactions
-----------------------

``-XQualifiedDo`` makes it possible to choose, for each individual do-expressions, what kind of monad-like notion they are about. Even if the monad-like notion doesn't support all the range of desugaring (for instance it doesn't have a ``fail``), this will still work, as long as the do-expression doesn't use the corresponding feature (in our example: pattern-binders).

For instance we could write operations for monoids:

::

  module Data.Monoid.QualifiedDo where
    import Prelude hiding ((>>))

    (>>) :: Monoid a => a -> a -> a
    (>>) = (<>)

  module X where
    import qualified Data.Monoid.QualifiedDo as Monoid

    f = Monoid.do
      Sum 2
      Sum 3
      Sum 5
      Sum 8

If one would try to use ``x <- u`` with ``Monoid.do``, GHC would
raise an error *even if there is a value for ``(>>=)`` in scope*.

Enabling ``-XQualifiedDo`` doesn't change the meaning of existing do-expressions.

When both ``-XQualifiedDo`` and ``-XRebindableSyntax`` are enabled, ``-XQualifiedDo`` only affects qualified ``do``'s and ``-XRebindableSyntax`` affects the unqualified ``do``'s.

In principle, ``-XQualifiedDo`` would not affect monad comprehensions, though we could
imagine a similar mechanism to qualify the names in the desugared expressions
given some suitable syntax to specify the qualifier.

Costs and Drawbacks
-------------------

The do-expression store, during type-checking, which expression they will use for ``(>>=)``, etc… So the core infrastructure is actually already there. We anticipate the cost of implementation and maintenance of this feature to be very low.

Alternatives
------------

Do with builders
~~~~~~~~~~~~~~~~

The initial version of the proposal was inspired by F#'s `computational expressions <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions>`_.

Instead of qualifying ``do``, it would attach an expression to it:

::

  <lexp> ⟶ do [@aexp] { stmts }

The optional expression should evaluate to a record containing the operations to use
when desugaring.

::

  module Control.Monad.Linear.Builder where

    data BuilderType = Builder
      { (>>=) :: forall m a b. Linear.Monad m => m a #-> (a #-> m b) #-> m b
      , (>>) :: forall m b. Linear.Monad m => m () #-> m b #-> m b
      , fail :: forall m a. Linear.MonadFail m => String -> m a
      , return :: forall m a. Linear.Monad m => a #-> m a
      }

    monadBuilder :: BuilderType
    monadBuilder = Builder
      { (>>=) = (Linear.>>=)
      , (>>) = (Linear.>>)
      , fail = Linear.fail
      , return = Linear.return }


  module X where

    import qualified Control.Monad.Linear as Linear
    import qualified Control.Monad.Linear.Builder as Linear

    f :: Linear.Monad m => a #-> m a
    f x = do @Linear.builder
      y <- someLinearFunction x
      Linear.return y

The main obstacle with this approach was that it was difficult to express the
desugaring of the do notation without knowing the type of the builder. And all
attempts to characterize the type ended up requiring impredicative types.

It was later suggested that the optional expression could be constrained to
a qualified variable.

::

  <lexp> ⟶ do @qvarid { stmts }

With this constraint, the desugaring could use the qualifier to qualify the
monad operations.

::

  f :: Linear.Monad m => a #-> m a
  f x = do @Linear.builder
    y <- someLinearFunction x
    Linear.return y

would desugar to

::

  f :: Linear.Monad m => a #-> m a
  f x =
    (Linear.>>=) Linear.builder (someLinearFunction x) (\y -> Linear.return y)

This effectively avoids the need to find the type of the builder for desugaring.
We haven't opted for this approach though, because it requires defining builders
while the qualified do requires no extra definitions.

Exclusive names
~~~~~~~~~~~~~~~

It has been noted during discussion of the proposal that using the usual names
when desugaring (``(>>=)``, ``return``, ``(>>)``, ``fail``, etc) could cause
undesired ambiguity when trying to resolve names in some cases. For instance,

::

  import Data.Monoid

  f = Data.Monoid.do
    Sum 2
    Sum 3

  main = putStr "Hello" >> putStrLn "World" -- (Data.Monoid.>>) or (Prelude.>>)?

One would have to write instead

::

  import qualified Data.Monoid.QualifiedDo as Data.Monoid
  import Data.Monoid -- Changed to not export (>>)

  f = Data.Monoid.do
    Sum 2
    Sum 3

  main = putStr "Hello" >> putStrLn "World"

Fiddling with the imports like this would not be necessary if ``-XQualifiedDo``
used different names like ``qualifiedBind``, ``qualifiedThen``,
``qualifiedReturn``, etc.

Although the solution is effective for the case of monoids, it has a couple
of drawbacks that make unclear whether it would be a net win.

Firstly, using exclusive names would make errors about out-of-scope names
harder to understand. Compare

::

  /tmp/test.hs:4:5: error:
      • Variable not in scope: Data.Monoid.qualifiedThen
    |
  4 | f = Data.Monoid.do
    |   Sum 2
    |   Sum 3
    |

with

::

  /tmp/test.hs:4:5: error:
      • Variable not in scope: Data.Monoid.>>
    |
  4 | f = Data.Monoid.do
    |   Sum 2
    |   Sum 3
    |

The reader can figure out that the do notation requires ``qualifiedThen``
only if she knows that ``-XQualifiedDo`` desugars to ``qualifiedThen`` when
regular ``do`` notation would desugar to ``(>>)``.

Secondly, the motivating examples of ``-XQualifiedDo`` are monad-like concepts
that can define ``(>>=)`` and ``return`` for explicit use without the ``do``
notation. Asking to define aliases like ``qualifiedBind`` and
``qualifiedReturn`` is additional work that would not solve the name ambiguities
when all of ``(>>=)``, ``return``, ``qualifiedBind`` and ``qualifiedReturn`` are
exported.

Desugar unqualified returns
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Initially, it had been conceived that ``-XQualifiedDo`` should be used
with an unqualified ``return``.

::

  import qualified Control.Monad.Linear as Linear

  g :: Linear.Monad m => a #-> m b
  g a = Linear.do
    b <- someLinearFunction a         -- Linear.>>=
    c <- anotherLinearFunction b      -- Linear.>>=
    return c                          -- Desugared to Linear.return

Unfortunately, it is difficult to characterize the locations at which
return should be desugared or left alone. For instance

::

  import qualified Some.Monad.M as M

  boolM :: (a -> M.M Bool) -> b -> b -> a -> M.M b
  boolM p a b x = M.do
      px <- p x
      y <- if px then
             return b   -- Prelude.return or M.return ?
           else
             return a   -- Prelude.return or M.return ?
      return y          -- Prelude.return or M.return ?

``-XRebindableSyntax`` solves this by affecting every occurrence of
``return``. Following that approach for ``-XQualifiedDo`` would
complicate writing ``do`` blocks where ``return`` is used on a
different monad.

::

  import qualified Control.Monad.Linear as Linear
  import qualified System.IO.Linear (fromSystemIO)

  g :: Linear.Monad m => a #-> m b
  g a = Linear.do
    b <- fromSystemIO (print () >> return b)   -- Control.Monad.return ?
    return b                                   -- Linear.return

Also, scoping rules would need to be added to deal with nested ``do`` blocks.

::

  import qualified Some.Monad.M as M
  import qualified Some.Monad.N as N

  condMM :: (a -> M.M Bool) -> M b -> M b -> a -> M.M b
  condMM p ma mb x = M.do
      px <- p x
      if px then N.do
        a <- ma
        return a        -- N.return ?
      else do
        b <- mb
        return b        -- Prelude.return ?

This alternative is feasible. But on balance, it is not clear whether it is
worth the cost of working with whatever scoping rules are chosen.

Related work
~~~~~~~~~~~~

* One could use ``-XRebindableSyntax`` and use a very general type class which encompasses all monads

  * This was the essence of the `OverloadedDo proposal <https://github.com/ghc-proposals/ghc-proposals/pull/78>`_, though type inference was never solved for this
  * A more recent idea is `supermonads <http://www.cs.nott.ac.uk/~psznhn/Publications/jfp2018.pdf>`_, which solves the type inference issue using a plugin

  It requires somewhat less work (“only” a plugin, rather than a change in GHC's compiler, at least it's more modular), and is more automatic, as the correct functions are picked automatically from the type. But there is no way that this will capture all the desired notion: some restrictions need be imposed for the sake of type inference.

* There is a way to emulate ``-XQualifiedDo`` in current GHC using ``-XRecordWildcards``: have no ``(>>=)`` and such in scope, and import a builder with ``Builder {..} = builder``. It is used in `linear-base <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/System/IO/Resource.hs#L119-L120>`_. This is not a very good solution: it is rather a impenetrable idiom, and, if a single function uses several builders, it yields syntactic contortion (which is why shadowing warnings are deactivated `here <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/System/IO/Resource.hs#L1>`_)

Extensions
~~~~~~~~~~

Qualified do with parameters
++++++++++++++++++++++++++++

At some point, this language extension could be modified to allow passing
parameters to the operations during desugaring.

::

  <lexp> ⟶ do @aexp … @aexp { stmts }

This would allow a user to fix the type of the monad like so

::

  do @(@Maybe)
    x <- (+1) <$> m
    return x

which would be equivalent to

::

  (>>=) @Maybe ((+1) <$> m) (\x -> return @Maybe x)

Or it could be used to pass information which is available locally

::
  f =
    M.do @x1 @x2
      x <- (+1) <$> m
      M.return x
    where
      x1 = …
      x2 = …

which would be equivalent to

::

  f =
    (M.>>=) x1 x2 ((+1) <$> m) (\x -> M.return x1 x2 x)
    where
      x1 = …
      x2 = …

Some commenters have expressed interest in these cases, which fall beyond
the scope of the current proposal.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

The implementation shouldn't require too much effort. Matthías Páll (`@tritlo <https://github.com/Tritlo>`_) volunteers himself for the attempt, in collaboration with Arnaud (`@aspiwack <https://github.com/aspiwack>`_).
