Type Annotated Quoters
======================

.. date-accepted:: 2019-08-01
.. ticket-url::
.. author:: winterland1989
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/125>`_.
.. contents::


The existing ``QuasiQuoter`` type is capable of producing expressions,
but cannot describe the types of those expressions. We propose adding a
``TQuasiQuoter a`` type capable of producing expressions of type ``a``.

Motivation
----------

``QuasiQuoter`` is a powerful tool to do compilation time code
construction, but the type ``QuasiQuoter`` is rather uninformative —
especially when it’s used exclusively as an expression splice.
``QuasiQuoter``\ s which appear in haddock documentation don’t describe
their resulting types, requiring a convention of comments. This is
clearly against the spirit of a language with a typesystem as strong as
Haskell’s.

Furthermore, by exposing the type of the resulting expression, a typed
quasiquoter can participate in typechecking.

Proposed Change Specification
-----------------------------

We will introduce a new datatype in ``Language.Haskell.TH.Quote``:

.. code:: haskell

   data TQuasiQuoter a = TQuasiQuoter
     { quoteTExp :: String -> Q (TExp a)
     }

and will add new syntactic sugar when ``-XTemplateHaskell`` is enabled:

.. code:: haskell

   [someTQQ|| hello world ||]

whose desugaring is:

.. code:: haskell

   $$(quoteTExp someTQQ "hello world")

This new typed quasiquoter syntax is identical to untyped quasiquoters,
except that it requires double bars. All of the `usual rules around
quasiquoted
strings <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#th-quasiquotation>`__
apply here.

Typed quasiquoters participate in typechecking and unification. We can
put given constraints on a value of ``TQuasiQuoter``, and they will be
available as usual in its definition. For example, the following is
allowed:

.. code:: haskell

   str :: IsString a => TQuasiQuoter a
   str = TQuasiQuoter $ \a -> [|| fromString a ||]

as is its concrete use:

.. code:: haskell

   someText :: Text
   someText = [str|| hello world ||]

and polymorphic use:

.. code:: haskell

   something :: IsString a => a
   something = [str|| hello world ||]

Examples
--------

We can use a typed quasiquoter to implement safe custom ``Num``\ eric
types, which can’t be overflowed at compile-time. While
``-Woverflowed-literals`` can help with built-in types, it won’t help
for custom types!

.. code:: haskell

   checkedNum
       :: forall a
        . (Bounded a, Num a, Integral a, Typeable a)
       => TQuasiQuoter a
   checkedNum = TQuasiQuoter $ \str ->
     let minVal = fromIntegral $ minBound @a
         maxVal = fromIntegral $ maxBound @a
         val = read @Integer str
      in if minVal <= val && val <= maxVal
         then [|| fromInteger val ||]
         else fail $ mconcat
           [ show val
           , " is out of bounds for "
           , show (typeRep $ Proxy @a)
           ]

Additionally, @yav gives an example in which we parse an AST from a
string, and then separately compile that down to a ``TExp``:

.. code:: haskell


   data Expr = Fun String Expr | Add Expr Expr | Var String
               deriving Show

   type Code a = Q (TExp a)

   -- Language quoter
   lam :: TQuasiQuoter Expr
   lam = TQuasiQuoter $ \input ->
     case pExpr (words input) of
       Just(e,[]) -> e
       _          -> fail "Parse error"

   pExpr :: [String] -> Maybe (Code Expr, [String])
   pExpr s = case s of
               "ADD" : s1 ->
                  do (a,s2) <- pExpr s1
                     (b,s3) <- pExpr s2
                     pure ([|| Add $$a $$b ||], s3)

               "FUN" : v : "->" : s1 ->
                  do (a,s2) <- pExpr s1
                     pure ([|| Fun v $$a ||] , s2)

               x : s1 ->
                 pure ([|| Var x ||], s1)

   data Val = VFun (Val -> Val) | VInt Int | VErr

   compile :: [(String,Code Val)] -> Expr -> Code Val
   compile env expr =
     case expr of
       Fun x e -> [|| VFun (\i -> $$(compile ((x, [|| i ||]) : env) e)) ||]

       Var x -> case lookup x env of
                  Just i -> i
                  _      -> [|| VErr ||]

       Add x y -> [|| case ($$(compile env x), $$(compile env y)) of
                       (VInt x, VInt y) -> VInt (x + y)
                       _ -> VErr ||]

Effect and Interactions
-----------------------

For expression quoter writers, adding ``TQuasiQuoter a`` mainly reduce
the documentation burden since the result expression’s type is already
annotated. Users can spot the result type much more easily and become
more confident in using these quoters. When beginners click through the
``TQuasiQuoter`` document link, they’re supposed to get the basic
knowledge on how to enable some language extensions and splice quoters
into their code.

`simonpj <https://github.com/simonpj>`__ raises another point, that this
proposal will improve error messages, consider:

.. code:: haskell

   qq :: TQuasiQuoter Char
   qq = ...

   blah = [qq|| unicode 78 ||] && True

With existing quasi-quote machinery we’d first have to run ``qq``,
splice in the resulting syntax tree, and then complain if it didn’t
typecheck. With a typed quasi-quoter we can complain right away: qq
returns a ``TExp Char`` and that doesn’t fit somewhere a ``Bool`` is
needed.

Finally, this proposal finishes the syllogism that
``Exp : QuasiQuoter :: TExp : ?``.

Alternatives
------------

In fact this proposal is inspired by the `Compile-time literal
values <https://github.com/ghc-proposals/ghc-proposals/pull/124>`__
proposal, and shared some goals, but this proposal is more about trying
to solve an existing issue with current quoters.

Implementation Plan
-------------------

`Matthew Pickering <https://github.com/mpickering>`__ has graciously
offered to implement this, and
`sighingnow <https://github.com/sighingnow>`__ was nominated by the
original author of this proposal.
