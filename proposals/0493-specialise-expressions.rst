Allow expressions in SPECIALISE pragmas
=======================================

.. author:: Richard Eisenberg, Simon Peyton Jones
.. date-accepted:: 2024-01-22
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/24359
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/493>`_.
.. contents::
.. sectnum::

GHC today supports ``SPECIALISE`` pragmas (American spelling also accepted) that cause
GHC to create a (partially) monomorphised version of a polymorphic function, often
increasing efficiency::

  addMult :: Num a => a -> a -> a
  addMult x y = x * 2 + y

  {-# SPECIALISE addMult :: Int -> Int -> Int #-}

With the ``SPECIALISE`` pragma, GHC will produce ::

  addMultInt :: Int -> Int -> Int
  addMultInt x y = x * 2 + y
  {-# RULES "addMult/Int" addMult = addMultInt #-}  -- NB: applies only at type Int

Note that ``addMultInt`` uses no indirection in its use of addition and multiplication,
unlike ``addMult``, which makes two calls to dictionary functions, confounding branch
prediction and slowing down execution. The ``RULE`` means that GHC will replace
all calls of ``addMult`` at ``Int`` to use the faster version.

The syntax of today's ``SPECIALISE`` requires a function and its specialised type.

This proposal suggests to generalise the syntax of ``SPECIALISE`` to allow a
function application with an optional type signature,
not just a function name and a type. Happily, this is backward
compatible, because type signatures are expressions, too. This will allow, for example ::

  {-# SPECIALISE addMult @Double #-}
  {-# SPECIALISE addMult (5 :: Int) #-}
  {-# SPECIALISE addMult 5 :: Int -> Int #-}

The first of these is a more succinct phrasing of what we have today: it simply
specialises ``addMult`` to work on ``Double``\ s, but without having to repeat the
entire type. The second does something new: it allows expression specialisation,
which can then lead to further evaluation within the function body. In this case,
GHC will create ::

  addMult5 :: Int -> Int -> Int
  addMult5 y = let x = 5 in x * 2 + y
  {-# RULES "addMult/5" addMult 5 = addMult5 #-}

GHC can then further optimize the right-hand side of ``addMult5`` to avoid doing
any multiplication at run time.

Motivation
----------
See the introduction. In addition to the examples there, we can imagine
a case like this ::

  frob :: Bool -> ...
  frob b ... = ...
    where
      ... if b then helper1 else helper2 ...

The function ``frob`` is a big, complicated function. Deep in its bowels, it
either uses ``helper1`` or ``helper2`` to do a critical step. However, most (or maybe all)
call sites of ``frob`` pass in a literal ``True`` or ``False``. Really, it would
make sense to have ``frobHelper1`` and ``frobHelper2`` as separate functions, but
that would cause unhappy duplication of all the complexity inherent in ``frob``.

With the ability to specialise expressions, we can write ::

  {-# SPECIALISE frob True #-}
  {-# SPECIALISE frob False #-}

This will make two copies of ``frob``, one for ``True`` and one for ``False``. These
will then be optimised to make direct calls to ``helper1`` or ``helper2``, respectively.
Call sites (that use a literal ``True`` or ``False``) will be rewritten to use the
specialised versions.

Proposed Change Specification
-----------------------------

1. Here is the new BNF for ``SPECIALISE`` pragmas::

     pragma ::= ...
             |  '{-#' specialise_keyword activation rule_foralls specexp [ '::' type ]'#-}'  -- (1)
             |  '{-#' specialise_keyword activation qvar '::' type ',' types1 '#-}'          -- (2) DEPRECATED

     specialise_keyword ::= 'SPECIALISE' | 'SPECIALIZE' | 'SPECIALISE INLINE' | 'SPECIALISE INLINE'

     specexp ::= qvar
              |  specexp aexp
              |  specexp '@' atype

       -- as today
     activation ::= ...  -- this encompasses "[2]" and "[~0]"

       -- as today
     rule_foralls ::= 'forall' rule_vars '.' 'forall' rule_vars '.'
                  |   'forall' rule_vars '.'
                  |   {- empty -}

       -- as today
     types1 ::= types1 ',' type
            |   type

#. The first production (1) includes ``{-# SPECIALISE f :: type #-}`` as a special case
   in which there are no arguments.  The ``:: type`` part is optional, because it is often
   unnecessary if arguments are supplied.

#. The second production (2) is there only to support the current (implemented but entirely undocumented)
   possiblity of having multiple types in one ``SPECIALISE`` pragma. With this proposal, GHC will
   deprecate this form.
   A new warning, controlled by ``-Wdeprecated-pragmas`` (in ``-Wdefault``), will be emitted when it
   is used, and GHC may remove support for the syntax altogether after at least two
   major releases with the warning.

#. As today, ``SPECIALISE`` pragmas may be written only at top-level or
   in a class or instance declaration, never in a ``let`` or ``where``.

#. The optional ``forall`` clauses operate just like in rewrite rules:
   If there is one ``forall``, it binds term variables. If there are two ``forall``\ s,
   the first binds type variables and the second binds term variables.

#. All free variables of a ``SPECIALISE`` pragma must be in scope, and the
   expression must be well typed.

#. The ``qvar`` at the head of the ``specexp`` must not be one of the forall'd variables.

#. We do not allow infix notation: the function to be specialised must be at the head.  One could change this choice, but it is simple and clear.

#. Assume there is a definition ``f = rhs``.  (It may be defined with arguments on the left of course.)   Then a ``SPECIALISE`` pragma ::

         {-# SPECIALISE [1] forall x,y. f True (x,y) #-}

   causes GHC to do the following:

   1. Create a fresh name (we'll call it ``f'``).

   #. Create a new top-level binding ``f' x y = rhs True (x,y)``.

   #. Create a new rewrite rule ``{-# RULES "f/f'" [1] forall x,y. f True (x,y) = f' x y #-}``.

   #. If the ``SPECIALISE INLINE`` pragma is used (or its American spelling), then GHC additionally
      adds ``{-# INLINE [act] f' #-}``. This behavior is unchanged from today.

   GHC does not need to build source syntax
   as written above, and then typecheck and desugar it; it just behaves *as if* that happened.
   In practice, inference will be run on the original pragma, and the new top-level binding
   and rewrite rule will be constructed (in Core) to be well-typed.

#. GHC will issue a warning (controlled by ``-Wuseless-specialisations`` and part of the default warnings)
   if a specialisation can be determined to be useless (that is, not specialise anything). Examples:
   ``{-# SPECIALISE addMult #-}``, ``{-# SPECIALISE forall x y. addMult x y #-}``, and
   ``{-# SPECIALISE addMult :: Num a => a -> a -> a #-}``. This warning should trigger when
   the elaborated expression is eta-equivalent to the function being specialised.

Examples
--------
See the introduction and Motivation_ sections. As an example with variables, we have ::

  (-) x y = ...rhs...
  {-# SPECIALISE forall (x :: Int). (-) x 1 #-}

This will cause the following declarations::

  minus' :: Int -> Int
  minus' x = (...rhs...) x 1
  {-# RULES "minus1" forall x. (-) x 1 = minus' x #-}

Now, every time we say ``any_expression - 1`` in our (optimised) program, we will actually
invoke ``minus'``.

Consider a recursive function ::

  f :: Bool -> Int -> Int
  f b x = ...(if b then e1 else e2)...(f b e3)...

  {-# SPECIALISE f True #-}

Then GHC will generate ::

  f' = (\b x -> ...(if b then e1 else e2)...(f b e3)...) True
  {-# RULES "f" f True = f' #-}

After simplifying the RHS of ``f'``, including applying the rewrite rule in its RHS, we get ::

  f' = \x -> ...e1...(f' e3)...

Note that ``f'`` has become self-recursive, through the application of the rewrite rule.

Consider another recursive function ::

    loop :: [Int] -> Int
    loop [] = 1
    loop (x:xs) = x * loop xs

    {-# SPECIALISE loop [] #-}
    {-# SPECIALISE forall x xs . loop (x:xs) #-}

This will generate ::

    loopNil = 1
    loopCons x xs = x * loop xs

    {-# RULES "loop/loopNil" loop [] = loopNil #-}
    {-# RULES "loop/loopCons" forall x xs . loop (x:xs) = loopCons x xs #-}

So a call like ``loop [1,2]`` will fire the ``loop/loopCons`` rule to give ::

    loop [1,2]  -->   loopCons 1 [2]

But ``loopCons`` is a simple non-recursive function, and may well inline (especially if you say ``SPECIALISE INLINE``) ::

    loopCons 1 [2]  -->  x * loop [2]

Now the process can repeat, and the loop is unrolled.


Effect and Interactions
-----------------------
1. This generalises the current syntax for specialisation pragmas in a natural way.
   Indeed, I have written specialisation pragmas using the type applications syntax
   just expecting them to work.

#. Specialisation is now possible for functions with ambiguous types, previously
   impossible.

#. Term-level specialisation is now possible, a new feature that will enable
   users to avoid repetition with no runtime cost.

#. Given how this builds on the existing machinery so nicely, the implementation burden
   is expected to be small.

#. The syntax allowing multiple types to be specified is not documented in the
   `manual <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/pragmas.html?highlight=specialise#specialize-pragma>`_
   and is rarely used, according to a `Hackage search <https://hackage-search.serokell.io/?q=SPECIALI%5BSZ%5DE.*%2C>`_, and does not scale to handle the expression-level specialisation of this
   proposal.  Hence the plan to remove this undocumented feature altogether.


Costs and Drawbacks
-------------------
1. It is a bit annoying that the multiple-types syntax is not covered by
   this proposal, but the world is not perfect.

Alternatives
------------
1. We do not have to do anything. But it seems the language is crying out
   for this generalisation, so doing nothing would be very unsatisfying.


Unresolved Questions
--------------------
None at this time.
