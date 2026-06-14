Rebindable Application
======================

.. author:: Mac Malone
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/275>`_.
.. sectnum::
.. contents::

While most of GHC's syntax is in some way customizable
(ex. through overloaded type classes and/or ``RebindableSyntax``),
the function application syntax ``f a`` (i.e. the juxtaposition syntax)
is not. I propose to change that.

Motivation
----------

In Haskell, there are a lot of types that behave like functions.
However, outside of the primitive function arrow (``->``), none of
these are permitted to use the juxtaposition syntax for function 
application (i.e. ``f a ``).

Newtypes
^^^^^^^^

There are many newtype wrappers around function types (ex. ``Predicate``, 
``Cont``) or more general types (ex. ``Identity``). 
Newtypes often wish to copy much of the original functionality of 
the wrapped type, hence why extensions like ``GeneralizedNewtypeDeriving`` 
were created.
However, wrapped function types at disadvantage. They are unable to 
transparently copy the functionality of their wrapped types because they 
cannot leverage the juxtaposition  syntax for function application. 

For example, consider the following definition of a ``Match`` type.

.. code-block:: haskell

  newtype Match a r = Match { runMatch :: a -> BoolCont r }
  newtype BoolCont r = BoolCont { runBoolCont :: r -> r -> r }

  matchComma :: Match Char r
  matchComma = Match \a -> BoolCont \y n -> case a of { ',' -> y; _ -> n } 

  matchComma' :: Char -> r -> r -> r
  matchComma' \a y n = case a of { ',' -> y; _ -> n } 

In this example, ``BoolCont`` is a continuation-passing style (CPS) 
version of ``Bool`` and ``Match`` is a CPS version of ``Predicate``. 
If we want use these types as functions we have to unwrap them first.

.. code-block:: haskell

  hasCommaPrefix :: [Char] -> Bool
  hasCommaPrefix (x:xs) = runBoolCont (runMatch matchComma x) False True

This is inelegant and cumbersome. If we drop the newtypes, we can instead 
write the above function like so:

.. code-block:: haskell

  hasCommaPrefix' :: [Char] -> Bool
  hasCommaPrefix' (x:xs) = matchComma' x False True

This is much cleaner. However, doing so loses the type distinction newtypes 
provide, which can be useful in many cases -- for example, in type classes. 
We can define specialized ``Monad`` instances for ``Match`` and ``BoolCont``, 
but the same  cannot be done for the unwrapped function type as it already has 
a ``Monad`` instance.

As such, it would be most convenient if newtypes around functions could also
use the juxtaposition syntax. 
It would also provide additional abstraction as newtypes for functions and 
regular functions could then be used interchangeably in many cases.

Alternative Application Operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are also many different function application operations.
For example, the ``Functor`` class has ``<$>``, which, to quote the
documentation, is "function application lifted over a Functor".

As such, it would be nice if these other application operators 
could also use the juxtaposition syntax.
For instance, if the syntax worked with ``Applicative``, 
this would allow code like:

.. code-block:: haskell

  f = pure g <*> a <*> b <*> c

To be written like:

.. code-block:: haskell

  f = pure g a b c

And if it worked on ``Exp`` (from Template Haskell), code like this:

.. code-block:: haskell

  mapE f xs = VarE 'map `AppE` f `AppE` xs

Could be written like:

.. code-block:: haskell

  map' = VarE 'map
  mapE f xs = map' f xs

  -- or
  mapE f xs = VarE 'map f xs

Proposed Change Specification
-----------------------------

I propose a new extension called ``RebindableApplication``. 
When this extension is turned on, the juxtaposition syntax for 
function application ``f a`` becomes syntactic sugar for ``f $ a``, 
where ``$`` is whatever ``$`` is currently in scope.
Function application still retains its original (left) fixity 
and precedence (ex. ``f a b`` desugars to ``(f $ a) $ b``).
Operator application also remains the same (ex. ``f $ a`` is not 
further desugared). 
To clarify these changes, the table below lists
my proposed desugaring for each kind of application syntax.

+-----------------------+------------------+-----------------------+
| Application           | Current Syntax   |  Proposed Desugaring  |
+=======================+==================+=======================+
| Function Application  | ``f a``          | ``f $ a``             |
+-----------------------+------------------+-----------------------+
| Type Application      | ``f @t``         | ``f @t``              |
+-----------------------+------------------+-----------------------+
| Operator              | ``a <> b``       | ``a <> b``            |
+-----------------------+------------------+-----------------------+
| Left Section          | ``(a <>)``       | ``\x -> a <> x`` *    |
+-----------------------+------------------+-----------------------+
| Right Section         | ``(<> a)``       | ``\x -> x <> a``      |
+-----------------------+------------------+-----------------------+

Technically, the left section would actually desugared to ``(<>) e`` 
(using primitive application) since GHC does not eta abstract 
the left section so as to support the ``PostfixOperators`` extension
(see `Issue #18151 <https://gitlab.haskell.org/ghc/ghc/issues/18151>`_). 
Regardless, the point of the above table is to demonstrate that only 
application  in the plain juxtaposition syntax is rebindable, application 
found elsewhere remains the same. 

To rebind function application, one sets the ``$`` currently
in scope. This can be done globally by declaring or importing a top-level
``$`` and locally by using ``let`` or ``where``.

Examples
--------

With ``RebindableApplication``, we can use a local rebind to
write the simplified examples shown in the motivation:

.. code-block:: haskell

  -- ``f = g <$> a <*> b <*> c`` can become
  f = let g' = pure g; ($) = (<*>) in g' a b c

  -- ``mapE = VarE 'map `AppE` f `AppE` xs`` can become
  mapE f xs = let map' = VarE 'map, ($) = AppE in map' f xs

Alternatively, we could use a type class and a global rebinding instead:

.. code-block:: haskell
   
  import Data.Functor.Identity
  import Data.Functor.Contravariant
  import Control.Monad.Trans.Cont
  import Language.Haskell.TH (Exp(..))

  import Prelude hiding (($))
  import qualified Data.Function as F

  class Applicable f a b | f -> a b where
    ($) :: f -> a -> b

  instance Applicable (a -> b) a b where
    ($) = (F.$)

  -- Newtype Examples

  instance Applicable f a b => Applicable (Identity f) a b where
    f $ a = runIdentity f $ a

  instance Applicable (Predicate a) a Bool where
    ($) = getPredicate

  instance Applicable (Cont r a) (a -> r) r where
    ($) = runCont

  -- Match Example

  instance Applicable (Match a r) a (BoolCont r) where
    ($) = runMatch

  instance Applicable (BoolCont r) r (r -> r) where
    ($) = runBoolCont

  -- We can now write this
  hasCommaPrefix :: [Char] -> Bool
  hasCommaPrefix (x:xs) = matchComma x False True

  -- TH Example

  instance Applicable Exp Exp Exp where
    ($) = AppE 

  -- ``mapE = VarE 'map `AppE` f `AppE` xs`` can now become
  mapE :: Exp -> Exp -> Exp
  mapE f xs = VarE 'map f xs

To recover the behavior of primitive application, one can use the ``$``
operator from ``Prelude``. If better support for unlifted types is needed,
one can also define a more primitive application operator ``$#`` like so:

.. code-block:: haskell

  infixl 9 $#
  ($#) :: forall a. a -> a
  ($#) = id

While this ``$#`` works with unlifted types, it unfortunately does not yet 
work for higher-rank types (i.e., those produced with ``RankNTypes``), though 
this may be resolved if the proposal in 
`#274 <https://github.com/ghc-proposals/ghc-proposals/pull/274>`_ 
gets accepted.

Effect and Interactions
-----------------------

This proposal allows different modes of function application to
all share the same syntax, which I would argue allows users to write
more concise (and, to a certain extent, clearer) code.

It also has the interesting effect of making application more
first-class syntactically (according to the definition Dijkstra outlined
`here <http://www.the-magus.in/Publications/ewd.pdf>`_).
The juxtaposition notation is now merely syntactic sugar for an
operator (namely ``$``).

Costs and Drawbacks
-------------------

I imagine that there will be some maintenance costs associated with
the proposed extension -- though given that the proposal is essentially purely
syntactic, I imagine such costs will be minor.

For learners, the new desugaring may be surprising.
However, since new users are just learning of the similarities and differences 
between the juxtaposition syntax and ``$``, they do not have original 
distinctions ingrained.
Thus, I argue that they will likely find the proposed desugaring much more
straightforward and, possibly, even expected.
Long time Haskellers, however, may find this confusing as they are most 
used to function application being built into the syntax -- though, being
experienced, they are also likely to adapt easier.

The proposed desugaring does, however, come with a number of drawbacks due to
the limitations of the function ``$``.
Due to the restrictions of levity polymorphism, ``$`` can not be fully levity
polymorphic. Thus modules with ``RebindableApplication`` can not use the
juxtaposition syntax for primitive operations and constructors like ``I#``.
Similar problems occur with higher-rank functions defined with ``RankNTypes``.

However, this problem can be somewhat mitigated with a operator like the 
``$#``  mentioned in the Examples section. 
Unfortunately, It does not solve  the ``RankNTypes`` problem (yet), 
and, as such, I consider this aspect to the weakest part of the proposal. 
To resolve this, I proposed the magic ``$#`` described below in the 
Alternatives section, but in the course of discussing this proposal that was 
thought to maybe be a bit too extreme (hence it being demoted to an 
alternative).

Alternatives
------------

There are a number of possible alternatives, two of which I will discuss here.

Do Nothing
^^^^^^^^^^

We can always do nothing. This would mean newtype wrappers around functions
would not be able to use the juxtaposition syntax and alternative application
operators like ``(<$>)`` would still need to be explicit in all circumstances.

Personally, I believe that this status quo is rather ugly and causes the
language to give unjustified primacy to functions represented by the function
arrow ``(->)`` as opposed to those presented other ways.
A similar critique was made by Dijkstra himself in the EWD note previously
referenced  (i.e. `this one <http://www.the-magus.in/Publications/ewd.pdf>`_).
As such, I do not believe it is correct to maintain the status quo.

Idiom Brackets
^^^^^^^^^^^^^^

If global rebindings of the juxtaposition syntax are considered too extreme,
we could use a bracketing syntax to limit the scope of the rebinding.
Instead of desugaring all occurrences of the juxtaposition syntax, we only
do so within the brackets. For example, using idiom brackets:

.. code-block:: haskell

  let map' = VarE 'map, ($) = AppE in (| map' f xs |)

could desugar to

.. code-block:: haskell

  let map' = VarE 'map, ($) = AppE in map' $ f $ xs

Magic ``$#``
^^^^^^^^^^^^

It is not currently possible to recover exactly the behavior of 
primitive application in a module with ``RebindableApplication`` enabled. 
The ``$#`` mentioned in the Examples section helps but does not support 
``RankNTypes``. 
To solve this, we could add new magic application operator ``$#``.
This operator would simply desugar to primitive application when used. 
As such, it would not be permitted to be use unsaturated. 
To clarify, the table below shows how ``$#`` would be desugared.

+---------------------+------------+---------------------+
| Use                 | Syntax     | Proposed Desugaring |
+=====================+============+=====================+
| Unsaturated         | ``($#)``   | Prohibited          |
+---------------------+------------+---------------------+
| Partially Saturated | ``($#) f`` | ``f``               |
+---------------------+------------+---------------------+
| Fully Saturated     | ``f $# a`` | ``f a``             |
+---------------------+------------+---------------------+
| Left Section        | ``(f $#)`` | ``f``               |
+---------------------+------------+---------------------+
| Right Section       | ``($# a)`` | ``\f -> f a``       |
+---------------------+------------+---------------------+

This ``$#`` operator could be located in ``GHC.Exts``
if it is implemented as an actual name and not built into GHC's syntax.


Unresolved Questions
--------------------

The name of the extension given in the proposal (i.e. ``RebindableApplication``)
and the symbol for primitive application (``$#``) could be changed if desired.


Implementation Plan
-------------------

**TBD**
