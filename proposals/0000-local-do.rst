Proposal title
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal introduces a new extension ``-XLocalDo`` which makes it possible to overload the meaning of a do-notation expression *on a case-by-case basis* (as opposed to the global effect of ``-XRebindableSyntax``), by writing ``do @builder``. The design is inspired by F#'s [computational expressions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions).


Motivation
------------

There are many kinds of monad-like things out there:

* monads
* indexed monads
* graded monads
* relative monads
* linear variants of all the above once `linear types <https://github.com/ghc-proposals/ghc-proposals/pull/111>`_
* …

All of these are theoretically compatible with the do-notation. And, in fact, really want the do-notation to work for them. After all, the entire existence of the do-notation can be ascribed to: “it's really annoying to program with ``(>>=)`` all the time”.

The prescribed solution to use ``-XRebindableSyntax``. But ``-XRebindableSyntax`` is a very blunt instrument:

* It affects many syntactic constructs (numerical literals, the ``if then else`` syntax, … (see the `full list <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax>`_)).
* It implies ``-XNoImplicitPrelude``
* It uses the same rebinding for all the do-expression in an entire file.

You may not want all this. For instance, with linear types ``if then else`` syntax cannot be meaningfully rebound to a function. And there is no reason why a file wouldn't have a piece code referring to a monad, one to a graded monad, and one to a linear relative monad.

Proposed Change Specification
-----------------------------

This proposal creates a new language extension ``-XLocalDo``.

When ``-XLocalDo`` is activated, the syntax of the ``do`` notation is changed to

::
  <lexp> ⟶ do [@<aexp>]

(``aexp`` means that the notation after the ``@`` is parsed as a variable, unless there are parentheses)

The additional expression is called the *builder* of the do-expression. A builder's type must be a record, other than that the type is immaterial.

Desugaring is then performed as follows:

* Desugaring a ``x <- u`` statement uses the ``(>>=)`` field of the builder
* Desugaring a ``u`` statement uses ``(>>)`` field of the builder
* Desugaring a ``pat <- u`` statement uses ``fail`` field of the builder
* ``-XApplicativeDo`` uses the ``(<*>)`` field of the builder

It is, crucially, not required that the record projections be in scope unqualified (otherwise projections of various builders would shadow one-another).

If a field is required by the desugaring process (and only if it's required!) but the builder's type doesn't have such a type, an error message is produced:

* “Desugaring statement <stmt> requires <field name> but builder <builder name> doesn't provide it”

The fields of a builder are subject to the same type restrictions as their counterparts with ``-XRebindableSyntax``.

When the ``@<aexp>`` annotation is omitted, the builder is taken to be whatever is named ``builder`` in scope.

A standard builder is added to ``Control.Monad`` and re-exported in the ``Prelude``:

::

  -- For simplicity, this ignores the namespacing issues

  data StandardBuilder = StandardBuilder
    { (>>=) :: Monad m => m a -> (a -> m b) -> m b
    , (>>) :: Monad m => m a -> m b -> m b
    , fail :: MonadFail m => m a
    , (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    }

  builder :: StandardBuilder
  builder = StandardBuilder (>>=) (>>) fail (<*>)

Effect and Interactions
-----------------------

``-XLocalDo`` make it possible to choose, for each individual do-expressions, what kind of monad-like notion they are about. Even if the monad-like notion doesn't support all the range of desugaring (for instance it doesn't have a ``fail``), this will still work, as long as the do-expression doesn't use the corresponding feature (in our example: pattern-binders).

For instance we could make a builder for monoids:

::

  module Data.Monoid.Builder where
    data MonoidBuilder = MonoidBuilder
      { (>>) :: Monoid a => a -> a -> a
      }

    builder :: MonoidBuilder
    builder = MonoidBuilder (<>)

  module X where
    import qualified Data.Monoid.Builder as Monoid

    f = do @Monoid.builder
      Sum 2
      Sum 3
      Sum 5
      Sum 8

If one would try to use ``x <- u`` with ``Monoid.builder``, GHC would
raise an error *even if there is a value for ``(>>=)`` in scope*.

Importing ``-XLocalDo`` doesn't change the meaning of existing do-expressions: they will pick up the ``builder`` from the ``Prelude``, which has the same meaning as current default.

``LocalDo`` interferes with ``RebindableSyntax``. We propose that ``LocalDo`` take precedence when both are enabled.

The syntax was chosen to resemble that of visible type applications (as it also makes visible arguments which were previously hidden). There is no syntax conflicts, as ``do`` is not actually a function, therefore the notation ``@<expr>`` cannot occur at this site currently. This is still true after `Type applications in patterns <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0031-type-applications-in-patterns.rst>`_, even if one chooses to use the whitespace syntax *and* writes the first statement on the same line as the ``do``: no pattern can start with an ``@``.

Costs and Drawbacks
-------------------

The do-expression store, during type-checking, which expression they will use for ``(>>=)``, etc… So the core infrastructure is actually already there. We anticipate the cost of implementation and maintenance of this feature to be very low.

Alternatives
------------

* One could use ``-XRebindableSyntax`` and use a very general type class which encompasses all monads

  * This was the essence of the `OverloadedDo proposal <https://github.com/ghc-proposals/ghc-proposals/pull/78>`_, though type inference was never solved for this
  * A more recent idea is `supermonads <http://www.cs.nott.ac.uk/~psznhn/Publications/jfp2018.pdf>`_, which solves the type inference issue using a plugin

  It requires somewhat less work (“only” a plugin, rather than a change in GHC's compiler, at least it's more modular), and is more automatic, as the correct functions are picked automatically from the type. But there is no way that this will capture all the desired notion: some restrictions need be imposed for the sake of type inference. Note as well that this proposal doesn't preclude an automatic approach when appropriate: simply import your very automatic builder in scope, and all the do-expressions without an explicit builder will use this.

* There is a way to emulate ``-XLocalDo`` in current GHC using ``-XRecordWildcards``: have no ``(>>=)`` and such in scope, and import a builder with ``Builder {..} = builder``. It is used in `linear-base <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/System/IO/Resource.hs#L119-L120>`_. This is not a very good solution: it is rather a impenetrable idiom, and, if a single function uses several builders, it yields syntactic contortion (which is why shadowing warnings are deactivated `here <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/System/IO/Resource.hs#L1>`_)

* Instead of changing the ``Prelude``, the standard builder could be
  hosted in a separate module (such as ``Ghc.LocalDo``), and the
  programmer could ``import Ghc.LocalDo`` when they use ``-XLocalDo``.

* An alternative to the ``@<aexpr>`` notation would be to use implicit parameters, somehow. But it's unclear how exactly it would look.

Unresolved Questions
--------------------

None.


Implementation Plan
-------------------
