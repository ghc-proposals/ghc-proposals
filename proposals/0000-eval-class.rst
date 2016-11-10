.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Bring back ``class Eval``
=========================

This proposal is to add a language extension that, when specified, rolls back the ``seq`` semantics to their Haskell 1.3
state.

Motivation
----------

The main benefits would be to make Haskell fully parametric again, to make η-conversion and foldr-build optimization
safe, and possibly to satisfy Bob Harper. Here is some background reading material that explains the existing problems:
  - `η-equivalence in Haskell <http://cstheory.stackexchange.com/questions/19165/is-eta-equivalence-for-functions-compatiable-with-haskells-seq-operation>`
  - `Haskell has no state monad <http://www.cse.chalmers.se/~nicsma/no-state-monad.html>`
  - `Hask is not a category <http://math.andrej.com/2016/08/06/hask-is-not-a-category/>`
  - `Correctness of short cut fusion <https://wiki.haskell.org/Correctness_of_short_cut_fusion#In_the_absence_of_seq>`
  - `Haskell is exceptionally unsafe
    <https://existentialtype.wordpress.com/2012/08/14/haskell-is-exceptionally-unsafe/>`

Proposed Change
---------------

**First**, Restore class ``Eval`` and its methods much as they were in Haskell 1.3 specification, only renaming the
method ``strict`` to its modern equivalent ``$!``
::
    class Eval a where
      ($!)    :: (a -> b) -> a -> b
      seq     :: a -> b -> b
      f $! x  =  x `seq` f x

Like ``Typeable``, the ``Eval`` class is implicitly derived for every data type and its instances cannot be explicitly
defined.

As a consequence of this change, the ``Eval`` class would start appearing in the inferred type context of expressions
using ``seq``. For example, the inferred type of function ``f x = seq x x`` would become ``f :: forall a. Eval a => a ->
a``.

**Second**, add a new language option, namely ``-XNoUniversalEval`` together with its evil twin ``-XUniversalEval``. The
latter option would be the default one.

In any module compiled with ``-XUniversalEval``, the class ``Eval`` would be satisfied by every type of kind
``*``. Furthermore, every explicit type signature would be considered equivalent to the same signature with the context
``Eval a =>`` added for every type variable ``a :: *``. The example function ``f`` above would thus still compile even
if accompanied by an explicit type signature ``f :: forall a. a -> a``.

In a module compiled with ``-XNoUniversalEval`` the class ``Eval`` would become a normal class, subject to the usual
type equivalence rules. Any polymorphic use of ``seq`` would trigger a compilation error, including the above
example. The same constraint would apply to bang patterns and the strict data bang, but monomorphic applications of
``seq`` and bang would not be affected. GHC should be able to apply more aggressive optimizations in a
``-XNoUniversalEval`` module.

**Third**, add the ``Eval =>`` context to the type signatures of ``Data.List.foldl'`` and all other strict functions in
the ``base`` library. That includes ``Data.Foldable.foldl'`` and other class methods whose instances are expected to be
strict.

**Fourth**, add the ``-WetaUnsafe`` option that warns about any polymorphic occurrence of ``seq``. In other words, the
warning would be issued for any use of ``seq`` in every module compiled with ``-XUniversalEval`` that would trigger an
error if ``-XNoUniversalEval`` was active instead. The warning would be off by default, but included in ``-Wall``.

This proposal is orthogonal to all existing language extensions. This includes even ``StrictHaskell``, though the
combination of this one with ``-XNoUniversalEval`` in the same module might prove impractical.

Drawbacks
---------

The main reason given for dropping the ``Eval`` class from Haskell 98 given in *A History of Haskell: Being Lazy With
Class* (§10.3) was ease of debugging. Specifically, if one wants for debugging purposes to temporarily invoke ``seq`` in
a polymorphic function, that forces adding the ``Eval a =>`` context to the explicit type signatures of that function
and all its polymorphic callers.

Personally, that justification strikes me as strange. Haskell is not otherwise known for weakening the language
properties in order to accommodate development procedures or tooling.

To make another point, I have often wished I could avoid adding the ``Show a =>`` context for debugging
purposes. Indeed, if the function ``show``, or at least ``traceShow`` should become polymorphic, that would greatly
simplify printf-style debugging. I'm not aware of anybody asking for this.

The main drawback to clamping down on ``seq`` today is the quantity of code that's using it unconstrained. Still, we
have to start somewhere. My hope is that one day ``-XNoUniversalEval`` will become the default and the
``-XUniversalEval`` pragma will be necessary to apply ``seq`` willy-nilly.

If this change were to happen today, there would certainly be plenty of broken code. The breakage would probably *not*
be in the low-level libraries that heavily depend on strictness annotations for optimization. That code is typically
monomorphic and thus wouldn't be affected.

Alternatives
------------

A previous version of this proposal started by adding a new module named ``Data.Eval``, exporting the class ``Eval`` and
its methods. There would thus be two variants of ``seq``, the polymorphic one in ``Prelude`` and the safe one in
``Data.Eval``, and users would opt into using the latter by importing ``Data.Eval``.

This cunning plan would require virtually no change to GHC, but unfortunately it fell apart on the ``foldl'`` and
``foldr'`` methods of the ``Foldable`` class. We can't simply export an alternative ``Foldable`` class from
``Data.Eval.Foldable`` because the two classes would be incompatible.

The ``-WetaUnsafe`` part of the proposal is meant as a gentle nudge away from the polymorphic ``seq`` and toward the
bright future. The current habits would probably continue without it. Stronger alternatives like turning the warning on
by default can be imagined, but they would likely bring too many complaints.

I had also considered extending the *SafeHaskell* inference mechanism. It could infer a module *EtaSafe* if it's *Safe*
or *Trustworthy*, all its imports are *EtaSafe*, and no ``seq`` use in the module is polymorphic. I dropped this idea
mostly because it seemed wrong to conflate ``unsafePerformIO`` and polymorphic ``seq``; they are not unsafe in the same
sense. Besides, I'm not convinced the *EtaSafe* certificate would attract much attention.

Unresolved Questions
--------------------

It would be nice to get some estimate of the proportion of existing packages that cannot be compiled with
``-XNoUniversalEval``.
