Deep Subsumption
================

.. author:: Matthew Pickering, Simon Peyton Jones, Jaro Reinders
.. date-accepted:: 2022-07-05
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8210
.. implemented:: 9.2.4
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
.. contents::

``DeepSubsumption`` is a languge extension which allows users to opt-into deep
subsumption as it existed before GHC-9.0.


1. Motivation
-------------

The `simplified subsumption proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst>`_
argued in favour of simplifying the subsumption judgement in GHC's type system, by removing:

* Covariance and contravariance of function types
* Deep skolemisation
* Deep instantiation.

The change was motivated by wanting to simplify both the implementation and language
semantics, as well as being a stepping-stone to the implementation of Quick-look impredicativity.

Unfortunately, the breakage study failed to accurately predict how annoying this
change would be to users. Some common patterns found in libraries now require
eta-expansion, when the compiler used to automatically insert the
appropriate lambdas.

This proposal suggests adding a new language extension, ``DeepSubsumption``,
enabled by default in the common case when the language is set to ``-XHaskell2010``,
which recovers the previous subsumption rules. This would allow
users to opt-in to deep subsumption as it was before GHC-9.0.


2. Proposed Change Specification
--------------------------------

We propose to add a language extension ``DeepSubsumption`` which restores the previous deep subsumption behaviour:

* The extension implements deep skolemisation and the co/contra subtyping rules, which were removed by simplified subsumption.
* It does not re-introduce deep instantiation.  Doing only shallow instantation is not a cause of breakage: it changes only some types reported in error messages and in GHCi.  Moreover, deep instantiation is fundamentally incompatible with the widely used ``TypeApplications`` extension.
* It makes no changes to the Quick Look algorithm, which implements `ImpredicativeTypes`.  That is, Quick Look treats function arrows as invariant, even if ``DeepSubsumption`` is on. As its name suggests, Quick Look takes a quick look at an application, searching for opportunities for impredicative instantiation, but leaves the main type inference algorithm unaffected.

When ``DeepSubsumption`` is on by default:

* ``DeepSubsumption`` will be part of the ``Haskell2010`` and ``Haskell98`` extension sets.
* ``DeepSubsumption`` will not be part of ``GHC2021``.

Like any other extension, ``DeepSubsumption`` can be turned on or off with a ``LANGUAGE`` pragma
or in a ``.cabal`` file. Because ``DeepSubsumption`` is part of ``Haskell2010`` and ``Haskell98``,
projects compiled with ``.cabal`` files that declare either of these to be the ``default-language``
will get the benefits of deep subsumption, much like it was implemented prior to GHC-9.0.
Note that any project using ``GHC2021`` must also have upgraded to use shallow subsumption, because
the two features came out in the same GHC release.

2.1 DeepSubsumption is not recommended
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``DeepSubsumption`` extension is not recommended:

* It makes the runtime semantics (including performance) of Haskell programs
  less predictable (due to silent eta-expansion), as the original proposal describes.
  The situation is even more complicated when type classes are involved.  You can find some intricate discussion on the `Simplified subsumption proposal discussion thread <https://github.com/ghc-proposals/ghc-proposals/pull/287>`_, especially towards the end.

* The interaction between ``DeepSubsumption`` and ``ImpredicativeTypes`` is hard to predict.  Quick Look treats function arrow as invariant, which is different to ``DeepSubsumption``, but it is hard to come up with concrete examples that show strange behaviour.  Perhaps surprisingly, the two different treatments of function arrow, while infelicitous, do not seem to have an immediately bad effects.

* ``DeepSubsumption`` (notably deep skolemisation) seems to be fundamentally incompatible with the accepted proposal 155: `Binding type variables in lambda expressions <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst>`_. Consider::

      f :: Int -> forall a. a -> a
      f = \x-> let v = x+x in
               \ @a -> blah

  To correctly bind the ``\ @a`` we must not deeply skolemise ``f``'s type at the outset;
  yet we must do so to implement ``DeepSubsumption``.  So GHC will reject an attempt to switch both on at once.

Despite these shortcomings, in a manner similar to
``NoMonoLocalBinds``, users who really want such a feature are free to
enable ``DeepSubsumption``, with the understanding that doing so might
introduce changes to type inference or runtime behaviour that are
difficult to predict.

Despite not being recommended, there is no deprecation plan for ``DeepSubsumption``, it will
be available as an extension indefinitely.

2.2 Warnings
^^^^^^^^^^^^

Given that we don't think that using ``DeepSubsumption`` is a good idea, we also
propose to improve diagnostics to help users migrate to simplified
subsumption.

* When ``-XDeepSubsumption`` is off, the error message can be improved to suggest
  eta-expansion (and optionally enabling ``DeepSubsumption``).

* When ``-XDeepSubsumption`` is on:

  * A new warning ``-Wdeep-subsumption``, in ``-Wcompat`` and ``-Weverything`` (but
    not other warning sets), will warn whenever deep subsumption is used, suggesting
    that the user eta-expand.

  * A new warning ``-Wauto-lambda-destroys-sharing``, in ``-W``, will warn (on a
    best effort basis) when deep subsumption creates a lambda that may destroy
    runtime sharing (and thus pessimise runtimes).

In the text above, "eta-expansion" is a short-hand used in this proposal. The actual
error message will be crafted to either avoid or introduce this terminology.

3. Examples
-----------

In this section we present two case studies about how migrating to simplified
subsumption has been challenging for users.


Example 1: Type synonyms with quantified variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The example given by ParetoOptimalDev on `Discourse <https://discourse.haskell.org/t/r-haskell-was-simplified-subsumption-worth-it-for-industry-haskell/4486>`_
was carefully analysed by Jaro R.

Certain libraries such as `pipes <https://hackage.haskell.org/package/pipes>`_ define a general data type
together with specialised type synonyms with universally quantified type variables. It
is key to use a type synonym rather than a newtype, so that the specialised
versions can still work with more general combinators.
For example, ``pipes`` defines the following data types::

  data Proxy x' x a b m r = ....

  type Producer' b m r = forall x' x . Proxy x' x () b m r

and also provides the ``fromHandle`` function, which uses the ``Producer'`` type synonym::

  fromHandle :: MonadIO m => Handle -> Producer' ByteString m ()

Using the ``fromHandle`` function can lead to compilation failures with simplified
subsumption. For example::

  withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode fromHandle

Here `fromHandle` has a forall to the right of its arrow,
whereas `withFile`'s third argument does not.

Solution 1: Eta-expansion
+++++++++++++++++++++++++

As described in the simplfied subsumption proposal, the simplest fix is to eta-expand
the call to ``fromHandle`` in the definition of ``readFreqSumFile``::

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode (\x -> fromHandle x)

However, ParetoOptimalDev isn't so satisfied by this solution because

1. It required many such "pointless" changes to the code base.
2. It seems "random" when you need to eta-expand or not, Haskell programmers expect
   eta-equivalence to hold (even though it does not and never has).
3. They view the benefits (simpler language, simpler semantics) as something that
   is not worth breaking. We have lived with deep subsumption for
   many years.

This led Jaro to explore some other alternatives.

Solution 2: Newtype Wrapper
+++++++++++++++++++++++++++

Simon PJ suggests making these type synonyms into newtypes::

  newtype Producer' b m r = MkProducer' (forall x' x. Proxy x' x () b m r)

If you implement all the required constraints for this type then you can just write the original::

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode PB.fromHandle

But this is not quite a good solution here, because you can't
automatically derive all the instances, and you cannot compose these producers
with other pipes.
This interoperability could possibly be restored by using the same tricks that
the ``optics`` library uses to get their lenses to compose, but that seems like
quite a big change here.

Solution 3: Rewrite type synonyms in place
++++++++++++++++++++++++++++++++++++++++++

Another simple change to resolve this is to avoid using type synonyms altogether,
by inlining their definition in-place::

  fromHandle :: MonadIO m => Handle -> Proxy x' x () ByteString m ()

Then the original non-eta-expanded implementation of ``readFreqSumFile``
typechecks without issues. However, this worsens the usability of the library, as
it becomes harder to understand that the ``Proxy`` in this case really
must be a producer. It is also another invasive change to rewrite all the type
signatures of all downstream libraries which use this pattern.

Solution 4: Deep Subsumption
++++++++++++++++++++++++++++

With this proposal, the user enables ``DeepSubsumption`` and the program continues
to typecheck as before::

  {-# LANGUAGE DeepSubsumption #-}

  ...

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode PB.fromHandle


Example 2: Type synonyms with implicit parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another consumer hit hard by the change is the `Integrated Haskell Platform <https://github.com/digitallyinduced/ihp/pull/1342>`_.
In particular they define a type synonym which contains an implicit parameter::

  type Html = (?context :: ControllerContext) => Html5.Html

which is used to create the ``renderUser`` combinator::

  renderUser :: User -> Html
  renderUser user = [hsx|<li>{get #name user}</li>|]

but now ``renderUser`` fails to typecheck in ``renderUsers`` without eta-expansion::

  forEach :: (MonoFoldable mono, Applicative m) => mono -> (Element mono -> m ()) -> m ()

  renderUsers :: [User] -> Html
  renderUsers users = [hsx|
    <ul>
      {forEach users renderUser}
    </ul>
  |]

Again, ``renderUser`` has a forall to the right of its function arrow (hidden under ``Html``),
while ``forEach``'s second arguemnt does not.
The "solution" is to eta-expand the call to ``renderUser``::

  renderUsers :: [User] -> Html
  renderUsers users = [hsx|
    <ul>
      {forEach users (\x -> renderUser x)}
    </ul>
  |]

But such changes were `deemed unsatisfactory <https://github.com/digitallyinduced/ihp/pull/1342#issuecomment-1058870639>`_
by the maintainers:

  All of them break existing IHP apps / require a lot of changes when updating.

In this situation too, the benefits of simplified subsumption are deemed to not be worth the costs
in terms of usability and user-friendliness. This too suggests re-instating the old behaviour as
an opt-in by adding a ``DeepSubsumption`` extension.


4. Effect and Interactions
--------------------------

The ``DeepSubsumption`` language pragma has all the drawbacks identified in
the simplified subsumption proposal, but crucially allows users to opt-in to
the drawbacks if their value judgement is different to that of the steering committee.

5. Costs and Drawbacks
----------------------

* We do not recommend that people use this feature. It makes the language
  more complicated and runtime performance less predictable.
* In situations where the eta-expansion behaviour is desired for its user-friendliness,
  the requirement to enable a strange ``DeepSubsumption`` extension might just lead to even more confusion.
* Alejandro Serrano `suggests <https://github.com/ghc-proposals/ghc-proposals/pull/287#issuecomment-1128134798>`_
  that reintroducing this feature will not alleviate any pain, because by the time it is introduced
  maintainers will have already updated their libraries to account for the changes, and will not want to
  introduce more churn by enabling ``DeepSubsumption`` and removing the eta-expansions they recently added.



6. Alternatives
---------------

* The alternative is to do nothing. Users will have to accept that simplified subsumption
  is here to stay and update their code appropiately.

7. Unresolved Questions
-----------------------

* We need to decide whether we would want to backport this feature to the 9.2 branch.


8. Implementation
-------------------

Fortunately, the implementation complexity of adding ``DeepSubsumption`` is modest, and well
localised.  We already have an MR that implements it: `!8210 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8210>`_.

9. Endorsements
---------------
