Deep Subsumption
==============

.. author:: Matthew Pickering, Simon Peyton Jones, Jaro Reinders
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

``DeepSubsumption`` is a languge extension which allows users to opt-into deep
subsumption as it existed before GHC-9.0.


Motivation
----------

The `simplified subsumption proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst>`_
argued in favour of simplifying GHC's type system by removing 4 cases from the subsumption judgement.
The change was motivated by wanting to simplify both the implementation and language
semantics, as well as being a stepping-stone to the implementation of Quick-look impredicativity.

Unfortunately, the breakage study failed to accurately predict how annoying this
change would be to users. Some common patterns found in libraries now require
"pointless" eta-expansion, when the compiler used to automatically insert the
appropiate lambdas.

This proposal suggests adding a new language extension, ``DeepSubsumption``,
which can be enabled to recover the previous subsumption rules. This would allow
users to opt-in to deep subsumption as it was before GHC-9.0.


Proposed Change Specification
-----------------------------

We propose to add a language extension ``DeepSubsumption`` which provides a best
effort attempt to restore the previous deep subsumption behaviour. The extension
will implement the 4 subsumption rules which were removed by simplified subsumption.

* The extension will be **on** by default, as-if it was added to the Haskell2010 extension set.
* The extension will be **off** in the GHC2021 extension set.

As the GHC2021 extension was introduced in GHC 9.0, the same release as simplified
subsumption, any user who has upgraded and is already specifying the GHC2021 extension
set will have had to update their code to work with deep subsumption.
Otherwise, older programs which are still using Haskell2010 should continue to work
as before, because DeepSubsumption will be enabled until the user updates to the
2021 extensions.


It is not recommended that people use this extension as it makes type inference
less predictable and the language semantics more confusing. However, in a manner
similar to ``NoMonoLocalBinds``, users who really want such a feature are free to
enable the extension, with the understanding that doing so might introduce changes
to type inference or runtime behaviour that are difficult to predict.

Warnings
^^^^^^^^

Given that we don't think that using ``DeepSubsumption`` is a good idea, we also
propose to add two warnings to help users migrate to simplified
subsumption.

* When ``-XDeepSubsumption`` is off, the error message can be improved to suggest
  eta-expansion (and optionally enabling ``DeepSubsumption``).
* When ``-XDeepSubsumption`` is on and used, we can warn about these occurences.
  This would be used to migrate module by module away from ``DeepSubsumption``.

Examples
--------

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

using the ``fromHandle`` function can lead to compilation failures with simplified
subsumption::

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode fromHandle


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

This led Jaro to explore some other alteratives.

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

This change is not backwards-compatible, as the ``DeepSubsumption`` extension won't be
available on earlier versions of GHC (in particular GHC-9.0). A backwards-compatible
change would require adding CPP.

Example 2: Type synonyms with implicit parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another consumer hit hard by the change is the `Integrated Haskell Platform <https://github.com/digitallyinduced/ihp/pull/1342>`_.

In particular they define a type synonym which contains an implicit parameter::

  type Html = (?context :: ControllerContext) => Html5.Html

which is used to create the ``renderUser`` combinator::

  renderUser :: User -> Html
  renderUser user = [hsx|<li>{get #name user}</li>|]

but now ``renderUser`` fails to typecheck in ``renderUsers`` without eta-expansion::

  renderUsers :: [User] -> Html
  renderUsers users = [hsx|
    <ul>
      {forEach users renderUser}
    </ul>
  |]

the "solution" is to eta-expand the call to ``renderUser``::

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


Effect and Interactions
-----------------------

* The ``DeepSubsumption`` language pragma has all the drawbacks identified in
  the simplified subsumption proposal, but crucially allows users to opt-in to
  the drawbacks if their value judgement is different to that of the steering committee.


Costs and Drawbacks
-------------------

* We really do not recommend that people use this feature. It makes the language
  more complicated and type inference less predictable.
* In situations where the eta-expansion behaviour is desired for its user-friendliness,
  the requirement to enable a strange ``DeepSubsumption`` extension might just lead to even more confusion.
* Alejandro Serrano `suggests <https://github.com/ghc-proposals/ghc-proposals/pull/287#issuecomment-1128134798>`_
  that reintroducing this feature will not alleviate any pain, because by the time it's introduced
  maintainers will have already updated their libraries to account for the changes, and will not want to
  introduce more churn by enabling ``DeepSubsumption`` and removing the eta-expansions they recently added.

Alternatives
------------

* The alternative is to do nothing. Users will have to accept that simplified subsumption
  is here to stay and update their code appropiately.

Unresolved Questions
--------------------

* We need to decide whether we would want to backport this feature to the 9.2 branch.


Implementation Plan
-------------------

* A draft patch has been prepared by Simon PJ. `!8210 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8210>`_.

Endorsements
-------------
