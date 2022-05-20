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
argued to simplify GHC's type system by removing 4 cases from the subsumption judgement.
The change was motivated by wanting to simplify both the implementation and language
semantics.

Unfortunately the breakage study failed to accurately predict how annoying this
change would be to users. Some common patterns found it libraries now require
"pointless" eta-expansion where the compiler used to automatically insert the
appropiate casts due to the 4 removed subsumption rules.

This proposal is about adding ``DeepSubsumption`` language extentsion which can
be enabled to add back the removed subsumption rules to allow users to opt-in to
deep subsumption as it was before GHC-9.0.


Proposed Change Specification
-----------------------------

We propose to add a language extension ``DeepSubsumption`` which provides a best
effort attempt to implement the restore the deep subsumption behaviour. The extension
will implement the 4 subsumption rules which were removed by simplified subsumption.
The extension is off by default and must be enabled in client code if they want
the subsumption rules and associated eta-expansion.

It is not recommended that people use this extension as it makes type inference
less predictable and the language semantics more confusing. However, in a manner
similar to ``NoMonoLocalBinds``, if the user really wants this then they are free to
enable the extension with the understanding that they take responsibility if things
go wrong!

Example 1
---------

The example given by ParetoOptimalDev on `Discourse <https://discourse.haskell.org/t/r-haskell-was-simplified-subsumption-worth-it-for-industry-haskell/4486>`_ was analysed carefully by
Jaro R.

Certain libraries such as `pipes <https://hackage.haskell.org/package/pipes>`_ define a general data type but also
export specialised type synonyms with universally quantified type variables. It
is key to use a type synonym rather than a newtype so that the the specialised
versions can still work with more general combinators.

For example, pipes defines the following data types::

  data Proxy x' x a b m r = ....

  type Producer' b m r = forall x' x . Proxy x' x () b m r

and also provides the ``fromHandle`` function, which uses the ``Producer'`` type synonym::

  fromHandle :: MonadIO m => Handle -> Producer' ByteString m ()

using the ``fromHandle`` function can lead to compilation failures with simplified
subsumption::

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode fromHandle


Solution 1: Eta-expansion
^^^^^^^^^^^^^^^^^^^^^^^^^

As described in the simplfied subsumption proposal, the simplest fix is to eta-expand
the call to ``fromHandle`` in the definition of ``readFreqSumFile``::

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode (\x -> fromHandle x)

However, ParetoOptimalDev isn't so satisfied by this solution because

1. It required many such "pointless" changes to the code base.
2. It seems "random" when you need to eta-expand or not, Haskell programmers expect
   eta-equivalence to hold (even though it does not and never has).
3. They view the benefits (simpler language, simpler semantics) as something that
   this breaking change is not worth it. We have lived with deep subsumption for
   many years.

This led Jaro to explore some other alteratives.

Solution 2: Newtype Wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Simon PJ suggests making these type synonyms into newtypes::

  newtype Producer' b m r = MkProducer' (forall x' x. Proxy x' x () b m r)

If you implement all the required constraints for this type then you can just write the original::

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode PB.fromHandle

But this is not quite a good solution here, because you can't
automatically derive all the instances and you cannot compose these producers
with other pipes.

This interoperability could probably be restored by using the same tricks that
the ``optics`` library uses to get their lenses to compose, but that seems like
quite a big change here.

Solution 3: Rewrite type synonyms in place
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another simple change to resolve this is to just rewrite the type synonyms in-place::

  fromHandle :: MonadIO m => Handle -> Proxy x' x () ByteString m ()

Then you can also just write the original non-eta-expanded version. But a
problem is that it is harder understand that the ``Proxy`` in this case really
must be a producer. It is also another invasive change to rewrite all the type
signatures of all downstream libraries which use this pattern.

Solution 4: Deep Subsumption
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With this proposal, the user enables ``DeepSubsumption`` and the program continues
to typecheck as before::

  {-# LANGUAGE DeepSubsumption #-}

  ...

  readFreqSumFile file = readFreqSumProd $ withFile file ReadMode PB.fromHandle


Example 2
---------

Another consumer hit hard by the change is the `Integrated Haskell Platform<https://github.com/digitallyinduced/ihp/pull/1342>`_.

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

But Marc Scholten is `not happy<https://github.com/digitallyinduced/ihp/pull/1342#issuecomment-1058870639>`_ with this proposal as

  All of them break existing IHP apps / require a lot of changes when updating.

So it seems the arguments for simplified subsumption are not persuasive enough for
him to accept the breakage. This points to a solution where we can have the best of
both worlds by adding a ``DeepSubsumption`` extension which can be enabled by
clients such as IHP if they desire this behaviour.



Effect and Interactions
-----------------------

* The ``DeepSubsumption`` language pragma has all the drawbacks identified in
  the simplified subsumption proposal, but crucially allows users to opt-in to
  the drawbacks if their value judgement is different to that of the steering committee.


Costs and Drawbacks
-------------------

* We really do not recommend that people use this feature. It makes the language
  more complicated and type inference less predictable.
* Alejandro Serrano `suggests <https://github.com/ghc-proposals/ghc-proposals/pull/287#issuecomment-1128134798>`_ that reintroducing this feature will not alleviate any pain because
  by the time it's introduced then everyone will have already felt it as all
  libraries will be upgraded to the 9.0 series.



Alternatives
------------

* The alternative is to do nothing, users must accept that simplified subsumption
  is here to stay and update their code appropiately.

Unresolved Questions
--------------------

* We need to decide whether we would want to backport this feature to the 9.2 branch.


Implementation Plan
-------------------

* A draft patch has been prepared by Simon PJ. `!8210 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8210>`_.

Endorsements
-------------
