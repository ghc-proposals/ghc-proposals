.. Notes on reStructuredText - delete this section before submitting
.. ==================================================================

.. The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

.. ::

..  like this
..  and

..  this too

.. To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Deriving Superclasses
=====================

.. author:: Benjamin
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
.. sectnum::
.. contents::

Sometimes a subclass of a typeclass can provide an alternative implementation of
parts or all of that typeclass. The typical example are ``Monad``s, which can
almost completely describe the ``Applicative`` and ``Functor`` instances for their
types. Using this extension would allow library maintainers to define default
implementations of methods for superclasses, giving the opportunity for more
performant default definitions and less need to define superclasses.

.. Here you should write a short abstract motivating and briefly summarizing the proposed change.


Motivation
----------
There are many examples of methods that could be more efficiently defined in a
subclass. Further, it is useful to be able to concisely define a typeclass
hierarchy even if you would want the ability to define different parts at different
times.

This proposal is also half of a pair I am presenting. The extension presented here
works better when considered with `Shared Class Methods <https://github.com/ghc-proposals/ghc-proposals/pull/707>`_. This proposal is loosely based off of `intrinsic typeclasses <https://gitlab.haskell.org/ghc/ghc/-/wikis/intrinsic-superclasses>`_.

.. Give a strong reason for why the community needs this change. Describe the use
.. case as clearly as possible and give an example. Explain how the status quo is
.. insufficient or not ideal.

.. A good Motivation section is often driven by examples and real-world scenarios.


Proposed Change Specification
-----------------------------
In a typeclass definition of ``C`` with superclass ``S`` (which has its own
superclass ``SS``), methods of ``S`` can have defaults declared in ``C``'s
definition. These implementations will only be the end implementation of the method
if there is not an explicit implementation of that method elsewhere. ``C``'s
definition can also include an implementation of ``SS``'s methods, and if there
is not an explicit definition of ``SS``'s methods then these ``C``'s implementations
will override any default that ``S`` or ``SS`` provides.

Subclasses will be able to provide only partial default implementations of
superclasses. This will use the mechanisms outlined in `Shared Class Methods <https://github.com/ghc-proposals/ghc-proposals/pull/707>`_
to have split instance definitions.

If a method name is shared between superclasses, then a default cannot be
declared for a method of that name. If the current typeclass and a superclass
share a method name, then the current typeclass's method will be the only one
that can be referred to.

General example
^^^^^^^^^^^^^^^

With typeclasses like this:
::
  class SS t where
    ss1 :: t -> Int

  class SS t => SA t where
    sa1 :: t -> Int
    sa2 :: t -> Int

    ss1 = sa1

  class SA t => C t where
    c1 :: t -> Int

    sa1 = c1
    sa2 = c1
    ss1 = sa2

You could define instances ``C``, ``SS``, and ``SA`` with a type ``data D = D``
like the following:
::
  instance C D where
    c1 = const 0

Which would result in the following equivalent instance definitions:
::
  instance C D where
    c1 = const 0

  instance SA D where
    sa1 = c1
    sa2 = c1

  instance SS D where
    ss1 = sa2

Note that ``ss1`` is defined as per ``C``'s default implementation instead of
``SA``'s.

Interactions with existing extensions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``MultiParamTypeClasses``
"""""""""""""""""""""""""

If the superclass is defined on exactly one of the parameters, then the superclass's
members can be defined for that parameter. Otherwise we fail out.

``UndecidableSuperClasses``
"""""""""""""""""""""""""""

If a typeclass is recursive, you will not be able to define members for a parent
typeclass because the names will conflict, so in this case you'd get an error
saying that the same member has been declared multiple times.

If a superclass has different members, then you'll be able to declare that
superclass's members; if that superclass has the current typeclass as a parent, you
won't be able to declare the superclass's parent-typeclass's members in the current
instance.

``DefaultSignatures``
"""""""""""""""""""""

You should be able to provide a default signature for a superclass's method in
a subclass.

``FlexibleInstances``
"""""""""""""""""""""

The type that is derived on shouldn't matter as long as it's the same for all
members of the typeclass.

``UndecidableInstances``
""""""""""""""""""""""""

Additional constraints on instances do not add additional superclasses.

Other extensions
""""""""""""""""

- ``ConstrainedClassMethods``
  - only changes the type signature of a method, not its implementation
- ``FunctionalDependencies``
  - shouldn't affect method implementations
- ``TypeSynonymInstances``
  - expand the type as expected
- ``NullaryTypeClasses``, ``OverlappingInstances``, ``IncoherentInstances``
  - extensions deprecated

.. Specify the change in precise, comprehensive yet concise language. Avoid words
.. like "should" or "could". Strive for a complete definition. Your specification
.. may include,

.. * BNF grammar and semantics of any new syntactic constructs
..   (Use the `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/>`_ or GHC's ``alex``\- or ``happy``\-formatted files
..   for the `lexer <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser/Lexer.x>`_ or `parser <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y>`_
..   for a good starting point.)
.. * the types and semantics of any new library interfaces
.. * how the proposed change interacts with existing language or compiler
..   features, in case that is otherwise ambiguous

.. Think about how your proposed design accords with our `language design principles <../principles.rst#2Language-design-principles>`_,
.. and articulate that alignment explicitly wherever possible.

.. Strive for *precision*. The ideal specification is described as a
.. modification of the `Haskell 2010 report
.. <https://www.haskell.org/definition/haskell2010.pdf>`_. Where that is
.. not possible (e.g. because the specification relates to a feature that
.. is not in the Haskell 2010 report), try to adhere its style and level
.. of detail. Think about corner cases. Write down general rules and
.. invariants.

.. Note, however, that this section should focus on a precise
.. *specification*; it need not (and should not) devote space to
.. *implementation* details -- the "Implementation Plan" section can be used for that.

.. The specification can, and almost always should, be illustrated with
.. *examples* that illustrate corner cases. But it is not sufficient to
.. give a couple of examples and regard that as the specification! The
.. examples should illustrate and elucidate a clearly-articulated
.. specification that covers the general case.

Proposed Library Change Specification
-------------------------------------
As part of this proposal we can and should define superclass derivation clauses
for typeclasses which have lawful representations. Here is an example using the
``Functor``-``Applicative``-``Monad`` hierarchy (without comments):
::
  class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    (<$) = fmap . const

  class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const

    fmap = liftA

  class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

    (<*>) = ap
    liftA2 = liftM2
    (*>) a b = a >>= \_ -> b
    (<*) a b = do
      res <- a
      _ <- b
      pure res

    fmap = liftM

.. Specify the changes to libraries in the GHC repository, especially ``base`` and
.. others under the purview of the
.. `Core Libraries Committee <https://github.com/haskell/core-libraries-committee>`_.

.. Generally speaking, if your proposal adds new function or data types, the place
.. to do so is in the ``ghc-experimental`` package, whose API is under the control of
.. the GHC Steering Committee.
.. After your proposal is implemented, stable, and widely used, you (or anyone
.. else) can subsequently propose to move those types into ``base`` via a CLC
.. proposal.

.. Sometimes, however, your proposal necessarily changes something in ``base``,
.. whose API is curated by the CLC.
.. In that case, assuming your proposal is accepted, at the point when it is
.. implemented (by you or anyone else), CLC approval will be needed for these
.. changes, via a CLC proposal made by the implementor.
.. By signalling those changes now, at the proposal stage, the CLC will be alerted
.. and have an opportunity to offer feedback, and agreement in principle.

.. See `GHC base libraries <https://github.com/Ericson2314/tech-proposals/blob/ghc-base-libraries/proposals/accepted/051-ghc-base-libraries.rst?rgh-link-date=2023-07-09T17%3A01%3A15Z>`_
.. for some useful context.

.. Therefore, in this section:

.. * If your proposal makes any changes to the API of ``base`` (including its
..   exports, types, semantics, and performance), please specify these changes
..   in this section.

.. * If your proposal makes any change to the API of ``ghc-experimental``, please
..   also specify these changes.

.. If you propose to change both, use subsections, so that the changes are clearly
.. distinguished.
.. Similarly, if any other libraries are affected, please lay it all out here.

Examples
--------
Using the ``Monad`` example in `Proposed Library Change Specification <#Proposed Library Change Specification>`_
we could define a new ``Monad`` ``M`` with merely the following (if we also use
the aforementioned shared class methods proposal):
::
  data Id a = MkId a

  instance Monad Id where
    pure = MkId
    (>>=) (MkId a) f = f a

This is a complete implementation for ``Functor``, ``Applicative``, and ``Monad``,
using only these two extensions.

Other features of this is that this avoids having inefficient defaults for some
methods; for example, the default implementation of ``(*>)`` has been found to
have performance issues in certain contexts, but an implementation like
``(*>) a b = a >>= \_ -> b`` does not demonstrate such issues. Implementing this
proposal and these default methods eliminates an entire swathe of inefficiencies.

.. This section illustrates the specification through the use of examples of the
.. language change proposed. It is best to exemplify each point made in the
.. specification, though perhaps one example can cover several points. Contrived
.. examples are OK here. If the Motivation section describes something that is
.. hard to do without this proposal, this is a good place to show how easy that
.. thing is to do with the proposal.

Effect and Interactions
-----------------------
Being able to define defaults for superclasses in subclasses fulfils the
motivation's desire for overriding superclass defaults and concise instance
definitions.

.. Your proposed change addresses the issues raised in the motivation. Explain how.

.. Also, discuss possibly contentious interactions with existing language or compiler
.. features. Complete this section with potential interactions raised
.. during the PR discussion.


Costs and Drawbacks
-------------------
This extension can make it hard to work out how a method is defined for a given
class, especially when there is no obvious location that a class is defined. This
can be mitigated using tools that allow you to go to the definitions of methods,
and proper documentation around subclasses and their derived superclasses.

.. Give an estimate on development and maintenance costs. List how this affects
.. learnability of the language for novice users. Define and list any remaining
.. drawbacks that cannot be resolved.


Backward Compatibility
----------------------
By itself this extension does not break any existing code, but care should be
taken when newly deriving superclass methods in existing typeclasses and
hierarchies.

.. How well does your proposal meet the stability principles described in our
.. `GHC stability principles <../principles.rst#3GHC-stability-principles>`_ document?

.. Will your proposed change cause any existing programs to change behaviour or
.. stop working? Assess the expected impact on existing code on the following scale:

.. 1. No breakage
.. 2. Breakage only in extremely rare cases (e.g. for specifically-constructed
..    examples, but probably no packages published in the Hackage package repository)
.. 3. Breakage in rare cases (e.g. a few Hackage packages may break, but probably
..    no packages included in recent Stackage package sets)
.. 4. Breakage in uncommon cases (e.g. a few Stackage packages may break)
.. 5. Breakage in common cases

.. (For the purposes of this assessment, GHC emitting new warnings is not
.. considered to be a breaking change, i.e. packages are assumed not to use
.. ``-Werror``.  Changing a warning into an error is considered a breaking change.)

.. Explain why the benefits of the change outweigh the costs of breakage.
.. Describe the migration path. Consider specifying a compatibility warning for one
.. or more compiler releases before the change is fully implemented. Give examples
.. of error messages that will be reported for previously-working code; do they
.. make it easy for users to understand what needs to change and why?

.. When the proposal is implemented, the implementers and/or GHC maintainers should
.. test that the actual backwards compatibility impact of the implementation is no
.. greater than the expected impact. If not, the proposal should be revised and the
.. steering committee approve the change.


Alternatives
------------
Implement either `intrinsic superclasses <https://gitlab.haskell.org/ghc/ghc/-/wikis/intrinsic-superclasses>`_
or `superclass defaults <https://gitlab.haskell.org/ghc/ghc/-/wikis/default-superclass-instances>`_ 
faithfully.

.. List alternative designs to your proposed change. Both existing
.. workarounds, or alternative choices for the changes. Explain
.. the reasons for choosing the proposed change over these alternative:
.. *e.g.* they can be cheaper but insufficient, or better but too
.. expensive. Or something else.

.. The PR discussion often raises other potential designs, and they should be
.. added to this section. Similarly, if the proposed change
.. specification changes significantly, the old one should be listed in
.. this section.

Unresolved Questions
--------------------
None currently.
.. Explicitly list any remaining issues that remain in the conceptual design and
.. specification. Be upfront and trust that the community will help. Please do
.. not list *implementation* issues.

.. Hopefully this section will be empty by the time the proposal is brought to
.. the steering committee.


Implementation Plan
-------------------
No implementer has been selected yet.

.. (Optional) If accepted who will implement the change? Which other resources
.. and prerequisites are required for implementation?

Endorsements
-------------
None.

.. (Optional) This section provides an opportunity for any third parties to express their
.. support for the proposal, and to say why they would like to see it adopted.
.. It is not mandatory for have any endorsements at all, but the more substantial
.. the proposal is, the more desirable it is to offer evidence that there is
.. significant demand from the community.  This section is one way to provide
.. such evidence.
