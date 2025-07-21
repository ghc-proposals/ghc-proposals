.. Notes on reStructuredText - delete this section before submitting
.. ==================================================================

.. The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

.. ::

..  like this
..  and

..  this too

.. To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Shared Class Members
====================

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

.. Here you should write a short abstract motivating and briefly summarizing the proposed change.

Extending or splitting typeclasses in Haskell is fraught with necessary changing
of boilerplate. Further, writing hierarchies of typeclasses in the first place
can require a lot of unneeded boilerplate. This proposal would introduce a new
extension (which should eventually be turned on in a future edition) that allows
members from superclasses to be defined in their subclasses, resulting in less
breakage from splitting typeclasses and also less boilerplate when writing classes.


Motivation
----------
Defining typeclass hierarchies can require a lot of repeated boilerplate, and if
those hierarchies are changed in future instances and implementations can break,
even if the changes are well intentioned or choreographed well in advance.

This proposal is also half of a pair I am presenting. The extension presented here
would allow more versatility in typeclass instances, which may be necessary for
my eventual proposal for `intrinsic typeclasses <https://gitlab.haskell.org/ghc/ghc/-/wikis/intrinsic-superclasses>`_
to work well in all cases.

.. This proposal is best examined via its `Examples <#Examples>`_.

.. The core issues to be solved are forwards compatibility with changes to typeclass
.. hierarchies as well as more flexible class declarations. This proposal would allow
.. reduction in boilerplate as well as further fearlessness in changes to the core
.. language.

.. Give a strong reason for why the community needs this change. Describe the use
.. case as clearly as possible and give an example. Explain how the status quo is
.. insufficient or not ideal.

.. A good Motivation section is often driven by examples and real-world scenarios.


Proposed Change Specification
-----------------------------

We add an extension ``SharedClassMembers`` which enables the following behaviour.
By default this extension should be enabled, or brought into a future language
edition, so that the full utility can be realised. The extension stands
on it own without that in consideration.

As with `intrinsic typeclasses <https://gitlab.haskell.org/ghc/ghc/-/wikis/intrinsic-superclasses>`_
we will use the terminology "members" instead of "methods" so we can talk about
associated types and associated data as well.

When an instance for a typeclass ``C`` is declared for type ``D``, with superclass
``SA``, the members of ``SA`` can be defined in the instance declaration for ``C``
as long as there are no other definitions for those members for type ``D``. This
applies even for superclasses of superclasses, but only for superclasses of the
typeclass itself, not additional typeclass constraints of the instance. All 
members for the superclass need to be defined in the same module (and likely the
same stage).

Additionally, the members of ``S`` can be declared in multiple different instance
blocks, whether that is child typeclasses or blocks of ``S``. All members of the
typeclass must have the same constraints on them, and be defined for exactly the
same type. Each member of that typeclass must also be declared exactly once.
There should at most one instance declaration of the superclass.

Within an instance declaration, a given named member can only be named once. If
a superclass shares member names with the current typeclass, you cannot declare
the superclass's shared member names in the same instance declaration. The current
typeclass's members will always take priority.

General example
^^^^^^^^^^^^^^^

With typeclasses like this:
::
  class SS t where
    ss1 :: t -> Int

  class SS t => SA t where
    sa1 :: t -> Int
    sa2 :: t -> Int

  class AC t where
    ac1 :: t -> Int

  class SA t => C t where
    c1 :: t -> Int

You could define an instance for ``C`` with a type ``data D = D`` like the
following:
::
  instance AC D => C D where
    c1 = ac1
    ss1 = c1
    sa1 = c1
    sa2 = c1

  instance AC D where
    ac1 = const 0

Note that ``AC`` cannot be defined for ``D`` in the same declaration as ``C D`` because
``AC`` is not a superclass of ``C``.

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

This proposal only affects instance implementations, not typeclass definitions.
Superclass methods will not be "defaultable" from a child typeclass's definition.

``FlexibleInstances``
"""""""""""""""""""""

The type that the superclass members are declared on must be the same in all cases,
so additional type options don't present issues.

``UndecidableInstances``
""""""""""""""""""""""""

Additional constraints on instances do not add additional superclasses which can
have members defined for them.

Other extensions
""""""""""""""""

- ``ConstrainedClassMethods``
  - defined methods have the same restrictions no matter where defined
- ``FunctionalDependencies``
  - defined on class definition not instance definition
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

No changes to existing libraries.

Examples
--------
.. This section illustrates the specification through the use of examples of the
.. language change proposed. It is best to exemplify each point made in the
.. specification, though perhaps one example can cover several points. Contrived
.. examples are OK here. If the Motivation section describes something that is
.. hard to do without this proposal, this is a good place to show how easy that
.. thing is to do with the proposal.

There are two main motivating examples, one that demonstrates future application
and another that can be realised now.

Future
^^^^^^
We have the existing typeclass ``Alternative``, defined as follows for ``Maybe``:
::
  class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some :: f a -> f [a]
    many :: f a -> f [a]

  instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

If we were to split ``Alternative`` into a non-empty ``Alt`` (as proposed
`here <https://github.com/haskell/core-libraries-committee/issues/272>`_), we
could change ``Alternative`` to the following:
::
  class Applicative f => Alt f where
    (<|>) :: f a -> f a -> f a

    some :: f a -> f [a]
    many :: f a -> f [a]

  class Alt f => Alternative f where
    empty :: f a

  instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

Note that the superclass's method ``<|>`` was defined in a subclass, meaning that
despite there being a change in how the typeclasses were defined, the implementations
can be defined as expected. This lets us be greatly forward compatible with our
typeclasses and instances.

Note that I am not suggesting that the above is a change we wish to do, just that
it's an example where the current proposal would be useful in reducing breakage.

Present
^^^^^^^

We can reduce on the amount of boilerplate needed to define different typeclasses.

Here is a simple example before and after for some arbitrary ``Monad`` transformer
``MT``, for which we have ``pureM :: Monad m => a -> MT m a`` and
``bindM :: Monad m => MT m a -> (a -> MT m b) -> MT m b`` predefined.

Before:
::
  instance Monad m => Functor (MT m) where
    fmap = liftM

  instance Monad m => Applicative (MT m) where
    pure = pureM
    (<*>) = ap

  instance Monad m => Monad (MT m) where
    (>>=) = bindM

And after:
::
  instance Monad m => Monad (MT m) where
    fmap = liftM
    pure = pureM
    (<*>) = ap
    (>>=) = bindM

This style can greatly reduce code-reading overhead, because instead of three
different, possibly disparate instance definitions, there is one that contains
all the members for the parent typeclasses.

Effect and Interactions
-----------------------
Reducing on boilerplate of typeclass definitions is an obvious outcome of this
proposal.

The forwards-compatibility feature can be realised only if this extension is
enabled by default when typeclass splitting occurs. This extension won't be able
to make compiling code fail, but can allow code broken by a dependency change
to now compile.

.. Your proposed change addresses the issues raised in the motivation. Explain how.

.. Also, discuss possibly contentious interactions with existing language or compiler
.. features. Complete this section with potential interactions raised
.. during the PR discussion.

Costs and Drawbacks
-------------------
This extension can complicate instance definitions, and may make it unclear where
a member originates from; in the above example with the ``Monad`` hierarchy,
``fmap`` could be a member of ``Monad``, ``Applicative`` or ``Functor``, which
could be confusing to a novice.

Further, allowing users to define different members of a typeclass scattered
across a module seems like it could result in bad practices, but I find it
unlikely that developers would choose to do this.

.. Give an estimate on development and maintenance costs. List how this affects
.. learnability of the language for novice users. Define and list any remaining
.. drawbacks that cannot be resolved.


Backward Compatibility
----------------------
This has no breaking changes as it is a new feature.


Alternatives
------------
We could choose not to implement this change, and accept that changing typeclass
hierarchies should be a breaking change, and that the boilerplate necessary for
writing instances is necessary or useful.

There is design space for this proposal to implement a pragma on the typeclass or
members to allow them to be defined in subclasses, or for the extension to be
enabled where the typeclass definition is and not the instance definition. I 
would recommend against this so that the only places where there will be changes
to operation are instance definitions; this is only meant to be changing instance
definitions, after all. It should not be up to typeclass authors to decide how
end users define instances for their own types. Further, it feels odd to me to
annotate members of a typeclass and have to propagate that out to all instance
definitions.

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
