Quantified Constraints
======================

.. author:: Ryan Scott
.. date-accepted:: 2018-04-23

.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/2893

.. implemented:: 8.6

.. highlight:: haskell

.. header::
   This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/109>`_.


.. contents::


Introduction
------------

This proposal introduces quantified constraints, which have been proposed years ago
in  `Derivable type classes <https://www.microsoft.com/en-us/research/publication/derivable-type-classes/>`_ (Section 7), to raise the expressive power of type classes to essentially first-order logic.
These quantified class constraints enable instance declarations that are currently
very difficult or even impossible to express in Haskell.

The classic motivating example is this ::

 data Rose f a = Branch a (f (Rose f a))

 instance (Eq a, ???) => Eq (Rose f a)
   where
     (Branch x1 c1) == (Branch x2 c2)
        = x1==x1 && c1==c2

From the ``x1==x2`` we need ``Eq a``, which is fine.  From ``c1==c2`` we need ``Eq (f (Rose f a))`` which
is *not* fine in Haskell today; we have no way to solve such a constraint.

This proposal offers a way to do so, by allowing **quantified constraints** to appear in
instances, and indeed in any type signature.  In this case ::

 instance (Eq a, forall b. (Eq b) => Eq (f b))
        => Eq (Rose f a)
   where
     (Branch x1 c1) == (Branch x2 c2)
        = x1==x1 && c1==c2

A quantified constraint is a bit like a local instance declaration.

The paper `Quantified class constraints <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_ (by Bottu, Karachalias, Schrijvers, Oliveira, Wadler, Haskell Symposium 2017) describes this feature in technical detail, with examples, and so is a primary reference source for this proposal.

There is a prototype implementation already available in GHC's repository, on branch `wip/T2893`.

Motivation
----------

Introducing quantified constraints offers two main benefits:

- Firstly, they enable terminating resolution where this was not possible before.  Consider for instance the following instance declaration for the general rose datatype ::

   data Rose f x = Rose x (f (Rose f x))

   instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
     (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2

  This extension allows to write constraints of the form ``forall b. Eq b => Eq (f b)``,
  which is needed to solve the ``Eq (f (Rose f x))`` constraint arising from the
  second usage of the ``(==)`` method.

- Secondly, quantified constraints allow for more concise and precise specifications. As an example, consider the MTL type class for monad transformers::

   class Trans t where
     lift :: Monad m => m a -> (t m) a

  The developer knows that a monad transformer takes a monad ``m`` into a new monad ``t m``.
  But this is property is not formally specified in the above declaration.
  This omission becomes an issue when defining monad transformer composition::

    newtype (t1 * t2) m a = C { runC :: t1 (t2 m) a }

    instance (Trans t1, Trans t2) => Trans (t1 * t2) where
      lift = C . lift . lift

  The goal here is to ``lift`` from monad ``m`` to ``t2 m`` and
  then ``lift`` this again into ``t1 (t2 m)``.
  However, this second ``lift`` can only be accepted when ``(t2 m)`` is a monad
  and there is no way of establishing that this fact universally holds.

  Quantified constraints enable this property to be made explicit in the ``Trans``
  class declaration::

    class (forall m. Monad m => Monad (t m)) => Trans t where
      lift :: Monad m => m a -> (t m) a

Here is a list of other sources that have sought quantified constraints:

- `GHC's ticket about quantified constraints <https://gitlab.haskell.org/ghc/ghc/issues/2893>`_, and `GHC's wiki page about quantified constraints <https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints>`_.  The ticket dates back nine years.  The wiki page has a list of open tickets about quantified constraints, many of them in response to this proposal and prototype implementation.
- `How to derive Generic for (some) GADTs using QuantifiedConstraints <https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/>`_, a blog post by Ryan Scott, Feb 2018.
- `Quantified class constraints <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_ is a Haskell 2017 paper that works out the idea in some detail, with examples.  Here is a `Reddit thread about it <https://www.reddit.com/r/haskell/comments/6me3sv/quantified_class_constraints_pdf/>`_.
- `Adding join to Monad <https://gitlab.haskell.org/ghc/ghc/issues/9123>`_: this ticket describes a real problem with GHC's role system, which currently prevents us adding ``join`` to ``Monad`` and still allowing ``deriving( Monad )``.  As `comment 29 <https://gitlab.haskell.org/ghc/ghc/issues/9123#note_92871>`_ shows, quantified constraints can solve this problem.   And `this blog post from Ryan Scott <https://ryanglscott.github.io/2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad/>`_ explains exactly how.
- `A blog post about higher-rank constraints <http://mainisusuallyafunction.blogspot.co.uk/2010/09/higher-rank-type-constraints.html>`_ -- slightly different terminology, but the same idea.
- `A genuine use-case <https://gitlab.haskell.org/ghc/ghc/issues/2893#note_123363>`_ taken from `How to twist pointers without breaking them <http://ozark.hendrix.edu/~yorgey/pub/twisted.pdf>`_.
- `A Hakell libraries mailing list thread <https://mail.haskell.org/pipermail/libraries/2017-December/028377.html>`_ about quantified constraints.
- A `Reddit thread <https://np.reddit.com/r/haskell/comments/7toutl/now_there_is_a_branch_to_play_with/>`_ about the quantified-constraints prototype.
- A bunch of other GHC tickets are listed on `the wiki page <https://gitlab.haskell.org/ghc/ghc/issues/2893>`_.

Proposed Change Specification
------------------------------
We propose to add a new GHC extension called ``{-# QuantifiedConstraints #-}``.
Currently, GHC allows only simple class constraints in class and instance contexts.
When this extension is enabled, constraints can contain type quantifiers and
implications in arbitrarily nested positions.

As an example, consider the declaration mentioned above, containing a quantified constraint::

 instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
   (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2

Extension name
~~~~~~~~~~~~~~~~

We propose the extension name ``QuantifiedConstraints``.

Syntax changes
~~~~~~~~~~~~~~~~

`Haskell 2010 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ defines a ``context`` (the bit to the left of ``=>`` in a type) like this ::

 context ::= class
         |   ( class1, ..., classn )

 class ::= qtycls tyvar
        |  qtycls (tyvar atype1 ... atypen)

We propose to extend ``class`` (warning: this is a rather confusingly named non-terminal symbol) with two extra forms, namely precisely what can appear in an instance declaration ::

 class ::= ...
       | context => qtycls inst
       | context => tyvar inst

The definition of ``inst`` is unchanged from the Haskell Report (roughly, just a type).
That is the only syntactic change to the language.

Notes:

- Where GHC allows extensions instance declarations we allow exactly the same extensions to this new form of ``class``.  Specifically, with ``ExplicitForAll`` and ``MultiParameterTypeClasses`` the syntax becomes ::

    class ::= ...
           | [forall tyavrs .] context => qtycls inst1 ... instn
           | [forall tyavrs .] context => tyvar inst1 ... instn

  Note that an explicit ``forall`` is often absolutely essential. Consider the rose-tree example ::

    instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where ...

  Without the ``forall b``, the type variable ``b`` would be quantified over the whole instance declaration, which is not what is intended.

- One of these new quantified constraints can appear anywhere that any other constraint can, not just in instance declarations.  Notably, it can appear in a type signature for a value binding, data constructor, or expression.  For example ::

   f :: (Eq a, forall b. Eq b => Eq (f b)) => Rose f a -> Rose f a -> Bool
   f t1 t2 = not (t1 == t2)

- The form with a type variable at the head allows this::

   instance (forall xx. c (Free c xx)) => Monad (Free c) where
       Free f >>= g = f g

  See `Iceland Jack's summary <https://gitlab.haskell.org/ghc/ghc/issues/14733#note_148352>`_.  The key point is that the bit to the right of the `=>` may be headed by a type *variable* (`c` in this case), rather than a class.  It should not be one of the forall'd variables, though.

  (NB: this goes beyond what is described in `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_, but does not seem to introduce any new technical difficulties.)


Typing changes
~~~~~~~~~~~~~~~~

See `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_.

Superclasses
~~~~~~~~~~~~~~~~

Suppose we have::

     f :: forall m. (forall a. Ord a => Ord (m a)) => m Int -> Bool
     f x = x == x

From the ``x==x`` we need an ``Eq (m Int)`` constraint, but the context only gives us a way to figure out ``Ord (m a)`` constraints.  But from the given constraint ``forall a. Ord a => Ord (m a)`` we derive a second given constraint ``forall a. Ord a => Eq (m a)``, and from that we can readily solve ``Eq (m Int)``.  This process is very similar to the way that superclasses already work: given an ``Ord a`` constraint we derive a second given ``Eq a`` constraint.

NB: This aspect of the proposal goes beyond `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_, but is specifically desired by users.

Overlap
~~~~~~~~~~~~~

Quantified constraints can potentially lead to overlapping local axioms.
Consider for instance the following example::

 class A a where {}
 class B a where {}
 class C a where {}
 class (A a => C a) => D a where {}
 class (B a => C a) => E a where {}

 class C a => F a where {}
 instance (B a, D a, E a) => F a where {}

When type checking the instance declaration for ``F a``,
we need to check that the superclass ``C`` of ``F`` holds.
We thus try to entail the constraint ``C a`` under the theory containing:

- The instance axioms : ``(B a, D a, E a) => F a``
- The local axioms from the instance context : ``B a``, ``D a`` and ``E a``
- The closure of the superclass relation over these local axioms : ``A a => C a`` and ``B a => C a``

However, the ``A a => C a`` and ``B a => C a`` axioms both match the wanted constraint ``C a``.
There are several possible approaches for handling these overlapping local axioms:

- **Pick first**.  We can simply select the **first matching axiom** we encounter.
  In the above example, this would be ``A a => C a``.
  We'd then need to entail ``A a``, for which we have no matching axioms available, causing the above program to be rejected.

  However, we can make a slight adjustment to the order of the instance context::

   class A a where {}
   class B a where {}
   class C a where {}
   class (A a => C a) => D a where {}
   class (B a => C a) => E a where {}

   class C a => F a where {}
   instance (B a, E a, D a) => F a where {}

  The first matching axiom we encounter while entailing ``C a``, is ``B a => C a``.
  We have a local axiom ``B a`` available, so now the program is suddenly accepted.

  This behaviour, where the ordering of an instance context determines
  whether or not the program is accepted, seems rather confusing for the developer.

- **Reject if in doubt**.  An alternative approach would be to check for overlapping axioms,
  when entailing a constraint.
  When multiple matching axioms are discovered, we **reject the program**.
  This approach might be a bit conservative, in that it may reject working programs.
  However, this does seem much more transparent towards the developer.
  He can be presented with a clear message, explaining why the program is rejected,
  so that he can make the necessary adjustments to his code.

- **Basic heuristic**.  Another option would be to check for overlapping axioms,
  but instead of rejecting the program,
  perform a **basic heuristic** to determine which of these axioms is more likely to succeed.
  This could result in more programs being accepted,
  compared to simply selecting the first matching axiom we find.
  However, this heuristic might add significant complexity to the compiler.
  Furthermore, when the heuristic does fail and the program is rejected,
  debugging this program would become very confusing indeed.

- **Backtracking**.  Lastly, a simple form of **backtracking** could be introduced.
  We simply select the first matching axiom we encounter and when the entailment fails,
  we backtrack and look for other axioms that might match the wanted constraint.

  This seems by far the most intuitive and transparent approach towards the developer,
  who no longer needs to concern himself with the fact that his code might contain
  overlapping axioms or with the ordering of his instance contexts.
  However, further investigation is needed to determine the impact of this on
  the compiler performance.

We propose to adopt **Reject if in doubt** for now.  We can see how painful it
is in practice, and try something more ambitious if necessary.

Instance lookup
~~~~~~~~~~~~~~~~~~~

In the light of the overlap decision, instance lookup works like this, when
trying to solve a class constraint ``C t``

1. First see if there is a given un-quantified constraint ``C t``.  If so, use it to solve the constraint.

2. If not, look at all the available given quantified constraints; if exactly one one matches ``C t``, choose it; if more than one matches, report an error.

3. If no quantified constraints match, look up in the global instances precisely as now.

Termination
~~~~~~~~~~~~~~~

GHC uses the `Paterson Conditions <http://downloads.haskell.org/~ghc/master/users-guide/glasgow_exts.html#instance-termination-rules>`_ to ensure that instance resolution terminates:

The Paterson Conditions are these:

- The Paterson Conditions: for each class constraint ``(C t1 ... tn)``
  in the context

   1. No type variable has more occurrences in the constraint than in
      the head

   2. The constraint has fewer constructors and variables (taken
      together and counting repetitions) than the head

   3. The constraint mentions no type functions. A type function
      application can in principle expand to a type of arbitrary size,
      and so are rejected out of hand

How are those rules modified for quantified constraints? In two ways.

- Each quantified constraint, taken by itself, must satisfy the termination rules for an instance declaration.

- After "for each class constraint ``(C t1 ... tn)``", add "or each quantified constraint ``(forall as. context => C t1 .. tn)``"

Note that the second item only at the *head* of the quantified constraint, not its context.  Reason: the head is the new goal that has to be solved if we use the instance declaration.

Of course, ``UndecidableInstances`` lifts the Paterson Conditions, as now.

Coherence
~~~~~~~~~~~


Although quantified constraints are a little like local instance declarations, they differ
in one big way: the local instances are written by the compiler, not the user, and hence
cannot introduce incoherence.  Consider ::

  f :: (forall a. Eq a => Eq (f a)) => f b -> f Bool
  f x = ...rhs...

In ``...rhs...`` there is, in effect a local instance for ``Eq (f a)`` for any ``a``.  But
at a call site for ``f`` the compiler itself produces evidence to pass to ``f``. For example,
if we called ``f Nothing``, then ``f`` is ``Maybe`` and the compiler must prove (at the
call site) that ``forall a. Eq a => Eq (Maybe a)`` holds.  It can do this easily, by
appealing to the existing instance declaration for ``Eq (Maybe a)``.

In short, quantifed constraints do not introduce incoherence.

Costs and Drawbacks
---------------------
There are currently no known drawbacks to this feature.


Alternatives
------------------
Several alternatives have already been considered.

GHC currently supports a form of cycle-aware resolution,
which enables writing the rose example mentioned above, without quantified constraints.
Unfortunately, this approach is not generally applicable since the
resolution process can diverge without cycling,
rendering the cycle-aware resolution useless in these scenarios.

Secondly, alternative encodings exist, such as the one presented in this paper:
`Simulating Quantified Class Constraints <https://dl.acm.org/citation.cfm?id=871906>`_
Unfortunately, they all render the code significantly longer, more complex
and none of these alternative encodings are generally applicable.

Implementation Plan
---------------------
The feature is fully implemented in branch ``wip/T2893`` of the GHC repository.


Additional Links
----------------
- `Quantified Constraints wiki <https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints>`_
- `Quantified Constraints ticket <https://gitlab.haskell.org/ghc/ghc/issues/2893>`_
