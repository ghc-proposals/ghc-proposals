.. proposal-number::

.. trac-ticket::

.. implemented::

.. highlight:: haskell

   This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/109>`_.

.. contents::

Quantified Constraints
======================

This proposal introduces quantified constraints, which have been proposed years ago
in  `Derivable type classes <https://www.microsoft.com/en-us/research/publication/derivable-type-classes/>`_ (Section 7), to raise the expressive power of type classes to essentially first-order logic.
These quantified class constraints enable instance declarations that are currently
very difficult or even impossible to express in Haskell.

The classic motivating example is this::

 data Rose f a = Branch a (f (Rose f a))

 instance (Eq a, ???) => Eq (Rose f a)
   where
     (Branch x1 c1) == (Branch x2 c2)
        = x1==x1 && c1==c2

From the ``x1==x2`` we need ``Eq a``, which is fine.  From ``c1==c2`` we need ``Eq (f (Rose f a))`` which
is *not* fine in Haskell today; we have no way to solve such a constraint.

This proposal offers a way to do so, by allowing **quantified constraints** to appear in
instances, and indeed in any type signature.  In this case::

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

- Firstly, they enable terminating resolution where this was not possible before.  Consider for instance the following instance declaration for the general rose datatype::

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

- `Quantified class constraints <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_ is a Haskell 2017 paper that works out the idea in some detail, with examples.  Here is a `Reddit thread about it `<https://www.reddit.com/r/haskell/comments/6me3sv/quantified_class_constraints_pdf/>`_.
- `Adding join to Monad <https://ghc.haskell.org/trac/ghc/ticket/9123>`_: this ticket describes a real problem with GHC's role system, which currently prevents us adding ``join`` to ``Monad`` and still allowing ``deriving( Monad )``.  As `comment 29 <https://ghc.haskell.org/trac/ghc/ticket/9123#comment:29>`_ shows, quantified constraints can solve this problem.
- `A blog post about higher-rank constraints <http://mainisusuallyafunction.blogspot.co.uk/2010/09/higher-rank-type-constraints.html>`_ -- slightly different terminology, but the same idea.
- `A genuine use-case <https://ghc.haskell.org/trac/ghc/ticket/2893#comment:17>`_ taken from `How to twist pointers without breaking them <http://ozark.hendrix.edu/~yorgey/pub/twisted.pdf>`_.
- `A Hakell libraries mailing list thread <https://mail.haskell.org/pipermail/libraries/2017-December/028377.html>`_ about quantified constraints.
- `GHC's ticket about quantified constraints <https://ghc.haskell.org/trac/ghc/ticket/2893>`_, and `GHC's wiki page about quantified constraints <https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints>`_.  (The ticket dates back nine years!)
- A `Reddit thread <https://np.reddit.com/r/haskell/comments/7toutl/now_there_is_a_branch_to_play_with/>`_ about the quantified-constraints prototype.
- A bunch of other GHC tickets are listed on `the wiki page <https://ghc.haskell.org/trac/ghc/ticket/2893>`_.

Proposed Change Specification
-----------------------------
We propose to add a new GHC extension called ``{-# QuantifiedConstraints #-}``.
Currently, GHC allows only simple class constraints in class and instance contexts.
When this extension is enabled, constraints can contain type quantifiers and
implications in arbitrarily nested positions.

As an example, consider the declaration mentioned above, containing a quantified constraint::

 instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
   (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2

Extension name
^^^^^^^^^^^^^^

We propose the extension name ``QuantifiedConstraints``.

Syntax changes
^^^^^^^^^^^^^^

`Haskell 2010 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ defines a ``context`` (the bit to the left of ``=>`` in a type) like this::
  
 context ::= class
         |   ( class1, ..., classn )

 class ::= qtycls tyvar
        |  qtycls (tyvar atype1 ... atypen)

We propose to extend ``class`` with an extra form, namely precisely what can appear in an instance declaration::
  
 class ::= ...
       | context => qtycls inst

The definition of ``inst`` is unchanged from the Haskell Report.
Where GHC allows extensions to istancce declarations (explicit foralls, multi-prarameter type classes) wea llow exactly the same extensions to this new form of ``class``.

That is the only syntactic change to the language.

Typing changes
^^^^^^^^^^^^^^

See `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_.

Overlap
^^^^^^^

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

- We can simply select the **first matching axiom** we encounter.
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

- An alternative approach would be to check for overlapping axioms,
  when entailing a constraint.
  When multiple matching axioms are discovered, we **reject the program**.
  This approach might be a bit conservative, in that it may reject working programs.
  However, this does seem much more transparant towards the developer.
  He can be presented with a clear message, explaining why the program is rejected,
  so that he can make the necessary adjustments to his code.

- Another option would be to check for overlapping axioms,
  but instead of rejecting the program,
  perform a **basic heuristic** to determine which of these axioms is more likely to succeed. 
  This could result in more programs being accepted,
  compared to simply selecting the first matching axiom we find.
  However, this heuristic might add significant complexity to the compiler.
  Furthermore, when the heuristic does fail and the program is rejected,
  debugging this program would become very confusing indeed.

- Lastly, a simple form of **backtracking** could be introduced.
  We simply select the first matching axiom we encounter and when the entailment fails,
  we backtrack and look for other axioms that might match the wanted constraint.

  This seems by far the most intuitive and transparent approach towards the developer,
  who no longer needs to concern himself with the fact that his code might contain
  overlapping axioms or with the ordering of his instance contexts. 
  However, further investigation is needed to determine the impact of this on
  the compiler performance.

Issues
^^^^^^

Quite a few interesting questions have arisen already from the prototype.  Here we list the main ones. Please identify any others.

- We'd like to allow this::

   instance (forall xx. c (Free c xx)) => Monad (Free c) where
       Free f >>= g = f g

  See `Iceland Jack's summary <https://ghc.haskell.org/trac/ghc/ticket/14733#comment:6>`_.

  The key point is that the bit to the right of the `=>` may be headed by a type *variable*, rather than a class.  It should not be one of the forall'd variables, though.

- Suppose we have::

     f :: forall m. (forall a. Ord a => Ord (m a)) => m Int -> Bool
     f x = x == x

  From the ``x==x`` we need an ``Eq (m Int)`` constraint, but the context only gives us a way to figure out ``Ord (m a)`` constraints.  If we sought ``Ord (m Int)`` we'd succeed, and could then extract an ``Eq (m Int)`` dictionary from the ``Ord`` one.  But it's not clear how to make this work in general without introducing a pile of new complexity, even including backtracking.

Costs and Drawbacks
-------------------
There are currently no known drawbacks to this feature.


Alternatives
------------
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


Unresolved questions
--------------------


Implementation Plan
-------------------
`Phabricator <https://phabricator.haskell.org/D4353>`_


Additional Links
----------------
- `Quantified Constraints wiki <https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints>`_
- `Quantified Constraints ticket <https://ghc.haskell.org/trac/ghc/ticket/2893>`_
