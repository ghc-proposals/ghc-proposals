.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Instance (Apartness) Guards
===========================

This proposal tackles the thorny topic of Overlapping Instances, for both type classes and Type Families. **The basic idea** is to annotate Instance headers with type-level apartness Guards (similar to term-level disequality guards), such that Instances do not overlap (after taking their guards into account). The guards stipulate the 'apartness' (dis-equality) needed between type vars in the instance head. Then:

* the human reader can understand each instance stand-alone, without needing to consider other potentially overlapping instances;
* the compiler can validate each instance incrementally, without surveying the whole-of-program-with-imports set of instances (which GHC does not try to do);
* the compiler in selecting instances for a usage site, can consider each candidate instance separately, no trying to pick the most specific from those in scope per module: if the instance applies after taking guards into account, it is sure to be the only eligible instance; so
* differing imported instances in different modules cannot lead to incoherent selections: within a scope, there must be no overlapping instances (after taking guards into account), so none of the methods/functions in imported modules can have pre-selected the 'wrong' instance;
* the check for "Instances inconsistent with Functional Dependencies" can apply more accurately; so
* Undecidable Instances (and the consequent possible incoherence) can be avoided.

This needs no new semantics over current Haskell. (In fact, in many ways it's merely semantic sugar for idioms used in the current HList library.) The semantics is given below by translating to Haskell 2010 + MPTCs, avoiding overlaps, but assuming a type-level type equality test.


Motivation
------------

The perils from Overlapping Instances are well-known, particularly for instances from imported modules. The remedies are also well-known and embodied in 'folk' techniques. Overlapping is a clumsy tool: to work smoothly, it needs instances ordered in a strict substitution relationship; or a great deal of type-level programming, difficult to follow through instance contexts calling auxiliary classes.

Currently: Overlapping class instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To take the standard example (adapted from the `User Guide section on Overlapping Instances <http://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#overlapping-instances>`_)::

  module A where
    class C a b where f :: a -> b -> String
    instance {-# OVERLAPPING #-} C Int b where f _ _ = "C Int b"

    x = f (5 :: Int) True

  module B where
    import A
    instance {-# OVERLAPPABLE #-} C a Bool where f _ _ = "C a Bool"

    y = f False True
	   z = f (5 :: Int) True

Module A compiles happily with ``x == "C Int b"``. Module B compiles up to and including the equation for ``y`` with ``y == "C a Bool"``, because ``f False True`` only matches the ``instance C a Bool``. 

Note that GHC accepts both instance declarations although they overlap. (Furthermore the pragmas have no effect whatsoever. The User Guide says "These potentially overlap, but GHC will not complain about the instance declarations themselves, regardless of flag settings.")

But the equation for ``z``, despite being identical to ``x``, is not accepted: ``Overlapping instances for C Int Bool arising from a use of ‘f’``. You might hope with those specific pragmas, GHC would favour ``C Int b``, but no. 

This does compile with INCOHERENTINSTANCES, which is liable to pick a 'random' instance; but there is currently no way for the programmer to express directly: in case of wanted ``C Int Bool``, select ``instance C Int b``. Furthermore if there are other overlapping instances declared (possibly through diamond imports) a different instance might be picked at random.

With Instance Guards:
* the programmer can express directly the behaviour in case of overlapping instance heads;
* the compiler can verify purely by looking at the instance definitions one-by-one that they are together well-behaved -- that is, each apart; so
* the programmer can be confident that subsequent imports will not introduce incoherence.

In this case, ``module A`` with the class declaration and method calls baked in and relying on ``instance C Int b`` must not be overlapped. To favour ``C Int b`` in the equation for ``z``, consistent with ``x``::

  module A where
    class {-# INSTANCEGUARDS #-} C a b where ... -- body as before
    instance C Int b  where ...                           -- body as before

  module B where                                    -- note no explicit flag settings
    import A

    instance C a Bool | a /~ Int where ...      -- body as before

The guard ``a /~ Int`` says: to pick this instance, ``a`` must not be ``Int``. IOW, although the two instance heads overlap, the guard 'de-overlaps' them/forces them apart.

This proposal is written to expect INSTANCEGUARDS are flagged per-class. Flagging them whole-module is liable to interact poorly with existing code using Overlapping Instances (module flag or pragmas).

With INSTANCEGUARDS flagged on class ``C``, the original ``instance C a Bool`` (without a guard) would be rejected as overlapping.

Currently: Overlapping Type Family instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider these two instances (from the `User Guide section on Apartness of Type Family equations <http://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#compatibility-and-apartness-of-type-family-equations>`_)::

  type family G a
  type instance G (a, Int)  = [a]
  type instance G (Char, b) = [b]  -- ILLEGAL overlap, as [Char] /= [Int]

There is no way to say with separate instances: in the overlapping case, use the first instance. With Instance Guards that’s expressed as::

  type family {-# INSTANCEGUARDS #-} G a
  type instance G (a, Int)  = [a]
  type instance G (Char, b) | b /~ Int = [b]

The guard ``| b /~ Int`` says: to select this instance, ``b`` must not be ``Int``. The reader and the compiler can now see these are apart.

And we could freely add other instances for ``G``, for other type constructors. (That preference for the first instance could be expressed through Closed Type Families, but contrast that we couldn't freely add other instances stand-alone, they must be included with the closed equations.)

Or perhaps the programmer intends ``G`` should never be called on ``(Char, Int)``, then::

  type family {-# INSTANCEGUARDS #-} G a
  type instance G (a, Int) | a /~ Char  = [a]
  type instance G (Char, b) | b /~ Int = [b]

The Closed Type Family equivalent would be::

  type family G a  where
    G (Char, Int)  = TypeError "(Char, Int) not supported"
    G (a,   Int)   = [a]
    G (Char, b) = [b]

(The reader must be careful to scan the sequence of equations. ``G (Char, b) = [b]`` does not mean what it would in a stand-alone instance. And again we couldn't freely add other equations.)

Currently: Functional Dependency coherence
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider this set of Instances under a Functional Dependency::

  class D a b | a -> b where ...
  instance D (Int, Bool) Char where ...
  instance D (Int, a'')   a'' where ...
  instance D (a',   a'')   a'  where ...

These heads overlap on the 'argument side' of the FunDep. They're well-behaved, in the sense: there is a strict substitution ordering. 

But GHC complains "Instances are inconsistent with Functional Dependencies", because it can find a substitution that unifies the 'argument side' of the FunDep, and under that substitution, the 'result sides' of the FunDep are not equal. We want to say: but those 'argument sides' can never unify; in case we have ``{ a ~ (Int, Bool)}`` then always pick the first instance, never the second or third.

We can tackle this with Overlapping Instances or Closed Type Families. (And the Instance Guards in this proposal can therefore be translated into today's Haskell.)

Currently: Overlapping Instances: Undecidable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can get the class to compile like this::

  class D a b | a -> b where ...
  instance D (Int, Bool) Char where ...
  instance {-# OVERLAPPING #-}  (b ~ a'') => D (Int, a'')   b where ...
  instance {-# OVERLAPPING #-}  (b ~ a') => D (a',   a'')   b  where ...

The first observation is that this is harder to read/understand: we must scan from instance head to constraints to understand what's going on. And in more realistic examples (such as within HList), there are stacked-up constraints, one calculating a result to plug into the next.

We've essentially 'fooled' the Instance consistency check: under substitution ``{ a ~ (Int, Bool)}``, the 'result sides' are not equal but they do unify ``{ b ~ Char }``. This is despite the instance constraint ``(b ~ a')``.

But we verge on incoherence: the bare ``b`` on the 'result side' escapes the Coverage Conditions, so we need ``UndecidableInstances``.

Currently: Closed Type Families: Closed, not Associated
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can get better coherence using Closed Type Families to simulate the Functional Dependency::

  class (F a ~ b) => D a b where ...

  type family F a where
    F (Int, Bool) = Char
    F (Int, a'')   = a''
    F (a', a'')    = a'

  instance D (Int, Bool) Char where ...
  instance (F (Int, a'') ~ b) => D (Int, a'') a'' where ...
  instance (F (a', a'')   ~ a) => D (a', a'')   a' where ...

First, again observe the difficulty of reading this: the type family equations are distant from the class instance. We'd ideally perhaps write those equations as Associated types within the instance (but can't, because they overlap so must be in a closed grouping). Note also the need to repeat the SuperClass constraint as an Instance constraint.

There's a further limitation on expressivity: the Closed Type Family is, um, *Closed*. I can add a *class* instance (perhaps in a different module)::

  instance D (Maybe a') a' where ...

But to insert an extra Type Family equation needs editting the original type family's closed equations, **even though there's no possible overlap** between the ``Maybe`` constructor vs the ``(,)``.

Proposal: Instance (Apartness) Guards for Type Classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's explicitly de-overlap these instances. For the FunDep version, that becomes::

  class {-# INSTANCEGUARDS #-} D a b | a -> b where ...
  instance D (Int, Bool) Char where ...                 -- most specific instance as before

  instance D (Int, a'')   a'' | a'' /~ Bool  where ...      -- the /~ says: a'' must be apart from Bool

  instance D (a', a'')     a' | a' /~ Int  where ...           -- a' must be apart from Int

Observe: these instances are direct, we can understand each instance (with guards) stand-alone and with no constraints to obscure the result type. The 'result sides' of the FunDep use type vars from the 'argument side', no Undecidable Instances.

These instances do not overlap (after taking guards into account) because:

* Under a unifying substitution of the instance head, say ``{ a ~ (Int, Bool), a' ~ Int, a'' ~ Bool}``,
* the compiler is to substitute into the guards, obtaining ``Bool /~ Bool`` for the second instance or ``Int /~ Int`` for the third instance.
* Those are contradictions. IOW any usage site (wanted equation) which unifies with one of the instances *ipso facto* will **not** unify with any other instance (after taking guards into account).

These instances are consistent with FunDeps, because:

* under a unifying substitution of the 'argument side' of the FunDep ``{ a ~ (Int, Bool), a'' ~ Bool}``
* the compiler is first to substitute into the guards, obtaining ``Bool /~ Bool`` (for the second instance).
* That's a contradiction, so no need to check the consistency of the 'result side'.

Proposal: Instance (Apartness) Guards for Type Families
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The apartness guards are also to apply for Type Families, meaning we can usually code the equations as Associated types (of course we retain the rule that Associated types are merely sugar for top-level Type Families)::

  class {-# INSTANCEGUARDS #-} (F a ~ b) => D a b where
    type F a
     ...

  instance D (Int, Bool) Char where
    type F (Int, Bool) = Char
     ...
  instance D (Int, a'') a'' | a'' /~ Bool where
    type F (Int, a'') | a'' /~ Bool = a''
    ...
  instance D (a', a'')   a' | a' /~ Int where
    type F (a', a'') | a' /~ Int = a'
     ...

Note that there's no need to repeat the Equality constraint on each instance, because it's substantiated by the Associated type equation.

(Those Associated type instances are a little cluttered with the guards. A nice-to-have would be to automatically copy them from the class instance.)



Proposed Change Specification
-----------------------------
Specify the change in precise, comprehensive yet concise language. Avoid words like should or could. Strive for a complete definition. Your specification may include,

* grammar and semantics of any new syntactic constructs
* the types and semantics of any new library interfaces
* how the proposed change addresses the original problem
* how the proposed change might interact with existing language or compiler features

Note, however, that this section need not describe details of the
implementation of the feature. The proposal is merely supposed to give a
conceptual specification of the new feature and its behavior.




Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the motivation. Detail how the proposed change interacts with existing language or compiler features and provide arguments why this is not going to pose problems.



Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.



Alternatives
------------
List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.



Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.



Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
