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
  type instance G (a, Int)             = [a]
  type instance G (Char, b) | b /~ Int = [b]

The guard ``| b /~ Int`` says: to select this instance, ``b`` must not be ``Int``. The reader and the compiler can now see these are apart.

And we could freely add other instances for ``G``, for other type constructors. (That preference for the first instance could be expressed through Closed Type Families, but contrast that we couldn't freely add other instances stand-alone, they must be included with the closed equations.)

Or perhaps the programmer intends ``G`` should never be called on ``(Char, Int)``, then::

  type family {-# INSTANCEGUARDS #-} G a
  type instance G (a,    Int) | a /~ Char  = [a]
  type instance G (Char, b)   | b /~ Int   = [b]

The Closed Type Family equivalent would be::

  type family G a  where
    G (Char, Int)  = TypeError "(Char, Int) not supported"
    G (a,    Int)  = [a]
    G (Char, b)    = [b]

(The reader must be careful to scan the sequence of equations. ``G (Char, b) = [b]`` does not mean what it would in a stand-alone instance. And again we couldn't freely add other equations.)

Currently: Functional Dependency coherence
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider this set of Instances under a Functional Dependency::

  class D a b | a -> b where ...
  instance D (Int, Bool)  Char where ...
  instance D (Int, a'')   a''  where ...
  instance D (a',  a'')   a'   where ...

These heads overlap on the 'argument side' of the FunDep. They're well-behaved, in the sense: there is a strict substitution ordering. 

But GHC complains "Instances are inconsistent with Functional Dependencies", because it can find a substitution that unifies the 'argument side' of the FunDep, and under that substitution, the 'result sides' of the FunDep are not equal. We want to say: but those 'argument sides' can never unify; in case we have ``{ a ~ (Int, Bool)}`` then always pick the first instance, never the second or third.

We can tackle this with Overlapping Instances or Closed Type Families. (And the Instance Guards in this proposal can therefore be translated into today's Haskell.)

Currently: Overlapping Instances: Undecidable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can get the class to compile like this::

  class D a b | a -> b where ...
  instance D (Int, Bool) Char where ...
  instance {-# OVERLAPPING #-}  (b ~ a'') => D (Int,  a'')   b where ...
  instance {-# OVERLAPPING #-}  (b ~ a')  => D (a',   a'')   b  where ...

The first observation is that this is harder to read/understand: we must scan from instance head to constraints to understand what's going on. And in more realistic examples (such as within HList), there are stacked-up constraints, one calculating a result to plug into the next.

We've essentially 'fooled' the Instance consistency check: under substitution ``{ a ~ (Int, Bool)}``, the 'result sides' are not equal but they do unify ``{ b ~ Char }``. This is despite the instance constraint ``(b ~ a')``.

But we verge on incoherence: the bare ``b`` on the 'result side' escapes the Coverage Conditions, so we need ``UndecidableInstances``.

Currently: Closed Type Families: Closed, not Associated
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can get better coherence using Closed Type Families to simulate the Functional Dependency::

  class (F a ~ b) => D a b where ...

  type family F a where
    F (Int, Bool)  = Char
    F (Int, a'')   = a''
    F (a',  a'')   = a'

  instance D (Int, Bool) Char where ...
  instance (F (Int, a'') ~ b) => D (Int, a'') a'' where ...
  instance (F (a',  a'') ~ a) => D (a',  a'') a'  where ...

First, again observe the difficulty of reading this: the type family equations are distant from the class instance. We'd ideally perhaps write those equations as Associated types within the instance (but can't, because they overlap so must be in a closed grouping). Note also the need to repeat the SuperClass constraint as an Instance constraint.

There's a further limitation on expressivity: the Closed Type Family is, um, *Closed*. I can add a *class* instance (perhaps in a different module)::

  instance D (Maybe a') a' where ...

But to insert an extra Type Family equation needs editting the original type family's closed equations, **even though there's no possible overlap** between the ``Maybe`` constructor vs the ``(,)``.

Proposal: Instance (Apartness) Guards for Type Classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's explicitly de-overlap these instances. For the FunDep version, that becomes::

  class {-# INSTANCEGUARDS #-} D a b | a -> b where ...
  instance D (Int, Bool) Char where ...                   -- most specific instance as before

  instance D (Int, a'')   a'' | a'' /~ Bool  where ...    -- the /~ says: a'' must be apart from Bool

  instance D (a',  a'')   a'  | a'  /~ Int   where ...    -- a' must be apart from Int

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
    type   F (Int, Bool) = Char
     ...
  instance D (Int, a'') a'' | a'' /~ Bool where
    type   F (Int, a'')     | a'' /~ Bool  = a''
    ...
  instance D (a',  a'') a'  | a' /~ Int where
    type   F (a',  a'')     | a' /~ Int  = a'
     ...

Note that there's no need to repeat the Equality constraint on each instance, because it's substantiated by the Associated type equation.

(Those Associated type instances are a little cluttered with the guards. A nice-to-have would be to automatically copy them from the class instance.)



Proposed Change Specification
-----------------------------

Class or Type Family declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is to be a class-level or Type Family-level pragma ``{-# INSTANCEGUARDS #-}``. This is not global, but applies per-class or per-Type Family for backwards compatibility/co-existence with other classes using overlapping Instances or Closed Type Families. (So the ``{-# OVERLAPS #-}`` etc instance-level pragma cannot be used on guarded classes.)

The ``INSTANCEGUARDS`` pragma means that all instances must be 'apart' (not unifiable), after taking guards into account. IOW:

* Either the instance heads do not unify; or
* If the instance heads unify, yielding a substitution, applying that substitution to the guards yields a contradiction for at least one of the instances; and
* the "Instances inconsistent with Functional Dependencies" check is also to use the guards to validate apartness of the 'argument side' of FunDeps, see example above at 'Proposal: Instance (Apartness) Guards for Type Classes'.


Instances -- type classes or Type Families
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any guards are to appear immediately right of the instance head, separated by a ``|``.

The guards are a comma-separated list of type comparisons. For example::

  instance D a b      | a /~ Int, b/~ Bool  where ...

  type instance F a b | a /~ Int, b /~ Bool   = ...

Instances can be validated incrementally for overlap:

* Either the instance heads do not unify; or
* If the the instance heads unify, yielding a substitution, applying that substitution to the result yields the same type; or
* applying that substitution to the guards yields a contradiction for at least one of the instances.


Guards: rules for comparands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. The comparands must be same-kinded.
#. Comparands can use Type constructors to arbitrary nesting.
#. Can only use type vars from the head.
    (I.e. not introduce extra vars, which contexts can do.)
#. Can use wildcard ``_`` as a type place-holder.
#. No type functions -- (it would be a lovely-to-have,
   but too hard, and would need stringent Coverage conditions.
   Perhaps consider for 'phase 2' allowing ``UndecidableInstances``.)

Is this expressive enough? Yes: it's a Boolean algebra with equality.

* there's disjunction between instances. (Needs a little care here, because instances must be apart, so this is exclusive or.)
* There's conjunction amongst the guards and patterns in the head.
* The equality is expressed through patterns in the head. To make that more explicit we can use an equality guard::

      instance C Int Bool where ...       -- translates to
  ==> instance C a b | a ~ Int, b ~ Bool

Negation is expressed through apartness guards. Negating a conjunction can be either direct::

      instance C a b | (a, b) /~ (Int, Bool)

Or via (the X-Or version of) deMorgan to Negation Normal Form::

      instance C Int b    | b /~ Bool           where ...
      instance C a   Bool | a /~ Int            where ...
      instance C a   b    | a /~ Int, b /~ Bool where ...

The logic can also be expressed in the Constraint Handling Rules framework of `Sulzmann & Stuckey 2002 <http://people.eng.unimelb.edu.au/pstuckey/papers/toplas3217.pdf>`_, section 8.1 ‘Overlapping Definitions’.

Instance guards will work for all the examples in HList. Here's a particularly gnarly Closed Type Families example from `this discussion <https://typesandkinds.wordpress.com/2013/04/>`_ "Andy Adams-Moran's example" (which is possibly unrealistic)::

  data T a
  type family Equiv x y :: Bool where
     Equiv a      a     = True        -- 1
     Equiv (T b)  (T c) = True        -- 2
     Equiv (t d)  (t e) = Equiv d e   -- 3
     Equiv f      g     = False       -- 4

Translating to guards::

  type family {-# INSTANCEGUARDS #-} Equiv x y :: Bool
  type instance Equiv a      a                                      = True
  type instance Equiv (T b) (T c) | (T b) /~ (T c)                  = True
  type instance Equiv (t d) (t e) | (t d) /~ (t e), (t d) /~ (T _)  = Equiv d e
  type instance Equiv (t d)  g    | (t d) /~ g,     g /~ (t _)      = False    -- 4a
  type instance Equiv  f     g    | f /~ g,         f /~ (_ _)      = False    -- 4b

Equations 1 to 3 translate smoothly. Equation 1 is (potentially) overlapped by all others, but appears first in Closed sequence so needs no guards. All other equations have their first guard to push apart from equation 1. That's sufficient for Equation 2. Equation 3 wholly overlaps equation 2, so that's easily de-overlapped.

Equation 4 is awkward: it wholly overlaps equations 1 and 3 (and therefore 2); but 1 and 3 only partially overlap. Equation 4a's second guard pushes apart from 3 (and actually makes the first guard superfluous). This catches ``Equiv (Maybe Int) (T Int)`` and ``Equiv (Maybe Int) Bool``; but leaves a 'gap', for example ``Equiv Bool (Maybe Int)``.  Translating therefore needs two de-overlapping instances. (There's various ways to express that. They all need (at least) two instances. I've chosen a way that applies an arbitrary asymmetry wrt the parameters.)

Guards: possible stricter rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the gnarly example above, for all of the comparisons, at least one comparand is a whole parameter from the instance head. Is that always possible? Consider::

  instance C Int Bool where ...
  instance {-# OVERLAPPING #-} C a   b     where ...

The easiest way to express that second instance through guards is::

  instance C a   b  | (a, b) /~ (Int, Bool)  where ...

We could express that using only whole-params, but verbosely needing three instances, see this same example wrt the 'Boolean algebra' discussion above.

Another possible rule is that at least one of the comparands be a bare type var.

Instance Selection
~~~~~~~~~~~~~~~~~~

Because each instance has been validated pair-wise as apart from each other instance, the compiler can confidently select a matching instance at a usage site, after confirming any guards hold.

#. First, match (unify) the usage site against the instance head, as currently.
#. If the head is apart, then reject this instance.
#. If they unify, this gives a substitution. 
#. If no guards, select this instance. 
#. Otherwise apply that substitution into the guards.
#. If all guards come out true, select this instance.
#. Otherwise (at least one of the guards yields a contradiction), reject.
#. (Possible optimisation for Type Families: if after unifying the heads the substitution into the result is the same, no need to check the guards -- that is current behaviour which allows for ‘coincident overlap’.)

Given instances [example from the User Guide/above]::

  type family {-# INSTANCEGUARDS #-} G a
  type instance G (a,    Int)           = [a]
  type instance G (Char, b)  | b /~ Int = [b] 

with a usage site wanting ``G (Char, Int)`` (the classic problem of partial overlap):

* Wanted ``G (Char, Int)`` unifies with the head of the second instance, with substitution ``{ b ~ Int }``.
* Apply that substitution to the guard, yielding ``Int /~ Int``.
* Contradiction, so reject that instance.

If the compiler tries a wanted ``G (Char, Int)`` against the first instance before trying the second; that unifies without contradiction; so can be safely selected, with no danger of overlap.

At no time need the compiler search for instances and (nervously) pick the 'last one standing' à la IncoherentInstances.


Instance constraints
~~~~~~~~~~~~~~~~~~~~

Guards have no effect on instance constraints.

Associated type equations
~~~~~~~~~~~~~~~~~~~~~~~~~

Must repeat the guards from the class instance heads. (Can this be relaxed?)

Class methods
~~~~~~~~~~~~~

Guards have no effect on method bodies.

??For bodies that call other methods, can we 'pass on' knowledge of apartness to help in selecting instances for those? Sounds fraught with danger. The inference rule would be::

  a /~ b, b ~ c ==> a /~ c

Imports/Exports
~~~~~~~~~~~~~~~

That a class or Type Family has ``INSTANCEGUARDS`` must be exported to all modules, in case they declare any instances for the class/TF. So it's quite possible for a class to declare itself guarded, even though no guards are used in its defining module.

The guards for each instance must be exported, to control instance selection in those modules. (And to validate overlaps and FunDep consistency for any instances declared.)

Semantics: Translation to Haskell 2010+MPTCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To explain the intended semantics, examples of guards will be translated to:

* Haskell 2010
* with Multi-Parameter Type Classes (+ FlexibleInstances)
* assuming a type-level type equality test
* but otherwise not using Overlapping instances

This is given as a proof of concept, not a proposed method of implementation.

The type-level type equality test could be a Closed Type Family::

  type family TEqual a b :: Bool  where
    TEqual a a = True
    TEqual a b = False

(As easily, type equality could be defined via a type class with Functional Dependencies and strictly overlapping instances, as has been stable in GHC since at least 2004.)

The translation might also need a type *equality* guard (which could be visible in the surface language). The most obvious purpose for equality guards is to express repeated type vars (which are not permitted under Haskell 2010)::

      instance e (HCons e l) ...                     -- repeated `e` translated to
  ==> instance e (HCons e' l) | e ~ e' ...           -- where `e'` is fresh

Each class with ``INSTANCEGUARDS`` is to be implemented by a case-analysis class called in the context for each instance. The case-analysis class has an extra parameter (typically a tuple -- similar to constraints) to match the result from the Type Equality tests arising from the guard. Examples::

      class C a b ...
  ==> class C_Case a b t ...

First reduce all instances to canonical form of bare type vars and guards::

      instance C Int b ...                            -- source decl
  ==> instance C a   b    | a ~ Int ...

      instance C a   Bool | a /~ Int ...              -- source decl
  ==> instance C a   b    | a /~ Int, b ~ Bool ...

Form the union of the guards, commoning up those which are merely ``~`` vs ``/~`` of the same comparands, and arrange in some canonical order. Then form the case-despatching constraint over the instance head with bare type vars; and the case branches::

 ==> instance (C_Case a b (TEqual a Int, TEqual b Bool)) => C a b

  ==> instance C_Case Int b  (True, t') ...            -- } heads do not overlap
      instance C_Case a Bool (False, True) ...         -- }



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
