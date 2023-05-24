Higher Order Patterns in Rewrite Rules
======================================

.. author:: Jaro Reinders
.. date-accepted:: 2023-01-23
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/555>`_.
.. sectnum::
.. contents::

Overview
--------

User defined rewrite rules allow for powerful optimisations such as
shortcut fusion.  This proposal extends rewrite rules with a
lightweight form of higher order matching to make rewrite rules
involving local variable bindings much more powerful.  Doing so is very cheap: it is
easy to explain and easy to implement.  But it unlocks powerful new expressiveness
in rewrite rules.

See discussion in `GHC ticket #22465 <https://gitlab.haskell.org/ghc/ghc/-/issues/22465>`_.

Motivation
----------

Having more powerful matching in rewrite rules is an Excellent Thing, especially
if it is easy to specify and implement.  We illustrate by showing two problems
that are solved by the change.

Removing the mapFB wart
~~~~~~~~~~~~~~~~~~~~~~~

The rules for ``map`` in the ``base`` library are intended to:

* fuse ``map`` with other list producers and/or consumers, and
* yield a simple call to ``map`` in the cases where no fusion happens.

Here is how you might hope to achieve this:
::

	{-# RULES
	"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (\x ys -> c (f x) ys) n xs)
	"mapList" [1]  forall f.    foldr (\x ys -> f x : ys) [] = map f
	#-}

Before (but not including) phase 1, rule "map" turns a call to ``map`` into a call to ``build`` and ``foldr``.
Now suppose that nothing fuses with the ``build`` or ``foldr``.  Then, in the following phase 1,
``build`` is inlined.  Recall that ``build g = g (:) []``.

So a call ``map f xs`` will now look like ``foldr (\x ys -> f x : ys) [] xs)``.  Finally, rule "mapList" can rewrite
it back to ``map f xs``.

But alas this simply doesn't work in practice. Suppose we had::

        map (\x -> x+1) xs

Then we'd get the expression ``foldr (\x ys -> x+1 : ys) [] xs``, and that doesn't syntactically match the pattern in ``mapList``.
We need a more powerful matcher to find a suitable ``f`` when matching.

Similarly, if we started with ``map p (map q xs)``, we would indeed get foldr/build fusion, and end up with ``foldr (\x ys -> p (q x) : ys) [] xs``.  Alas again this does not syntactically match the pattern in "mapList".  Yet if, when matching rule "matchList", the matcher could
cough up the binding ``f :-> \x -> p (q x)``, we could rewrite the ``foldr`` call to ``map (\x -> p (q x)) xs``, which is of course what we want.

Since we do not have this more powerful matcher, the ``base`` uses a clever (but ultimately inadequate) hack.  The actual rules are these::

	{-# RULES
	"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
	"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
	"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
	"mapFB/id"  forall c.           mapFB c (\x -> x)       = c
	#-}

where::

	mapFB c f = \x ys -> c (f x) ys

Here the ``mapFB`` combinator abstracts the little pattern from ``map``, which means that ``mapList`` can spot it.
But alas we need extra rules "mapFB` and "mapFB/id" to get map/map fusion to work.

But the hack does not scale well.  For example `issue #22361 <https://gitlab.haskell.org/ghc/ghc/-/issues/22361>`_ shows an example of nested fusion that does not work well -- the ``mapFB`` itself gets in the way of fusion


This unsatisfactory ``mapFB`` hack is replicated in many other functions in ``base``.

Optimising the concatMap function under stream fusion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another source, even more powerful, motivation for this proposal is the optimisation of the ``concatMap`` function under stream fusion. This celebrated challenge has been an open problem for a very long time (see e.g. `this comment in GHC issue #915 <https://gitlab.haskell.org/ghc/ghc/-/issues/915#note_26104>`_).
It's an important one too: in their paper `"The Hermit in the stream" <https://dl.acm.org/doi/10.1145/2543728.2543736>`_, Farmer et al describe an entire plugin for GHC devoted to this one task.  Here's part of the abstract

    Stream Fusion, a popular deforestation technique in the Haskell community, cannot fuse the concatMap combinator. This is a serious limitation, as concatMap represents computations on nested streams. The original implementation of Stream Fusion used the Glasgow Haskell Compiler's user-directed rewriting system. A transformation which allows the compiler to fuse many uses of concatMap has previously been proposed, but never implemented, because the host rewrite system was not expressive enough to implement the proposed transformation.

    In this paper, we develop a custom optimization plugin which implements the proposed concatMap transformation, and study the effectiveness of the transformation in practice. We also provide a new translation scheme for list comprehensions which enables them to be optimized. Within this framework, we extend the transformation to monadic streams. Code featuring uses of concatMap experiences significant speedup when compiled with this optimization. This allows Stream Fusion to outperform its rival, foldr/build, on many list computations, and enables performance-sensitive code to be expressed at a higher level of abstraction.


See also

* The earlier paper `From lists to streams to nothing at all <https://dl.acm.org/doi/10.1145/1291151.1291199>`_
* `GHC issue #915 <https://gitlab.haskell.org/ghc/ghc/-/issues/915>`_ 

Thus motivated, Duncan Coutts proposed using the following rewrite rule in `"Stream Fusion: Practical shortcut fusion for coinductive sequence types" (Section 4.8.3) <https://ora.ox.ac.uk/objects/uuid:b4971f57-2b94-4fdf-a5c0-98d6935a44da/download_file?file_format=pdf&hyrax_fileset_id=m8450e05775b1a9a35267c4e58184492e&safe_filename=Thesis%2BPDF%2C%2Bstandard%2Blayout&type_of_work=Thesis>`_:
::

	"concatMap"   forall next f.   concatMap (\x -> Stream next (f x)) = concatMap' next f

In GHC today, this rule only matches if the target contains a literal application of some function ``f`` to the local variable ``x``.
This proposal would allow matching the above rule to more complicated targets like ``concatMap (\x. Stream next (x * 2 + x))`` producing ``concatMap' next (\x -> x * 2 + x)``.

*By using more powerful matching, we solve the long-standing problem of fusing
concatMap under stream fusion.*  In turn, this could
potentially make stream fusion general enough to replace foldr/build
fusion in base.

Proposed Change Specification
-----------------------------

This proposal only changes the semantics of rewrite rules. No new syntax is introduced.

Let us start with some terminology of rewrite rules.
Consider the rule:
::

	{-# RULES "wombat"  forall f x.  foo x (\y. f y) = bar x f  #-}

* 	**Template**.
	The LHS of a rule is called its *template*.
* 	**Template variables**.
	The ``forall``'d variables are called the *template variables*.
	In rule "wombat", ``f`` and ``x`` are template variables.
* 	**Local binders**.
	The *local binders* of a rule are the variables bound inside the template.
	Example: ``y`` is a local binder of rule "wombat".
	A local binder is specifically not a template variable, nor is it free in the entire rule.
* 	**Target**.
	The rule matcher matches the LHS of the rule (the template) against an expression in the program (the *target*).
* 	**Substitution**.
	A successful match finds a *substitution* S: a binding for each template variable, such that applying S to the LHS yields the target.
* 	After a successful match we replace the target expression with the substitution S applied to the RHS of the rule.


In GHC today, a template variable ``v`` matches any expression ``e`` if

* ``e`` has the same type as ``v``
* No local binder of the template is free in ``e``.

The change proposed here makes matching more powerful by introducing the notion of a **higher order pattern**
(a sub-expression of the template) that matches *any* target expression of the same type as the higher order pattern:

* 	**Definition**.
	A **higher order pattern (HOP)** is a sub-expression of the template of form ``f x y z`` where:

	- ``f`` is a *template variable*
	- ``x``, ``y``, ``z`` are *local binders* (like ``y`` in rule "wombat" above; see definitions).
	- The arguments ``x``, ``y``, ``z`` are *distinct* variables
	- ``x``, ``y``, ``z`` must be term variables (not type applications).

* 	**Matching of higher order patterns (HOP-matching)**.
        A higher order pattern ``f x y z`` (in the template) matches *any target expression* ``e`` provided:

	- The target has the same type as the template
	- No local binder is free in ``e``, other than ``x``, ``y``, ``z``.

	If these two condition hold, the higher order pattern ``f x y z`` matches the target expression ``e``, yielding the substitution ``[f :-> \x y z. e]``.
	Notice that this substitution is type preserving, and the RHS of the substitution has no free local binders.

*       **Ambiguity breaking**.  When matching template ``(etmpl x)`` against target ``(etarget x)``, do *not* use HOP-matching even if ``(etmpl x)`` is a HOP; instead simply match ``etmpl`` (which is also a HOP) against ``etarget``.  See the next subsection for the justification for this refinement.


Uniqueness of matching and backward compatibility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We do not introduce any new syntax, so you might worry that, in the case where
the existing rule-matching in GHC finds a match, the HOP-matching rules might find a different
and perhaps-incompatible match.  But in fact not: if the existing mechanism finds a match,
the HOP-matching will also find a match, and it will be identical up to eta-equivalence.

For example consider::

     RULE "old" forall f. foo (\x y z -> negate (f z x)) = map f

and the target expression::

     foo (\p q r -> negate (wim r p))

We can see that:

* The existing rule-matcher will succeed, binding ``[f :-> wim]``, rewriting the target to ``map wim``
* Under this proposal since ``(f z x)`` is a higher-order pattern, HOP-matching ``(f z x)`` against the target ``(wim r p)`` will succeed, binding ``[f :-> \r p -> wim r p]``, thus rewriting the target to ``map (\r p -> wim r p)``.

You might worry that the two results are not quite the same, because eta-reduction is not sound in Haskell.
That is the reason for the "ambiguity breaking" bullet in the specification.
With this refinement, in all cases where the existing matching mechanism succeeds, the new mechanism will give the same results because it will simply use the old existing mechanism.

As a more complicated example, consider this rule and target:
::

	RULE "funny"   foo (\x y. Just (f x y))

	Target:  ...(foo (\ p q. Just (h (p+1) q)))....

Then during matching we will encounter:
::

	Template:    f x y
	Target:      h (p+1) q      [p:->x, q:->y]

The renaming ``[p:->x, q:->y]`` is done by the matcher (today) on the fly, to make the bound variables of the template and target "line up".

Now, we can:

1. Either use HOP-matching to succeed with ``[f :-> \x y. h (x+1) y]``.
2. Or use the existing decompose-application rule, and then match ``(f x)`` against ``(h (p+1))`` and ``y`` against ``q``.
   Now the match of ``(f x)`` against ``(h (p+1))`` can only succeed by using HOP-matching yielding ``[f :-> \x. h (x+1)]``.

The refined rule picks (2), which yields success with as few lambdas as possible in the match.


Related work
~~~~~~~~~~~~

There are two notable streams of research: *higher order matching* and *higher order unification*. Both problems are about finding a substitution such that two expressions containing variables become equal. The difference is that unification applies to two expressions that both can contain (unification) variables, while matching applies to one expression with (template) variables and one concrete expression. Matching is an easier problem to solve.

The main related work on matching is `"Higher-order matching for program transformation" <https://www.sciencedirect.com/science/article/pii/S0304397500004023>`_ by De Moor and Sittampalam. This work identifies a subset of the higher order matching problem which is decidable and always has a finite set of possible substitutions. This subset is strictly larger than the higher order patterns that we consider in this proposal. For example, their subset is able to find substitutions to match `forall f x. f x` to `0`. Our proposal does not support this form of higher order matching.

However De Moor and Sittampalam identify the (potential) importance of the special case that we exploit:

    There is a wealth of related work on higher-order matching and unification [5, 7, 11, 13, 16, 18, 24, 25], to name just a few. One important concept identified in some of these works (in particular [16, 18]) is that of a restricted notion of higher-order pattern. To wit, a restricted pattern is a normal term where every occurrence of a free function variable is applied to a list of distinct local variables, and nothing else. For such restricted patterns, much simpler and more efficient matching and unification algorithms are possible. Our algorithm returns all higher-order matches for rules where the pattern satisfies the above restriction; in fact there is at most one such match. We have not yet investigated the efficiency of our algorithm in this important special case.

Existing work on higher order unification has revealed that higher order unification of higher order patterns is a useful decidable subset of the general problem. This was first observed by Miller in `"Unification Under a Mixed Prefix" <https://repository.upenn.edu/cis_reports/454/>`_. (Miller called our higher order patterns just 'patterns', but we decided against using that terminology because of the confusion with regular pattern matching in Haskell.) Nowadays higher unification with Miller's pattern restriction is commonplace in dependently typed languages such as `Agda <http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf>`_ and `Idris <https://www.type-driven.org.uk/edwinb/papers/impldtp.pdf>`_, which shows that higher order unification with this restriction is still very powerful.

One notable extension of Miller's patterns is the extension by Duggan in his paper `"Unification with extended patterns" <https://doi.org/10.1016/S0304-3975(97)00141-2>`_. Duggan extends the simple higher order pattern form to allow projections of local binders as arguments to the template variables. Such an extension might also be applicable to the higher order patterns in this proposal.  András Kovács writes about further extensions `"in this StackOverflow" <https://cstheory.stackexchange.com/questions/50914/swapping-arguments-of-variables-in-higher-order-pattern-unification/50918#50918>`_.

Examples
--------

* 	One of the simplest examples is this rule:
	::

		{-# RULES "foo" forall f. foo (\x -> f x) = "RULE FIRED" #-}

	It would match expressions like:
	::

		foo (\x -> x * 2 + x)

* 	The higher order pattern may involve multiple locally bound variables, e.g.:
	::

		{-# RULES "foo" forall f. foo (\x y z -> f x y z) = "RULE FIRED" #-}

	Which would match:
	::

		foo (\x y z -> x * y + z)

	But not every variable has to occur in the match. It would also match this expression where ``y`` does not occur:
	::

		foo (\x y z -> x * 2 + z)

* 	Locally bound variables may only occur once in a higher order pattern.
        But that doesn't mean we reject such non-linear template variable applications in a pattern.
        Consider the following rule:
	::

		{-# RULES "foo" forall f. foo (\x -> f x x) = "RULE FIRED" #-}

	This would **not** match:
	::

		foo (\x -> x + (x*2))

	But it does contain the valid HOP ``f x``, so it would match:
	::

		foo (\x -> (bar x . baz) x)

* 	Similarly if the template variable ``f`` is applied to non-variable arguments then it only matches a literal application.
	Consider this rule:
	::

		{-# RULES "foo" forall f. foo (\x y -> f x 2 y) = "RULE FIRED" #-}

	The template sub-expression ``f x 2 y`` is not a HOP, so the rule would **not** match:
	::

		foo (\x y -> x * 2 + y)

	But again it does contain the valid HOP ``f x``, so it would match:
	::

		foo (\x y -> (bar x . baz) 2 y)

Effect and Interactions
-----------------------

The main effect of this proposal is that rewrite rules involving higher order patterns now match more expressions.
But the additional matches are guaranteed to be beta equivalent, so this change does not cause existing rules to become semantically incorrect.

The only potentially-contentious interactions could occur due to rules that now overlap under the new rules, for example:
::

	{-# RULES
	"foo->bar"  forall f x.  foo x (\y. f y) = bar x f
	"foo->baz"  forall   x.  foo x (\y. y * 2 + y) = baz x
	#-}

Previously, only the rule ``"foo->baz"`` would fire when encountering the expression ``foo x (\y. y * 2 + y)``, but now the rule ``"foo->bar"`` also matches.  However, when multiple rules match, GHC picks the most specific; and
in fact ``"foo->baz"`` is more specific than ``"foo->bar"``, so the former will "win".
We are not aware of any rule-sets whose behaviour would change under this proposal.


Costs and Drawbacks
-------------------

The changes required for this proposal are small (the core of the change is an addition of just 22 lines of code).
Small changes can add up, but we think the benefits far outweigh this cost in this case.


Alternatives
------------

Roughly in order of cheap to expensive alternatives:

1. 	Do nothing.

2. 	Introduce explicit syntax for higher order patterns.
	This requires modifying the parser and bikeshedding over syntax, but it may make the rules completely backwards compatible and the intent of the programmer is clearer to the compiler so the compiler can give better error messages and warnings.
	We have chosen against this alternative, because we do not think any existing rewrite rules depend critically on the previous behaviour and we expect error messages and warnings can still be written for the most common mistakes with a bit more effort.

3. 	Use lambda binders instead of applications to figure out the scope of local variables automatically.
	For example the "mapList" rule could look like this:
	::

		"mapList" [1]  forall f.    foldr (\x ys -> f : ys) [] = map (\x -> f)

	Where the rule matcher would recognise that the ``\x ->`` binders on the left and the right is the same.
	From this we could deduce that the variables ``x`` should be allowed to occur in ``f``.
	We have not chosen this syntax because it is less explicit about which locally bound variables are allowed to occur in which template variables.

4. 	Implement more powerful higher order matching, for example as proposed by De Moor and Sittampalam in `"Higher-order matching for program transformation" <https://www.sciencedirect.com/science/article/pii/S0304397500004023>`_.

	They show an example of higher order matching that is not covered by this proposal, namely the template ``forall f x. f x``.
	Here they apply one template variable ``f`` to another template variable ``x``.
	This often leads to ambiguity.
	For example if we match that template against the term 0 we can get many possible substitutions: ::

		[f :-> \a -> a, x :-> 0]
		[f :-> \a -> 0]
		[f :-> \g -> g 0, x :-> \a -> a]
		[f :-> \g -> g (g 0), x :-> \a -> a]

	We expect that this alternative would require much more significant changes to the rule matcher in GHC.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

The proposed changes have already been implemented in `!9343 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9343>`_.
Only tests still need to be written.
