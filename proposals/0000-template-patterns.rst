Template Patterns in Rewrite Rules
=================================

.. author:: Jaro Reinders
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/555>`_.
.. sectnum::
.. contents::

User defined rewrite rules allow for powerful optimisations such as shortcut fusion.

Unfortunately, rewrite rules which contain local variable bindings, for example in lambdas or case expressions, are not very useful because rewrite rules cannot match arbitrary expressions which contain occurrences of locally bound variables.

This proposal extends rewrite rules with a lightweight form of higher order matching to make rewrite rules involving local variable bindings much more powerful.

Motivation
----------

There are two practical problems that motivat this change.
The first is a wart in the current fusion mechanism in base which is exemplified by the ``mapFB`` function.
The second is a major roadblock to the adoption of stream fusion, namely optimising the ``concatMap`` function. 

Removing the mapFB wart
~~~~~~~~~~~~~~~~~~~~~~~

The most immediate motivation is the removal of warts in the foldr/build fusion mechanism.

As a refresher, here is a snippet of ``Note [The rules for map]`` which explains how a part of foldr/build fusion works (remember phase numbers decrease towards zero which is the last phase):

	Up to (but not including) phase 1, we use the "map" rule to
	rewrite all saturated applications of map with its build/fold
	form, hoping for fusion to happen.

	In phase 1 and 0, we switch off that rule, inline build, and
	switch on the "mapList" rule, which rewrites the foldr/mapFB
	thing back into plain map.

These are the rules in question:
::

	{-# RULES
	"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
	"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
	"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
	"mapFB/id"  forall c.           mapFB c (\x -> x)       = c
	#-}

where
::

	mapFB c f = \x ys -> c (f x) ys

The ``mapFB`` helper function is necessary to avoid the limitations of lambdas in rewrite rules.
But unfortunately `issue #22361 <https://gitlab.haskell.org/ghc/ghc/-/issues/22361>`_ shows that the presence of ``mapFB`` can inhibit optimisations.

Ideally, we would like to write the rules where ``mapFB`` is inlined as follows:
::

	{-# RULES
	"map"     [~1] forall f xs. map f xs                     = build (\c n -> foldr (\x ys -> c (f x) ys) n xs)
	"mapList" [1]  forall f.    foldr (\x ys -> f x : ys) [] = map f
	#-}

Note how the two ``mapFB`` rules are now unnecessary, because the simplifier can now operate on the lambdas directly.

The reason why the rules are not implemented like this is that it is unlikely to match anything in practice.
For example take the program:
::

	foo xs = map (\x -> x * 2 + x) xs

Before phase 1, the program is transformed into:
::

	foo xs = foldr (\x ys -> x * 2 + x : ys) [] xs

In phase 1, when we try to match the "mapList" rule to this function all parts match except for ``f x`` which should match ``x * 2 + x``.
The current rule matcher will only match ``f x`` literally to a application of some function ``f`` to the locally bound variable ``x``.
The expression ``x * 2 + x`` is not literally an application, so the rule does not match.
Under this proposal the rule will match and recover the original program:
::

	foo xs = map (\x -> x * 2 + x) xs

Optimising the concatMap function under stream fusion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another source of motivation for this proposal is the optimisation of the ``concatMap`` function under stream fusion.

This problem has plagued stream fusion for a very long time (see e.g. `discussion on issue #915 <https://gitlab.haskell.org/ghc/ghc/-/issues/915#note_26104>`_).
Duncan Coutts proposed using the following rewrite rule in `"Stream Fusion: Practical shortcut fusion for coinductive sequence types" (Section 4.8.3) <https://ora.ox.ac.uk/objects/uuid:b4971f57-2b94-4fdf-a5c0-98d6935a44da/download_file?file_format=pdf&hyrax_fileset_id=m8450e05775b1a9a35267c4e58184492e&safe_filename=Thesis%2BPDF%2C%2Bstandard%2Blayout&type_of_work=Thesis>`_:
::

	"concatMap"   forall next f.   concatMap (\x -> Stream next (f x)) = concatMap' next f

Currently, this rule only matches if the target contains a literal application of some function ``f`` to the local variable ``x``.
This proposal would allow matching the above rule to more complicated targets like ``concatMap (\x. Stream next (x * 2 + x))`` producing ``concatMap' next (\x -> x * 2 + x)``.

This could potentially make stream fusion general enough to replace foldr/build fusion in base.

Proposed Change Specification
-----------------------------

This proposal only changes the semantics of rewrite rules. No new syntax is introduced.

Let us start with some terminology of rewrite rules.
Consider the rule:
::

	{-# RULES "wombat"  forall f x.  foo x (\y. f y) = bar x f  #-}

* 	*Template*.
	The LHS of a rule is called its *template*.
* 	*Template variables*.
	The ``forall``'d variables are called the *template variables*.
	In rule "wombat", ``f`` and ``x`` are template variables.
* 	*Local binders*.
	The *local binders* of a rule are the variables bound inside the template.
	Example: ``y`` is a local binder of rule "wombat".
* 	*Target*.
	The rule matcher matches the LHS of the rule (the template) against an expression in the program (the *target*).
* 	*Substitution*.
	A sucessful match finds a *substitution* S: a binding for each template variable, such that applying S to the LHS yields the target.
* 	After a successful match we replace the target expression with the substitution S applied to the RHS of the rule.


In GHC today, a template variable ``v`` matches any expression ``e`` if

* ``e`` has the same type as ``v``
* No local binder of the template is free in ``e``.

The change proposed here is that a **template pattern** matches any expression (of the same type):

* 	*template pattern*.
	A template pattern is an expression of form ``f x y z`` where:

	- ``f`` is a template variable
	- ``x``,``y``,``z`` are locally bound in the template (like ``y`` in rule "wombat" above).

	They are specifically not template variables, nor are they free in the entire rule.

	- The arguments ``x``, ``y``, ``z`` are *distinct* variables
	- ``x``, ``y``, ``z`` must be term variables (not type applications).

* 	A template pattern ``f x y z`` matches *any expression* ``e`` provided:

	- The target has the same type as the template
	- no local binder is free in ``e``, other than ``x``, ``y``, ``z``.

*	If these two condition hold, the template pattern ``f x y z`` matches the target expression ``e``, yielding the substitution ``[f :-> \x y z. e]``.
	Notice that this substitution is type preserving, and the RHS of the substitution has no free local binders.

Uniqueness of matching
~~~~~~~~~~~~~~~~~~~~~~

Consider this rule and target:
::

	RULE "funny"   foo (\x y. Just (f x y))

	Target:  ...(foo (\ p q. Just (h (p+1) q)))....

Then during matching we will encounter:
::

	Template:    f x y
	Target:      h (p+1) q      [p:->x, q:->y]

The renaming ``[p:->x, q:->y]`` is done by the matcher (today) on the fly, to make the bound variables of the template and target "line up".

Now, we can:

* Either use the new template-application rule to succeed with ``[f :-> \x y. h (x+1) y]``.
* Or use the existing decompose-application rule to match ``(f x)`` against ``(h (p+1))`` and ``y`` against ``q``.  This will succeed, with ``[f :-> \x. h (x+1)]``.

Critically, *it doesn't matter which we do*.
We get the same result either way.
That's encouraging.

More generally, we think that if a match exists it is unique (moudulo eta-reduction).

Examples
--------

* 	One of the simplest examples is this rule:
	::

		{-# RULES "foo" forall f. foo (\x -> f x) = "RULE FIRED" #-}
	
	It would match expressions like:
	::

		foo (\x -> x * 2 + x)

* 	The template pattern may involve multiple locally bound variables, e.g.:
	::

		{-# RULES "foo" forall f. foo (\x y z -> f x y z) = "RULE FIRED" #-}

	Which would match:
	::

		foo (\x y z -> x * y + z)
	
	But not every variable has to occur in the match. It would also match this expression where ``y`` does not occur:
	::

		foo (\x y z -> x * 2 + z)

* 	Locally bound variables may only occur once.
	Consider the following rule:
	::

		{-# RULES "foo" forall f. foo (\x -> f x x) = "RULE FIRED" #-}

	This would **not** match:
	::

		foo (\x -> x * 2 + x)
	
	But it does contain the valid subrule ``f x``, so it would match:
	::

		foo (\x -> (bar x . baz) x)

* 	Similarly if the template variable ``f`` is applied to non-variable arguments then it only matches a literal application.
	Consider this rule:
	::

		{-# RULES "foo" forall f. foo (\x y -> f x 2 y) = "RULE FIRED" #-}
	
	This would **not** match:
	::

		foo (\x y -> x * 2 + y)`
	
	But again it does contain the template pattern ``f x``, so it would match:
	::

		foo (\x y -> (bar x . baz) 2 y)

Effect and Interactions
-----------------------

The main effect of this proposal is that rewrite rules involving template patterns now match more expressions.
But the additional matches are guaranteed to be beta equivalent, so this change does not cause existing rules to become semantically incorrect.

The only contentious interactions could occur due to rules that now overlap under the new rules, for example:
::

	{-# RULES
	"foo->bar"  forall f x.  foo x (\y. f y) = bar x f
	"foo->baz"  forall   x.  foo x (\y. y * 2 + y) = baz x
	#-}

Previously, the rule ``"foo->baz"`` would always fire when encountering the expression ``foo x (\y. y * 2 + y)``, but now the rule ``"foo->bar"`` also matches.
However, we do not expect that this occurs in practice.


Costs and Drawbacks
-------------------

1. 	The changes required for this proposal are small (the core of the change is an addition of just 22 lines of code).
	Small changes can add up, but we think the benefits far outweigh this cost in this case.

2. 	This proposal causes a silent change of behaviour of existing code.
	It is possible to come up with an artificial system of rewrite rules that produces suboptimal results due to this change.
	We do not expect this to happen in practice.

Alternatives
------------

Roughly in order of cheap to expensive alternatives:

1. 	Do nothing.

2. 	Introduce explicit syntax for template patterns.
	This requires modifying the parser and bikeshedding over syntax, but it may make the rules completely backwards compatible and the intent of the programmer is clearer to the compiler so the compiler can give better error messages and warnings.
	We have chosen against this alternative, because we do not think any existing rewrite rules depend critically on the previous behaviour and we expect error messages and warnings can still be written for the most common mistakes with a bit more effort.

3. 	Use lambda binders instead of applications to figure out the scope of local variables automatically.
	For example the "mapList" rule could look like this:
	::

		"mapList" [1]  forall f.    foldr (\x ys -> f : ys) [] = map (\x -> f)
	
	Where the the rule matcher would recognise that the ``\x ->`` binders on the left and the right is the same.
	From this we could deduce that the variables ``x`` should be allowed to occur in ``f``.
	We have not chosen this syntax because it is less explicit about which locally bound variables are allowed to occur in which template variables.

4. 	Implement more powerful higher order matching, for example as proposed by De Moor and Sittampalam in `"Higher-order matching for program transformation" <https://www.sciencedirect.com/science/article/pii/S0304397500004023>`_.
	We expect that this alternative would require much more significant changes to the rule matcher in GHC.


Unresolved Questions
--------------------

1. 	What to do with polymorphic template variables?
	Consider the code:
	::

		foo :: (forall a. [a] -> Int) -> Int
		foo len = len [1,2,3] + len "abc"
		{-# NOINLINE foo #-}

		{-# RULES "foo" forall (f :: forall a. [a] -> Int). foo (\xs -> 1 + f xs) = 2 + foo f #-}
	
	Here, the template variable ``f`` has a polymorphic type.
	With explicit type abstractions and applications the rule looks like this:
	::

		{-# RULES "foo" forall (f :: forall a. [a] -> Int). foo (/\a. \(xs::[a]) -> 1 + f @a xs) = 2 + foo f #-}  
	
	The proposal could be change such that this rule would match the expression:
	::

		foo (/\b. \(ys::[b]). 1 + (reverse @b (take @b 3 ys)))
	

	However, if we change the type of the template variable ``f`` to ``forall a. a -> Int``, then the rule with explicit type abstractions and applications looks like this:
	::

		{-# RULES "foo" forall (f :: forall a. a -> Int). foo (/\a. \(xs::[a]) -> 1 + f @[a] xs) = 2 + foo f #-}  
	
	(Note: we assume deep subsumption here for simplicity of presentation)

	Now ``@[a]`` is no longer a plain locally bound variable, so this is no longer a template pattern.

	This seems fragile and we do not know of any practical programs that requires polymorphic template variables in template patterns.

2. 	The name "template pattern" is still up for debate.
	Suggestions are welcome.

Implementation Plan
-------------------

The proposed changes have already been implemented in `#9343 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9343>`_.
Only tests still need to be written.
