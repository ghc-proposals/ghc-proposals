Implicit Module Names
=====================

.. author:: John Ericson
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

Allow plain ``module where``, inferring the name from context.

Motivation
----------

Education
~~~~~~~~~

It is generally bad for things to know their own name.
There are many examples of this maxim being followed or broken, but one close to the heart of Haskell and functional programming in general is the idea that functions are expressions like any other, and when they are bound to a name it is no different than any other.
Few students are outright taught this, but I think we should at least set a good example.
Modules are *not* a good example because a module's definition and its name are mixed in together, syntactically;
there is no way to write one without the other.

If this seems too dogmatic, consider also @cdsmith's argument in `Proposal 316`_.
``Module Foo where`` is just more boilerplate students have to write before they understand what it means.
That applies to the ``module`` and ``where`` too, but let's focus on the ``Foo``: students need to name a module before they've necessarily learned what imports are, or that there can be more than one module at all---in other words, they need learn to name modules whether or not they've learned the things that motivate modules having names.
The non-compositionality of modules knowing their own name translates to either an artificial steepness of in the curriculum, or rote steps which are only motivated later.

A last addendum on `Proposal 316`_: it proposes making the implicit module declaration be ``module Main where`` instead of ``module Main(main) where``, getting rid of the boilerplate of the ``module`` and ``where``, too, in the common case of exporting everything.
But if that proposal builds on top of this proposal, it can instead propose the implicit module declaration be equivalent to ``module where``.
This is clearly the most minimal declaration---unlike before there was no unique minimum---and therefore a better candidate for an implicit default on strictly mathematical grounds.

Build Systems
~~~~~~~~~~~~~

It's a real shame, and one that haunts me, that there is currently no widely used build system for Haskell that has both fine-grained incremental builds and correct-by-construction caching.
The bedrock of such systems is content-addressing; content-addressing is the simplest correct-by-construction form of caching, and can also be construed as optimal.
Not knowing your own name dovetails with content-addressing, in that by rigorously separating the essential and non-essential bits of a context, we get more cache hits.

Let's make this concrete.
In the short term, implementing this change will be trivial and lead to no build system benefits, as we just take the name inferred from the file path and proceed as before with that.
But suppose we actually build each module anonymously, only binding it to name only for sake of downstream modules imports.
Now, if we build modules identical except for their name, we can just build one and uses it for the other.
This arises whenever one renames a module.

More broadly, the types of refactors to separate items from their names is usually good to reduce complexity / entanglement of ideas in general, and GHC is a big knot that badly needs untying.
Superficial as this change may seem, anything that spurs is in this direction is good.

Finally, it has long been a goal of Cabal and other build tools to stop relying on ``ghc --make`` and instead track module dependencies and cache modules themselves.
[This is the best way to achieve the correct and fine-grained builds, other than throwing even more complexity in GHC that doesn't belong there.]
For many reasons, including sub-module builds, it is good if GHC is fed syntax directly rather than told to look at a file path.
If it can neither deduce the module name (and thus something about its dependencies) from the file name or spoon-fed syntax, it is forced to do the build system's bidding to a greater degree.
This means fewer bugs and better enforced separation of concerns.

Proposed Change Specification
-----------------------------

There is a new language extension ``ImplicitModuleNames``.

Syntax
~~~~~~

When that extension is enabled, the grammar for modules is extended from

::

  module → module modid [exports] where body
         | body

to

::

  module → module modid [exports] where body
         | module [exports] where body
         | body

Semantics
~~~~~~~~~

Module imports are already resolved in an implementation specific manner.
(For example, none of downsweep, up-sweep, 1 module per file, etc are in the Haskell Report.)
With ``ImplicitModuleNames``, the implementation also decides what the module's name is.

GHC in particular already has expects modules to have names based on their file path---if the explicit module name and file name don't correspond GHC will complain.
With ``ImplicitModuleNames`` GHC will simply assign name the module based on its existing lookup logic.

Examples
--------

::

  module where

::

  module (Foo, Bar, baz) where

Effect and Interactions
-----------------------

Note that this is similar to how package imports work today.
One doesn't write down the package name in the Haskell source, but GHC knows it from context, and is able to resolve imports accordingly.

Costs and Drawbacks
-------------------

- More than one way to do things.
  This is only fixed with this proposal by taking drastic step of disallowing the writing module names, making the new way the only way.
  This is not proposed at this time.

- Change for non-immediate benefits.
  Yes, I know that throughout the motivation I am essentially arguing for a butterfly effect---we make a small change now, and get the benefits later and indirectly.
  Many people have a "historically oriented" view of language design, where past precedent is paramount and things should only be changed with concrete and guaranteed benefits.
  I fully admit under that criteria, this proposal fails.
  I do not advocate that view, and am more worried about accumulating small mistakes over time, each of which is fairly benign, or, relatedly, being stuck in a local maxima of design value.
  Housecleaning and experimentation both broaden our horizons.

Alternatives
------------

- Status quo.

- `Proposal 316`_ as written.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

I (@Ericson2314) can do it, though I am happy to help someone else do it as it's a good easy training ticket.

Endorsements
-------------

.. _`Proposal 316`: https://github.com/ghc-proposals/ghc-proposals/pull/a316
