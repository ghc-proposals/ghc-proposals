.. proposal-number:: 

.. trac-ticket:: 

.. implemented:: 

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/44>`_.

.. contents::

No if-then-else
==============

One may wish to define terms named ``if``, ``then``, or ``else``, but these are keywords in Haskell. This proposes to add an extension ``NoIfThenElse`` which makes the parser rather parse these as normal identifiers.



Motivation
------------
In a language such as Haskell with non-strict semantics, an if-then-else control structure is not needed: one can rather define a term, for example ``bool :: a -> a -> Bool -> a``, with this effect. Such terms are first-class: they can be used as function arguments, exported from and imported into modules, and used however else one might use a term. Haskell's if-then-else syntax is not first-class, which complicates the language and denies the user three very convenient names. One may, for example, wish to define::

	if :: Bool -> (a -> a) -> a -> a
	if False _ = id
	if True  f = f

as the author did shortly before writing this document, or::

	if :: Bool -> a -> a -> a
	if True  x _ = x
	if False _ y = y

as proposed `here <https://wiki.haskell.org/If-then-else>`__ by another name.



Proposed Change Specification
-----------------------------
``NoIfThenElse`` shall be recognized as a language extension. If enabled, the parser shall parse ``if``, ``then``, and ``else`` as normal identifiers rather than keywords.



Effect and Interactions
-----------------------
With the ``NoIfThenElse`` extension, one can define terms named ``if``, ``then``, and ``else``. To the author's knowledge, this interacts with no other features.

Teachers would optionally have a dialect of Haskell sans this strange syntax.

The Haskell Prime committee would have a path towards scrubbing if-then-else from the language altogether, should they ever wish to.



Costs and Drawbacks
-------------------
The development cost should be zero; see `Implementation Plan`_.

The extension ought to be easy to explain to novices: Haskell has if-then-else syntax for legacy reasons; one can disable it with an extension if one wishes to use these names. Indeed, on learning the non-strict semantics of Haskell, a novice may rather ask why Haskell has if-then-else at all.

Like any such proposal, a drawback of this is adding another flag in the compiler.

Another drawback is needing to enable the pragma in any module which uses terms named ``if``, ``then``, or ``else``, whether it defines or imports them.



Alternatives
------------
The alternatives are to not use these names or to fork Haskell. To fork Haskell would lead to linguistic fragmentation and would mean that code written in the new language could not use code written in Haskell, a major and likely fatal loss. To not use these names is to lose three short and memorable names which would be the natural choice in some cases.



Unresolved questions
--------------------
None known



Implementation Plan
-------------------
A `patch <https://phabricator-files.haskell.org/file/data/fvlko53egtrg4fa6wayb/PHID-FILE-gcr6ehxxwhdaewovvfgb/D3052.diff>`_ has already been written.
