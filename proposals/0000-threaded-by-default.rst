Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Make RTS threaded by default
==============

.. proposal-number:: <blank>
.. trac-ticket:: 16126
.. implemented:: <blank>
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Today, in order to use the threaded variant of the runtime, one have to pass the `-threaded` flag. The proposal suggests making `-threaded` the default: in order to get the threaded RTS one would not have to do anything, while to fallback to the current, single-threaded mode, one would use the new ``-single-threaded`` flag.


Motivation
------------
Parallel hardware is here. It is hard to imagine a setting without an access to multiple cores. Defaulting to single-threaded RTS seems to be a legacy these days, and the one we should cleanup at some point. The proposal suggest exactly this. We should default on the common case, which is parallel hardware today, and provide a way to fallback for users with special needs. The latter can be done by the means of the new ``-single-threaded`` flag.



Proposed Change Specification
-----------------------------

***** below is still the template *****

Specify the change in precise, comprehensive yet concise language. Avoid words like should or could. Strive for a complete definition. Your specification may include,

* grammar and semantics of any new syntactic constructs
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler features, in case that is otherwise ambiguous

Note, however, that this section need not describe details of the implementation of the feature. The proposal is merely supposed to give a conceptual specification of the new feature and its behavior.


Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features. 


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------
List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.


Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
