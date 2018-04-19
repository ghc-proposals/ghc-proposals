Type annotated quoters
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Currently we use an uninformative ``QuasiQuoter`` type to handle all kinds of quoters, this bring troubles to expression quoter users. I propose adding ``QuasiQuoterExp a`` to handle expression quoters, where ``a`` denote the spliced expression's type.

Motivation
------------

``QuasiQuoter`` is a powerful tool to do compilition time code constrcution, but the type ``QuasiQuoter`` is rather uninformative, especially when it's an expression quoter. Those ``QuasiQuoter`` s appeared in documents on hackage not only didn't give user useful information, but also scared beginners away. If expression quoters' type can carry spliced expression's type, these problem is relieved.


Proposed Change Specification
-----------------------------

Introduce a new data type ``data QuasiQuoterExp a = QuasiQuoterExp (String -> Q Exp)`` in ``Language.Haskell.TH.Quote`` module.

* Allow ``quoter :: QuasiQuoterExp a`` to be spliced in the same way a ``QuasiQuoter`` which has ``quoteExp`` defined.
* The spliced expression should be annotated with type ``a``, and this will be checked during type checking.
* Add document on how ``QuasiQuoterExp a`` should be used to splice an `a` typed expression into user's code. 


Effect and Interactions
-----------------------

For expression quoter writers, adding ``QuasiQuoterExp a`` mainly reduce the documentation burden since the result expression's type is already annotated. Users can spot the result type much more easily and become more confident in using these quoters. When beginners click through the ``QuasiQuoterExp`` document link, they're supposed to get the basic knowledge on how to enable some language extension and splice quoters into their code.

Costs and Drawbacks
-------------------

The major cost is that we have another magic data type under ``QuasiQuoter`` extension, but since it's exported from template haskell package, it shouldn't be an issue.

Alternatives
------------

In fact this proposal is inspired by the `Compile-time literal values <https://github.com/ghc-proposals/ghc-proposals/pull/124>` proposal, and shared some goals, but this proposal is more about trying to solve an existing issue with current quoter system.


Unresolved questions
--------------------

Well, i don't have yet.


Implementation Plan
-------------------
I strongly recommend my friend `@sighingnow <https://github.com/sighingnow>`  ; )
