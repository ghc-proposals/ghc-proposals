Extend ``-Wall`` with ``incomplete-uni-patterns`` and ``incomplete-record-updates``
==============

.. author:: tomjaguarpaw
.. date-accepted:: 2018-02-07
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15656
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/71>`_.
.. contents::

Extend ``-Wall`` with ``incomplete-uni-patterns`` and ``incomplete-record-updates``
because they warn only about things that should typically not occur in well-written
modern Haskell.

Motivation
------------

``-Wall`` does not warn on the following, but ``-Wincomplete-uni-patterns`` does

::

    myHead :: [a] -> a
    myHead = \(a:as) -> a

``-Wall`` does not warn on the following, but ``-Wincomplete-record-updates`` does

::

    data Foo
      = Bar { barInt :: Int, barString :: String }
      | Baz

    mySetter :: Int -> Foo -> Foo
    mySetter int foo = foo { barInt = int }

These code samples are in a style which is generally discouraged in modern Haskell
and I suspect the vast majority of developers would want to be warned about these
issues as standard.  ``-Wall`` seems like a good standard place in which to apply
them.

Historically, ``incomplete-uni-patterns`` seems to have been kept out of ``-Wall``
by https://gitlab.haskell.org/ghc/ghc/issues/4905.  It received no discussion nor
counterpoints.  Moreover, the change was made 6 years ago and standards for Haskell
code in the wild have tightened somewhat since then.

I made this suggestion `on Haskell Reddit
<https://www.reddit.com/r/haskell/comments/6q9tcp/ghc_warnings_you_should_use_in_addition_to_wall/dkvrk0e/>`_
and it received net 48 upvotes with no dissenting responses.

(Thanks to `Dennis Gosnell <https://functor.tokyo/blog/2017-07-28-ghc-warnings-you-should-enable>`_
for bringing this issue to my attention.)

Proposed Change Specification
-----------------------------

Extend the meaning of ``-Wall`` to include ``incomplete-uni-patterns`` and
``incomplete-record-updates``.

Effect and Interactions
-----------------------

Users using ``-Wall`` will be warned when they use ``incomplete-uni-patterns`` and
``incomplete-record-updates``


Costs and Drawbacks
-------------------

Minimal development and maintenance costs.  Existing code compiled with ``-Wall``
will start warning if it uses ``incomplete-uni-patterns`` or
``incomplete-record-updates``

Alternatives
------------

Specify ``incomplete-uni-patterns`` and ``incomplete-record-updates`` manually.

Unresolved questions
--------------------

None

Implementation Plan
-------------------

Minimal planning required.
