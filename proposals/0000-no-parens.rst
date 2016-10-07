.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Allow omitting parentheses tuples
==============

Minor syntactic change: When unambiguous, allow user to omit
parentheses around tuples. This reduces clutter and aligning them can
be a PITA.

Motivation
----------

Example from `There is no Fork
<community.haskell.org/~simonmar/papers/haxl-icfp14.pdf>`_ (even more
intense example from Figure 5)

.. code-block:: haskell
    case (f', x') of
      (Done g,        Done y       ) -> ...
      (Done g,        Blocked cr c ) -> ...
      (Blocked br c,  Done y       ) -> ...
      (Blocked br1 c, Blocked br2 d) -> ...

With proposal

.. code-block:: haskell
    case (f', x') of
      Done g,        Done y        -> ...
      Done g,        Blocked cr c  -> ...
      Blocked br c,  Done y        -> ...
      Blocked br1 c, Blocked br2 d -> ...

Along with proposal `#18
<https://github.com/ghc-proposals/ghc-proposals/pull/18>`_ we can go
from

.. code-block:: haskell
    foldB :: Foldable f => f B -> B
    foldB = foldBy (\a b -> case (a, b) of (T, T) -> T; _ -> F) T

to

.. code-block:: haskell
    foldB :: Foldable f => f B -> B
    foldB = foldBy (\case2 (T, T) -> T; _ -> F) T

to

.. code-block:: haskell
    foldB :: Foldable f => f B -> B
    foldB = foldBy (\case2 T, T -> T; _ -> F) T

Proposed Change
---------------

Drawbacks
---------

Alternatives
------------

Use parentheses.

Unresolved Questions
--------------------
