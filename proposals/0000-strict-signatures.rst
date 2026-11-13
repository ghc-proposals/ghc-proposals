Decorative Signatures
=====================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/776>`_.
.. sectnum::
.. contents::

We propose to allow to manually mark arguments 
with decorators-modifiers in signatures before ``->`` .


Motivation
----------

Reading the signature often has very good understanding what the function do.
But sometimes it does not provide enough information, 
and we must add commentary to explain the difference.
::

    -- Left-associative fold of a structure, lazy in the accumulator.
    foldl  :: Foldable t => (b -> a -> b) -> b -> t a -> b

    -- Left-associative fold of a structure but with strict application of the operator.	
    foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b 

Is any difference between these 2 function?	Strictness!
So let's mark "accumulator" as strict with a modifier just for documentation purpose!
::

    -- now
    foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b 
	
    -- new syntax
    foldl' :: Foldable t => (b -> a -> b) -> b -> t a %Strict -> b 


So, with new syntax we could understand the difference just from the signature ::

    foldl  :: Foldable t => (b -> a -> b) -> b -> t a %Lazy -> b

    foldl' :: Foldable t => (b -> a -> b) -> b -> t a %Strict -> b 


Proposed Change Specification
-----------------------------

Introduce a new extension ``DecorativeSignatures``.

With this extension we allow one or some decorators-modifiers 
in signatures before ``->`` to be valid
regardless of the real role of the argument.
After checking, the modifier is fully ignored.

Next decorators-modifiers are allowed:

- which describe Strictness

  - ``%Strict``
  - ``%Lazy``

- which describe Size

  - ``%NonEmpty``
  - ``%Finite``
  - ``%Infinite``

- which describe Direction

  - ``%Left2R``
  - ``%Right2L``

- which describe Complexity

  - ``%O_1``
  - ``%O_n``
  - ``%O_n2``
  - ``%O_n3``
  - ``%O_logN``
  - ``%O_nLogN``
  - ``%O_minNM``

- which describe Condition

  - ``%IfFound``
  - ``%IfNotFound``
  - ``%AllFounded``
  - ``%AllButFounded``
  - ``%FirstFound``
  - ``%LastlFound``
  - ``%FirstNotFound``
  - ``%LastlNotFound``
  - ``%UntilFound``
  - ``%AfterFound``

Examples
--------

Several examples with Stricness decorators
::

    foldl  :: Foldable t => (b -> a -> b) -> b -> t a %Lazy -> b

    foldl' :: Foldable t => (b -> a -> b) -> b -> t a %Strict -> b 
	
    strict :: MonadIO io => Shell ByteString %Strict -> io ByteString

Several examples with Size decorators
::

    head :: HasCallStack => [a] %NonEmpty -> a

    foldl' :: (b -> a -> b) -> b -> t a %Finite -> b 

    (++) :: [a] %Finite -> [a] -> [a]

Several examples with Direction decorators
::

    foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a %Right2L -> m 

    traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a %Left2R -> f (t b)

Several examples with Complexity decorators
::

    nub :: Eq a => [a] %O_n2 -> [a]

    delete :: Eq a => a -> [a] %O_n -> [a] 

    uncons :: [a] %O_1 -> Maybe (a, [a]) 

    zip :: [a] %O_minNM -> [b] %O_minNM -> [(a, b)] 

Several examples with Condition decorators
::

    break :: (a -> Bool) %UntilFound -> [a] -> ([a], [a]) 

    dropWhile :: (a -> Bool) %AfterFound -> [a] -> [a] 



Effect and Interactions
-----------------------

Extension implies ``Modifiers`` extension.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of this extension has low difficulty.


Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.


Alternatives
------------

The primary alternative is "status quo".


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Acknowledgments
---------------


Endorsements
-------------
