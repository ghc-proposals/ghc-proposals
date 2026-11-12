Strict Signatures
=================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/776>`_.
.. sectnum::
.. contents::

We propose to allow to manually mark maybe strict arguments 
with ``%Strict`` (or ``%Lazy`` ) modifier in signatures before ``->`` .


Motivation
----------

Reading the signatures often has very good understanding what the function do.
But sometimes it consists not enough information and we must add commentaries
to explain what the difference.
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

Introduce a new extension ``StrictSignatures``.

With this extension we allow modifier ``%Strict`` or ``%Lazy`` 
in signatures before ``->`` to be valid
regardless on real strictness or laziness of the argument.
After checking the modifier is fully ignored.

	
Examples
--------

Several examples with new syntax
::

    foo :: a %Strict -> a %Lazy -> a
	foo !x y = ... some-expr
	
    strict :: MonadIO io => Shell ByteString %Strict -> io ByteString


Effect and Interactions
-----------------------

Extension implies ``Modifiers`` extension.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of separator change has low difficulty.


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
