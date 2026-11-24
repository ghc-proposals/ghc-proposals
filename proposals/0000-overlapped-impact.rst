Overlapped Impact
===================

.. author:: Viktor WW
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/778>`_.
.. sectnum::
.. contents::

We propose to add optional **impact** to ``OVERLAPPING`` / ``OVERLAPABLE`` / ``OVERLAPS`` 
pragmas to have more flexible control over which instance is chosen when they overlap. 


Motivation
----------

GHC allows writing overlapping instances.

Unfortunately, GHC is not expressive enough to resolve
at compile time or at runtime which instance should be chosen.

GHC has limited capacity for pragmas to mark instances: 
``OVERLAPPING`` , ``OVERLAPABLE`` , ``OVERLAPS`` and ``INCOHERENT``.
It also has several extensions for more liberal instances, 
like ``FlexibleInstances`` and ``UndecidableInstances`` .

But these tools are not expressive enough to describe which instance must be chosen,
especially when the number of instances is more than 3-5.
::

    instance {-# OVERLAPPABLE #-} Count [a] where ...
	
    instance {-# OVERLAPPING #-} Count [Char] where ...
	
    --
	
    instance CountI [a] where ...
	
    instance {-# INCOHERENT #-} CountI [Char] where ...

This proposal suggests adding **Impact** to Overlapping Instances to allow 
more accurate calculation of which instance must be chosen.
::

    instance {-# OVERLAPPING -1    #-} Formatter a where ...
	
    instance {-# OVERLAPPING 1 % 2 #-} (Formatter a) => Formatter (Maybe a) where ...

    instance {-# OVERLAPPING 3 % 2 #-} (Formatter a) => Formatter [a] where ...

    instance {-# OVERLAPPING 3     #-} (Formatter a) => Formatter [Maybe a] where ...

    instance {-# OVERLAPPING 4     #-} (Formatter a) => Formatter (Maybe [a]) where ...
	
    instance {-# OVERLAPPING 6     #-} Formatter (Maybe Int) where ...

    instance {-# OVERLAPPING 7     #-} Formatter [Int] where ...

    instance {-# OVERLAPPING 8     #-} Formatter (Maybe [Int]) where ...

    instance {-# OVERLAPPING 9     #-} Formatter [Maybe Int] where ...

    instance {-# OVERLAPPING 10    #-} Formatter Int where ...


Proposed Change Specification
-----------------------------

Introduce a new extension ``OverlappedImpact``.

With this extension, we could explicitly write impact 
in ``OVERLAPPING`` / ``OVERLAPABLE`` / ``OVERLAPS`` pragmas.

*Note: We use pseudo-pragma* ``NOOVERLAP`` *here for instances without any pragma.*

For backward compatibility, implicit impact in pragmas is allowed.
And we also remain ``OVERLAPABLE`` and ``OVERLAPS`` pragmas, even they becomes redundant.

How to define the impact from the pragmas? Very easy:

.. code:: none

    NOOVERLAP     -> impact = 0 

    OVERLAPPING j -> impact = +j

    OVERLAPABLE j -> impact = -j

And we have the next dependencies for other variations:
  
.. code:: none

    OVERLAPPING = OVERLAPPING 1

    OVERLAPABLE = OVERLAPABLE 1
	
    OVERLAPS    = OVERLAPS 1
	
    OVERLAPS j  = OVERLAPPING j + OVERLAPABLE j

    --
    
    OVERLAPPING j = OVERLAPABLE -j  

    OVERLAPABLE j = OVERLAPPING -j

``Ratio Int`` type is chosen to be able to interpose any amount of 
instances between any impact values.

How is impact influencing instance choosing? 
We changed the choosing algorithm to the next one:

#. Filter all instances by the head.

#. If no candidates remain - then *FAIL*

#. If just 1 candidate remains - then SUCCESS

#. **UPDATED**: Filter all remaining candidates **to the highest Impact** from the remaining ones

#. If just 1 candidate remains - then SUCCESS

#. If there are no incoherent instances - then *FAIL*

#. Try to resolve all incoherent candidates to the more specific one

#. If just 1 candidate remains - then SUCCESS

#. More than 1 candidate remains - *FAIL*


In theory, if all instances for the same class have unique "singleton" values, 
then non-overlapping is guaranteed at runtime.

Syntax
~~~~~~~~~~~~

The formal grammar changes for impacts in pragmas is:

.. code:: abnf

    impact ::= [ '-' ] numr [ [ '%' ] denom ]        ;-- new
	
    numr   ::= dec_digit {dec_digit}                 ;-- new
	
    denom  ::= dec_digit {dec_digit}                 ;-- new
	
	
    prgm_overl_ing  ::= '{-#' 'OVERLAPPING' [ impact ] '#-}'    ;-- upd
	
    prgm_overl_able ::= '{-#' 'OVERLAPABLE' [ impact ] '#-}'    ;-- upd
	
    prgm_overl_s    ::= '{-#' 'OVERLAPS'    [ impact ] '#-}'    ;-- upd
	
Sure, default values (when a value is omitted) are:

- ``default numr   = 1``
- ``default denom  = 1``
- ``default impact = 1``

The factor ``impact :: Ratio Int`` has rational type,
but we parse it from raw to avoid dependence on ``Data.Ratio``
in the Module.

Pragma ``OVERLAPPING`` de facto became universal, 
so in the future ``OVERLAPABLE`` and ``OVERLAPS`` pragmas would be Deprecated.

Examples
--------

Examples with new syntax
::

    instance {-# OVERLAPPING -2    #-} context1 => C Int b     where ...  -- (A)
    instance {-# OVERLAPPING 4     #-} context2 => C a   Bool  where ...  -- (B)
    instance {-# OVERLAPPING 1 % 2 #-} context3 => C a   [b]   where ...  -- (C)
    instance {-# OVERLAPPING 5     #-} context4 => C Int [Int] where ...  -- (D)
	
    -- (D) > (B) > (C) > (A)


Effect and Interactions
-----------------------

None.


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
