Deprecating Exports
===================

.. proposal-number:: blank
.. trac-ticket:: blank
.. implemented:: blank
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This is my GSoC 2018 project for Haskell.org

The project should be helpful with easing the transition between different versions of code. 
This will be achieved by adding a support for a new deprecation pragma, which will allow the library writers to 
easily specify, which exports from a module are to be deprecated. 

Motivation
------------
As described in the ticket https://ghc.haskell.org/trac/ghc/ticket/4879 there is sometimes a need to deprecate certain exports from a module.

Proposed Change Specification
-----------------------------
The changes proposed are based on the first design option from the ticket https://ghc.haskell.org/trac/ghc/ticket/4879

The syntax would be as follows:

::

    module Data.List
    (  ...
        {-# DEPRECATE lines "Exported from Data.String instead" #-}
        , lines
        ...t 
    ) where
    ...

The semantics are also described in the aforementioned ticket as follows: 

You would get a deprecation warning:

* If you explicitly import a deprecated export: ::
    
    import Data.List (lines)
* If you refer to the deprecated export, when fully importing a module: ::
    
    import Data.List
    foo = lines
* If you import the same symbol from different modules and only some of them are deprecated exports then referring to the symbol won't give a deprecation warning. For example the following should not give deprecation warnings: ::
    
    import Data.List
    import Data.String
    foo = lines


Effect and Interactions
-----------------------
If implemented correctly, this should not cause any side-effects as the GHC could only display warning messages as a result of the pragma.
All the other behaviour is expected to remain the same. 


Costs and Drawbacks
-------------------
The mentors expect that I would be able to finish the project in 6 weeks.
Unless the unforeseen occurs, I think this is a reasonable estimate and I intend to do my best to stick to this schedule.

Alternatives
------------
As far as I know there are no real alternatives to this feature.

Right now you can only specify that an export from a module is deprecated in a comment, however, the GHC would not bring that up during compile time.
You can also remove the export altogether but the whole point of deprecation warning is to still allow the users to use the method before it is finally removed.


Unresolved questions
--------------------
There are 2 different proposed designs (again, see: https://ghc.haskell.org/trac/ghc/ticket/4879).
I am leaning towards the first one as it readily shows next to an export that it is being deprecated but I am very open to any discussion regarding this.


Implementation Plan
-------------------
I would aim to implement the proposed changes as part of my GSoC 2018 commitment.
To achieve this, I will maintain regular communications with my mentors Matthew Pickering and Erik de Castro Lopo and the broader GHC developer community.
