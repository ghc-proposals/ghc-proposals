Deprecating Exports
===================

.. author:: Alanas Plascinskas
.. date-accepted:: 2018-07-14
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/4879
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/134>`_.
.. contents::

This is my GSoC 2018 project for Haskell.org

The project should be helpful with easing the transition between different versions of code.
This will be achieved by adding a support for a new deprecation pragma, which will allow the library writers to
easily specify, which exports from a module are to be deprecated.

Motivation
------------
As described in the ticket https://gitlab.haskell.org/ghc/ghc/issues/4879 there is sometimes a need to deprecate certain exports from a module.

Proposed Change Specification
-----------------------------
The changes proposed are based on the first design option from the ticket https://gitlab.haskell.org/ghc/ghc/issues/4879

The syntax would be as follows:

::

    module Data.List
    (  ...
        {-# DEPRECATED "Exported from Data.String instead" #-}
        lines,
        ...t
    ) where
    ...

The deprecation pragma would have an effect on an export that immediately follows it, only top level exports would be considered.

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

* However, in the above example, you would get a deprecation warning if you were to refer to the `lines` symbol by name ::

    foo = Data.String.lines

* It is also important that the warning is not triggered whenever a name is used within a hiding clause, i.e.: ::

    module A ( {-# DEPRECATED "blah" #-} x ) where { x = True }
    module B where { import A hiding (x) }

* A symbol exported by a module is deprecated if all export specifiers for that symbol have a DEPRECATED pragma. It is an error if a symbol is exported multiple times with DEPRECATED pragmas where the deprecation messages differ ::

    -- only T(C) is deprecated
    module M
      ( {-# DEPRECATED "don't use the constructor" #-} T(C)
      , T(D)  -- or T, pattern D
      ) where

    data T = C ...
    pattern D ...

    -- T is deprecated
    module M
      ( {-# DEPRECATED "don't use the constructor" #-} T(C)
      , {-# DEPRECATED "don't use the constructor" #-} T(D)  -- or T, pattern D
      ) where

    data T = C ...
    pattern D ...

    -- error
    module M
      ( {-# DEPRECATED "message1" #-} T(C)
      , {-# DEPRECATED "message2" #-} T(D)  -- or T, pattern D
      ) where

    data T = C ...
    pattern D ...


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
UPDATE: The proposed design now does not have an export identifier and so the question below is resolved.

There are 2 different proposed designs:

1 ::

    module Data.List
    (  ...
        {-# DEPRECATE lines "Exported from Data.String instead" #-}
        , lines
        ...
    ) where
    ...

2 ::

    {-# DEPRECATE_EXPORT lines "Exported from Data.String instead" #-}


I am leaning towards the first one as it readily shows next to an export that it is being deprecated but I am very open to any discussion regarding this.


Implementation Plan
-------------------
I would aim to implement the proposed changes as part of my GSoC 2018 commitment.
To achieve this, I will maintain regular communications with my mentors Matthew Pickering and Erik de Castro Lopo and the broader GHC developer community.
