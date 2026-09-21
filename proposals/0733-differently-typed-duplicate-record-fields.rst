Differently-typed duplicate record fields
=========================================

.. author:: Brandon Chinn
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/733>`_.
.. sectnum::
.. contents::

This proposal proposes lifting the restriction that the same record field name must have the same type

Motivation
----------

When ``-XDuplicateRecordFields`` is enabled, you're allowed to do:

::

  data Foo1 = Foo1 { x :: Int }
  data Foo2 = Foo2 { x :: Bool }

But you're not allowed to do

::

  data Foo = Foo1 { x :: Int } | Foo2 { x :: Bool }

This errors at definition site:

::

  <interactive>:4:1: error: [GHC-91827]
    • Constructors Foo1 and Foo2 give different types for field ‘x’
    • In the data type declaration for ‘Foo’

This wouldn't be useful for selector functions, but it can still be useful for documentation or when pattern matching, e.g. with ``NamedFieldPuns``.

Proposed Change Specification
-----------------------------

When ``-XDuplicateRecordFields`` is enabled, allow duplicate record field names to have different types. Duplicate record fields with different types will have the following behaviors:

* Selector functions are not generated (like existential fields)
* ``HasField`` instances are not generated (like existential fields)
* Record updates are not supported

Attempts to use a selector function on a field ``foo`` with multiple types within a data type will error with something like: "Cannot use selector function for field 'foo', which has multiple types: Int, Bool". This could be implemented either as a ``TypeError`` constraint or built-in to the compiler. Similar errors will be thrown for ``HasField`` and record updates as well.

Examples
--------

::

  {-# LANGUAGE DuplicateRecordFields #-}
  {-# LANGUAGE LambdaCase #-}
  {-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE RecordWildCards #-}

  data MyError
    = UserNotFound { user :: Text }
    | UserNotAdmin { user :: User }

  loadAdmin :: Text -> IO (Either MyError User)
  loadAdmin name =
    lookupUser name >>= \case
      Left _ -> pure $ Left UserNotFound{user = name}
      Right user
        | not (isAdmin user) -> pure $ Left UserNotAdmin{user = user}
        | otherwise -> pure $ Right user

  renderError :: MyError -> Text
  renderError = \case
    UserNotFound{user} -> "User not found: " <> user
    UserNotAdmin{..} -> "User not admin: " <> userName user <> " (role = " <> userRole user <> ")"

Effect and Interactions
-----------------------

Interactions with other extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

None

Costs and Drawbacks
-------------------

* If a data type has two fields named ``foo`` and only changes the type of one of them, it will no longer throw an error at definition site, but it might start erroring at use-site (e.g. selector function)

* Might cause confusions for beginner who lose selector functions when they "just added another constructor"

Backward Compatibility
----------------------

No breakage, only allows previously forbidden programs.

Alternatives
------------

* Add a new extension ``DuplicateRecordFieldMultiType`` to enable this behavior (would imply ``DuplicateRecordFields``)

Future work
~~~~~~~~~~~

It may be possible to lift some of these restrictions, e.g.

::

  data Foo = Foo1 { x :: Int } | Foo2 { x :: Bool }

  updateFoo :: Foo -> Foo
  updateFoo = \case
    foo@Foo1{} -> foo{x = 1}
    foo@Foo2{} -> foo{x = True}

But this doesn't seem straightforward to implement now, so we'll start small and iterate in the future.

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.
