Extra MaybeField Selectors
==========================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/639>`_.
.. sectnum::
.. contents::

This proposal add safety and error-less runtime (on wrong use of field selector) by using ``ExtraMaybeFieldSelectors`` instead of partial field-selectors.


Motivation
----------

Partial field-selectors could produce runtime errors and this is extremely weak design.

This proposal add simplicity, safety and error-less runtime by using ``ExtraMaybeFieldSelectors`` instead of partial field-selectors.


Proposed Change Specification
-----------------------------

Let we have next Sum-Type ``data`` ::

  data Message 
      = SendChat  { message :: Text,      user :: UserId }
      | Signup    { eventId :: EventId,   user :: UserId }
      | SendOrder { signalId :: SignalId }


This creates several functions (if ``NoFieldSelector`` is off): ::

  message  :: Message -> Text     -- or runtime Error

  eventId  :: Message -> EventId  -- or runtime Error

  signalId :: Message -> SignalId -- or runtime Error
  
  user     :: Message -> UserId   -- from SendChat or Signup or runtime Error



Extra functions
~~~~~~~~~~~~~~~

We suggest a language extension: with ``ExtraMaybeFieldSelectors`` extra code is produced:
  
- For each *de-jure* Sum-Type: ``data`` with at least one ``|`` sign we create maybe-fields-selectors (or several constructors on GADTs definitions)

- For each selector ``<field> :: <dataType> -> <fieldType>`` we create function ``<field>'maybe :: <dataType> -> Maybe (<fieldType>)``


For this specific example ``data Message`` next functions will be added  ::

  message'maybe  :: Message -> Maybe Text

  eventId'maybe  :: Message -> Maybe EventId

  signalId'maybe :: Message -> Maybe SignalId

  user'maybe     :: Message -> Maybe UserId



*Note: all this code is safe in use, it is error-less in runtime.*


Import, export
~~~~~~~~~~~~~~

Sure, we could manually write functions for import and export. We suggest new syntax for field-depended mentioning by adding ``'maybe`` to data-type ::

  module M
    ( S(x), S(x)'maybe
    , T(..), T(..)'maybe
    ) where 
    -- ...

Examples
--------

Several-parts field type
~~~~~~~~~~~~~~~~~~~~~~~~

If we have several-parts field type, than parentheses must be added to final type: ``<field>'maybe :: <dataType> -> Maybe (<fieldType>)`` ::

  data OptionRec a = None | Some { fromSome :: Maybe a }

  fromSome'maybe :: OptionRec a -> Maybe (Maybe a)


GADTs fields-selectors
~~~~~~~~~~~~~~~~~~~~~~

If data type is written using GADTs, this extension create function for each field selector, even if fields are *de-facto* not a Sum-type ::

  data Tag = A | B | C

  data Foo (a :: Tag) where
     FooA :: { fooa :: () } -> Foo A
     FooB :: { foob :: Int } -> Foo B
     FooC :: { fooc :: Maybe Bool } -> Foo C
     FooD :: { food :: Char } -> Foo C

  -- (Foo A) and (Foo B) are de-facto not a Sum-Types, but they are de-jure a Sum-Type

  fooa'maybe :: Foo A -> Maybe ()

  foob'maybe :: Foo B -> Maybe Int

  fooc'maybe :: Foo C -> Maybe (Maybe Bool)

  food'maybe :: Foo C -> Maybe Char


Effect and Interactions
-----------------------

We expect this proposal could also affect ``HasField`` class.  we add similar classes `HasMaybeField`/`SetMaybeField`  ::

 create additional  classes `HasMaybeField`/`SetMaybeField` 

  class HasMaybeField x r a | x r -> a where
    getMaybeField :: r -> Maybe a

  class SetMaybeField x r a | x r -> a where
    modifyMaybeField :: (a -> a) -> r -> Maybe r
    default modifyMaybeField :: (r_rep ~ LiftedRep, a_rep ~ LiftedRep, HasMaybeField x r a) => (a -> a) -> r -> Maybe r
    modifyMaybeField f r = (\fn -> setMaybeField @x fn r) <$> (f <$> (getMaybeField @x r))

    setMaybeField :: a -> r -> r
    default setMaybeField :: a_rep ~ LiftedRep => a -> r -> Maybe r
    setMaybeField v = modifyMaybeField @x (\ _ -> v)

    {-# MINIMAL modifyMaybeField | setMaybeField #-}

And we instead of writting ``r { user = blah } :: r`` write ``r { user'maybe = blah } :: Maybe r`` and for updating several fields we desugars with Maybe-Monad ::

  r { user'maybe = blah, message'maybe = bar }

  -- desugars (this is not optimal desugaring)
  do
    r1 <- r  { user'maybe = blah }
    r2 <- r1 { message'maybe = bar }
    return r2

We expect this proposal affects ``OverloadedRecordDot`` and ``OverloadedRecordUpdate`` extensions for maybe-selectors.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Alternatives
------------

A partial alternative is `Partial Field Behavior #535 <https://github.com/ghc-proposals/ghc-proposals/pull/535>`_


Unresolved Questions
--------------------

Name-collision is possible, but it is highly unlikely (with "``'maybe``" in fields names) ::

  data T
    = A {f'maybe :: Int, f   :: Char}
    | B {f   :: Char, f'maybe :: Int}

  f'maybe :: T -> Maybe Char

  f'maybe'maybe  :: T -> Maybe Int


Implementation Plan
-------------------

It is unclear.
