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
  
  user :: Message -> UserId -- from SendChat or Signup or runtime Error



Extra functions
~~~~~~~~~~~~~~~

We suggest a language extension: with ``ExtraMaybeFieldSelectors`` extra code is produced:
  
- For each *de-jure* Sum-Type: ``data`` with at least one ``|`` sign we create maybe-fields-selectors (or several constructors on GADTs definitions)

- For each partial selector ``<field>`` with type signature ``<field> :: <dataType> -> <fieldType>`` we create function ``<newname> :: <dataType> -> Maybe <fieldType>``

- For each **unique** partial selector ``<field>`` we create function ``maybeFs<Field>`` function (with capitalized first letter of field-selector), where "Fs" is short from "field-selector"
  
- For each **repetitive**/**duplicated** partial selector ``<field>`` we create function ``maybeDp<Constructor>Fs<Field>`` function (with capitalized first letter of field-selector), where "Dp" is short from "duplicated"


For this specific example ``data Message`` next functions will be added  ::

  maybeFsMessage  :: Message -> Maybe Text

  maybeFsEventId  :: Message -> Maybe EventId

  maybeFsSignalId :: Message -> Maybe SignalId

  
  maybeDpSendChatFsUser  :: Message -> Maybe UserId

  maybeDpSignupFsUser    :: Message -> Maybe UserId


*Note: all this code is safe in use, it is error-less in runtime.*


Import, export
~~~~~~~~~~~~~~

Sure, we could manually write functions for import and export. We suggest new syntax for field-depended mentioning by adding ``.maybe`` to data-type ::

  module M
    ( S(x), S(x).maybe
    , T(..), T(..).maybe
    ) where 
    -- ...

Examples
--------

Several-parts field type
~~~~~~~~~~~~~~~~~~~~~~~~

If we have several-parts field type, than parentheses must be added to final type: ``<newname> :: <dataType> -> Maybe (<fieldType>)`` ::

  data OptionRec a = None | Some { fromSome :: Maybe a }

  maybeFsFromSome :: OptionRec a -> Maybe (Maybe a)


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

  maybeFsFooa :: Foo A -> Maybe ()

  maybeFsFoob :: Foo B -> Maybe Int

  maybeFsFooc :: Foo C -> Maybe (Maybe Bool)

  maybeFsFood :: Foo C -> Maybe Char


Effect and Interactions
-----------------------

We expect this proposal could also affect ``HasField`` class ::

  class HasField (x :: k) r a | x r -> a where
    
    -- new function
    hasMaybeField :: r -> Maybe (a -> r, a)
    hasMaybeField = Just . hasField
	
    hasField :: r -> (a -> r, a)
    hasField = fromJust . hasMaybeField
	
    -- define just "hasField" makes "hasMaybeField" unsafe
    {-# MINIMAL hasMaybeField | hasField #-}
	
  getMaybeField :: forall x r a . HasField x r a => r -> Maybe a
  getMaybeField = fmap snd . hasMaybeField @x

  setMaybeField :: forall x r a . HasField x r a => r -> a -> Maybe r
  setMaybeField r a = fmap fst (hasMaybeField @x r) <*> (pure a)

  setMaybeFieldIgnore :: forall x r a . HasField x r a => r -> a -> r
  setMaybeFieldIgnore r a = fromJust $ (setMaybeField @x r a) <|> (Just r)

  getField :: forall x r a . HasField x r a => r -> a
  getField = snd . hasField @x

  setField :: forall x r a . HasField x r a => r -> a -> r
  setField = fst . hasField @x


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

Name-collision is possible, but it is highly unlikely (with "FsFs" collisions) ::

  data T
    = A   {fsB :: Int, b   :: Char}
    | AFs {b   :: Char, fsB :: Int}

  maybeDpAFsFsB  :: T -> Maybe Int -- Dp + A + Fs + FsB

  maybeDpAFsFsB  :: T -> Maybe Int -- Dp + AFs + Fs + B


Implementation Plan
-------------------

It is unclear.
