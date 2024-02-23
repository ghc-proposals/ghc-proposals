Extra MaybeField Selectors
====================

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

Partial field-selectors could produce runtime errors and this is extremely weak design. Also duplicated partial field-selectors could have "magical" type of sub-types.

This proposal add simplicity, safety and error-less runtime by using ``ExtraMaybeFieldSelectors`` instead of partial field-selectors.


Proposed Change Specification
-----------------------------

Let we have next Sum-Type ``data`` ::

  data Message 
      = SendChat  { message :: Text,      user :: UserId,  uid :: UserId }
      | Signup    { eventId :: EventId,   user :: UserId,  uid :: EventId }
      | SendOrder { signalId :: SignalId, user :: UserId,  uid :: SignalId }


This creates several functions (if ``NoFieldSelector`` is off): ::

  message  :: Message -> Text     -- or runtime Error

  eventId  :: Message -> EventId  -- or runtime Error

  signalId :: Message -> SignalId -- or runtime Error

  
  user :: Message -> UserId

  uid  :: Message -> "magical" (UserId | EventId | SignalId)


Extra functions
~~~~~~~~~~~~~~~

We suggest a language extension: with ``ExtraMaybeFieldSelectors`` extra code is produced:
  
- For each partial selector ``<field>`` with type signature ``<field> :: <dataType> -> <fieldType>`` we create function ``<newname> :: <dataType> -> Maybe <fieldType>``

- For each **unique** partial selector ``<field>`` we create function ``maybe<Field>`` function (with capitalized first letter of field-selector)
  
- For each **repetitive**/**duplicated** partial selector ``<field>`` we create function ``maybe<Construcor><Field>`` function (with capitalized first letter of field-selector)


For this specipic example ``data Message`` next functions will be added  ::

  maybeMessage  :: Message -> Maybe Text

  maybeEventId  :: Message -> Maybe EventId

  maybeSignalId :: Message -> Maybe SignalId

  
  maybeSendChatUser  :: Message -> Maybe UserId

  maybeSignupUser    :: Message -> Maybe UserId

  maybeSendOrderUser :: Message -> Maybe UserId

  
  maybeSendChatUid  :: Message -> Maybe UserId

  maybeSignupUid    :: Message -> Maybe EventId

  maybeSendOrderUid :: Message -> Maybe SignalId


*Note: all this code is safe in use, it is error-less in runtime and have "ordinary" types in signatures.*

Import, export
~~~~~~~~~~~~~~

Sure, we could manually write functions for import and export. We suggest new syntax for field-depended mentioning ::

  module M
    ( S(x), S(x).maybe
    , T(..), T(..).maybe
    ) where 
    -- ...


Effect and Interactions
-----------------------

We expect this proposal affects ``OverloadedRecordDot`` extension for maybe-selectors.

We expect this proposal affects ``DisambiguateRecordFields`` extension for reading only maybe-selectors.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.

Backward Compatibility
----------------------

This proposal is fully backward compatible.

Alternatives
------------

A partial alternative is `Partial Field Behavior #535 <https://github.com/ghc-proposals/ghc-proposals/pull/535>`_

Implementation Plan
-------------------

It is unclear.
