.. proposal-number::

.. trac-ticket::

.. implemented::

Dollar type operator as default
===============================

Define the dollar operator (``type a $ b = a b``) as a default when using the
``TypeOperators`` extension.

Motivation
----------

The function application operator is often thought of as ‘that separator that
lets you get rid of brackets’. Although it's just an infix function, like any
other, and logically speaking, doesn't belong in a type signature, looking at
it from a naïve perspective, I expect to be able to use it in type signatures
with the same bracket-destroying effect. Obviously, that'd be silly, since it
is not a type operator, but it can be with ``TypeOperators``, and it could be
the main reason why that extension is used.

Proposed Change
---------------

When using the ``TypeOperators`` language extension, we should be able
to ``value :: IO $ Maybe Int`` rather than ``value :: IO (Maybe Int)``,
without having to define ``type a $ b = a b``. It should be defined by
default when I use the language extension.

Drawbacks
---------

It forces us to start thinking about a Prelude-like default operators for
the ``TypeOperators`` extension. It adds bloat to the extension and assigns
assigns it more than one responsibility (violating the single responsibility
principle).

Alternatives
------------

We could have a ``TypeOperatorsDefaults`` extension to add a Prelude for
type operators.

Unresolved Questions
--------------------

Are there any parts of the design that are still unclear? Hopefully this section
will be empty by the time the proposal is brought up for a final decision.
