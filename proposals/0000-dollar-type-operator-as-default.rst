.. proposal-number::

.. trac-ticket::

.. implemented::

Dollar type operator as default
===============================

Define the dollar type operator (`type a $ b = a b`) as a default when using
the `TypeOperators` extension.

Motivation
----------

Though, I have admittedly only used the `TypeOperators` extension once, I
think that the main reason it is used is to have the function application
operator available in type signatures. That's what I used it for.

Proposed Change
---------------

When using the `TypeOperators` language extension, we should be able
to `value :: IO $ Maybe Int` rather than `value :: IO (Maybe Int)`,
without having to define `type a $ b = a b`. It should be defined by
default when I use the language extension.

Drawbacks
---------

It forces us to start thinking about a Prelude-like default operators
for the `TypeOperators` extension. It adds bloat to the extension and
assigns it more than one responsibility (violating the SRP).

Alternatives
------------

We could have a `TypeOperatorsDefaults` extension to add a Prelude for
type operators.

Unresolved Questions
--------------------

Are there any parts of the design that are still unclear? Hopefully this section
will be empty by the time the proposal is brought up for a final decision.
