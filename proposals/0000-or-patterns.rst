.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Or patterns
===========

We propose a new syntax extension for "or-patterns". An or pattern is
essentially a list of patterns, where patterns match exactly the same set of
variables of same types. The right hand side is shared by all these patterns,
and can refer to the variables matched by the patterns.

Main advantages of this extension are:

1. With or patterns we can avoid `_` wildcard patterns which can
   unintentionally match constructors as types are being extended.

2. It allows more code reuse as right hand sides can be shared by many
   patterns.

Motivation
----------

There are two motivations as summarised in the abstract.

**First,** `_` patterns make code harder to maintain. They essentially mean "match
every other pattern", which also includes "patterns that may be enabled in the
future" e.g. when a new constructor is added to a type.

In my experience this is rarely the intention. Usually, when a new constructor
is added, the programmer needs to revisit functions on the type and update them
accordingly. But if functions use `_` patterns this is not easy as she won't be
getting any compile time warnings about functions she needs to update.

This is also against the Haskell way of refactoring programs. Haskell is well
known for its features that make refactoring easier than most other languages
[1]_, but `_` patterns actually make refactoring harder.

As an example, GHC developers would know that adding a new constructor to an
existing type means many compile-run-edit cycles, with no compile-time help,
because of `_` patterns. Given that by default we don't get stack traces in
Haskell, and also GHC takes a lot of time to build, this wastes GHC developers'
time.

Or patterns solve this problem by allowing programmers to explicitly match a
list of constructors in a concise way. As an example, suppose we had this type:

::
    data T = T1 String | T2 | T3

We might want to write a function on this like

::
    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT _      = Nothing

Now suppose that some time later we add a new constructor:

::
    data T = T1 String | T2 | T3 | T4 String

We need to update `stringOfT` but unfortunately we don't get a warning because
we used a `_` pattern.

Or patterns solve the problem by allowing us to do this:

::
    stringOfT :: T -> Maybe String
    stringOfT (T1 s)        = Just s
    stringOfT (T2{} | T3{}) = Nothing

This function doesn't match `T4`, so we get our warning.

**Second,** or patterns allow more code reuse. In our previous example, we might
extend `stringOfT` to something like

::
    stringOfT :: T -> Maybe String
    stringOfT (T1 s) = Just s
    stringOfT (T4 s) = Just s
    stringOfT _      = Nothing

While this is not too bad (and we can always introduce new functions for similar
right hand sides), as the number of constructors increase this becomes
repetitive.

Or patterns can solve this problem like this

::
    stringOfT :: T -> Maybe String
    stringOfT (T1 s | T4 s) = Just s
    stringOfT (T2{} | T3{}) = Nothing

Now we have code reuse, and we get nice warnings next time a new constructor is
added.

Details
-------

.. TODO ..

Proposed Change
---------------

TODO

Drawbacks
---------

TODO

Alternatives
------------

TODO

Unresolved Questions
--------------------

TODO

.. [1] For a recent talk on this topic, see https://www.youtube.com/watch?v=_K6UAq4hjAs
