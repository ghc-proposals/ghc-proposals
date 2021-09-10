Proposal title
==============

.. author:: Brandon Chinn
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/436>`_.
.. contents::

``-XRecordWildCards`` does not give any warning or indication if you forget to use a field. This proposal adds syntax (optionally enabled by another extension) to give a warning if a field is unused.


Motivation
----------

Some functions require all the fields of a record to be used, or at least considered, e.g. ``hash`` or ``toJSON``, or even something more security-facing. Currently, if one uses ``RecordWildCards`` and they add a field to the record later on, nothing will tell the developer to take a look at these functions.

::

 data Foo = Foo { a :: Int, b :: Bool }
 
 instance ToJSON Foo where
   toJSON Foo{..} =
     object
       [ "a" .= a
       , "b" .= b
       ]
       
 instance Hashable Foo where
   hash Foo{..} = hash (a, b)
 
 -- adding a 'c' field does not raise any indication of needing to look at `toJSON` or `hash`

Existing workarounds are described in `this blog post <https://cs-syd.eu/posts/2021-09-10-undefined-trick>`_, all of which leave something to be desired:

* Positional arguments break when fields are reordered, which matters in functions like `toJSON`
* Using `undefined` is a bit of a hack, plus it fails `-Wunused-pattern-binds`. It also fails to explicitly say which field is unused (which isn't strictly necessary, but it's nice to have)

The `safe-wild-cards <https://hackage.haskell.org/package/safe-wild-cards>`_ package provides a Template Haskell solution to this, but it would be nice to support this natively.

Proposed Change Specification
-----------------------------

When ``-XRecordWildCards`` is enabled, in addition to supporting ``Foo{..}`` in a pattern match, also support ``Foo{..!}``, which desugars to ``Foo{a = a, b = b}``.

Examples
--------

Now, the example in the Motivation section could be written as

::

 instance ToJSON Foo where
   toJSON Foo{..!} =
     object
       [ "a" .= a
       , "b" .= b
       ]
       
 instance Hashable Foo where
   hash Foo{..!} = hash (a, b)
   
Notice the lack of any change, except for swapping ``Foo{..}`` for ``Foo{..!}``.


Effect and Interactions
-----------------------
The desugared syntax would raise an ``-Wunused-matches`` warning if a new field is added but not used.

As far as I know, the proposed ``Foo{..!}`` syntax doesn't conflict with any current syntax.


Costs and Drawbacks
-------------------
Since it's just syntax sugar, development "shouldn't" take long. Likewise, I don't see this adding much maintenance cost.

It's completely optional, so novice users can avoid this syntax completely.

Primary drawback is updating tooling/editor support for the new syntax.


Alternatives
------------

* Add an additional extension, e.g. ``-XRecordWildCardsStrict``, that implies ``-XRecordWildCards`` and enables this extension. I'm not opposed to this, but since the change doesn't break existing ``-XRecordWildCards`` syntax, I don't see a reason to break out yet another extension.

Unresolved Questions
--------------------


Implementation Plan
-------------------

Endorsements
-------------
