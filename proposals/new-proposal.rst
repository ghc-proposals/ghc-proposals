Bring back PatternSignatures
============================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/119>`_.
.. sectnum::
.. contents::

This proposal de-deprecates ``PatternSignatures`` and gives it a useful meaning without pulling in ``ScopedTypeVariables``.

Motivation
------------
Originally, ``PatternSignatures`` was a an extension on its own, but at some point it started to imply
``ScopedTypeVariables`` and eventually was deprecated in favor of the latter. This has always bothered me
and I often find myself in situations where I need to use a pattern signature without having any need for scoped
type variables. This need has increased with more polymorphic functions in ``base`` (e.g. post FTP).

I too often thoughts “I should have raised this point when it was time, but it is too late now”. But maybe it is not
too late… hence this proposal.

The concrete motivation is to be able to write something like this::

   {-# LANGUAGE OverloadedStrings #-}
   foo :: Monad m => m Int
   foo = do
     list <- return ""
     return $ length list

Currently, this fails with (much shortened)::

    Test.hs:4:18: error:
        • Could not deduce (Data.String.IsString (t0 a0))
            arising from the literal ‘""’     
    Test.hs:5:12: error:
        • Could not deduce (Foldable t0) arising from a use of ‘length’

Ah, the FTP strikes again. So to fix this, I have to specify ``list``’s type. 
In Haskell98 I can add a type signature to the use of ``list``, but that is ugly: Types should
be declared where stuff is brought into scope! So I want to write::


   {-# LANGUAGE OverloadedStrings #-}
   foo :: Monad m => m Int
   foo = do
     list :: String <- return ""
     return $ length list
     
but I get::

    Test.hs:4:3: error:
        Illegal type signature: ‘String’
          Type signatures are only allowed in patterns with ScopedTypeVariables

Ok, that works, but why am I bothered with ``ScopedTypeVariables``? Furthermore, ``ScopedTypeVariables`` is not
conservative; it may actually break my program somewhere!

What I really want in this case is a pattern signature, and it would be nice if I could
just state that ``PatternSignatures``. Ergo:

Proposed Change Specification
-----------------------------

The language extensions ``PatternSignatures`` is no longer deprecated. If enabled, it allows signatures on patterns.

The pattern signature may contain type variables, but if ``ScopedTypeVariables`` is not enabled, the type variables are not in scope outside the pattern. They are also *not* abstracted over; instead they may unify with exiting (possibly internal) type variables, as specified by ``ScopedTypeVariables``.

The extension ``ScopedTypeVariables`` is not affected, i.e. it still implies ``PatternSignatures``. 

Effect and Interactions
-----------------------
With this change in place, I’d write::

   {-# LANGUAGE OverloadedStrings #-}
   {-# LANGUAGE PatternSignatures #-}
   foo :: Monad m => m Int
   foo = do
     list :: String <- return ""
     return $ length list
     
and be happy.

With ``PatternSignatures``, but without ``ScopedTypeVariables``, this code::

    f (x :: [a]) = blah

would succeed if it would suceed with ``ScopedTypeVariables``. The only difference
is that ``a`` is not brought into scope.

Costs and Drawbacks
-------------------

GHC already supports ``PatternSignatures``, albeit deprecated and implying ``ScopedTypeVariables``.
I expect the implementation cost to be small and consist of these steps:

* Make ``ScopedTypeVariables`` imply ``PatternSignatures``, but not the other way around.
* If a pattern signature is found, check that ``PatternSignatures`` is enabled.
* If a pattern signature binds a variable, check that ``ScopedTypeVariables`` is enabled.
* If neither is enabled and a pattern signature is found, tell the user to enable
  ``PatternSignatures`` or ``ScopedTypeVariables``, depending on whether a type variables
  is bound.

This proposal would break  only code that
 
* Is using ``-XPatternSignatures`` to get scoped type variables and
* has been ignoring the deprecation warning (which has been in place since GHC 6.10!)

Assuming people have followed the deprecation warning, then they are all already using
``PatternSignatures``, so no breakage occurs. Users who care about backward compatiblity 
can continue to use ``-XPatternSignatures``.

One commenter pointed out that he currently concludes from the presence of pattern signatures
(even if they do not mention type variables) that `ScopedTypeVariables` must be enabled; this
(possibly trained and subconscious) reasoning would have be un-learned.

Alternatives
------------

One could argue that ``ScopedTypeVariables`` need not imply ``PatternSignature`` (i.e. on its own, it could just
apply to ``forall`’ed type variables), but that would cause more breakage.
