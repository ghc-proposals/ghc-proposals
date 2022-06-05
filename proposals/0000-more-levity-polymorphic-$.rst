
Levity-polymorphic ``$``
========================

.. author:: Keith Wygant
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

``$`` currently has type ``forall r a (b :: TYPE r). (a -> b) -> a -> b``. Change it to ``forall ra rb (a :: TYPE ra) (b :: TYPE rb). (a -> b) -> a -> b``.

Motivation
----------
Currently you can write expressions like ``(\(I# i) -> i) $ 1 + 1``, but not ``I# $ 1# +# 1#``. This change would allow both.


Proposed Change Specification
-----------------------------
The new definition of ``$``:
::
    infixr 0 $
    ($) :: forall ra rb (a :: TYPE ra) (b :: TYPE rb). (a -> b) -> a -> b
    ($) f = f

Examples
--------
As stated above, this would allow expressions like ``(2# *#) $ 1# +# 8#``.

Effect and Interactions
-----------------------
Prefix ``($) f``, and, with ``PostfixOperators``, postfix ``(f $)``, would become eta equivalent to ``f`` rather than wrapping ``f``.


Costs and Drawbacks
-------------------
The type of ``$`` may be slightly more difficult for novices to understand, but, since the current definition is already partially levity polymorphic, the mental cost of ``:: TYPE r`` is already there.

If code uses ``($) bottom`` to wrap a bottom-value function before forcing it in an expression (say, storing it in a strict data structure), that expression will become bottom. Hopefully nobody is doing this.

If the thunk ``(\ f x -> f x) g`` is more efficient than ``(\ f -> f) g``, this change will make code less efficient. But hopefully ``(\ f -> f) g`` with evaluate to ``g`` during compilation and, if anything, this change may remove a few unnecessary thunks.


Alternatives
------------
Leave ``$`` as is.

Unresolved Questions
--------------------
Does not address levity polymorphism in general, e.g. should ``.`` be as levity polymorphic as possible? What about ``Data.Function.&``?
