.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell

This is a GHC proposal `under discussion`_.

.. _`under discussion`: https://github.com/ghc-proposals/ghc-proposals/pull/23

Forced Class Instantiation
==========================

A declaration
::
  instance force Cls Ty

instantiates any ``Cls a`` constraint encountered, including in error messages, otherwise ambiguos types.

Motivation
----------

I am teaching a Haskell class right now, and in the first few lectures, I had to tell them: „Whenever you see ``Foldable t => … t a …`` anywhere, just mentally replace this with ``… [a] …``.”. Similarly, when discussing ``IO`` before discussing `Monad`, I had to tell them the same for these two. Finally, the first few weeks (because of using CodeWorld_), we turned on ``OverloadedStrings``, but I really do not want want to see them `IsString a` and rather have them see ``Text`` in error messages.

.. _CodeWorld: http://code.world/haskell

The students coped, but it would be better if the compiler did that for them. So the main motiviation is to tell them to include a line like
::
  instance force Foldable []

in their file (students accept instructions to include stuff they do not understand yet, so this is fine) and get a lower entry level.

But it is useful not only for teaching. Especially
::
  instance IsString Text
is useful for anyone who does not want literals overloaded but really wants them to be monomorphically the desired ``Text`` type.

Proposed Change
---------------
I propose to introduce a declaration 
::
  instance force Cls Ty1 Ty2 …
  
The declaration is valid if the mentioned instance exists (i.e. if the constraing ``Cls Ty1 Ty2`` can be completely solved). If not, as in ``instance force Eq [a]``, a usual error message (“Could not solve constraint Eq [a]: No instance [a]” or similar) is produced, and the program is rejected.

The declaration has the effect that, within the scope of one module, the existance of the type class ``Cls`` is hidden from the user, by consistenly instantiationg it with the instance given in the declaration.

The affects:
 
* Inferred types.
* Types printed with ``:print``, even for imported identifiers.
* Types printed with ``:browse`` or ``:info`` (``:info`` might point out that the type is force-instantiated)
* Type error messages.
* Type inference: Ambiguities involving ``Cls`` should be resolved in favor of these types, (similar to but taking precedence over defaulting)

Occurrences of ``Cls`` that cannot be instantiated (e.g. ``foo :: (forall t. Foldable t => t () -> Bool) -> …``) are left in place.

Examples
--------

All the following examples have ``instance force Foldable []`` in scope.

* ::

    good = length . filter odd

  Works as now. The inferred type of ``good``, e.g. printed with ``-fwarn-missing-signatures``, is ``Num a => [a] -> Int``.

* ::

    bad1 = length (Just True)
    bad2 = length 1
    bad3 = length (1,2,3)
    
  Does not work. Error messages is the same as if ``length`` had type ``[a] -> Int``.
  
* ::

    bad4 :: Foldable f => f a -> f a
    
  Does not work. In the scope of a ``instance force Cls …``, the class ``Cls`` is effectively not in scope. (But the error message should be more specific, i.e. ``Constraint Foldable cannot be used in line 23 due to forced instantiation on line 10``.

* ::

    instance Foldable Maybe where …
    
  Does not work either, for the same reasons as above (``Foldable`` effectively not in scope).
  
* ::

   module MonoLength (length) where
   import qualified Preldue 
   instance force Foldable []
   length = Prelude.length
   
  Works, and exports ``length`` with type ``[a] -> Int``.

* ::

   module MonoLength (length) where
   import Prelude (length)
   
  Works, and re-exports ``length`` with type ``Foldable f => f a -> Int``.
  
  (One could envision this re-export exporting a monomorphic ``length``  that would, if imported somewhere along the original ``lenght``, be shadowed by that, but that would be a pretty different feature and proposal.)
  
* ::

    foo = withFoldable (length . filter id)
 
  where ``withFoldable`` is an imported function with higher rank type ``(forall f. Foldable f => f Bool -> Int) -> Int``. This fails. There are two ways of describing (and possibly implementing) this failure:
  
  1. Simply because in this module, it is not possible to write an expression with a ``Foldable`` instance, ``withFoldable`` cannot be used, and the type error message is the usual message one gets when passing a monomorphic thing to such a polymorphic function.
  2. Already the mention of ``withFoldable`` fails, because the compiler detects a use of ``Foldable`` that it cannot instantiate::
  
      Cannot use withFoldable :: (forall f. Foldable f => f Bool -> Int) -> Int
      as the parameters of the constraint Foldable in its type cannot be instnatiated to []
      as requested by instance force Foldable [] on line 10.
  

Drawbacks
---------

* It might lead to more instead of less confusion if ``:type Prelude.length`` has different output depending on the current context.
* Some keywords are required.
* The interaction with ``defaulting`` might be confusing.

Alternatives
------------

Some of the above can be achieved using
::
  instance t ~ [] => Foldable t where
    …copies of all methods…

with judiuous use of allowing overlapping and incoherent instances, but would not affect, for example, ``:browse``. Furthermore, copying all method definitions is ugly and might not be possible if the only way to get the original definition is via the class method.

Unresolved Questions
--------------------

Is there a better syntax?


