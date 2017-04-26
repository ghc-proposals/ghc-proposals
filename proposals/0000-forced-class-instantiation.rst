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

instantiates any ``Cls a`` constraint encountered, including in error messages, otherwise ambiguous types.

Motivation
----------

Error messages
~~~~~~~~~~~~~~

I am teaching a Haskell class right now, and in the first few lectures, I had to tell them: „Whenever you see ``Foldable t => … t a …`` anywhere, just mentally replace this with ``… [a] …``.”. Similarly, when discussing ``IO`` before discussing `Monad`, I had to tell them the same for these two. Finally, the first few weeks (because of using CodeWorld_), we turned on ``OverloadedStrings``, but I really do not want want to see them `IsString a` and rather have them see ``Text`` in error messages.

.. _CodeWorld: http://code.world/haskell

The students coped, but it would be better if the compiler did that for them. So the main motivation is to tell them to include a line like
::
  instance force Foldable []

in their file (students accept instructions to include stuff they do not understand yet, so this is fine) and get a lower entry level.

Remove polymorphism
~~~~~~~~~~~~~~~~~~~

But it is useful not only for teaching. Especially
::
  instance force IsString Text
is useful for anyone who does not want literals overloaded but really wants them to be monomorphically the desired ``Text`` type. This would avoid ambiguity when a string literal is passed to an overloaded function, e.g.
::
  print "Hello"

Proposed Change
---------------
I propose to introduce a declaration 
::
  instance force Cls Ty1 Ty2 …
  
The declaration is valid if the constraint ``Cls Ty1 Ty2`` can be solved from the empty context using any number of instances in scope. If not, as in ``instance force Eq [a]``, a usual error message (“Could not solve constraint Eq [a]: No instance [a]” or similar) is produced, and the program is rejected.

The declaration has the effect that, within the scope of one module, the existance of the type class ``Cls`` is hidden from the user. In particular:

* The user cannot use ``Cls`` in type signatures in this module, as it is out of scope. (The ``force instance`` declaration itself is exempt of this.)
* Any function imported from outside the module (including the methods of ``Cls``) with a type ``Cls a b => T[a,b]`` constraint now has type ``T[Ty1,Ty2]``. More general, a function imported from outside the module with a type ``Cls Ty1' Ty2' => T`` constraint now has a type that results from simplifying ``(Ty1' ~ Ty1, Ty2' ~ Ty2) => T``.
* If multiple ``force instance`` declarations are in scope, then first all of them are converted to ``~`` constraints, and then the resulting type is simplified. This can lead to an unusable type (``Int ~ Bool => …``). If the user tries to use such a function, then a helpful error message is provided.
* The type of functions with occurrences of ``Cls`` that cannot be instantiated (e.g. ``foo :: (forall t. Foldable t => t () -> Bool) -> …``) are left in place, with the likely effect that they cannot be used in this module.

This type change affects thus:
 
* Types printed with ``:print``, even for imported identifiers.
* Types printed with ``:browse`` or ``:info`` (``:info`` might point out that the type is force-instantiated)
* Type error messages.

If the type is changes for every imported function, then type inference is not affected. But it might be possible to achieve the same effect using a different  imlementation strategy that hooks into type inference.

If an imported function is re-exported using an explicit export list, it is exported with its original type. In other word, ``force instance`` does not affect re-exports in any way.

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

* Is there a better syntax?

* What happens with multiple `force instance` declarations. Is that unambiguous?


