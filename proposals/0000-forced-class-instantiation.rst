.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell

Forced Class Instantiation
==========================

A declaration
```haskell
instance force Cls Ty
```
instantiates any `Cls a` constraint encountered, including in error messages, otherwise ambiguos types.

Motivation
----------

I am teaching a Haskell class right now, and in the first few lectures, I had to tell them: „Whenever you see `Foldable t => … t a …` anywhere, just mentally replace this with `… [a] …`.”. Similarly, when discussing `IO` before discussing `Monad`, I had to tell them the same for these two. Finally, the first few weeks (because using [CodeWorld](http://code.world/haskell)), we turned on `OverloadedStrings`, but I really do not want want to see them `IsString a` and rather have them see `Text` in error messages.

The students coped, but it would be better if the compiler did that for them. So the main motiviation is to tell them to include a line like
```haskell
instance force Foldable []
```
in their file (students accept instructions to include stuff they do not understand yet, so this is fine) and get a lower entry level.

But it is useful not only for teaching. Especially
```haskell
instance IsString Text
```
is useful for anyone who does not want literals overloaded but really wants them to be monomorphically the desired `Text` type.

Proposed Change
---------------
I propose to introduce a declaration 
```haskell
instance force Cls Ty1 Ty2 …
```
which, restricted to the scope of one modules, hides the existance of the type class `Cls` to the user. This includes:
 
 * Inferred types.
 * Types printed with `:print`, even for imported identifiers.
 * Types printed with `:browse` or `:info` (`:info` might point out that the type is force-instantiated)
 * Type error messages.
 * Type inference: Ambiguities involving `Cls` should be resolved in favor of these types, (similar to but taking precedence over defaulting)

Occurrences of `Cls` that cannot be instantiated (e.g. `foo :: (forall t. Foldable t => t () -> Bool) -> …`) are left in place.

Drawbacks
---------

 * It might lead to more instead of less confusion if `:type Prelude.length` has different output depending on the current context.
 * Some keywords are required.
 * The interaction with `defaulting` might be confusing.

Alternatives
------------

Some of the above can be achieved using
```haskell
instance t ~ [] => Foldable t where
  …copies of all methods…
```
with judiuous use of allowing overlapping and incoherent instances, but would not affect, for example, `:browse`. Furthermore, copying all method definitions is ugly and might not be possible if the only way to get the original definition is via the class method.

Unresolved Questions
--------------------

Is there a better syntax?


