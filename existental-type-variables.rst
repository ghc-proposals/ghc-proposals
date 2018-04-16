Scoped Type variables for existentials
======================================
.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/96>`_.
.. sectnum::
.. contents::

This extension allows users to bind existential variables to data constructors in patterns.


Motivation
------------
Data types in Haskell can introduce existential type variables. For example, the type a below does not appear in the result type of the data constructor, so it is considered an “existential”.

::

 data T where
 MkT :: forall a. Show a => a -> T

In a pattern match, in order to bind the type variable, we need to use a type annotation.

::

 f :: T -> String
 f x = case x of
 MkT (y :: a) -> show y  -- binds a in RHS

With our proposed extension, a Haskell coder could use the syntax from visible type applications to bind the variable instead:

::

 f’ :: T -> String
 f’ x = case x of
 MkT @a y -> show (y :: a)

This is particularly important for ambiguous existential type variables.

::

   data U where
      MkU :: forall a. Typeable a => U

In this case, the type variable only appears in the constraint, not in any of the arguments of the data constructor. We can use a type applications to specify which type when we construct a ``U``

::

 x :: U
 x = MkU @String


However, we cannot use the pattern binding syntax to bind the type variable when we destruct this type.  The only solution to this problem is to use a ``Proxy``, which is clunky.

::

  data Proxy a = Proxy

  data U’ where
      MkU’ :: forall a. Typeable a => Proxy a -> U’

  g :: U’ -> String
  g = case x of
        MkU’ (p :: Proxy a) -> show (typeRep @a)
           -- using Type.Reflection

With this proposed extension we would not need a proxy and could instead write:

::

  g :: U -> String
  g = case x of
        MkU @a p -> show (typeRep @a) -- using Type.Reflection


Proposed Change Specification
-----------------------------

This sort of scoped type variable binding only applies to the existential arguments to a data constructor. Therefore, we must first specify the difference between “universal” and “existential” variables.

Consider a datatype declaration, where some data type ``T`` is parameterized by ``n`` type arguments (of kinds ``k1 .. kn``). Furthermore, each constructor ``Ki`` explicitly binds all type variables that appear in the constructor’s type (``b1 .. bm``), and the result type of ``Ki`` is ``T`` applied to ``n`` types, (``t1 .. tn``). (Data constructors Ki may also have a context and term arguments; these are not shown).

::

    data T :: k1 -> … -> kn -> * where
       Ki :: forall b1 .. bm. … -> T t1 .. tn

In this format, any type variable ``bj`` that appears, by itself, as an argument to ``T`` is considered universal; that is, ``bj`` equals some ``ti``. All others are existential.

For example,

::

  data T :: * -> * -> * where
    K1 :: forall a1 a2 b. b -> T a1 a2
             -- a1 and a2 are universal, b is existential
    K2 :: forall a. T a a
             -- a is universal
    K3 :: forall a b. T [a] [b]
             -- a and b are both existential
    K4 :: forall a b c. a -> T a a

When we pattern match an argument of type T, we can bind a prefix of the existential variables in the order that they appear, before binding all of the arguments of the constructor and binding each existential variable as a var or an underscore.

::

   f = case (K1 True) of
         K1 @b x ->      ...  -- cannot bind a1 or a2 (universal)
         K2 ->           ...  -- no existentials
         K3 @c ->        ...  -- binds a but not b
                              -- (don’t need to use same name)
         K4 @b x ->      ...  -- cannot bind c after writing x

Comments

1. The compiler would issue the same "unused binding" warnings as it does for regular bindings of variables in a pattern match.

2. Users can use @_ to avoid binding an existential.  For example, if we only wanted to bind the second existential above we could write

::

   f = case … of
         K3 @_ @c -> ...

3. Note that the current rules of GHC dictate that constructors must bind their existentials prenex.

- For example, the following datatype definition is *not* allowed

::

  data T where
     K :: Int -> forall a. a -> T

  f (K x @a y) =  ...

(This doesn’t disallow higher-rank arguments to data constructors.)

4. Universal variables cannot be bound with this mechanism.

5. This mechanism includes data constructor patterns found in case statements and function definitions. However, it does not include ``let`` declarations or ``where`` clauses because GHC does not allow existentials to be introduced at this point; allowing this would lead to skolem escape.

6. If the data constructor does not include a forall in its type, listing the order of the existential variables, then we determine the order of the existentials using left-to-right ordering of how the variables appear in the type. (If any variables' kinds mention other variables, the variables will be reordered by a stable topological sort.) This ordering is stable because the programmer wrote the type of the constructor explicitly. (This is similar to what happens with explicit type applications.)

::

 data T2 :: * -> * where
       -- user does not include an explicit forall
       MkT2 :: a -> b1 -> b2 -> T a

       -- this would be interpreted as if the user wrote
       MKT2’ :: forall a b1 b2. a -> b1 -> b2 -> T a

7. Old-style syntax

This proposal is compatible with the non-GADT syntax for existential variables. In that case, the existentials must be listed with an explicit forall (as always).

::

    data T3 a = forall b1 b2. MkT3 b1 b2

8. This extension should be enabled by a new ``ExistentialTypeVariables`` flag as it introduces another way that type variables can be brought into scope in the program. The flag would additionally enable the ``ScopedTypeVariables`` flag.

9. If the extension flag is not included, we will produce a warning at compile time asking the programmer if they meant to enable ``ExistentialTypeVariables`` if they were to write code that the extension would recognize as an attempt at binding an existential type variable.

10. The pre-existing mechanism of binding existential variables through type annotations on data constructor arguments will still be available.

11. Pattern synonyms can also bind existential variables during pattern matching. Note that the rules for which variables are unviersal versus which are existential are not the same as for data constructors. For pattern synonyms, the universal type variables are the user-specified universals, the variables mentioned in CReq (the constraints required to match the pattern, as discussed `here <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#typing-of-pattern-synonyms>`_), and all of the variables mentioned in the result type. The existentials are all the rest.

::

  data T a where
    MkT :: forall a b. (Show b) => a -> b -> T a

  pattern P1 :: (Num a, Eq a) => (Show b) => b -> T a
  pattern x = MkT 42 x

  f :: T Int -> String
  f (P1 @b x) = show (x :: b)


Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features.


This change makes the order that type variables are listed in a data constructor part of the interface to that data constructor. Client code that uses existential binding may break if library authors reorders the listing of existential variables in a forall.

This extension is compatible with the current treatment of scoped type variables. However, it may be incompatible with any future extension that binds universal variables (more in ‘Unresolved questions’).

Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.

This extension is potentially confusing for users as it only applies to “existential” type variables. Universal variables cannot be bound, but all of them must be listed in a “forall” when declaring the type of a data constructor.

Another confusing thing about the extension is that it brings with it an asymmetry with the TypeApplications extension (the @ syntax is used differently in a pattern than it is in an expression). Consider the following data constructor:

::

  data T a where MkT :: forall a b. a -> b -> T a

With both extensions enabled a programmer, a programmer would not be allowed to write the following code:

::

  f :: forall a. T a -> T a
  f (MkT @a @b x) = MkT @a @b x

and would instead have to write the following: 

::

  f :: forall a. T a -> T a
  f (MkT @b x) = MkT @a @b x

While the first seems more intuitive, the extension does not allow binding universal variables, so the second version is all that's left.

Alternatives
------------
List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.

The `first discussed alternative <https://mail.haskell.org/pipermail/ghc-steering-committee/2018-March/000452.html>`_ is to instead require that when deconstructing a pattern, the user must use the same @ binding syntax to bind the universals as well, while additionally allowing the user to "bind" the universals to their actual types (such as @Int). All universal variables that are bound implictly give an equality between the type variables and their actual types. The user would not be allowed to bind a type variable that would shadow a type variable that is already in scope. The extension avoids the programmer being confused by the constructor having a different number of type variables in its declaration than it does when being deconstructed. However, it still has an abrupt difference between universals and existentials (as with the original proposal). In the case of this proposed extension, the programmer may be confused on when they are actually allowed to "bind" with an actual type and when they are only allowed to bind to bring a type variable into scope. Additionally, the implicit type equalities that we get from binding the universals may confuse the programmer as well (the programmer now has to remember all of the implicit equalities in scope). 

The `second discussed alternative <https://mail.haskell.org/pipermail/ghc-steering-committee/2018-March/000454.html>`_ is a subset of the first. It requires that the user bind all the type variables (both existentials and universals) when deconstructing a pattern. As with the previous alternative, all universal variables that are bound implictly give an equality between the type variables and their actual types and the user would not be allowed to bind a type variable that would shadow a type variable that is already in scope. This proposed alternative, unlike the previous one, does not have the abrupt difference between universals and existentials that both the original proposal and the previous alternative do. This alternative still does not solve the issue of the programmer having to remember all of the implicit type equalities in scope and the confusion that this may bring with it.

The third alternative was informally discussed in the `pull request for this proposal <https://github.com/ghc-proposals/ghc-proposals/pull/96>`_, it suggests to require universals to be “bound” with @_ (and perhaps with their actual types @Int) to avoid confusion. This is a valid point, but requiring them to be "bound" in this way could be argued to be confusing to a programmer as well (the @ binding syntax would be used in two different contexts in the same pattern). Additionally, binding only existential variables avoids the clutter that requiring universal to be "bound" as well involves.


Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


- This proposal does not include binding “universal” variables outside of data constructors. The following examples would not be supported.

::

  foo :: forall a. a -> a
  foo @b x = (x :: b)


  (\ @a x -> (x :: a)) :: forall b. b -> b


Although this extension would also be useful, it is a separate feature.

Implementation Plan
-------------------

Emmanuel “Emma” Suarez has volunteered, with mentorship by Richard Eisenberg and Stephanie Weirich
