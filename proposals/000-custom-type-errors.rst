.. proposal-number:: 

.. trac-ticket:: 

.. implemented:: 

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Custom type errors
==================

This proposes the addition of two combinators over ``Constraint``s, namely ::

    IfUndischarged :: Constraint -> ErrorMessage -> Constraint
    IfApart        :: Type -> Type -> Constraint -> Constraint -> Constraint

which enable library authors to provide custom error messages for their libraries.

In addition, this proposal also motivates changing the constraint solving order to prioritize arguments to functions over the check of the application itself.

This proposal does *not* address whether this facility should be used at all by the libraries bundled with GHC. This might be the topic of another proposal in the future, if this one gets accepted.


Motivation
------------
Haskell is well-known for its ability to embed DSLs, and the usage of its strong type system to guarantee invariants in those languages. The main disdvantage is that type errors become unwidely long and complex, and in many cases expose internals of the implementation. In short, the "ultimate abstraction" offered by EDSLs is broken when the code is ill-typed.

The community has already acknowledged this problem partially with the introduction of ``TypeError``. See `the original proposal <https://ghc.haskell.org/trac/ghc/wiki/Proposal/CustomTypeErrors>`_ and an `example package <https://github.com/turingjump/bookkeeper#readme>_`. But why should we stop here? There are many other error conditions we may want to customize. In this proposal we focus in other two.

Undischarged constraints
~~~~~~~~~~~~~~~~~~~~~~~~
These are error messages which are shown when a given constraint is left undischarged at the end of the solving process. The archetypal example is a type class instance constraint which is not satisfies. For example, if it turns out that ``(==)`` is applied to a type for which no ``Eq`` instance exists, the error message could become ::

  * X does not implement equality.
    Maybe you want to derive it using `deriving Eq`?
  * In the expression: ...

We definitely could include a generic framework to hint auto-derivation of built-in classes such as ``Eq`` or ``Ord``. But in the current Haskell ecosystem some libraries use generic deriving, others Template Haskell, and so on, so a generic facility seems useful.

Inequalities
~~~~~~~~~~~~
The ability to attach a custom error message whenever an equality constraint fails is helpful in a variety of scenarios. The simpler one is simply explaining that two arguments should coincide. For example, if ``(==)`` is applied to terms of different type, we could say ::

  * == is applied to arguments of different types `Bool` and `Int`
  * In the expression: ...

Another possibility is to give a description of the involved types in domain-specific terms. Take for example the `diagrams library <https://hackage.haskell.org/package/diagrams-core-1.4/docs/Diagrams-Core.html#t:QDiagram>`_, it defines a ``QDiagram b v n m`` type indexed by back-end, vector space, number field and annotation types. If we try to combine two diagrams of different vector space, we get a message similar to ::

  * Couldn't match expected type 'V2' with actual type 'V3'

Instead, we could express this message in a more meaningful way ::

  * Couldn't match vector space 'V2' with 'V3'


Proposed Change Specification
-----------------------------
This proposal defines two new "constraint combinators" to express constraint behavior in the event of a type error.

The first combinator is ``IfUndischarged``. In short, ``IfUndischarged c msg`` shows the message when the constraint ``c`` is left undischarged by the constraint solver. A simple example using ``Eq`` ::

    (==) :: IfUndischarged (Eq a) (ShowType a :<>: Text " does not implement equality."
                                  :$$: Text "Maybe you want to derive it using `deriving Eq`?")
         => a -> a -> Bool

If it turns out that ``(==)`` is applied to a type for which no ``Eq`` instance exists, the error message becomes ::

  * X does not implement equality.
    Maybe you want to derive it using `deriving Eq`?
  * In the expression: ...

The second combinator is ``IfApartUnsafe`` (the naming shall become clear later). In short, ``IfApartUnsafe a b no yes`` rewrites to ``no`` if ``a`` and ``b`` are apart *at that point in the solving process* and to ``yes`` otherwise. Note that this is *extremely unsafe*, since any knowledge about ``a`` and ``b`` is lost if the second branch is taken, and for that reason we should only expose ::

    type IfApart a b no yes = IfApartUnsafe a b no (a ~ b, yes)

Using this synonym we can give a better type to ``(==)`` ::

    (==) :: IfApart a b
              (TypeError (Text "== is applied to arguments of different types"))
              (IfUndischarged (Eq a) (ShowType a :<>: ...))
         => a -> b -> Bool

One of the nice things about `IfUndischarged` and `IfApartUnsafe` being combinators is that we can abstract error patterns using type-level programming such as type families. See the following `prototype implementation <https://git.science.uu.nl/f100183/ghc/blob/wip/when-not/libraries/base/GHC/TypeErrors.hs>`_ of a proposed ``GHC.TypeErrors`` module. Using it we could write ::

    (==) :: CustomErrors [ a :~/: b :=>: Text "== is applied to arguments of different types"
                         , Undischarged (Eq a) :=>: ShowType a :<>: Text " does not implement equality."]
         => a -> b -> Bool

You might have noticed that I have emphasized "at that point in the solving process" when describing ``IfApartUnsafe``. The reason is that we cannot defer indefinitely every apartness check, so sometimes we need to take the ``yes`` branch and continue. Thus, the order in which constraints are solved becomes quite important. I propose to change the default from "don't care" to an order in which constraints coming from arguments to a function are given priority with respect to those coming from the application itself.

Updates to the solver
~~~~~~~~~~~~~~~~~~~~~
Inside GHC solver each constraint would come with an optional attached message and a priority number. Whenever a new item has to be taken out of the work list, the ones with highest priority should come before. If more than one constraint have the same priority, then the choice is done as now -- first canonicalization, then interaction, and so on.

Whenever the solver finds a ``IfUndischarged c msg``, it emits the constraint ``c`` with the additional message attached. This message has no influence on the rest of the solving process, but if it ends with errors and the constraint should be reported, the message is used. Indeed, we could see its definition as (note that ``UndecidableSuperClasses`` is required) ::

    class c => IfUndischarged c msg

Priorities and messages are inherited by constraints stemming from another one by solving. For example, if we have ``IfUndischarged (Eq [a]) msg``, we would end up with a constraint ``Eq a`` where the message ``msg`` is kept.

Finally, the solver has to be updated with the following new rules for inequality checks ::

    [G] IfApartUnsafe a b no yes ==> yes
    [W] IfApartUnsafe a b no yes
          | if a and b are apart ==> no
          | otherwise            ==> yes

Indeed, we could see `IfApartUnsafe` as defined whenever compilation is successful as (again, using ``UndecidableSuperClasses``) ::

    class yes => IfApartUnsafe a b no yes


Effect and Interactions
-----------------------
In principle, there should be no effect for already-existing code. Right now people do not assume any specific ordering on the constraint solver, which means that we can change it as explained here.

If implemented as-is, using ``(==)`` with the aforementioned type signature would impose a performance cost. Whereas before we just needed a ``Eq`` dictionary and a type application, now we need two of them and a complex proof of the big constraint. Luckily, this is easy to fix by using an ``INLINE`` pragma ::

  {-# INLINE (==) #-}
  (==) :: CustomErrors [ ... ] => a -> b -> Bool
  (==) = eq

where ``eq`` is the function with the simpler type signature. Preliminary research shows that this solves most of the problems, but no check has been done for interaction with other pragmas or optimizations.

One problematic aspect of this way to encode custom errors is that the amount of type variables in a signature tends to grow (for ``(==)`` we have already duplicated it!). This might or might not be a problem, depending on who you ask.

Finally, it seems that the approach we use for solving interacts in weird ways with ambiguity checking. In our prototype built over branch 8.3, with some complex usages of ``CustomErrors`` some signatures are marked as ambiguous (and thus require ``AllowAmbiguousTypes`` which are not). This might require improvements to the ambiguity checker, maybe making it aware of ``IfUndischarged`` and ``IfApart``.


Costs and Drawbacks
-------------------
At first glance, the development and maintenance costs should be small.

In principle, this extension should make libraries easier to learn. One problem remains, though: how to show type signatures in an understandable way. For most people ::

    (==) :: Eq a => a -> a -> Bool

conveys much more meaning that the long signature. Whether we should show one or the other in the interactive environment is discussed as an unresolved questions.

One drawback of this approach is that it requires library authors to use ``DataKinds`` whenever they want to use custom errors. As a side-effect, some of the type definitions in a module might be promoted to the kind level. Right now we do not have a way to control data type promotion, so this is unavoidable.


Alternatives
------------
In this proposal, ``IfUndischarged`` is associated to a constraint in each usage site. Another alternative is having the message attached to the type class itself. This is the route taken by Scala with their `implicitNotFound annotation <http://www.scala-lang.org/api/2.12.0/scala/annotation/implicitNotFound.html>`_

That alternative, however, is less flexible than the current proposal, since you could always export a new constraint which includes the message:

    type Eq' a = IfUndischarged (Eq a) (Text "blah blah")

Note, however, that this approach has the drawback of having different names for the annotated (``Eq'``) and original (``Eq``) type classes. The user has to remember that the latter should be used when writing a new instance, but the former when writing the signature of a function.


Unresolved questions
--------------------

Messages or hints?
~~~~~~~~~~~~~~~~~~
In the description above, I have replaced the default error messages by custom ones completely. Maybe a better choice, especially for ``IfUndischarghed``, it to add the information as a *hint* or *suggestion*, in addition to the default message.

How to print the annotated signature?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the current prototype, as a side-effect of the way in which GHCi computes the type to print, the signature of a function is always simplified. In this case, that means that no trace of ``IfUndischarged`` or ``IfApart`` is shown. Is this the right behavior? (I think it is)

The same question should be asked about Haddock. Maybe the smallest, simplified signature should be the one in the main documentation, and the error information should get some specific markup. Of course, this means that now Haddock has to inspect the types of the documented values, something which is not done as of now.

Generalization
~~~~~~~~~~~~~~
What happens if we need to infer a type with a constraint which has an attached message? Do we at it using ``IfUndischarged``? This definitely seems like a wrong path, although it is also surprising that if I write:

    eq = myAnnotatedEq

then ``eq`` gets a different type signature than ``myAnnotatedEq``.

Kind errors
~~~~~~~~~~~
Using ``TypeInType`` it is also possible to apply this technique to kind (which are really type) errors. I haven't found any use case for this, however.


Implementation Plan
-------------------
I (@serras) have produced a prototype based on some point of the 8.3 branch, which is available `here <https://git.science.uu.nl/f100183/ghc/commits/wip/when-not>`_. I volunteer for implementing the final design coming from this proposal.