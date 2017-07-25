.. proposal-number:: 

.. trac-ticket:: 

.. implemented:: 

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/59>`_.

.. contents::

Custom type errors
==================

This proposes the addition of a combinator to decorarate a ``Constraint`` with a custom message, namely ::

    WithMessage :: Constraint -> ErrorMessage -> Constraint

In addition, this proposal also motivates fixing the solving of constraints in some degree to obtain the maximum gain from these error messages.

This proposal does *not* address whether this facility should be used at all by the libraries bundled with GHC. This might be the topic of another proposal in the future, if this one gets accepted.

Further information (with a more academic flavor) is available in `this draft <http://www.staff.science.uu.nl/~f100183/type-errors-draft.pdf>`_ and this `presentation at Curry On! 2017 <https://www.youtube.com/watch?v=LuqSkWOcnSA>`_.


Motivation
----------
Haskell is well-known for its ability to embed DSLs, and the usage of its strong type system to guarantee invariants in those languages. The main disdvantage is that type errors become unwidely long and complex, and in many cases expose internals of the implementation. In short, the "ultimate abstraction" offered by EDSLs is broken when the code is ill-typed.

The community has already acknowledged this problem partially with the introduction of ``TypeError``. See `the original proposal <https://ghc.haskell.org/trac/ghc/wiki/Proposal/CustomTypeErrors>`_ and an `example package <https://github.com/turingjump/bookkeeper#readme>`_. But why should we stop here? There are many other error conditions we may want to customize.

For example, if it turns out that ``(==)`` is applied to a type for which no ``Eq`` instance exists, the error message could become ::

  * X does not implement equality.
    Maybe you want to derive it using `deriving Eq`?
  * In the expression: ...

We definitely could include a generic framework to hint auto-derivation of built-in classes such as ``Eq`` or ``Ord``. But in the current Haskell ecosystem some libraries use generic deriving, others Template Haskell, and so on, so a generic facility seems useful.

Another possibility is to give a description of the involved types in domain-specific terms. Take for example the `diagrams library <https://hackage.haskell.org/package/diagrams-core-1.4/docs/Diagrams-Core.html#t:QDiagram>`_, it defines a ``QDiagram b v n m`` type indexed by back-end, vector space, number field and annotation types. If we try to combine two diagrams of different vector space, we get a message similar to ::

  * Couldn't match expected type 'V2' with actual type 'V3'

Instead, we could express this message in a more meaningful way ::

  * Couldn't match vector space 'V2' with 'V3'


Proposed Change Specification
-----------------------------
This proposal defines a constraint combinator which attaches a custom error message to a constraint. A simple example using ``Eq`` follows ::

    (==) :: Eq a `WithMessage` ShowType a :<>: Text " does not implement equality."
                                  :$$: Text "Maybe you want to derive it using `deriving Eq`?"
         => a -> a -> Bool

If it turns out that ``(==)`` is applied to a type for which no ``Eq`` instance exists, the error message becomes ::

  * X does not implement equality.
    Maybe you want to derive it using `deriving Eq`?
  * In the expression: ...

The same constraint combinator can be applied to equality constraints ::

    (==) :: a ~ b `WithMessage` Text "== is applied to arguments of different types"
         => a -> b -> Bool

Inside GHC solver each constraint would come with an optional attached message. Whenever the solver finds a ``WithMessage c msg``, it emits the constraint ``c`` and records the message. This message has no influence on the rest of the solving process whatsoever, except in the case in which that constraint ought to be reported as an error. Indeed, we could see its definition as (note that ``UndecidableSuperClasses`` is required) ::

    class c => WithMessage c msg


Problems with equalities
~~~~~~~~~~~~~~~~~~~~~~~~
The idea is essentially the same as in section 4 of the linked draft paper, but let me describe a simpler scenario which shows the problem. Suppose we decorate the ``(++)`` function as follows ::

    (++)
      :: ( lst1 ~ [a] `WithMessage` Text "The first argument is not a list"
         , lst2 ~ [b] `WithMessage` Text "The second argument is not a list"
         , a ~ b `WithMessage` Text "The types of the lists do not coincide: "
                                :$$: ShowType a :<>: Text " versus " :<>: ShowType b ()
       => lst1 -> lst2 -> [a]

I think this is the kind of custom errors we should expect DSL authors to write. And by writing those, the expected error messages produced by the compiler should be ::

    > True ++ "a"
    <interactive>: error:
        * The first argument is not a list
        * In ...

    > [True] ++ "a"
    <interactive>: error:
        * The types of the lists do not coincide:
          Bool versus Char
        * In ...

Take the last expression, ``[True] ++ "a"``, and let's write down the constraints generated by GHC ::

    1. lst1 ~ [a]     |
    2. lst2 ~ [b]     > from the function 
    3. a    ~ b       |

    4. lst1 ~ [Bool]  > from the first argument

    5. lst2 ~ [Char]  > from the second argument

Note that all constraints are equalities of the form ``variable ~ thing``, so as far as I know the solver will non-deterministically choose the order in which solve the constraints. The happy path is to start with 4 and 5, which replace ``lst1`` and ``lst2`` in constraints 1 and 2, respectively to obtain:

    1'. [Bool] ~ [a]
    2'. [Char] ~ [b]

The next step is considering 1' and 2', which are simplified to ``a ~ Bool`` and ``b ~ Char`` respectively. Once those are substituted, constraint 4 has effectively become ``Bool ~ Char``, which is an error, and we report the message "The types of the lists do not coincide".

Alas, there is another way to go. After getting 1' and 2', we could also consider constraint 4. This gives us the slightly different set of constraints ::

    1''. [Bool] ~ [a]
    2''. [Char] ~ [a]

From 1'' we obtaint ``a ~ Bool``, which is then substituted in 2'' to get ``[Char] ~ [Bool]``. Boom! Here is the error! But the undischarged message is that from the initial second constraint, so the user sees the message "The second argument is not a list". This is clearly wrong!

There are two problems to face to give a solution:

1. We should not give the error "The second argument is not a list", even if this is the only left constraint. This is clearly not the intention of the DSL writer.
2. We should try, as much as possible, to consider the constraint ``a ~ b`` at a later stage than ``lst1 ~ [a]`` and `lst2 ~ [b]`.

Propagation of messages
~~~~~~~~~~~~~~~~~~~~~~~
The solution to problem (1) is to limit the propagation of error messages when a constraint is simplified. The rule is that for every constraint ``C`` which is simplified into a set of constraints ``D1, ..., Dn``

1. If the constraint ``C`` is of the form ``f t1 ... tn ~ g s1 ... sn``, the constraint ``f ~ g`` (the constraint among heads) inherits the message, whereas all the ``t ~ s`` start blank.
2. For any other constraint, the constraint is inherited if the simplification generates only one constraint. This covers the case where ``Eq [a]`` is simplified to ``Eq a``. My guess is that such a linear path of constraints still relate well to the original message.

Constraint priorities
~~~~~~~~~~~~~~~~~~~~~
The solution to (2) is an heuristic to prioritize some constraints among others, so that the maximum amount of error messages are reported. The rules are:

1. A constraint ``a ~ t1`` must be considered before ``a ~ t2`` if ``t1`` contains a type constructor as its head and ``t2`` does not.
2. A constraint ``a ~ t1`` must be considered before ``b ~ t2`` if ``t1`` contains ``b`` as free variable.

Any other constraints are ordered as usual. In particular, this proposal does not say anything about constraints of the form ``F a ~ G b`` where ``F`` and ``G`` are type families.


Effect and Interactions
-----------------------
In principle, there should be no effect for already-existing code. Right now people do not assume any specific ordering on the constraint solver, which means that we can change it as explained here.

One problematic aspect of this way to encode custom errors is that the amount of type variables in a signature tends to grow (for ``(==)`` we have already duplicated it!). This might or might not be a problem, depending on who you ask.

Finally, it remains to be seen whether ``WithMessage`` imposes any performance cost. In principle it should not be the case, since the run-time representation of ``C `WithMessage` m`` should be the same as ``C`` itself.


Costs and Drawbacks
-------------------
At first glance, the development and maintenance costs should be small.

In principle, this extension should make libraries easier to learn. One problem remains, though: how to show type signatures in an understandable way. For most people ::

    (==) :: Eq a => a -> a -> Bool

conveys much more meaning that the long signature. Whether we should show one or the other in the interactive environment is discussed as an unresolved questions.

There is a downside to the implementation. As far as I understand, once GHC chooses a constraint ``t1 ~ t2``, it unifies as much as possible. For example, ``[(Int,Bool)] ~ [(a,b)]`` will end up unifying ``a`` with ``Int`` and ``b`` with ``Bool``. But for this approach to work, it should instead put the constraints ``a ~ Int`` and ``b ~ Bool`` back in the bag of constraints, instead of performing unification directly, because maybe another constraint should be prioritized.


Alternatives
------------
In this proposal, ``WithMessage`` is associated to a constraint in each usage site. Another alternative for type classes is having the message attached to the class itself. This is the route taken by Scala with their `implicitNotFound annotation <http://www.scala-lang.org/api/2.12.0/scala/annotation/implicitNotFound.html>`_

That alternative, however, is less flexible than the current proposal, since you could always export a new constraint which includes the message ::

    type Eq' a = Eq a `WithMessage` Text "blah blah"

Note, however, that this approach has the drawback of having different names for the annotated (``Eq'``) and original (``Eq``) type classes. The user has to remember that the latter should be used when writing a new instance, but the former when writing the signature of a function.


Unresolved questions
--------------------

Messages or hints?
~~~~~~~~~~~~~~~~~~
In the description above, I have replaced the default error messages by custom ones completely. Maybe a better choice is to add the information as a *hint* or *suggestion*, in addition to the default message.

How to print the annotated signature?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the current prototype, as a side-effect of the way in which GHCi computes the type to print, the signature of a function is always simplified. In this case, that means that no trace of ``WithMessage``. Is this the right behavior? (I think it is)

The same question should be asked about Haddock. Maybe the smallest, simplified signature should be the one in the main documentation, and the error information should get some specific markup. Of course, this means that now Haddock has to inspect the types of the documented values, something which is not done as of now.

Generalization
~~~~~~~~~~~~~~
What happens if we need to infer a type with a constraint which has an attached message? Do we at it using ``WithMessage``? This definitely seems like a wrong path, although it is also surprising that if I write ::

    eq = myAnnotatedEq

then ``eq`` gets a different type signature than ``myAnnotatedEq``.

Kind errors
~~~~~~~~~~~
Using ``TypeInType`` it is also possible to apply this technique to kind (which are really type) errors. I haven't found any use case for this, however.


Implementation Plan
-------------------
I (@serras) have produced a prototype based on some point of the 8.3 branch, which is available `here <https://git.science.uu.nl/f100183/ghc/commits/wip/when-not>`_. I volunteer for implementing the final design coming from this proposal.