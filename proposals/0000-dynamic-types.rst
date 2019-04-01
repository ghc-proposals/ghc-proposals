Dynamic Typing
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/217>`_.
.. sectnum::
.. contents::

Haskell is widely regarded as a leading strongly typed, lazy, functional programming language.
Of course, we all know that it is also an excellent *imperative* programming language.
With ``-XStrict``, we can consider it to be an excellent *strict* programming language.
This proposal completes the cube by describing how Haskell can be an excellent *dynamic* programming
language by allowing users to opt out of our pesky type system.

Motivation
------------
`Many programmers prefer dynamic types <https://adtmag.com/articles/2019/01/08/tiobe-jan-2019.aspx>`_, and
we must wonder whether our static type system is holding back increased adoption of Haskell.

For example, perhaps we want to write this program::

  personIds = ["Richard", 4, "Simon", 12, "Stephanie", 15]
  getId per [] = "none"
  getId per (name:id:rest)
    | per == name = id
    | otherwise   = getId per rest

We *could* use an association list or a map, but why should we be forced to structure our data?

In addition, GHC's discipline around encapsulating side effects in monads can be frustrating. It would
be nice to write this::

  fact 0 = 1
  fact n =
    let _ = putStrLn ("Evaluating " ++ show n) in
    n * fact (n-1)

Proposed Change Specification
-----------------------------

1. Define a function in a new module ``GHC.Dynamic``::

     wrapDyn :: Typeable a => a -> Dynamic
     wrapDyn x
       | Just HRefl <- typeOf x `eqTypeRep` typeRep @Dynamic = x
       | otherwise                                           = Dynamic typeRep x

   Note that ``Dynamic`` already exists in ``Data.Dynamic``.

2. Introduce a new extension ``-XDynamicTypes`` that triggers the following behavior.

3. Insert a call to ``wrapDyn`` at every expression node in the program.

4. Include the new rule ``{-# RULES "wrapDyn" wrapDyn @Dynamic = id #-}``.

5. When GHC discovers a type equality error (say, that ``Int`` is expected but
   a ``Dynamic`` is found), insert a call to ``Data.Dynamic.fromDynamic`` in order to dynamically cast the
   ``Dynamic`` to the desired type. In the failure case, call ``Control.Exception.Base.typeError``.
   (If the type disagreement occurs under ``Functor``\s and/or ``Contravariant``\s and/or ``Profunctor``\s,
   lift the call to ``fromDynamic`` accordingly.)

6. When GHC needs to solve a class constraint, emit code to do a dynamic lookup for instances.
   This will examine the runtime type of the value in question and provide the appropriate instance.
   If no such instance exists (at runtime), call ``Control.Exception.Base.typeError``.

7. Introduce a new extension ``-XImpure`` that triggers the following behavior; it
   does not imply nor is implied by ``-XDynamicTypes``.

8. ``-XImpure`` implies ``-XStrict``.

9. Whenever a value of type ``IO a`` is forced, a call to ``unsafePerformIO`` is inserted.

10. Whenever a value of type ``IO a`` is used in a context expecting something of type ``a``,
    a call to ``unsafePerformIO`` is inserted. (If ``-XDynamicTypes`` is in effect, this rule
    takes precedence over the ``-XDynamicTypes`` interpretation.)

11. Whenever a value of type ``a`` is used in a context expected something of type ``IO a``,
    a call to ``pure`` is inserted. (If ``-XDynamicTypes`` is in effect, this rule
    takes precedence over the ``-XDynamicTypes`` interpretation.)
   
Effect and Interactions
-----------------------

* Both motivating examples work as expected.

* Note that this is different than ``-fdefer-type-errors``: a deferred type error is not
  treated as a dynamic type-check, but instead crashes the program at runtime. For example,
  half of the elements of the ``personIds`` list would throw an exception when evaluated
  (and it's unpredictable which half is the bad half!). With ``-XDynamicTypes``, all elements
  can be accessed without trouble.

* Note that ``-XImpure`` implies ``-XStrict``. Lazy evaluation and impurity do not mix well.
  Users may choose to specify ``-XNoStrict`` after ``-XImpure``, but they will get what they deserve.

* Inserting a call to ``unsafePerformIO`` when a value of type ``IO a`` is forced means that
  programmers can easily use impure code in pure code, simplifying programming greatly.

* The insertion of ``fromDynamic`` will handle most type errors, but not all. Any errors
  that remain may be deferred with ``-fdefer-type-errors``.

Costs and Drawbacks
-------------------

* Haskell would become so popular that voices of reason would be drowned out.
  
Alternatives
------------

* The best point of comparison is ``-fdefer-type-errors``, but that does not go nearly far enough, as
  described above.

Unresolved Questions
--------------------

* This proposal covers only dynamic types in terms, but not dynamic kinds in types. Can we lift
  these ideas to the type level, eliminating pesky kind errors?

* This proposal covers only type errors, but not other mistakes a programmer might make.
  Currently, ``-fdefer-type-errors`` also defers scoping errors. Perhaps we want ``-XDynamicTypes``
  to also defer scoping errors. Alternatively, we could imagine looking for similarly-spelled
  in-scope identifiers; if we find only one, just use it, keeping in the spirit of the rest
  of this proposal.

* This proposal does not address lexer or parser errors. These seem harder to address, but
  perhaps we can get by simply by ignoring the input string once we have such an error up
  until the next line that begins at column 0. With this approach, if an infrequently-used
  function has a parsing error, the application will still build and be useful.

Unacknowledgments
-----------------

This proposal bears striking similarity to content available `here <https://www.reddit.com/r/haskell/comments/b7p4xg/finally_dynamically_typed_programming_in_haskell/>`_, but was developed independently.
