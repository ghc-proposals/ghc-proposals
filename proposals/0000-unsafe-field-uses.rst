Warning flag for unsafe use of partial fields
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/184>`_.
.. sectnum::
.. contents::

Currently, a `WARNING` or `DEPRECATED` pragma on a record field label triggers for any use of a field label -- accessing a value, creating a record, record update, pattern matches, and named field puns.
I propose that GHC add a mechanism whereby *unsafe* usages of record labels trigger warnings, and safe usages do not.
Safe usages include record creation, record pattern matches, ``NamedFieldPuns``, and ``RecordWildCards``.

Motivation
------------

Sum types are great. Record fields are great. However, the combination -- the combination is bad.
Consider this datatype.

::

   data Foo 
      = A { a :: Int } 
      | B { b :: Char }

This gives us a pair of unsafe accessor functions ``a :: Foo -> Int`` and ``b :: Foo -> Char``.
These functions will throw an error at runtime if used on the wrong constructors.

::

   λ> a (B 'c')
   *** Exception: No match in record selector a

The prudent Haskell developer, always afraid of runtime exceptions, will create a rule for the codebase: No record fields on sum types!
And all will be well.

Unless, of course, you're using the record fields for generic derivation of serialization.
Or you're primarily using the field labels in pattern matching, perhaps with ``NamedFieldPuns`` or ``RecordWildCards``.
Or you're using record creation syntax for these constructors.

All of these are "safe" usages of record fields, even in sum types. Observe the following behavior.

::

   main = do
      let d = A { a = 3 }
          e = B { b = 'c' }
          f = A { b = 'c' } -- ERROR

      case d f
         A { a } -> print a -- fine
         B { b } -> print b -- fine
         A { b } -> print b -- ERROR
         
GHC already correctly errors whenever we use a field unsafely for creation and pattern matching.

Then, the change requires significant boilerplate to manually write the generic code.
Any use of the record field for creation or punning requires a switch to position-dependent code, which is much less enjoyable to write.

We can get ``Generic``-driven derivation back by defining the record fields, but not exporting them.
Unfortunately, there's no way to recover the ``NamedFieldPuns``, ``RecordWildCards``, or safe record creation syntax this way.
You might think, "Ah! I know! I will use a ``WARNING`` pragma on the field labels. Then, no one can use them as a function, but perhaps GHC doesn't warn when they're used in safe contexts, like record creation or pattern matching."
Unfortunately, GHC does warn when the labels are used in *any* context.

So, there is no safe and convenient way to have record labels on sum types, all because they can be misused as accessor functions and in update syntax.

With the proposed fix, I would expect to see the following behavior.

::

   main = do
      let d = A { a = 3 } -- fine
          e = B { b = 'c' } -- fine
          x = a d -- WARNING
          y = d { a = 4 } -- WARNING

      case d of
         A { a = r } -> print r -- fine
         A { a } -> print a -- fine
         A {..} -> print a -- fine
         -- etc


Proposed Change Specification
-----------------------------

This "want" has a few possible strategies.

* A new pragma for partial sum types.
* A new warning flag that triggers on any unsafe usage of a partial record field.

New Pragma
++++++++++

This pragma could attach to a datatype or individual fields. It might have a name like.

::

   {-# NoUnsafeFieldUse Foo #-}
   data Foo = A { a :: Int } | B { b :: Char }

This would then issue a warning in any unsafe usage of a field, but safe usage would be permitted.

A New Flag
++++++++++

This solution takes the form of a new warning flag, ``-Wunsafe-field-uses``.
When this flag is enabled, GHC will issue a warning for any unsafe usage of a record selector.
The check looks like

1. Collect a set ``PartialLabels`` of record labels in sum types.
2. For any usage of a label in ``PartialLabels``, issue a warning if it used in an unsafe manner.

This requires the least amount of work to enable for a project -- it is a compile-time warning flag that can easily be added into a project configuration file.
It works for every single definition that fits the case, without additional boilerplate.

Effect and Interactions
-----------------------

Both of the above solutions allows a user to define a sum type with record fields and use the record fields safely (or, at least, receive a warning if used unsafely).
Neither solutions change any existing behavior.
Unless the user explicitly opts in to this behavior, they will notice no changes.

Costs and Drawbacks
-------------------

This proposal would require a modification to GHC's warning and error system.
It involves the creation of a new warning compiler flag, or a new source pragma.
I do not expect that it will add significant maintenance burden once implemented.
I do not expect that it will have any effect on the difficulty of learning the language.

Alternatives
------------

You can live with the possibility of a runtime error, unchecked by the compiler.

You can live with the boilerplate of manually defining all the stuff you'd get from ``Generics``.

You can live with never using record field labels.

Unresolved Questions
--------------------

Should we even allow this at all? 

Which solution is preferred?


Implementation Plan
-------------------

I volunteer to implement whichever solution gets chosen.
