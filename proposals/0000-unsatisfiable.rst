Unsatisfiable
=============

.. author:: Adam Gundry
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/433>`_.
.. sectnum::
.. contents::

This proposal introduces a new built-in constraint form, ``Unsatisfiable``, as a
mechanism for introducing custom type error messages. It is similar to the
existing ``TypeError`` built-in type family, but it is more predictable when
errors are reported.


Motivation
----------

GHC allows users to define custom type error messages using the ``TypeError``
type family.  This is defined (in ``GHC.TypeLits``) as follows::

  type family TypeError (a :: ErrorMessage) :: b where

``TypeError`` is poly-kinded, so it can be instantiated wherever needed:
typically either on the right-hand side of a type family definition to mark an
invalid case, or in a context (instantiating it at kind ``Constraint``) to
indicate that a constraint should not be solved.

When GHC is reporting custom type errors it renders the user-supplied
``ErrorMessage`` argument in place of the usual type error message. The details
of the ``ErrorMessage`` type are not crucial here: it allows concatenation of
literal strings and pretty-printed types.  For further details of the current
design, see the `GHC user's guide on TypeError
<https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/type_errors.html>`_.

However, type families do not currently enjoy a clearly-defined operational
semantics. It is not well specified under what circumstances GHC will reduce
type families during the constraint solving stage of type inference, so it is
not obvious when a ``TypeError`` will be encountered and reported as an error.
In practice, GHC solves constraints normally, then when reporting unsolved
constraints, it searches the constraint type for occurrences of ``TypeError``
and reports any that it finds.  Unfortunately, this can lead to surprising
behaviour, as the following subsections describe.



``TypeError`` can report errors too lazily
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is a tempting but problematic use of ``TypeError``, because it
does not report an error when one would be expected::

  type family NotCharBad x :: Type where
    NotCharBad Char = TypeError (Text "Oops")
    NotCharBad x    = x

  f :: x -> [NotCharBad x] -> [NotCharBad x]
  f _ r = r

  oops :: Bool
  oops = null (f 'x' [])

The intention is that ``f`` should not be callable with ``x ~ Char``.  However
type-checking ``oops`` does not depend on how ``NotCharBad Char`` reduces, so
the ``TypeError`` is not triggered.

See `#13775 <https://gitlab.haskell.org/ghc/ghc/-/issues/13775>`_ for another
example of a type error being reported too lazily.

Where it is important that certain inputs are prohibited, a better pattern is to
use type families that return constraints::

  type family NotCharGood x :: Constraint where
    NotCharGood Char = TypeError (Text "Oops")
    NotCharGood x    = ()

  g :: NotCharGood x => x -> [x] -> [x]
  g _ r = r

Now type-checking ``null (g 'x' [])`` will need to solve the ``NotCharGood
Char`` constraint, and hence will trigger the ``TypeError``.


``TypeError`` can report errors too eagerly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous subsection, we saw a program that did not report a custom type
error when one would be expected, and that this can be avoided by using
``TypeError`` at kind ``Constraint``.  However, the opposite problem exists as
well: sometimes custom type errors are reported when they are not expected.

As a simple example, it is not possible to directly write a function which will
trigger a type error when used, because this is rejected at the definition
site::

  foo :: TypeError (Text "Don't call foo") => Int
  foo = 0

As a more substantial example, consider the following (where ``If`` is the type
family from ``Data.Type.Bool``)::

  -- Using a type family rather than a type synonym to work around #20181
  type family ExpectTrue x :: Constraint where
    ExpectTrue x = If x (() :: Constraint) (TypeError (Text "Input was False!"))

  h :: ExpectTrue x => proxy x -> ()
  h _ = ()

  -- This works:
  eg1 _ = h (Proxy @True)

  -- This yields an error (expected):
  eg2 _ = h (Proxy @False)

  -- This yields an error (unexpected):
  eg3 p = h p

Here ``h`` is supposed to be called with ``x ~ True``.  As one would expect,
explicitly instantiating ``x ~ True`` allows it to be called, while
instantiating ``x ~ False`` triggers the error.  However, ``eg3`` demonstrates
that the error is triggered also when ``x`` is unconstrained.  This is because
GHC infers that::

  eg3 :: If x () (TypeError (Text "Input was False!")) => proxy x -> ()

Even though ``x`` is not yet known, and the ``If`` type family application may
reduce to eliminate the ``TypeError`` depending on the value of ``x``, GHC still
reports the occurrence of the ``TypeError``.

See `#14771 <https://gitlab.haskell.org/ghc/ghc/-/issues/14771>`_ and `#16906
<https://gitlab.haskell.org/ghc/ghc/-/issues/16906>`_ for more examples of
errors being reported too eagerly.




``TypeError`` can report too many errors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One of the uses of ``TypeError`` is to point out that a class instance is
impossible, with a domain-specific error message. For example::

  class ReflexiveEq a where
      reflexiveEq :: a -> a -> Bool

  instance TypeError (Text "Equality is not reflexive on Double") => ReflexiveEq Double where
      reflexiveEq _ _ = False

This displays the custom type error when trying to use the instance::

  ghci> reflexiveEq 0 (0 :: Double)

  <interactive>:28:1: error:
      • Equality is not reflexive on Double
      • In the expression: reflexiveEq 0 (0 :: Double)

However, when superclasses are involved, the instance may need to carry
additional constraints, which can result in too many errors being reported.  For
example::

  class Eq a => ReflexiveEq' a where
      reflexiveEq' :: a -> a -> Bool
      reflexiveEq' = (==)

  instance (TypeError (Text "Can't compare functions with reflexiveEq"), Eq (a -> b)) => ReflexiveEq' (a -> b)

  instance TypeError (Text "Can't compare functions with (==)") => Eq (a -> b) where
      _ == _ = False

Notice that the instance for ``ReflexiveEq'`` had to repeat the ``Eq (a -> b)``
constraint in the instance context, because otherwise a type error is reported
at the instance definition site.  But this approach means both errors are
reported at the use site::

  ghci> reflexiveEq' id not

  <interactive>:36:1: error:
      • Can't compare functions with (==)
      • In the expression: reflexiveEq' id not
        In an equation for ‘it’: it = reflexiveEq' id not

  <interactive>:36:1: error:
      • Can't compare functions with reflexiveEq
      • In the expression: reflexiveEq' id not
        In an equation for ‘it’: it = reflexiveEq' id not

Ideally, when using a custom type error to report a domain-specific error
message for a particular instance, it would not be necessary to add other
constraints to the instance context.


``TypeError`` and ``-fdefer-type-errors``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A further issue arises with ``-fdefer-type-errors``.  When this flag is enabled,
GHC turns type errors into compile-time warnings, and replaces the erroneous
code with runtime exceptions.  However, when using ``TypeError`` to prevent a
definition from being called, there is nothing to guarantee that the error will
be triggered at runtime.  For example::

  ghci> let v = reflexiveEq 0 (0 :: Double)

  <interactive>:32:9: warning: [-Wdeferred-type-errors]
      • Equality is not reflexive on Double
      • In the expression: reflexiveEq 0 (0 :: Double)
        In an equation for ‘v’: v = reflexiveEq 0 (0 :: Double)
  ghci> v
  False

We would expect evaluation of ``v`` to throw an error, but it does not!  Instead
it uses the "redundant" method definition from the ``ReflexiveEq Double``
instance to yield a value.  See `#16249
<https://gitlab.haskell.org/ghc/ghc/-/issues/16249>`_ and `#18310
<https://gitlab.haskell.org/ghc/ghc/-/issues/18310>`_ for discussion of this
issue.

Ideally, it would be possible for the definition of the ``reflexiveEq`` method
in the ``ReflexiveEq Double`` instance to explicitly make use of the fact that
the context is inconsistent, rather than giving a bogus definition that is not
expected to be called.  Then under ``-fdefer-type-errors``, evaluation of the
bogus evidence for the constraint would be forced, resulting in the expected
runtime error.


Proposed Change Specification
-----------------------------

The ``GHC.TypeError`` module (the planned new home for ``TypeError`` per `!6066
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6066>`_) is extended with
the following definitions::

  class Unsatisfiable (e :: ErrorMessage) where
    unsatisfiable' :: a

  unsatisfiable :: forall e {rep} (a :: TYPE rep). Unsatisfiable e => a
  unsatisfiable = unsatisfiable' @e (##)

The ``Unsatisfiable`` class and ``unsatisfiable`` function are exported, but the
``unsatisfiable'`` class method is not.

``Unsatisfiable`` constraints have the following properties:

* During constraint solving, the solver treats ``Unsatisfiable`` constraints
  like any other class with no instances.  In particular, an ``Unsatisfiable e``
  Given constraint can solve a corresponding ``Unsatisfiable e`` Wanted
  constraint, but not ``Unsatisfiable e'`` for some distinct ``e'``.

* At the end of constraint solving:

  - If at least one Given constraint of the form ``Unsatisfiable e`` is present,
    the constraint solver will automatically solve all Wanted constraints
    (including any ``Unsatisfiable`` Wanted constraints). The evidence for a
    Wanted ``w`` consists of a call to ``unsatisfiable @e @w``.

  - Otherwise, if a Wanted constraint of the form ``Unsatisfiable e`` remains
    unsolved, a type error is reported but the usual "unsolved constraint" error
    message is replaced by the custom message that results from normalising and
    rendering the type ``e``.

* An ``Unsatisfiable`` constraint is never automatically generalised.

* If an ``Unsatisfiable`` Given constraint is present during pattern-match coverage
  checking, the match is trivially regarded as total.

* If a class instance has an ``Unsatisfiable`` Given constraint in the context,
  it bypasses the functional dependency check.

* GHC will report an error if a user attempts to define an instance for
  ``Unsatisfiable``.

* The representation of an ``Unsatisfiable e`` constraint in Core is equivalent
  to the dictionary ``newtype CoercibleDict e = CoercibleDict (forall a . a)``.
  This is GHC's normal representation of a class with a single method.

The rendering of ``ErrorMessage`` values works just as for ``TypeError``. But
unlike ``TypeError``, mere presence of ``Unsatisfiable`` somewhere within a
constraint type does not trigger an error.


Optional proposed change: ``Warning`` class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*The change proposed in this subsection is optional, and may be removed without
affecting the rest of the proposal.*

``GHC.TypeError`` is also extended with the following definition::

    class Warning (e :: ErrorMessage)

``Warning`` constraints have the following properties:

* During constraint solving, the solver treats ``Warning`` constraints like any
  other class with no instances.

* At the end of constraint solving, if a Wanted constraint of the form ``Warning
  e`` remains unsolved, a warning diagnostic is emitted, displaying the message
  that results from normalising and rendering the type ``e``.

* An ``Warning`` constraint is never automatically generalised.

* The presence of ``Warning`` constraints does not affect pattern-match coverage
  checking or the functional dependency check.

* GHC will report an error if a user attempts to define an instance for
  ``Warning``.

* The representation of an ``Warning e`` constraint in Core is equivalent to the
  dictionary ``data WarningDict e = WarningDict``.  This is GHC's normal
  representation of a class with no methods.

See `#17027 <https://gitlab.haskell.org/ghc/ghc/-/issues/17027>`_ for a request
for a feature like this.  Unlike the suggestion in that ticket, the ``Warning``
class proposed here can only be used at kind ``Constraint``, for similar reasons
to ``Unsatisfiable``.

The warning diagnostics emitted by this feature are controlled by a new warning
class ``-Wtype-warnings``, which is enabled by default, but may be disabled with
``-Wno-type-warnings``.

This provides a more flexible alternative to ``WARNING`` and ``DEPRECATED``
pragmas, because warnings may be displayed only when definitions are used at
particular types, and the messages may be constructed dynamically.



Examples
--------

For more substantial examples, see the `unsatisfiable package by Oleg Grenrus
<https://hackage.haskell.org/package/unsatisfiable>`_.  This package implements
a type-checker plugin that roughly corresponds to the design of the
``Unsatisfiable`` constraints in this proposal.

#. The following definitions are accepted (assuming ``DataKinds``,
   ``FlexibleContexts`` and ``TypeApplications`` are enabled)::

     type Msg = Text "Cannot call 'uncallable'."

     uncallable :: Unsatisfiable Msg => ()
     uncallable = unsatisfiable @Msg

     uncallable' :: Unsatisfiable Msg => ()
     uncallable' = uncallable

   The definition of ``uncallable`` is accepted because ``Unsatisfiable`` appears
   as a Given, not a Wanted.  In ``uncallable'``, the Wanted arising from the
   occurrence of ``uncallable`` is solved using the Given in the context.

#. The following definition results in a custom type error message (as the
   ``Unsatisfiable Msg`` constraint is reported rather than being generalised
   over)::

     rejected = uncallable  -- error

#. The following definition is accepted::

     unusual :: Unsatisfiable Msg => Char
     unusual = 42  -- no error

   Here the presence of ``Unsatisfiable Msg`` in the context means that the ``Num
   Char`` constraint arising from the body of ``unusual`` is discharged
   automatically.

#. The following definition is accepted::

     k :: Unsatisfiable (Text "No") => ()
     k = uncallable  -- no error

   The Given ``Unsatisfiable (Text "No")`` solves the Wanted ``Unsatisfiable
   Msg``, even though the messages are different. This shows that error messages
   can be changed.

#. Recall the following example from the Motivation, adapted for
   ``Unsatisfiable``::

     type ExpectTrue x = If x (() :: Constraint) (Unsatisfiable (Text "Input was False!"))

     h :: ExpectTrue x => proxy x -> ()
     h _ = ()

     eg1 _ = h (Proxy @True)   -- no error

     eg2 _ = h (Proxy @False)  -- error

     eg3 p = h p               -- no error

   As with the ``TypeError`` version, ``eg1`` is accepted and ``eg2`` is rejected
   with a custom type error message.  Unlike the ``TypeError`` version, ``eg3`` is
   accepted, with the inferred type::

     eg3 :: If x () (Unsatisfiable ('Text "Input was False!")) => proxy x -> ()

   This is just the result of the normal constraint-solving behaviour.  Since the
   type constructor at the head of the constraint is ``If``, the special-purpose
   treatment of ``Unsatisfiable`` does not come into play.  In contrast, using
   ``TypeError`` results in this definition being rejected, because GHC searches
   deeply inside the type for applications of ``TypeError``.

#. The ``ReflexiveEq`` example from the Motivation can now be written like this::

     class Eq a => ReflexiveEq a where
         reflexiveEq :: a -> a -> Bool
         reflexiveEq = (==)

     instance Unsatisfiable (Text "Can't compare functions with reflexiveEq") => ReflexiveEq (a -> b)

     type DoubleMsg = Text "Equality is not reflexive on Double"
     instance Unsatisfiable DoubleMsg => ReflexiveEq Double where
         reflexiveEq = unsatisfiable @DoubleMsg

   Even though ``Eq`` is a superclass of ``ReflexiveEq``, the instance does not
   need to list it in the context, because the Given ``Unsatisfiable``
   constraint suffices to solve the ``Eq (a -> b)`` constraint.  This means that
   use sites will not accidentally duplicate error messages as with the current
   behaviour of ``TypeError``.

   Moreover, when ``-fdefer-type-errors`` is used to call ``reflexiveEq 0 (0 ::
   Double)``, this will result in a runtime exception that correctly blames the
   use of ``-fdefer-type-errors`` to bypass the ``Unsatisfiable`` constraint.

#. The following is regarded as total by the pattern-match coverage checker
   (thanks to `/u/ComicIronic on Reddit
   <https://www.reddit.com/r/haskell/comments/p2ao7v/unsatisfiable_a_ghcproposal_for_better_custom/h8je78s/>`_
   for the example)::

     data MyGADT a where
       MyInt :: MyGADT Int

     type family IsBool a where
       IsBool Bool = ()
       IsBool a    = Unsatisfiable (Text "Must be Bool")

     foo :: IsBool a => MyGADT a -> Void
     foo x = case x of {}

   This means it is possible to use ``Unsatisfiable`` to get custom error
   messages, without needing to write cases that are in practice inaccessible.
   Similar functionality was recently implemented for ``TypeError`` by Sam
   Derbyshire (see `#20180
   <https://gitlab.haskell.org/ghc/ghc/-/issues/20180>`_).

#. The following is accepted despite the apparent functional dependency violation::

     class C a b | a -> b
     instance Unsatisfiable (Text "No") => C a b

   That is, an instance can be ruled out with a custom type error even where
   this would otherwise conflict with the functional dependencies.  A practical
   use case for this `arises in the optics library
   <https://hackage.haskell.org/package/optics-core-0.4/docs/Optics-Internal-Optic-Subtyping.html#t:JoinKinds>`_.
   Without this rule, a workaround is possible by building an otherwise unused
   cycle in the context, but this runs the risk of exposing the cycle as a
   constraint solver loop::

     instance (Unsatisfiable (Text "No"), C a b) => C a b

#. Assuming the ``Warning`` option is included, the following ``decode``
   function will emit a warning if it is used at type ``Integer``, but the
   program will otherwise compile normally::

     type family WarnInteger a where
       WarnInteger Integer = Warning (Text "Integer may require unbounded memory!")
       WarnInteger a       = ()

     decode :: (FromJSON a, WarnInteger a) => ByteString -> Maybe a


Effect and Interactions
-----------------------

The points at which ``Unsatisfiable`` constraints trigger type error messages
are well-specified, and fit well with GHC's constraint-based type inference
algorithm.  This means it should be simpler and more predictable than
``TypeError``.  The examples above demonstrate that the issues raised in the
Motivation have now got more principled solutions.

``Unsatisfiable`` does not subsume ``TypeError`` entirely, because the former is
restricted to kind ``Constraint``, whereas the latter is kind-polymorphic.  Thus
there may be situations where ``TypeError`` is preferable, and both will remain
available for use.  Correspondingly, this proposal does not lead to significant
backwards incompatibility.


Interaction with deferred type errors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As usual, when ``-fdefer-type-errors`` is enabled, unsolved constraint errors
will be deferred to runtime by the compiler automatically generating a
dictionary that throws a runtime exception if evaluated.  The message attached
to the exception contains the type error that was deferred (including its source
position).

This proposal does not change this behaviour; a use of ``Unsatisfiable`` merely
changes the message that is produced.  For example::

    {-# OPTIONS_GHC -fdefer-type-errors #-}

    f :: Unsatisfiable (Text "Blah blah") => a -> a
    f x = x+1

    main = print (f True)

Compiling this program will emit a warning corresponding to the deferred type
error.  Executing it will throw a runtime exception like this::

    *** Exception: Unsatisfiable.hs:24:15: error:
        • Blah blah
            arising from a use of ‘f’
        • In the first argument of ‘print’, namely ‘(f True)’
          In the expression: print (f True)
          In an equation for ‘main’: main = print (f True)
    (deferred type error)


Costs and Drawbacks
-------------------

This is yet another feature, and will require some implementation effort, but it
should not require extensive changes to GHC's existing constraint solving
behaviour.

The differences between ``Unsatisfiable`` and ``TypeError`` may be subtle for
novice users, but the increased convenience of ``Unsatisfiable`` for more
advanced users defining custom type errors in libraries seems worth it.


Alternatives
------------

There have been various requests for more powerful alternatives to
``TypeError``.  This proposal is deliberately simple. `Proposal #59
<https://github.com/ghc-proposals/ghc-proposals/pull/59>`_ and `proposal #278
<https://github.com/ghc-proposals/ghc-proposals/pull/278>`_ were more ambitious
attempts to improve custom type errors, but both have been abandoned due to
their complexity.

`#18978 <https://gitlab.haskell.org/ghc/ghc/-/issues/18978>`_ suggests
introducing ``Annotate :: ErrorMessage -> Constraint -> Constraint`` where
``Annotate e c`` renders the message ``e`` if the constraint ``c`` cannot be
solved, and is equivalent to ``c`` otherwise.  This is similar to
``WithMessage`` from `proposal #59
<https://github.com/ghc-proposals/ghc-proposals/pull/59>`_.  ``Unsatisfiable``
is the special case of ``Annotate`` where the constraint can never be solved.
It might make sense to introduce ``Annotate`` together with or instead of
``Unsatisfiable``, but it is not immediately obvious how to deal with
constraints that are *simplified* rather than solved outright.

Another possible alternative to this proposal would be to refine the strategy
GHC uses for searching for occurrences of ``TypeError``, possibly adding
special-case behaviour when ``TypeError`` is used at kind ``Constraint``.  This
would avoid the need for a separate ``Unsatisfiable`` class.  However, in the
absence of a well-defined operational semantics for type-level evaluation (which
would clearly specify when a ``TypeError`` should be "triggered" during
constraint solving), it seems inevitable that ``TypeError`` will be somewhat ad
hoc.  In contrast, restricting the kind to ``Constraint`` means that it is much
easier to specify when ``Unsatisfiable`` should produce an error message.

Having Given ``Unsatisfiable`` constraints automatically solve all Wanted
constraints is not strictly necessary, though it has been requested several
times (`#14983 <https://gitlab.haskell.org/ghc/ghc/-/issues/14983>`_, `#18310
<https://gitlab.haskell.org/ghc/ghc/-/issues/18310>`_).  The user could instead
be required to call ``unsatisfiable`` explicitly to produce a value of type
``Dict c``, defined by ``data Dict c where Dict :: c => Dict c``.

Similarly, the pattern match coverage checker could remain ignorant of
``Unsatisfiable`` constraints.  Instead the user could explicitly write a call
to ``unsatisfiable``.

The presence of an ``Unsatisfiable`` constraint in an instance context could
allow the class methods to be omitted, with the compiler automatically inserting
calls to ``unsatisfiable``.  This seems like unnecessary magic, however, because
the user can write explicit calls to ``unsatisfiable`` instead, and in the
presence of ``-fdefer-type-errors`` it can matter whether a class method is
defined or calls ``unsatisfiable``.

The proposed definition of ``unsatisfiable`` is levity-polymorphic, so it can be
used directly at unlifted types.  This is consistent with ``error``, but is not
strictly necessary.  A consequence is that ``import GHC.TypeError (Unsatisfiable
(..))`` will not import ``unsatisfiable``, so users preferring explicit imports
will need to write ``import GHC.TypeError (Unsatisfiable, unsatisfiable)``
instead.  While mildly annoying, this preserves freedom to tweak the class
definition in the future without breaking backwards compatibility, and is
consistent with ``Coercible`` and ``coerce``, which likewise have to be imported
separately.


Unresolved Questions
--------------------

Should the optional ``Warning`` feature be included?
