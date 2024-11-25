Custom type warnings
====================

.. author:: Adam Gundry
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/454>`_.
.. sectnum::
.. contents::

This proposal introduces a new built-in constraint form, ``Warning``, as a
mechanism for introducing custom warnings using type information.  This was
originally brought up in the discussion of `proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_ but has been
separated out so that its design can be discussed in greater detail.


Motivation
----------

GHC currently allows users to define custom type error messages using the
``TypeError`` type family.  `Proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_ supplements this
with a type class, ``Unsatisfiable``::

  class Unsatisfiable (e :: ErrorMessage) where
    unsatisfiable :: a

This class has more predictable behaviour and carries evidence of the
unsatisfiability of the constraint, but may only be used at kind ``Constraint``
(whereas ``TypeError`` is poly-kinded).  Both ``TypeError`` and
``Unsatisfiable`` carry an ``ErrorMessage`` that may contain both literal
strings and instructions to render types as part of the message.

Custom type errors are a very useful feature for libraries that wish to provide
good-quality domain-specific error messages to their users.  However, sometimes
library authors may wish to define domain-specific warnings, making use of type
information (e.g. warning when a definition is used at at particular type, or
when a particular class instance is used).  See `#17027
<https://gitlab.haskell.org/ghc/ghc/-/issues/17027>`_ for a request for a
feature like this.

Warnings may currently be specified only in a limited manner via the ``WARNING``
and ``DEPRECATED`` pragmas, which emit a fixed warning message whenever a marked
definition is used.  There is no way for these pragmas to make use of type
information, either to control when the warning is emitted or to vary the
message that is displayed.

This proposal introduces a new built-in constraint form ``Warning``, somewhat
similar to ``Unsatisfiable``, which allows user-defined warning messages.  These
constraints have a clear semantics for how they are solved and hence when
warning messages are emitted.

For example, the following ``decode`` function will emit a warning if it is used
at type ``Integer``, but not at any other types::

     type family WarnInteger a where
       WarnInteger Integer = Warning "my-json-lib/decode-integer" (Text "Integer may require unbounded memory!")
       WarnInteger a       = ()

     decode :: (FromJSON a, WarnInteger a) => ByteString -> Maybe a
     decode = ...

This warning is enabled by default, but may be disabled by passing the
command-line option ``-Wno-custom:my-json-lib/decode-integer``.  It is possible
for use sites to suppress the warning (regardless of flags), for example::

    decode_no_warning :: forall a . FromJSON a => ByteString -> Maybe a
    decode_no_warning = suppressWarning @"my-json-lib/decode-integer" decode


Proposed Change Specification
-----------------------------

The ``GHC.TypeError`` module (the planned new home for ``TypeError`` and
``Unsatisfiable`` per `!6066
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6066>`_ and `proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_) is extended with
the following definitions::

    type Warning :: Symbol -> ErrorMessage -> Constraint
    class Warning flag msg

    suppressWarning :: forall flag r . ((forall msg. Warning flag msg) => r) -> r

The ``Warning`` class resembles the ``Unsatisfiable`` class from `proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_, but represents a
constraint which can be solved at any time by causing a warning to be emitted.

``Warning`` constraints have the following properties:

* During constraint solving, the solver treats ``Warning`` constraints like any
  other class with no instances.

* At the end of constraint solving, if a Wanted constraint of the form ``Warning
  "flag" msg`` remains unsolved, a warning diagnostic is emitted, using the
  message that results from normalising and rendering the type ``msg``, and the
  constraint is then solved with the trivial dictionary.  If the flag parameter
  is not a literal ``Symbol``, the constraint is not automatically solved.  (See
  below for discussion of how users may control whether this warning is
  displayed.)

* If a Wanted ``Warning`` constraint is solved using another mechanism
  (e.g. because an identical Given constraint is in scope, or because of an
  ``Unsatisfiable`` Given constraint), then no warning is emitted.

* A ``Warning`` constraint is never automatically generalised.

* The presence of ``Warning`` constraints does not affect pattern-match coverage
  checking or the functional dependency check.

* GHC will report an error if a user attempts to define an instance for
  ``Warning``.

* The representation of a ``Warning flag msg`` constraint in Core is equivalent
  to the dictionary ``data WarningDict flag msg = WarningDict``.  This is GHC's
  normal representation of a class with no methods.


Flags to control warnings
~~~~~~~~~~~~~~~~~~~~~~~~~

The first parameter to the ``Warning`` constraint represents the flag used to
control whether the warning is displayed.  Individual warning flags may be
enabled or disabled using the ``-Wcustom:flag`` or ``-Wno-custom:flag`` options,
and their priority may be controlled using the ``-Werror=custom:flag`` or
``-Wwarn=custom:flag`` options, just as for GHC's built-in warnings.

By default, all custom warnings are enabled, as if they belonged to the
``-Wdefault`` warning bin.  It is not possible to vary the warning bin to which
a custom warning belongs.

For example, assuming a ``Warning "flag" (Text "Message")`` constraint has been
solved automatically:

==========================  =======================
Warning flags               Result
==========================  =======================
None                        Warning displayed
``-Wcustom:flag``           Warning displayed
``-Wall -Wno-custom:flag``  No warning displayed
``-Werror=custom:flag``     Error message displayed
==========================  =======================

To assist users in identifying where a custom warning originates from, we
recommend that library authors choose flag names that start with the name of the
library, followed by a forward slash and a more specific warning name.  They may
use additional forward slash separated components to introduce a hierarchy of
flag names.  For example, the ``text`` library might introduce a warning flag
named ``text/io/encoding-sensitivity``.  This convention is not enforced by the
compiler, however.


Suppressing warnings
~~~~~~~~~~~~~~~~~~~~

The ``suppressWarning`` function is provided so that users can explicitly
silence a warning within a particular definition, regardless of which
command-line flags are in effect::

    suppressWarning :: forall flag r . ((forall msg . Warning flag msg) => r) -> r

This requires a type application specifying the particular ``flag`` being
suppressed, and uses ``QuantifiedConstraints`` to locally solve all ``Warning``
constraints with that flag (regardless of the error message).

It is not possible to suppress a single message selectively.  This is preferable
to needing to specify the message explicitly, as the message may be long and
depend on implementation details of the library.

The implementation of this function requires compiler support, because it needs
to construct a dictionary for the ``forall msg . Warning flag msg`` constraint
to pass to the argument, which is straightforward in Core but difficult in
Haskell.


Examples
--------

#. The Motivation section gave an example of a ``decode`` function will emit a
   warning if it is used at type ``Integer``, but not at other types::

     type family WarnInteger a where
       WarnInteger Integer = Warning "my-json-lib/decode-integer" (Text "Integer may require unbounded memory!")
       WarnInteger a       = ()

     decode :: (FromJSON a, WarnInteger a) => ByteString -> Maybe a
     decode = ...

     no_warning :: ByteString -> Maybe Int
     no_warning = decode

     emits_warning :: ByteString -> Maybe Integer
     emits_warning = decode

     no_warning_either :: WarnInteger Integer => ByteString -> Maybe Integer
     no_warning_either = decode

   In the final definition, the presence in the context of the ``WarnInteger
   Integer`` Given constraint (which simplifies to an application of
   ``Warning``) defers to call sites the warning that would otherwise result
   from calling ``decode``.

#. Andreas Klebinger in `#17027
   <https://gitlab.haskell.org/ghc/ghc/-/issues/17027>`_ gives an example which
   can be slightly reformulated to work with ``Warning``::

     type FitsIn :: Nat -> Type -> Constraint
     type family FitsIn x ty where
        FitsIn 0 ty       = ()
        FitsIn 1 ty       = ty ~ Word8
        ...
        FitsIn a ty       = ( Warning "fits-in" (Text "The byte size " :<>: ShowType a :<>:
                                                 Text " is defaulted to Integer.")
                            , ty ~ Integer )

   This demonstrates that the warning message can depend on the type.

#. It is possible to define a class instance that emits a warning if used, for
   example::

     type FoldablePairWarning = Warning "base/foldable-pair" (Text "Foldable on pairs may cause confusion.")

     instance FoldablePairWarning => Foldable ((,) a) where
        ...

#. Users can suppress a warning by acknowledging it in the source code, for example::

      foo = suppressWarning @"base/foldable-pair" $ length (1,2)


Effect and Interactions
-----------------------

None.


Costs and Drawbacks
-------------------

Where a definition uses a ``Warning`` constraint, a dictionary argument will
potentially be passed at runtime.  In practice this should typically be
specialised away, but in corner cases this may affect semantics or runtime
performance.  For example, adding a class constraint to an otherwise
non-polymorphic definition may lead to loss of sharing.

The extra development cost of this feature should be relatively low, as it has
been designed to fit with GHC's existing constraint solver and is similar to the
``Unsatisfiable`` constraint.  It is a little unclear how much complexity the
custom warning flags will introduce.

This feature is primarily targeted at advanced users, but should help library
authors make their libraries more accessible to beginners by providing suitable
domain-specific warnings.  Thus the effect on accessibility of the language to
novice users is expected to be a net positive.


Alternatives
------------

``TypeError`` vs. ``Unsatisfiable``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal makes ``Warning`` analogous to the ``Unsatisfiable`` class
introduced in `proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_.  That is, all
warnings must be constructed at kind ``Constraint``.  As discussed in the
previous proposal, this makes it possible to clearly specify the points at which
warnings should trigger.

It would be possible to make an analogue of ``TypeError`` instead, i.e. a type
family usable at any kind.  This would be more flexible, albeit potentially less
predictable.  Since there is no evidence carried by the dictionary, the
restriction to ``Constraint`` is less strongly motivated than for
``Unsatisfiable``, however.


Pragmas vs. classes
~~~~~~~~~~~~~~~~~~~

The ``WARNING`` and ``DEPRECATED`` pragmas already allow warnings to be attached
to modules or identifiers, such that any use of the identifiers will trigger the
warning.  They cannot currently be used on class instances.

This proposal is more flexible than ``WARNING`` and ``DEPRECATED`` pragmas,
because:

* Warnings may be displayed only when definitions are used at particular types.

* Messages may be constructed dynamically, rendering types as part of the
  message.

* Warning flags may be specified for individual messages, giving
  fine-grained control to users, rather than all warnings being controlled by a
  pair of flags (``-Wwarnings-deprecations`` and ``-Wdeprecations``).

* It is possible to selectively postpone a warning by adding the corresponding
  constraint to the context.

* It is possible to suppress a warning in source code by adding a call to
  ``suppressWarning``.

A reasonable alternative to this proposal would be to extend ``WARNING`` and
``DEPRECATED`` so that they could be attached to class instances, and perhaps
could gain some of the other features proposed here.


Control over warning flags and bins
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In GHC, every warning flag belongs to a *warning bin* such as ``-Wdefault`` or
``-Wall``, so that users can control how many warnings are emitted.  This
version of the proposal does not allow the warning bin to be specified for an
individual constraint, in the interests of simplicity; instead all custom
warnings are output by default.  This aligns with the common recommendation of
preferring to enable most warnings with ``-Wall``.

It would be possible to include warning bin information with a slight variant of
the design, for example::

    data WarningBin = Wdefault | W | Wall | Weverything

    type WarningWithBin :: Symbol -> WarningBin -> ErrorMessage -> Constraint
    class WarningWithBin flag bin msg

In principle, this can be added later if there is demand for it.  However, this
design would allow the same warning flag to be given multiple different bins,
which is perhaps slightly surprising.

Similarly, one might imagine specifying whether a warning is controlled by
standard warning groups such as ``-Wcompat`` or by existing warning flags such
as ``-Wdeprecations``, rather than always requiring the ``custom:`` prefix.


Combining the parameters
~~~~~~~~~~~~~~~~~~~~~~~~

As proposed, the ``Warning`` class has two separate parameters for the flag and
message.  Various other designs are possible for encoding this information (see
`discussion on proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433#issuecomment-953219137>`_).
In particular (as `suggested by David Feuer
<https://github.com/ghc-proposals/ghc-proposals/pull/454#issuecomment-968417439>`_),
the interface could encourage users to define an empty datatype representing
each warning, for example::

   type IsWarning :: Type -> Constraint
   class IsWarning key where
     type Name key :: Symbol
     type Msg  key :: ErrorMessage

   suppressWarning :: forall key r . (IsWarning key => r) -> r

   data IntegerTooBig
   instance IsWarning IntegerTooBig where
     type Name IntegerTooBig = "integer-too-big"
     type Msg  IntegerTooBig = Text "Here's the message for an integer being too big"

The datatype provides an identity for each warning, which can be used when
calling ``suppressWarning @IntegerToBig``, rather than using quantified
constraints as proposed above.

However, this alternative approach requires introducing a more complex class
into the basic API (with two associated types) and attaching a warning to a
definition becomes less lightweight (because of the need to define a datatype).

Instead, the approach recommended in the proposal is simple and minimal.
Library authors can be encouraged to define constraint synonyms for warnings,
such as::

    type MyLibraryWarning x = Warning "my-lib/my-warning" (Text "Blah blah" :<>: ShowType x)

This is not required, but can keep unnecessary noise out of type signatures and
avoid repetition.

If a more elaborate scheme is desired, it is possible to build one on top of the
interface proposed here, entirely outside ``base`` (see `this gist
<https://gist.github.com/adamgundry/b3b9a131003e5f016992f9e8183aa59b>`_ for
details).


Glob support
~~~~~~~~~~~~

Users may wish to disable all warning flags from a particular library, or within
a particular category.  One way to achieve this would be to support glob-style
flags such as ``-Wno-custom:text/*``.  This is one motivation behind the
recommendation that flag names should be prefixed by the name of the library.

However, glob support is not part of the current proposal, in the interests of
simplicity.  If in the future custom warnings become sufficiently widely used
that glob support becomes necessary, this question can be revisited.


Unresolved Questions
--------------------

None.
