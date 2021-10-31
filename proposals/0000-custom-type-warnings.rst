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
warning messages are emitted.  They allow users to specify how such warnings are
controlled by GHC's command-line warning flags such as ``-Wall``.

For example, the following ``decode`` function will emit a warning if it is used
at type ``Integer``, but not at any other types::

     type family WarnInteger a where
       WarnInteger Integer = Warning "decode-integer" Wdefault (Text "Integer may require unbounded memory!")
       WarnInteger a       = ()

     decode :: (FromJSON a, WarnInteger a) => ByteString -> Maybe a
     decode = ...

This warning is enabled by default, but may be disabled by passing the
command-line option ``-Wno-custom:decode-integer``.


Proposed Change Specification
-----------------------------

The ``GHC.TypeError`` module (the planned new home for ``TypeError`` and
``Unsatisfiable`` per `!6066
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6066>`_ and `proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_) is extended with
the following definitions::

    type Warning :: Symbol -> WarningBin -> ErrorMessage -> Constraint
    class Warning flag bin msg

    data WarningBin = Wdefault | W | Wall | Weverything

The ``Warning`` class resembles the ``Unsatisfiable`` class from `proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433>`_, but represents a
constraint which can be solved at any time by causing a warning to be emitted.

``Warning`` constraints have the following properties:

* During constraint solving, the solver treats ``Warning`` constraints like any
  other class with no instances.

* At the end of constraint solving, if a Wanted constraint of the form ``Warning
  "flag" bin msg`` remains unsolved, a warning diagnostic is emitted, using the
  message that results from normalising and rendering the type ``msg``, and
  provided the warning bin ``bin`` is enabled.  Note that if the flag parameter
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

* The representation of a ``Warning flag bin msg`` constraint in Core is
  equivalent to the dictionary ``data WarningDict flag bin msg = WarningDict``.
  This is GHC's normal representation of a class with no methods.


Flags to control warnings
~~~~~~~~~~~~~~~~~~~~~~~~~

The first parameter to the ``Warning`` constraint represents the flag used to
control whether the warning is displayed.  Individual warning flags may be
enabled or disabled using the ``-Wcustom:flag`` or ``-Wno-custom:flag`` options,
and their priority may be controlled using the ``-Werror=custom:flag`` or
``-Wwarn=custom:flag`` options, just as for GHC's built-in warnings.

In addition, every flag belongs to a *warning bin* such as ``-Wdefault`` or
``-Wall``, given by the second parameter to the ``Warning`` constraint.
Available warning bins are represented by the ``WarningBin`` datatype.

For example, assuming a ``Warning "flag" Wall (Text "Message")`` constraint has
been solved:

* No warning flags: no warning displayed

* ``-Wcustom:flag``: warning displayed

* ``-Wall``: warning displayed

* ``-Wall -Wno-custom:flag``: no warning displayed

* ``-Werror=custom:flag``: error message displayed

In general, it is possible for the same warning flag to be used multiple times
with different bins, in which case GHC decides whether to display these
independently, based on the command-line options.  For example, a single module
might give rise to both ``Warning "flag" Wdefault (Text "Message 1")`` and
``Warning "flag" Wall (Text "Message 2")``, and would display only one message
by default but both messages under ``-Wall``.  This is a departure from GHC's
normal behaviour that each warning flag belongs to exactly one bin, but it seems
unlikely to cause significant difficulties.


Examples
--------

#. The Motivation section gave an example of a ``decode`` function will emit a
   warning if it is used at type ``Integer``, but not at other types::

     type family WarnInteger a where
       WarnInteger Integer = Warning "decode-integer" Wdefault (Text "Integer may require unbounded memory!")
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
   ``Warning``) suppresses the warning that would otherwise result from calling
   ``decode``; instead it will be deferred to call sites.

   If the library author wishes for the warning to be enabled when ``-Wall`` is
   in effect but not by default, they may use ``Warning "decode-integer" Wall``
   in place of ``Warning "decode-integer" Wdefault``.

#. Andreas Klebinger in `#17027
   <https://gitlab.haskell.org/ghc/ghc/-/issues/17027>`_ gives an example which
   can be slightly reformulated to work with ``Warning``::

     type FitsIn :: Nat -> Type -> Constraint
     type family FitsIn x ty where
        FitsIn 0 ty       = ()
        FitsIn 1 ty       = ty ~ Word8
        ...
        FitsIn a ty       = ( Warning "fits-in" W (Text "The byte size " :<>: ShowType a :<>:
                                                   Text " is defaulted to Integer.")
                            , ty ~ Integer )

   This demonstrates that the warning message can depend on the type.

#. It is possible to define a class instance that emits a warning if used, for
   example::

     instance Warning "foldable-pair" Wall (Text "Foldable on pairs may cause confusion.")
           => Foldable ((,) a) where
        ...


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

* Warning flags and bins may be specified for individual messages, giving
  fine-grained control to users, rather than all warnings being controlled by a
  pair of flags (``-Wwarnings-deprecations`` and ``-Wdeprecations``).

* It is possible to selectively postpone a warning by adding the corresponding
  constraint to the context.

A reasonable alternative to this proposal would be to extend ``WARNING`` and
``DEPRECATED`` so that they could be attached to class instances, and perhaps
could gain some of the other features proposed here.


Control over warning flags and bins
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As proposed, the ``Warning`` class has three separate parameters for the flag,
bin and message.  Various other designs are possible for encoding this
information (see `discussion on proposal #433
<https://github.com/ghc-proposals/ghc-proposals/pull/433#issuecomment-953219137>`_).

The approach proposed here is simple and minimal.  It may require some
repetition and does not guarantee that each flag belongs to a single bin, but
that seems unlikely to be a problem in practice.  In particular, library authors
can easily define constraint synonyms, such as::

    type MyLibraryWarning msg = Warning "my-library" Wall msg

This can keep unnecessary noise out of type signatures and avoid most of the
repetition.  If a more elaborate scheme is desired, it is possible to build one
on top of the interface proposed here, entirely outside ``base`` (see `this gist
<https://gist.github.com/adamgundry/b3b9a131003e5f016992f9e8183aa59b>`_ for
details).


Unresolved Questions
--------------------

Should it be possible for custom warnings to be controlled by the ``-Wcompat``
warning group, or by standard warning flags such as ``-Wdeprecations``?  The
current proposal introduces a ``custom:`` prefix to the flags, which makes them
clearly separate from built-in warnings.
