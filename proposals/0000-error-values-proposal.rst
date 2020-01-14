Errors as values
================

.. author:: Alp Mestanogullari
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/306>`_.
.. contents::

Define and use a hierarchy of data types for all the errors,
warnings and suggestions that GHC emits. Only render the error values
in GHC-the-program, at the very last minute, to allow GHC API users to
grab those error values instead of the current error documents. This new
representation would be a lot easier to work with for IDE tooling developers
and GHC API users in general: pattern matching on the new data types would
replace parsing of the error messages.

Motivation
----------

Up until this day, the errors that GHC emits are represented as mere
textual documents. They often involve fancy layout constructs but GHC's
code never manipulates errors, warnings or suggestions as values of some
algebraic data types that are later rendered in the user's terminal; errors
only ever exist as documents.

This means that developers of IDE-style tooling (e.g
`Haskell IDE Engine <https://github.com/haskell/haskell-ide-engine>`_) have
to parse the error messages and warnings to implement some of their
features. This is not ideal and is made worse by the possibility that some
error messages might change in their contents or formatting, from a GHC
version to another. It would be a lot simpler for those developers to
put their hands on good old Haskell values that describe the errors,
warnings and suggestions that GHC reports for a given Haskell module.
The IDE tools and GHC API programs in general would then be able to
inspect those values and extract any relevant information without having
to parse any text, by simply traversing ADTs (e.g collect suggestions
and offer a feature that applies them all with a simple command or
keystroke). The ``ghc`` library would also come with code for rendering
those errors, making it easy for GHC API consumers to reuse all of GHC's
error printing infrastructure.

**Note**: the textual rendering of the errrors, warnings and suggestions
should remain identical, this proposal really is about GHC's
"internal" (it is exposed to GHC API users, so not entirely internal)
representation of errors.

Proposed Change Specification
-----------------------------

The current representation of errors and warnings in GHC is based on the
following data types.::

    data ErrMsg = ErrMsg {
            errMsgSpan        :: SrcSpan,
            errMsgContext     :: PrintUnqualified,
            errMsgDoc         :: ErrDoc,
            errMsgShortString :: String,
            errMsgSeverity    :: Severity,
            errMsgReason      :: WarnReason
            }

    data ErrDoc = ErrDoc {
            errDocImportant     :: [MsgDoc],
            errDocContext       :: [MsgDoc],
            errDocSupplementary :: [MsgDoc]
            }

    type MsgDoc = SDoc

    -- for completeness: warnings are represented with the same type
    -- as errors.
    type WarnMsg = ErrMsg

where ``SDoc`` is a type from GHC's pretty printing infrastructure that
represents configurable textual documents.

GHC then maintains a bag of ``ErrMsg`` and a bag of ``WarnMsg`` as
compilation proceeds and reports them when appropriate.::

    data TcLclEnv = TcLclEnv
      { ...
      , tcl_errs :: TcRef Messages     -- Place to accumulate errors
      , ...
      }

    type Messages        = (WarningMessages, ErrorMessages)
    type WarningMessages = Bag WarnMsg
    type ErrorMessages   = Bag ErrMsg

We propose to replace ``ErrDoc`` with several algebraic data types, each
representing the different errors/warnings that might arise from a given
GHC subsystem. For example (simplified):::

    data RenamerError
        = NotInScope OccName [Name] -- unknown name, suggestions
	| ...

    data TypecheckerError
        = OccursCheck Type Type
	| ...

    ...

We could even split error types further if necessary, making it a
slightly more elaborate/deep hierarchy. The exact shape of the said hierarchy
has yet to be determined, as it will be best informed by staring at the
error generation code that GHC has today. We would also provide a toplevel
error data type, ``GHCError``, which would be a sum of the error types from all
subsystems. This would allow us to store and more generally treat
uniformly errors from different systems:::

    data GHCError
      = PsError ParseError
      | RnError RenamerError
      | TcError TypecheckerError
      | ...

One can then define helper functions such as
``notInScope :: OccName -> Name -> GHCError`` to be able to easily construct
error values from within, say, the guts of a renamer function, without
having to make the intermediate layers and wrapping visible there. We would
create, manipulate and store ``GHCError`` values until the very last moment,
when it is time to render the errors and report them. This would require
implementing ``errorMessage :: GHCError -> ErrDoc``, and would be equivalent
to all the ``ErrDoc`` building code that GHC has right now.

Error consumers (the GHC program, GHC API users) would be presented with
``GHCError`` values, and would be free to just call ``errorMessage`` on them
to generate error message documents, or do something more interesting with
some or all of the error values, using good old pattern matching to provide a
specific interpretation for the errors of interest.

For error producers, the main change is that the different subsystems will
define error types and helper functions to build error values. In order for
``GHCError`` to be able to refer to all the different error types, and for
those types to use names from the module where ``GHCError`` is defined, we will
quite likely have to introduce ``.hs-boot`` files to work around
the import cycles induced by such an architecture. The exact details should be
figured out at implementation time, but there might be a way to get away with
just one ``.hs-boot`` file (must be confirmed).

It is important to note that ``errorMessage`` ties this proposal back with
the existing system. Right now, GHC immediately emits error messages
(i.e a textual representation of the errors) and has a lot of code for
rendering all the relevant information (e.g expressions or types)
with some helpful messages. This proposal merely suggests that we keep
this code but call it much later, when GHC's job with the module is done
and the compilation has failed (for errors) or succeeded with warnings,
that we need to report too. GHC would simply keep around all the relevant
information that the textual rendering of those errors requires,
as values of suitably defined algebraic data types, with all the
expressions, types, contexts, suggestions and more stored in fields of
those ADTs.

If necessary, we could define a separate sum type for warnings and
update the definitions of ``ErrorMessages`` and ``WarningMessages``
given earlier as follows:::

    -- defined as a direct sum of the warning data for each warning, or
    -- as a subsystem-driven hierarchy (like for errors) if required
    data GHCWarning
      = UnnecessaryImport ModuleName
      | ...

    warningMessage :: GHCWarning -> ErrDoc

    type ErrorMessages   = Bag GHCError
    type WarningMessages = Bag GHCWarning
    type Messages        = (WarningMessages, ErrorMessages) -- as before

(The alternative being to just store ``GHCError`` values in both bags and
augment ``GHCError`` with a constructor dedicated to warnings.)

Finally, we would have to update some error reporting infrastructure
to take ``GHCError`` values as arguments instead of ``ErrDoc``. That is
the point at which the actual rendering of error messages would happen,
under this proposal, right before calling the code that logs the said errors.

A consequence of this is that the ``Messages`` type that GHC API users
consume would now carry error and warning **values** that they can render
but also inspect, without parsing. A lot of the work would be about
actually moving all the error rendering code away from where we create
errors, and defining suitable types that carry the data around until
it is time to report the errors to the user.

Effect and Interactions
-----------------------

By turning errors into proper values, tooling authors would be able to
get rid of their error parsing code and finally be able to concisely
inspect, render or "customize" error messages. This is the main attraction of
this proposal. However, we list below a few compelling applications that are at
best very cumbersome to write with the current error documents and which would
be made a lot easier if the current proposal is accepted.

* An IDE tool might want to gather the bindings and their types, as listed by
  GHC in some type errors, to allow editors to display those in a tooltip or
  minibuffer, helping the developer figure out the right combination of those
  bindings by visually placing this information next to the code.

* An IDE tool might use the suggestions that GHC would embed in error
  values to present automated refactoring options to the user (e.g enabling
  language extensions, importing a module, fixing a typo in the spelling of
  a name).

* Display squiggles of different colors depending on the nature of the error
  (e.g "not in scope" errors in some color, "couldn't match" errors in another,
  and so on, or perhaps discriminating in larger groups). This could help
  Haskell developers focus on one of those groups (e.g typos in names,
  missing imports) before proceeding with the more subtle type errors.

* An IDE tool might ask GHC to defer expensive analyses typically done
  during error message construction (e.g. `computing valid hole fits
  <https://gitlab.haskell.org/ghc/ghc/issues/16875#note_210045>`_) and instead
  query GHC for the analysis result asynchronously (or even only when
  requested by the user), shrinking the edit/typechecking iteration time.

* If GHC ever wants to assign error codes to all the possible errors that it can
  produce (e.g to give an in-depth explanation of all errors and possible
  solutions in some error reference document), we could very easily derive or
  manually implement sensible schemes quite trivially from the error data types.

Costs and drawbacks
-------------------

The potential import cycles induced by this architecture and the ``.hs-boot``
file(s) that we might add to work around them are going to add a little bit of
maintenance overhead which we believe is largely compensated by having errors
become proper values.

Another drawback is that the wrapping in ``ParserError``, ``GHCError`` and
friends can be a bit verbose, becoming more verbose still as we introduce
additional levels to the hierarchy. This can be mitigated by going for a
rather flat hierarchy like the one presented above, with ``GHCError`` at the
top, the subsystem-specific error types below and nothing else: the different
constructors of those subsystem-specific error types would contain all the
relevant information for a specific error that GHC can emit, the indirection
would stop there. This option seems to be a good compromise and that is why
it is the one we used in the previous section.

The major cost of implementing this proposal is the sheer amount of
refactoring that will be necessary to emit error values and move the
rendering to much later, essentially delegating this work to each subystem
and combining everything in the implementation of ``errorMessage``.

Alternatives
------------

We considered open variants of this design, where we do not build sum types
all the way up to ``GHCError``, but where ``GHCError`` is an
existential wrapper around an open union of error types that provide
suitable instances:::

    class HasErrMsg e where
      errorMessage :: e -> ErrDoc

    data GHCError where
      GHCError :: (Typeable e, HasErrMsg e) => e -> GHCError

This would allow us to work around the whole import cycles problem,
at the price of being a lot more cumbersome to use: error consumers would
have to use ``Typeable`` to implement specific behaviours for some types of
errors. This price is likely a higher one to pay in the long run than the
import cycles that we would work around when implementing the current proposal,
as the cost will likely be non-trivial when/if we implement the proposal, but
very small afterwards, especially with a flat hierarchy. GHC does not get a
new subsystem nor an error infrastructure redesign all that often.

Unresolved Questions
--------------------

We have not fully fleshed out the entire list of error types that would have
to be defined, since we believe this is something that will be best done by
scanning GHC's code, looking for functions that emit error messages and trying
to adapt them to emit a suitably wrapped error **value**. This however did not
seem very relevant to describing the idea behind this proposal, as it is mostly
about determining what constructors we should have in the "leaf error types"
and which pieces of data have to be stored in those constructors, while the
proposal puts forward an idea that does not strictly depend on the concrete
errors that are constructed, stored and reported.

Implementation Plan
-------------------

Well-Typed LLP will implement this proposal with financial support from
Richard Eisenberg, under NSF grant number 1704041.
