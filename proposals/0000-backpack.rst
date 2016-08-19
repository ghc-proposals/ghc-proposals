.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Backpack
========

#. `Introduction`_

   a. `Motivation`_

   b. `Overview`_

#. `Identifiers`_

#. `Units`_

   a. `Unit structure`_

   b. `Installed unit database`_

   c. `Signatures`_

   d. `Dependencies`_

#. `Library components`_

   a. `Library structure`_

   b. `Exports`_

   c. `Includes`_

   d. `Mix-in linking`_

   e. `Modules and signatures`_

#. `Setup interface`_

#. `Drawbacks`_

#. `Alternatives`_

#. `Unresolved Questions`_

Introduction
------------

Backpack is a proposal to add *mix-in libraries* to Haskell.  Mix-in
libraries can have *signatures* which permit implementations of values
and types to be deferred, while allowing a library with missing
implementations to still be type-checked.

Motivation
~~~~~~~~~~

Suppose that you are writing a string-processing library which, in
principle, could operate on ``ByteString``, ``Text`` and ``String``
equally well.  Ideally, you would be able to write a library to
support all three types, while at the same time achieving the following
properties:

1. **No copy-pasting.** It shouldn't be necessary to duplicate
   implementation code which is identical across all possible
   representations.

2. **Just normal Haskell.** The code you write should
   look identical to the code you would have written if you had
   just written your implementation against one representation
   (except perhaps with a stronger abstraction barrier.)
   In particular, the ability to parametrize over *types*
   is extremely important.

3. **No performance overhead.** The compiled code for
   each representation should be equivalent in performance to what you
   would have gotten by compiling it against the representation directly
   (modulo performance issues related to the API.)

4. **Egalitarian interfaces.** You should be able to declare what
   abstract interface your implementation depends on, without
   having to rely on some central authority having defined what
   it means for something to be *string-like.*

5. **Interface composibility.** Oppositely, it should be possible
   to publish and reuse an interface declaration, possibly extending it
   as necessary.

6. **Extensibility.** If I so choose, it should be possible to publish
   my library in such a way that a user could provide their own
   backing representation.

7. **Package-level modularity.** It should be possible to parametrize
   over an entire package, since libraries of this form often
   encompass multiple modules.

8. **Separate type-checking.** It should be possible to typecheck
   a parametrized package without committing to any particular
   implementation.

Backpack seeks to solve all of these problems, where
existing language features in Haskell are not up to the task.

**Dictionary passing style**  The dictionary-passing approach to modularity
defines a dictionary type, with type parameters for every type we
want to parametrize over, and fields for every value or function we
parametrize over.  For example, a dictionary parametrizing over
string implementations might look like this::

    data StrDict str
        = StrDict {
            null :: str,
            append :: str -> str -> str
        }

    concat :: StrDict str -> [str] -> str
    concat d xs = foldr (append d) (null d) xs

Dictionary passing fails criteria (2), (3), (5) and (7):

* **Just normal Haskell?**  While the encoding cost of this scheme is
  not too heavy, it is still not as nice as the code you could write
  if you didn't need to be parametric over multiple implementations.
  A more serious problem is that it is not possible to define new
  types based on the record (indeed, one could say this is the *point*
  of a module system).  "F-ing modules" demonstrates that
  the definition of types in this way is simply a mode of use of
  universal and existential quantifiers: however, such encodings would be
  extremely unwieldy to do by hand.  (Although, it does suggest someone
  could write a Template Haskell extension to elaborate ML-style modules
  into existentially quantified Haskell records.  We leave this for
  a future RFC.)

* **No performance overhead?**  Modulo optimizations, code written
  this way clearly must be indirected through a record of functions.
  For some interfaces, this would constitute an unacceptable overhead.
  If GHC can inline ``concat`` into a call-site where the the dictionary
  is known, it is possible to avoid this overhead; however, ``concat``
  must be re-optimized at every such call-site, and its code duplicated!

* **Interface composibility?**  Dictionary-passing style inherits
  many of the same problems that plague Haskell's record system:
  records of this form simply cannot be composed in a nice way.
  If you want to extend ``StrDict`` to contain another field,
  you have to define a new type; if someone else does the same,
  you have to explicitly convert to the two.  Structural *row types*
  (and encodings thereof) would help alleviate this problem.

* **Package-level modularity?**  It is clear that passing a dictionary
  operates only on a per-function basis, and there is no special
  dispensation for modularizing an entire package, except laboriously
  adding a dictionary to every function in the package.

Dictionary passing does have the benefit that it is easy to swap
implementation at runtime: so-called *first-class modules*
are expressly not a problem Backpack seeks to address (you should
use dictionary-passing in this case!)

**Traditional type classes** permit dictionaries to be computed automatically
based on the *types* at a dictionary call site.  For example,
instead of defining a ``StrDict`` type, we simply define a
``StrLike`` type class::

    class StrLike str where
        null :: str
        append :: str -> str -> str

    concat :: StrLike str => [str] -> str
    concat xs = foldr append null xs

Clearly, type classes can reduce some of the encoding overhead
seen in dictionary-passing style.  But it does not solve all
the issues of dictionary-passing style (performance, composibility,
and applicability to the package level all apply to type classes),
and they introduce some issues of their own:

1. If there is no natural type for what we are modularizing
   over (e.g., we are simply parametric over an implementation),
   type class resolution cannot be carried out without introducing
   a dummy proxy type.

2. In a multi-parameter type class, some methods may not mention
   all of the types in the type class: these methods cannot be
   resolved unambiguously without introducing a functional dependency.
   Similarly, every type parameter must be listed in the constraints,
   even if they are not used.  If there is a natural type to
   modularize over, these issues can be alleviated by introducing
   an *associated type*.

3. And of course, you still have to put the damn constraint on
   every parametrized function.

**Preprocessor.**  A classic way to swap out a backing implementation
is to replace it textually using a pre-processor.  In such a scheme, you
might write your Haskell program like this::

    import Data.ByteString as Str

    #include "Impl.hs"

Besides the intrinsic ugliness of such an approach, it fails to define
the *interface* between an implementation and the backing
implementations it relies on.  Without this type information, separate
typechecking is impossible.  Separate typechecking is extremely
useful: otherwise, you get extremely difficult to understand error
messages, as is seen in C++ template error messages.

Overview
~~~~~~~~

In this section, we give a high-level overview of Backpack, using the
string-processing problem as our running example. In this section, we
will introduce important terminology (**bolded**) that we will be used
in the rest of this specification.

Backpack solves this string-processing problem in the following way:
To parametrize over a string implementation, a user of Backpack writes a
**signature** describing the necessary supported interface for their
strings  (an *egalitarian interface.*) A signature is much like an
``hs-boot`` file, in that it contains only type signatures and type
declarations, but no implementations::

    -- in Str.hsig
    signature Str where
    data Str
    null   :: Str
    append :: Str -> Str -> Str

Other modules in the library import this signature and can use
the types and functions declared in the signature as if it were
an ordinary module (i.e., *just normal Haskell*, with *no copy-pasting*)::

    -- in Concat.hs
    module Concat where
    import Str
    concat xs = foldr append null xs

Signatures are declared in the Cabal file as a ``required-signature``::

    library concat-indef
        required-signatures: Str
        exposed-modules: Concat

A library with required signatures is called an **indefinite library**.
As it is missing implementations for its signatures, it cannot be
compiled; however, it can still be type-checked (*separate
type-checking*) and registered with the compiler, so that it can be used
by other indefinite libraries which depend on it.  In contrast, a
**definite library** is a library with no signatures: any library that
doesn't use Backpack features is a definite library.

An indefinite library can be **instantiated** (possibly multiple times)
with implementations for all of its required signature, allowing it
to be compiled.  Instantiation happens automatically when a user depends
on an indefinite library and a library which exposes modules with the
same names as the signatures (any library which fulfills the signatures
is valid, making this a *extensible* mechanism for *package-level
modularity*)::

     library str-bytestring
         exposed-modules: Str

     library concat-bytestring
         build-depends: str-bytestring, concat-indef
         reexported-modules:
             -- Concat from concat-indef is instantiated
             -- with Str from str-bytestring.  We can
             -- reexport it under a qualified name for
             -- more convenient use.
             Concat as Concat.ByteString

Thus, indefinite libraries can be thought of parametrized modules,
where the actual parameters are computed automatically by wiring
up signatures with similarly named module implementations.  This process
of determining the explicit instantiations is called **mix-in linking**.

An indefinite library can be instantiated to various degrees.
Compilation does not occur unless *all* signatures are implemented,
allowing a compiler can optimize as if Backpack was not present (*no
performance overhead.*)  An indefinite library can also be partially
instantiated, or not instantiated at all: indefinite libraries can be
combined with other indefinite libraries to form combined libraries
which are still indefinite::

    -- in the Cabal file
    library stringutils-indef
        -- No Str module in scope, so Str is left uninstantiated,
        -- giving stringutils-indef an (implicit) requirement
        -- on Str.
        build-depends: concat-indef
        exposed-modules: StringUtils

    -- in StringUtils.hs
    module StringUtils where
    import Concat
    import Str -- the signature is importable

The required signatures of a library are not necessarily syntactically
apparent (a ``required-signature`` field is specified only for every
locally available ``hsig`` file); this is by design, to allow users to
reuse signatures by putting them in libraries. (TODO: Mechanism for
explicitly stating what the requirements are)

Backpack is quite flexible about the way the uninstantiated
signatures can be handled:

* If you depend on two indefinite libraries, both of which
  have the same required signature (e.g., ``Str``), then you
  have a single required signature ``Str`` that is the union
  of these two signatures: signatures are identified only
  by module name.  To keep these two requirements separate,
  you would rename one of the requirements to a different name
  using the ``backpack-includes`` directive::

    library one-string
        -- One requirement, named Str
        build-depends: concat-indef, stringutils-indef

    library two-string
        -- Two requirements, Str and Str2
        build-depends: concat-indef, stringutils-indef
        backpack-include:
            stringutils-indef requires (Str as Str2)

* In addition to the inherited requirements from dependencies,
  a user can also define a local ``hsig`` to refine the signature
  further (i.e., define extra types).

Identifiers
-----------

In this section, we describe the grammar of identifiers in Backpack.
These identifiers are used, for example, to determine the unique
name associated with each entity in Haskell, which in turn determines
when two types are equal.

::

    ComponentId ::= [A-Za-z0-9-_.]+
    UnitId      ::= ComponentId
                  | ComponentId "[" ModuleSubst "]"
    ModuleSubst ::= ( ModuleName "=" Module ) +
    -- from Haskell'98
    ModuleName  ::= [A-Z][A-Za-z0-9_']* ( "." [A-Z][A-Za-z0-9_']* ) +
    Module      ::= UnitId ":" ModuleName
                  | "<" ModuleName ">"

.. _ComponentId:

A **component identifier** consists of an arbitrary sequence of
alphanumeric letters, dashes, underscores and periods.  Component
identifiers are uniquely allocated by the package manager (e.g.,
``cabal-install``), and in practice, encode the package name, package
version, component name, and a hash (which is computed over the source
code sdist tarball, Cabal flags, GHC flags and component identifiers of
direct dependencies of the component.)  Effectively, a component
identifier uniquely identifies precisely what is necessary to
typecheck a component (and, if it has no requirements, compile it
as well.)  A component identifier uniquely identifies a typechecked
indefinite library and compiled non-Backpack libraries.
We will use the metavariable ``p`` to represent component identifiers.

.. _UnitId:

A **unit identifier** consists of a component identifier combined with a
module substitution describing how the library is instantiated.
(The term **unit** refers to the intermediate language of units which
mediates between the compiler and the package system; see the section `Units`_
for more information.) Non-Backpack
libraries do not have a module substitution (since they have no
signatures to fill).  A unit identifier with no free module variables
(see below) uniquely identifies a definite library.  We will use the
metavariable ``P`` to represent unit identifiers.

.. _Module:

A **module identifier** is either a concrete module or a module
variable.  A concrete module consists of a module name (the module
being identified) and a unit identifier (the library it is a member of.)
A module variable consists only of a module name, and specifies the
name of an unfilled requirement.  Instantiation takes place by
substituting module variables with concrete module identifiers;
e.g., ``p[A=<A>]:B`` is instantiated to ``p[A=q:C]:B`` by applying
the substitution ``<A>`` maps to ``q:C``.  We may speak of the
**free module variables** of a module or unit identifier in the
conventional sense.  Module variables are bound by the requirements
of their defining library.  We will use the metavariable ``M`` to
represent module identifiers, and ``m`` to represent module names.

Module identifiers are used to ascribe unique names to Haskell entities:
a Haskell entity ``n`` defined in a module with the module identity
``M`` (notice that the unique name of a declaration in a module is
not well-specified without extra information, since only module
names are specified in syntax) is ascribed the unique name ``M.n``:
two identifiers are only equal if their unique names are equal.

.. _ModuleSubst:

A **module substitution** is a mapping from module names to modules identifiers.
Any module name occurs only once in a substitution, and a substitution
is in canonical form if it is sorted by module name.  We will use
the metavariable ``S`` to represent substitutions.

Module substitutions can be applied to identifiers::

    -- Substitution on UnitId
    (p[S])⟦S'⟧   = p[S⟦S'⟧]

    -- Substitution on Module
    <m>⟦⟧        = <m>
    <m>⟦m=M,  S⟧ = M
    <m>⟦m'=M, S⟧ = <m>⟦S⟧    (m ≠ m')
    (P:m)⟦S⟧     = P⟦S⟧:m

    -- Substitution on ModuleSubst (NOT substitution composition)
    (m=M, S')⟦S⟧ = m=M⟦S⟧, S'⟦S⟧

.. _`generalized unit id`:

A unit identifier can be **generalized** by replacing every mapping from
``m=M`` in its module substitution with
``m=<m>``.  This concept is useful when defining an
index over both indefinite and definite libraries. Naively, we would
have to use ``Either ComponentId UnitId`` as the key, since indefinite
libraries are uniquely identified by component identifiers, while
definite libraries identified by unit identifiers with no free module
variables. Generalization lets us embed ``ComponentId`` in ``UnitId``,
allowing us to use unit identifiers as the key uniformly across all
types of libraries.

Units
-----

A unit defines a collection of modules and dependent units parametrized
over a set of required signatures.  A non-Backpack library
is simply a unit with no requirements.  A unit with requirements can be
typechecked; if it is equipped with a module substitution, such a unit
can also be compiled.  Concretely, a unit corresponds directly to the
command line flags and input files the compiler is invoked with to
typecheck/compile the modules and signatures.

It is not expected that Backpack users program directly with units:
units are an intermediate language reminiscent of ML's *applicative
functors*, where the instantiation of every dependency must be
*explicitly* specified; user's instead write `library components`_ in
a Cabal file, which are then
converted via `mix-in linking`_ into units that can then be typechecked
and compiled.

Unit structure
~~~~~~~~~~~~~~

To discuss the unit language in a more user friendly form, we
define an abstract syntax tree for units and show how to translate
this AST into the command line arguments that the compiler accepts.

::

    unit  ::= "unit" ComponentId "where" "{"
                udecl_0 ";" ... ";" udecl_n
              "}"
    udecl ::= "dependency" UnitId ModuleRenaming
            | "module"    ModuleName
            | "signature" ModuleName

A unit begins with a header recording the component identity.  The
body of a unit consists of any number of dependencies, modules
and signatures.

Here is an informal translation of this AST into command line flags:

For typechecking an indefinite unit via ``ghc --make``:

1. ``"dependency" UnitId ModuleRenaming`` is translated into
   the flag ``-unit-id "UnitId ModuleRenaming"`` (the
   unit identity and module renaming are passed as a single
   argument with a literal space separating them).

2. ``"module" ModuleName`` is translated into the argument ``ModuleName``,
   identifying an ``hs`` file in the include path.

3. ``"signature" ModuleName`` is translated into the argument
   ``ModuleName``, identifying an ``hsig`` file in the include path.

4. The header of a unit ``"unit" ComponentId`` is translated into
   ``-this-unit-id UnitId``, where ``UnitId`` consists of ``ComponentId``
   and a generalized module substitution ``m=<m>``, for each ``m``
   in the free module variables of ``dependency`` and each ``m``
   in ``signature m``.

A single module or signature in an indefinite unit can be
typechecked with ``ghc -c`` by specifying only that module name.
(this motivates the "redundant" specification of requirements in
``this-unit-id``; it's not redundant when compiling with ``ghc -c``!)

For compiling a definite unit with module substitution ``ModuleSubst``,
the only difference is ``-this-unit-id`` now takes a ``UnitId``
consisting of ``ComponentId`` and the specified substitution
``ModuleSubst`` to instantiate the module with.  Note that the
unit identifiers from dependencies do NOT have the substitution
applied to them (GHC can read off the necesary substitution from
``-this-unit-id``; in fact, applying this substitution would
lose information.)

Not all unit identifiers are valid as arguments to
``this-unit-id``: only fully generalized unit identifiers
(in the case of typechecking indefinite units) or unit identifiers
with no free module variables (in the case of compiling a definite
unit) are permissible.

We impose an additional requirement that there must be an ``hsig`` file
for every required signature of a unit (even if all nontrivial
requirements are inherited from dependencies).  This is to preserve the
compilation model of "one source file, one invocation of the compiler."

Installed unit database
~~~~~~~~~~~~~~~~~~~~~~~

The **installed unit database** (previously known as the installed
package database) records the set of units which can be referenced by
``dependency`` declarations.  A unit identifier uniquely identifies
a unit in the installed unit database.

Logically, entries in the installed unit database can be classified
into three types:

1. Non-Backpack libraries (units), for which the unit identifier is
   equivalent to the component identifier,

2. Definite Backpack units, where the unit identifier has a nontrivial
   substition with no free module variables, and

3. Indefinite Backpack units, where the unit identifier is in most
   general form.

Definite Backpack units and non-Backpack libraries never depend on
indefinite Backpack units.  Indefinite Backpack units never refer
to definite Backpack units (this makes typechecking possible even
when no compilation has occurred at all); e.g., a dependency on a
partially instantiated unit identifier refers to the indefinite
unit identified by the generalized unit identifier.

An entry in the installed unit database records a variety of
information, but the most important for the purposes of Backpack
are the **exposed modules**, which are a mapping from module names
to module identities, which specify what modules get brought into
scope when we declare a dependency on the library.

Signatures
~~~~~~~~~~~~~~~~~

A **signature** defines a collection of type signatures, datatypes,
abstract types, type synonyms, classes, etc.  A signature
contains enough information to typecheck any modules which
depend on it, but not enough to compile them.

::

    udecl ::= "signature" ModuleName

A **source signature** is a locally defined signature
signature file whose contents are exactly that of an `hs-boot file <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules>`_,
except with the following differences:

* The file extension is ``hsig``, not ``hs-boot``.

* A signature at module ``A`` is imported directly by using ``import A``
  (no ``{-# SOURCE #-}`` pragma is necessary.)

* It is an error to define a module with the same name as a
  signature; in this version of Backpack, signatures are
  *not* used to implement mutual recursion (although, in
  principle, they can be).

* TODO not implemented: Explicit declarations of type equalities, e.g.,
  ``instance T ~ S`` are permitted.

Notably, type classes and instances can be declared in a signature file.

A module **implements** a signature in the same way an
``hs`` file implements an ``hs-boot`` file, except with
the following differences:

* A declaration in a signature may be implemented by a
  *reexport* in the implementation, so long as the reexported
  entity implements the declaration according to these rules.

* An abstract data type declaration ``data T`` can be implemented
  by a type synonym with the same kind.

A notable restriction carried over from ``hs-boot`` files is that a
polymorphic function cannot be used to implement a monomorphic one: the
user must monomorphize it in the implementing module. (TODO: maybe
we can lift this by adding dummy definitions to impedance match.)

Signatures can be **merged** with other signatures to form
merged signatures.  The merge of signature S1 and S2 is
the minimal signature S, such that any module which implements S
also implements S1 and S2.  Signature merging can be specified
algorithmically as follows:

* For each occurrence name exported by both signatures,
  if the declaration d implements another declaration d',
  d and d' merge to d.  (So, for example, ``data T = MkT`` implements
  ``data T``, so they merge to ``data T = MkT``.)  This is
  deterministic, as the *implements* relation for signatures
  is trivial.

* The other declarations are simply unioned.

The **local merged signature** of a unit at some module name ``m``
is the merge of all signatures from unit dependencies which were
instantiated with ``<m>``, as well as the local source signature at ``m``.
This merged signature is what is brought into scope when a module
or source signature writes ``import m``.

Dependencies
~~~~~~~~~~~~

::

    udecl ::= "dependency" UnitId ModuleRenaming
    ModuleRenaming    ::= ""
                        | "(" entry "," ... "," entry ")"
                        -- TODO: "hiding" "(" ModuleName "," ... "," ModuleName ")"
    entry ::= ModuleName
            | ModuleName "as" ModuleName

A ``dependency`` declaration instantiates an external unit,
possibly parametrized over the requirements of the enclosing unit.
A ``dependency`` declaration has two primary functions:

1. First, all occurrences of a module variable in the
   unit identifier of a unit dependency specify all external
   signatures which will be **inherited** to this unit.
   The inherited signatures for each module variable are merged with the
   corresponding local source signature to form the local merged
   signature that represents this module variable.

2. Second, module substitution of the dependencies **instantiates**
   the unit, essentially by rewriting the unit's internal imports on its
   signatures to the specified modules (or local merged signatures).
   The resulting instantiated modules are brought into scope (modulo a
   module renaming) and can be imported by source signatures and modules.

Merging and instantiation happen in an interleaved fashion, as
a local source signature may be used to instantiate an external
dependency, which in turn contributes an unfilled requirement
to be merged into another signature.  For clarity of exposition,
we will present them separately.

Signature inheritance
'''''''''''''''''''''

The **inherited signatures** are a mapping from required module names to a
*set* of module identifiers identifying signatures from external units.
The inherited signatures induced by a ``dependency`` are determined by
the following recursive procedure:

Given a unit identity ``p[S]``:

1. For each entry of the form ``m = <m'>`` in ``S``, add
   a mapping from ``m'`` to ``p[S]:m`` to the in-scope signatures.

2. For each entry of the form ``m = P:m'`` in ``S``, recursively
   process ``P``.

For example, a unit with the declaration::

    dependency p[A=q[B=<H>]:C,D=<H>]:E

brings into scope ``q[B=<H>]:B`` and ``p[A=q[B=<H>]:C,D=<H>]:D`` under
the signature name ``H``.

The inherited signatures for a module name are merged together with
the source signature to form the local merged signature, which
is subsequently used to fill all occurrences of ``<m>``.

Unit instantiation
''''''''''''''''''

A unit with requirements can be thought of as a collection of modules
which import some signatures.  The process of *instantiating* such a
unit replaces the imports of these signatures with the modules specified
by the module substitution::

    unit p where          p/A.hs ---imports---> p/H.hsig
      signature H
      module A -- import H

    -- Import on p/H.hsig is replaced with import to q/H.hs
    dependency p[H=q:H]   p/A.hs -\             p/H.hsig <-\
                                   \--imports-> q/H.hs ----/ implements

    -- Import on p/H.hsig is replaced with import of local merged signature H
    dependency p[H=<H>]   p/A.hs -\             p/H.hsig
                                   \--imports-> (local merged signature H)

For each dependency, we must show that each implementing module
*implements* the signature of the unit it is instantiating.  We can
check if a unit identity is *well-typed* with the following recursive
procedure: given a unit identity ``p[S]``, for each entry ``m = M`` in
``S``, if ``M`` has the form ``P:m`` recursively check that ``P`` is
well-typed.  Then, check that ``M`` *matches* the signature ``p[S]:m``.

The instantiated modules are then brought into scope for import
in the following ways:

1. The included modules can be specified explicitly by listing them
   in the ``ModuleRenaming``. An entry can either be a bare ``ModuleName``,
   in which case the exposed module at that name is brought into scope,
   or using ``m as m'``, in which case the exposed module at ``m``
   is brought into scope under the name ``m'``.

2. If the ``ModuleRenaming`` is omitted, all modules exposed by the
   specified unit are brought into scope.

Library components
---------------------

A library component defines a collection of provided modules and
required signatures in an environment that is created by a set of
``backpack-includes`` and ``build-depends``.  Library components are
specified in the Cabal file and are the user-facing interface for
working with Backpack.  This environment of in-scope modules implies how
requirements of the dependent components are wired up through mix-in
linking; no explicit instantiation is necessary.  A library exports some
modules, making them available to other components; required signatures
are always exported.

Components may reference each other through ``build-depends``, which
implicitly bring all of the exposed modules of the referenced component
into scope, or an explicit ``backpack-includes``, which can specify
which modules to bring under scope or instantiate a referenced component
multiple times.

Components are used for name-space control, which in turn specifies how
mix-in linking is carried out.  Higher-order components are out of scope
for this proposal: components are parametrized by the requirements they
define or inherit.

Cabal's library components predate Backpack, but for completeness
we give a full description of it here.

Library structure
~~~~~~~~~~~~~~~~~~~

A library component defines a scope containing declarations for modules
and signatures.

::

    library ::=
        "library" ( PackageName )?
            library-fields *

    library-fields ::=
        "exposed-modules:"      exposed-module        ...     exposed-module
      | "other-modules:"        other-module          ...     other-module
      | "required-signatures:"  required-signature    ...     required-signature
      | "reexported-modules:"   reexported-module "," ... "," reexported-module
      | buildinfo-fields
      | ... -- Cabal supports more fields

    build-info ::=
        "backpack-includes:"    backpack-include  "," ... "," backpack-include
      | ... -- Cabal supports more fields

A library begins with a header: the keyword ``library``, an
optional library name (if omitted, the name defaults to the
name of the package), and then a series of library fields defining
what is brought into scope, what is defined and what is exported.

Cabal also defines test suite, benchmark and executable components
which only include ``build-info`` fields; we will ignore them for
the purposes of this specification.

Exports
~~~~~~~

::

    exposed-module    ::= ModuleName
    reexported-module ::= ( PackageName ":" ) ModuleName ( "as" ModuleName )?

The ``exposed-modules`` field consists of a list of module names to
be exported by the component.  Declaration identifies the locally
defined modules (not signature) that are exported by the library
component.

The ``reexported-modules`` field consists of a list of possibly
package qualified module name to be reexported from a component, possibly under
a different name.  Every named module must be in scope.  The (possibly)
qualified module name must unambiguously identify a module: while it is
not an error to have to modules in scope under the same name, it is an
error to reexport such a module name without qualification.  Like in
Haskell, it is possible to construct a scope where it is not possible
to unambiguous refer to a module name.

The unqualified names of every exposed and reexported module must be
distinct.  For example, the following component is invalid::

    exposed-modules: A
    reexported-modules: B as A

Reexported modules are NOT available for locally defined modules to
``import``; they strictly affect the exports of a component.

Includes
~~~~~~~~

::

    backpack-include  ::= PackageName IncludeRenaming

    IncludeRenaming    ::= ModuleRenaming ( "requires" ModuleRenaming )?
                         -- TODO: proposed alternate syntax
                         | ModuleRenaming ( "satisfy" WithModuleRenaming ) ?

    WithModuleRenaming ::= ""
                         | "(" with_entry "," ... "," with_entry ")"
    with_entry ::= ModuleName "with" ModuleName

Entities exported by a library component can be brought into scope in
another component via the ``backpack-includes`` field.

What provisions are brought into scope
''''''''''''''''''''''''''''''''''''''

Exactly which provided modules are to be brought into scope in two
ways:

1. The imported module names can be specified explicitly by listing them
   in parentheses.  A module name can be renamed using the ``as``
   keyword: ``p (A as B)`` imports the module exported from component
   ``p`` with name ``A`` under the new name ``B``.

2. If the module renaming is omitted, all modules provided by the
   specified component are brought into scope.

Package qualified modules
'''''''''''''''''''''''''

For each module brought into scope, it is brought into scope both as an
unqualified module name, and a package-qualified name qualified by the
package name of the ``backpack-include`` which brought it into scope.

A programmer can refer to a package-qualified in several situations:

1. With the GHC extension ``PackageImports``, a package qualified
   import ``import "pkgname" M`` can disamiguate between two
   modules which have the same unqualified name.

2. In the ``reexported-modules``, the package qualifier can be used
   to disambiguate which module should be reexported.

Implicit build-depends includes
'''''''''''''''''''''''''''''''

::

    buildinfo-fields ::=
        "build-depends:"        build-depend      "," ... "," build-depend
      | ...

    build-depend      ::= PackageName VersionBound

Traditionally, the ``build-depends`` field both specifies
version bounds for each external package dependency of the component
(to be used by the dependency solver) AND brings all of the
exported modules of that component into scope.

We preserve this behavior by introducing the following "implicit
include" rule: every package name ``p`` in ``build-depends`` which is
not mentioned by any include in ``backpack-includes`` adds an implicit
include ``p`` (with the default provision and requirement renaming).
Since the implicit include is only added when the package name is not
mentioned by ``backpack-includes``, it can be suppressed simply by specifying
an include, e.g., ``backpack-includes: p ()``, which does not bring any
provided modules into scope.

Conversely, as the dependency solver requires version bounds for all
external packages, any package name referenced in a ``backpack-include``
must also be mentioned in a ``build-depends`` version bound, so that
the dependency solver solves for it.

Mix-in linking
~~~~~~~~~~~~~~

An included component may also specify some requirements.  Like
provided modules, these requirements are brought into the same scope
as provided modules.  However, when a requirement has has the
same name as a module, mix-in linking occurs.  Mix-in linking
follows the following rules:

1. Unlike provided modules, a requirement cannot be hidden; it is
   always brought into scope.  Like provided modules, they can be
   renamed using the ``as`` keyword in the module renaming
   after the ``requires`` keyword.

   TODO: An alternative proposed syntax is ``satisfy`` keyword:
   ``p (Impl) satisfy (Str with ByteString, Path with FilePath)``
   specifies that the holes ``Str`` and ``Path`` are brought
   into scope under the names ``ByteString`` and ``FilePath``,
   respectively, making it clearer in intent.

2. If a requirement is brought into scope under the same module
   name as an unambiguous provided module, the requirement is *linked*:
   that module is used to instantiate the component with this
   requirement.  It is an error if the module is ambiguous.

3. If a requirement is brought into scope without being linked
   against an implementation, it automatically becomes
   a requirement of this component.  Components inherit
   unlinked requirements of components they depend on.

4. If two requirements are brought into scope under the same name,
   they are *merged* into a single requirement, which is merged
   by itself.  (This process is carried out by the compiler
   under the name of `signature merging`_.)

5. Every include of a component generates a fresh set of requirements.
   These requirements may be *merged* together, but they do not
   have to be (i.e., if they are renamed).

Intuitively, every component can be represented as a box with
outgoing wires labeled by module name for the modules it provides, and
incoming wires labeled by module name for the signatures it requires.
When two wires have the same module name, they are linked up.  The
wiring diagram then is translated into unit identifiers which
are passed to the compiler in the unit language.

Modules and signatures
~~~~~~~~~~~~~~~~~~~~~~

::

    exposed-module     ::= ModuleName
    other-module       ::= ModuleName
    required-signature ::= ModuleName

The ``exposed-modules``, ``other-modules`` and ``required-signatures``
field specify the Haskell modules (``hs``) and signatures (``hsig``)
which are locally defined by this package.  It is NOT required for all
the transitive requirements of a component
to be listed in ``required-signature``: only requirements which have
locally defined ``hsig`` files are needed.

These modules are added to the scope *after* all ``backpack-includes``
have been linked together, but before ``reexported-modules`` is
processed.  This is because using a locally defined module to implement
an included component constitutes a mutually recursive reference, which
we consider out-of-scope for this proposal.  (TODO: Does this work right? Test.)

Setup interface
---------------

The ``./Setup configure`` interface is extended with a new
``--instantiated-with`` flag, which may be specified multiple times,
taking an argument with the grammar ``ModuleName "=" Module`` (i.e., an
entry in a module substitution).  This parameter specifies how the
public library (if the package is being configured for all components)
or the specified component (if the package is being configured in
one-component mode) should be instantiated.  The module specified in
this flag MUST NOT contain any free module variables; that is to say,
this flag is only used to instantiate a package with *definite modules*.
Combined with the ``--cid`` parameter, this forms the unit identifier
of the library we are compiling.

In all situations (including instantiated components), the ``--dependency``
flag is used to specify a component identifier, NOT a unit identifier. The
``Setup`` script is responsible for performing `mix-in linking`_ in order to determine
the actual unit identifier dependencies, when a unit is fully instantiated,
which are then passed to the compiler.

Drawbacks
---------

Language design is always about trade-offs, and Backpack positions
itself in a very specific part of the design space:

* It is NOT a compiler only change; it is a cross-cutting design
  that affects GHC, Cabal and cabal-install.  An alternative
  package manager like Stack will not get support for Backpack
  out of the box; it must be taught how to instantiate Backpack
  packages.  Similarly, it will not integrate seamlessly with GHCi,
  which only knows about GHC and not Cabal, and may cause a headache
  for OS distributors, who now have to create
  a package for every instantiation of a library.

* It operates at the package level, which makes it inconvenient
  for some uses of small scale modularity.  We believe this is
  compensated for by Haskell's support for type classes, which
  already provide a very convenient mechanism for small-scale
  modularity, and that modularity at the *large-scale* is where
  a module system can have the most leverage.

* Backpack does not and cannot give a guarantee that if an
  implementation matches a signature, it will be guaranteed
  to link against all code that uses that signature.  In fact,
  in the presence of open type families such a guarantee
  `is impossible <http://blog.ezyang.com/2014/09/open-type-families-are-not-modular/>`_.

Alternatives
------------

* The most obvious alternative, competing design are ML functors.
  However, adding ML functors to Haskell would have made
  Haskell's already complicated surface language even more
  complicated, and would have inherited all of the same
  complexity problems of ML functors, including the preponderance
  of *sharing constraints*.

* Backpack implements applicative functors, so that so long
  as a unit is instantiated in the same way (i.e., its unit identity
  is the same) the types are also equivalent.  Backpack could
  have equally well been generative, which would have simplified
  many implementation considerations.  It is unclear how important
  applicativity actually is in practice, but given that instantiation
  via mix-in linking is an *implicit* process, it seems like the
  right choice.

* If one is willing to give up on typechecking against interfaces,
  Backpack's functionality could be implemented entirely at the
  package management layer.  To a limited extent, vendoring already
  allows users to replace dependencies on packages with customized
  code; the only missing ingredient is to instantiate a package
  multiple times with different vendored packages.

* A more radical redesign of Haskell would do away with the concept
  of a *module*, such that every type and expression is its own
  "atomic module" (ala 1ML).  Such a change seems difficult to
  retrofit onto Haskell, and poses some thorny implementation
  questions (e.g., what gets put into a dynamic library?)

Unresolved Questions
--------------------

* The original POPL'14 Backpack paper described a module system
  that supports mutual recursion; in the interest of getting
  Backpack out there, we have decided not to support this.
  In principle it is supportable, but requires compiler support
  for cross-package mutual recursion.

* We do not have any formal proofs about this iteration of Backpack,
  though heuristically we believe it to be sound as it is not too
  different from POPL'14 Backpack, which Scott Kilpatrick showed
  to be sound with respect to an elaboration to Haskell with ``hs-boot``
  files.
