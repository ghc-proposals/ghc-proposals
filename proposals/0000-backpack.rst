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

   c. `Pipeline`_

#. `Identifiers`_

#. `GHC`_

   a. `Mixed library structure`_

   b. `GHC command line flags`_

   c. `Installed library database`_

   d. `Signatures`_

   e. `Dependencies`_

#. `Cabal`_

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

Locally defined ``hsig`` files are declared in the Cabal file via the
``signatures`` field::

    library concat-indef
        signatures: Str
        exposed-modules: Concat

Signatures can also be inherited from other libraries (more on this
shortly); we refer to the set of all locally defined and inherited
signatures as the set of **required signatures**.
A library with required signatures is called an **indefinite library**.
As it is missing implementations for its required signatures, it cannot be
compiled; however, it can still be type-checked (*separate
type-checking*) and registered with the compiler, so that it can be used
by other indefinite libraries which depend on it.  In contrast, a
**definite library** is a library with no required signatures (any library that
doesn't use Backpack features is a definite library).

An indefinite library can be **instantiated** (possibly multiple times)
with implementations for all of its required signatures, allowing it to
be compiled.  Instantiation happens
automatically when a user depends on an indefinite library and another
library which provides modules with the same name as the signatures.
For implementation reasons, it is only possible to fill required signatures with
modules from ``build-depends`` (and not locally defined ones)::

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
but rather than explicitly specifying each parameter, it is
implicitly specified with module namespaces.  This process
of determining the explicit instantiations is called **mix-in linking**.

An indefinite library can be instantiated to various degrees.
Compilation does not occur unless *all* required signatures are implemented,
allowing a compiler can optimize as if Backpack was not present (*no
performance overhead.*)  An indefinite library can also be partially
instantiated, or not instantiated at all.  If a required signature is not
instantiated, it gets inherited by the user of the library::

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

It's worth reiterating that contents of a ``signatures`` field
do not specify the *required* signatures of a library, since a library
may also inherit many other required signatures from its dependencies.
(TODO: A user can explicitly specify all implicit signatures using
the ``implicit-signatures`` field.)

Backpack is quite flexible about the way the uninstantiated
required signatures can be handled:

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
  a user can also define a local ``hsig`` to refine the required
  signature further (i.e., define extra types).

The current implementation of Backpack in GHC has some notable
user-facing limitations:

1. It is not possible to define a module in a library, and then
   use it to immediately to instantiate an indefinite library::

        library concat-bytestring-bad
            build-depends: concat-indef -- has Str requirement
            exposed-modules: Str, ConcatUser -- can't use these to fill

   Instead, ``Str`` must be pulled out into a separate library
   of its own (Cabal 2.0 supports multiple libraries in a package,
   making this less burdensome.)  The reason for this restriction
   is to simplify implementation of the build system: if this
   mode of use was allowed, it would be necesary to first build
   ``Str``, then build ``concat-indef``, and then come back to
   ``concat-bytestring-bad`` and finish building the rest of the
   modules.

   Note, however, it is permissible to inherit a signature while also
   defining a local signature.

2. Mutual recursion is not allowed.  For example, these libraries
   cannot be instantiated with each other::

        library p
            signatures: A
            exposed-modules: B
        library q
            signatures: B
            exposed-modules: A

   Signature merging can also result in mutual recursion; suppose
   a library has these two signatures::

        signature A where
        signature B where
            import A

   and another library has the import swapped: merging these would
   result in a cycle between ``A`` and ``B``; thus it is not allowed.

   Eventually we do want to support mutual recursion in all these cases
   (the theory certainly allows for it), but we declared it as out of
   scope for the initial release of Backpack.

Pipeline
~~~~~~~~

An important aspect of the Backpack design is that it is separated
across GHC and Cabal, with the compiler and package manager handling
separate concerns of the design.  So while the overall
pipeline of how a Backpack library is compiled is technically not
necessary to understand the language extensions defined by Backpack,
it is helpful for motivating the structure of this
specification.

.. image:: https://raw.githubusercontent.com/ezyang/ghc-proposals/backpack/proposals/backpack-pipeline.png

The initial input into the compilation pipeline for is
your **local source code**, as well as the the **Hackage index**
which provides possible external packages that you might
be building::

    impl-0.1.tgz
    p-0.1.tgz
    p-0.2.tgz
    q-0.1.tgz

The very first thing the package manager does is do **dependency
resolution**, in order to pick specific versions and flags of
the depended upon packages, settling the specific, transitive
source code that will be built. (In cabal-install, this is done
with a backtracking solver that looks at version bounds
in ``build-depends``; in Stack, there is always a specific version
assignment that is used.)  The result is we get a series of
package descriptions (described in `Library structure`_) which refer to
specific versions of other packages::

    library impl-0.1
        exposed-modules: H

    library p-0.2
        signatures: H
        exposed-modules: P

    library q-0.1
        build-depends: p-0.2, impl-0.1
        exposed-modules: Q

At this point, we perform **mix-in linking**, taking each dependency
on an indefinite library and filling in requirements based on the
module names which are in scope.  We call these **mixed libraries**,
and they are described in `mixed library structure`_::

    library impl-0.1
        exposed-modules: H

    library p-0.2 <H>
        signature H
        module P

    library q-0.1
        dependency p-0.2[H=impl-0.1:H]
        dependency impl-0.1
        module Q

Two kinds of libraries can be output by mix-in linking:
**definite libraries** which have no holes (``impl-0.1``
and ``q-0.1``), and **indefinite libraries** which have
holes.  This is our provisional install plan.

This install plan is not complete, however:
the design of Backpack dictates that we must rebuild
an indefinite library whenever it is instantiated. The
**instantiation** step takes indefinite libraries which
are referenced by definite libraries and instantiates them according to
``dependency`` declarations.  In our running example, we add one
instantiation of ``p`` to our install plan::

    instantiate p-0.2[H=impl-0.1:H]

At this point, we transition from Cabal to GHC, with the mixed
library being translated into a series of command line flags for
GHC.

Indefinite libraries are typechecked only (no compilation
occurs).  We run GHC with the flags (described in more detail in `GHC
command line flags`)::

    ghc -this-unit-id p-0.2[H=<H>] \
        -fno-code -fwrite-interface \
        H P

Definite libraries (including those which are fully instantiated) get
compiled.  To build the instantiated copy of ``p-0.2``, for example, we
run GHC with the flags::

    ghc -this-unit-id p-0.2+k2Fa9xZlb \
        -instantiated-with "H=impl-0.1:H" \
        H P

(``p-0.2+k2Fa9xZlb`` is a hashed version of ``p-0.2[H=impl-0.1:H]``
which will be used for symbols and filepaths.)  The results
are installed to the `Installed library database`_.

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
                  | ComponentId "+" ModuleSubstHash
    ModuleSubst ::= ( ModuleName "=" Module ) +
    ModuleSubstHash ::= [A-Za-z0-9]+
    -- from Haskell'98
    ModuleName  ::= [A-Z][A-Za-z0-9_']* ( "." [A-Z][A-Za-z0-9_']* ) +
    Module      ::= UnitId ":" ModuleName
                  | "<" ModuleName ">"

.. _ComponentId:

A **component identifier** intuitively identifies the transitive closure
of source code, and is represented as an arbitrary sequence of
alphanumeric letters, dashes, underscores and periods.  Component
identifiers are uniquely allocated by the package manager (e.g.,
``cabal-install``), and in practice, encode the package name, package
version, component name, and a hash (which is computed over the source
code sdist tarball, Cabal flags, GHC flags and component identifiers of
direct dependencies of the component.)

A component identifier tracks only direct dependencies (i.e.,
``build-depends``) as determined by the dependency solver, but
not indirect dependencies (i.e., how an indefinite library is
instantiated).  A component identifier uniquely identifies a source library
(whether it's definite or indefinite.)
We will use the metavariable ``p`` to represent component identifiers.

Example: the component identifier for ``concat-indef``
might be ``concat-indef-0.1-abcdefg``.

.. _UnitId:

A **unit identifier** consists of a component identifier combined with a
module substitution describing how the library is instantiated.
Non-Backpack libraries do not have a module substitution (since they
have no signatures to fill).  A unit identifier with no free module
variables (see below) uniquely identifies an instantiated library for
which we can compile code.  We will use the metavariable ``P`` to
represent unit identifiers.

A fully instantiated unit identifier which have compiled (or are
compiling) is specified in a compressed form, a **hashed unit
identifier.**  This hashed unit identifier is used for symbol
names and file paths.

Example: a fully uninstantiated unit identifier for ``concat-indef``
would be ``concat-indef-0.1-abcdefg[Str=<Str>]``; if instantiated
with ``str-bytestring``, it's unit identifier is
``concat-indef-0.1-abcdefg[Str=str-bytestring-0.2-xxx:Str]``.
The hashed version of this instantiated unit id might
look like ``concat-indef-0.1-abcdefg+xyzzyx``.

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

Example: the module identifier for ``Str`` from ``str-bytestring``
is ``str-bytestring-0.2-xxx:Str``; the module identifier for
``Concat`` frmo an uninstantiated ``concat-indef`` is
``concat-indef-0.1-abcdefg[Str=<Str>]:Concat``.

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

GHC
---------------

A library is a collection of modules and dependencies on other libraries,
which is parametrized by a set of required signatures.  Libraries live
in Cabal files, and look like this::

    library stringutils-indef
        build-depends: concat-indef
        exposed-modules: StringUtils

Cabal transforms a library into an intermediate form called a **mixed
library**, in which the dependencies are made explicit.  This
transformation involves both dependency solving (picking the source
code to be used) and *mix-in linking*::

    library stringutils-indef-0.1-xxx <Str>
        dependency concat-indef-0.1-abcdefg[Str=<Str>]
        module StringUtils

You can see that the ``build-depends`` has been translated into
a ``dependency``, which has an explicit instantion of ``Str`` with
the module variable ``<Str>`` (the names have also been expanded
with hashes, a side effect of dependency resolution).

From a mixed library, Cabal can easily generate a sequence of calls
to GHC with command line flags and input files::

    ghc -this-unit-id "stringutils-indef-0.1-xxx[Str=<Str>]" \
        -unit-id "concat-indef-0.1-abcdefg[Str=<Str>]" \
        --make Str.hsig StringUtils.hs

Mixed library structure
~~~~~~~~~~~~~~

To discuss mixed libraries in a more user friendly form, we define an
abstract syntax tree for mixed libraries and show how to translate this
AST into the command line arguments that the compiler accepts.

::

    mlib  ::= "library" ComponentId
                "<" ModuleName "," ... "," ModuleName ">"
              "where" "{"
                mdecl_0 ";" ... ";" mdecl_n
              "}"
    mdecl ::= "dependency" UnitId ModuleRenaming
            | "module"    ModuleName
            | "signature" ModuleName

A mixed library begins with a header recording its component identity
and a list of its required signatures.  The body of a library consists
of any number of dependencies, modules and signatures.

For example, ``concat-indef`` and ``stringutils-indef`` would have the
following ASTs::

    library concat-indef-0.1-abcdefg <Str> where
        signature Str
        module Concat

    library stringutils-indef-0.1-xxx <Str> where
        dependency concat-indef-0.1-abcdefg[Str=<Str>]
        module StringUtils

There are two operations we can perform on a mixed library with
required signatures:

1. We can **typecheck** it, which can be done with the library
   all by itself and generates interface files or

2. We can **compile** it against some instantiation of its signatures
   to implementations, giving us object code.

**Typechecking an uninstantiated mixed library.** Each
declaration in ``ghc --make`` desugars into a flag (defined in
the next section) or an argument:

1. ``"dependency" UnitId ModuleRenaming`` is translated into
   the flag ``-unit-id "UnitId ModuleRenaming"`` (the
   unit identity and module renaming are passed as a single
   argument with a literal space separating them).

2. ``"module" ModuleName`` is translated into the argument ``ModuleName``,
   identifying an ``hs`` file in the include path.

3. ``"signature" ModuleName`` is translated into the argument
   ``ModuleName``, identifying an ``hsig`` file in the include path.
   Every required signature *must* have an ``hsig`` file (unlike
   the Cabal syntax, where required signatures can be implicit);
   it is expected that Cabal generates blank signature files for
   all inherited signatures.  (This restriction simplifies the
   compilation model: one source file per compilation product. It may be
   lifted in the future.)

4. The header of a mixed library ``"library" ComponentId "<" ModuleName + ">"``
   is translated into
   ``-this-unit-id UnitId``, where ``UnitId`` consists of ``ComponentId``
   and a generalized module substitution ``m=<m>``, for each ``ModuleName``.

Thus, these two ASTs would translate into these two command lines::

    ghc -this-unit-id "concat-indef-0.1-abcdefg[Str=<Str>]" \
        --make Str.hsig Concat.hs

    ghc -this-unit-id "stringutils-indef-0.1-xxx[Str=<Str>]" \
        -unit-id "concat-indef-0.1-abcdefg[Str=<Str>]" \
        --make Str.hsig StringUtils.hs

**Compiling an instantiated mixed library.**  To compile an instantiated
mixed library, we specify specify an instantiated unit identifier::

    ghc -this-unit-id "concat-indef-0.1-abcdefg+xyz12345" \
        -instantiated-with "Str=str-bytestring-0.2-xxx:Str" \
        --make Str.hsig Concat.hs

    ghc -this-unit-id "stringutils-indef-0.1-xxx+hijklm" \
        -instantiated-with "Str=str-bytestring-0.2-xxx:Str" \
        -unit-id "concat-indef-0.1-abcdefg+xyz12345" \
        --make Str.hsig StringUtils.hs

There are a few other differences in the command line format:

1. ``-this-unit-id`` is provided in *hashed form*.
   We must also provide how this unit is to be instantiated
   using the `-instantiated-with` flag.  The hashed unit identifier is
   passed to GHC (rather than GHC computing it itself) so that Cabal can
   allocate the hash, and also use it to name the library in the file
   system.

2. The ``-unit-id`` flag accepts a hashed unit identifiers.

GHC command line flags
~~~~~~~~~~~~~~~~~~~~~~~

In this section, we summarize the accepted command line flags of GHC:

``-this-unit-id``
    Specifies the unit identifier of the library we are compiling
    (the **home library**).  This unit identifier must either be
    completely uninstantiated or a hashed unit identifier (partially
    instantiated unit identifiers are illegal.)  If it is
    uninstantiated, this means we are typechecking the code only.

``-instantiated-with``
    The *module substitution* of the library we are
    instantiating.

``-unit-id``
    Specifies the unit identifier of a library we depend on.
    The module variables of these unit identifiers can only refer
    to module variables in ``-this-unit-id``.

``ghc --make`` accepts a list of modules and signatures that are
to be typechecked or compiled.

Installed library database
~~~~~~~~~~~~~~~~~~~~~~~

The **installed library database** (previously known as the installed
package database) records uninstantiated and instantiated libraries
that have been typechecked or compiled (respectively) so that they can be reused
in later invocations of GHC.

Logically, the installed library database is composed of three parts:

1. Uninstantiated indefinite libraries, which are uniquely
   identified by component identifier.  These libraries have only been
   typechecked, but they can be instantiated on the fly according to a
   unit identifier::

        library p <H>
            signature H

2. Fully instantiated indefinite libraries, which are uniquely identifed by
   unit identifier with no free module variables in its instantiating
   substitution. These libraries have been compiled::

        library p[H=himpl:H]
            signature H

3. Definite libraries, which don't have any signatures and
   thus can't be instantiated in any meaningful way.  These libraries
   have been compiled.  These are uniquely identified by component
   or unit identifier (as without instantiations, these identifiers
   are equivalent)::

        library p
            module M

To typecheck an indefinite library, you need to have first
installed all the uninstantiated and definite libraries you
depend on.  To compile a definite library, you need the
fully instantiated and definite libraries you depend on.

Instantiated/definite libraries cannot depend on uninstantiated
libraries (since there's no compiled code to actually depend on);
however, uninstantiated libraries can depend on
instantiated/definite libraries.

(TODO: This ties into some tricky implementation business, where
an uninstantiated library depends on a non-immediate instantiated
library, because it partially instantiates a dependency which
in turn instantiates the full library.)

Note that there are never partially instantiated libraries in
the database: instead, these instantiations are computed "on the
fly" from the corresponding fully uninstantiated library.

During the course of compilation, a user may look up an uninstantiated
or instantiated/definite library by querying for a component
or unit identifier (respectively).
An entry in the installed library database records a variety of
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

    mdecl ::= "signature" ModuleName

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

* Entities defined in the signature ``H`` are given the
  original name ``{H.T}``, stating that ``T`` comes from
  some unspecified module.  (This module need not be ``<H>``,
  since the implementor may reexport it from elsewhere.)  For example::

    signature H where
    data T
    x :: T

  ...defines two entities with original names ``{H.T}`` and ``{H.x}``.

* TODO not implemented: Explicit declarations of type equalities, e.g.,
  ``instance T ~ S`` are permitted.

Notably, type classes and instances can be declared in a signature file.

A module **implements** a signature in the same way an
``hs`` file implements an ``hs-boot`` file, except with
the following differences:

* A declaration in a signature may be implemented by a
  *reexport* in the implementation, so long as the reexported
  entity implements the declaration according to these rules.
  For example::

    module M(Bool) where
    import Prelude(Bool)

  ...is an adequate implementation of::

    signature M where
    data Bool

* An abstract data type declaration ``data T`` can be implemented
  by a type synonym with the same kind.  For example::

    module M where
    type T = Bool

  ...is an adequate implementation of::

    signature M where
    data T

A notable restriction carried over from ``hs-boot`` files is that a
polymorphic function cannot be used to implement a monomorphic one: the
user must monomorphize it in the implementing module. (TODO: maybe
we can lift this by adding dummy definitions to impedance match.)

Signatures can be **merged** with other signatures to form
merged signatures.  The merge of signature S1 and S2 is
the minimal signature S, such that any module which implements S
also implements S1 and S2.  Signature merging can be specified
algorithmically as follows:

* For each export of the signatures which have the same
  occurrence name, we unify their original names, with
  the flexible variables (ones that can be the target
  of a unifying substitution) of the form ``{S.n}``,
  and otherwise union the other exports.
  So for example, if we merge these two signatures::

    signature A(Bool,x) where
        data Bool -- original name {A.Bool}
        x :: Bool

    signature A(Bool,y) where
        import Prelude(Bool)
        y :: Bool

  ...the resulting signature is::

    signature A(Bool, x, y)
        import Prelude(Bool)
        -- {A.Bool} was unified to base:Prelude.Bool
        x :: Bool
        y :: Bool

* For each occurrence name exported by both signatures,
  if the declaration d implements another declaration d',
  d and d' merge to d.  (So, for example, ``data T = MkT`` implements
  ``data T``, so they merge to ``data T = MkT``.)  This is
  deterministic, as the *implements* relation for signatures
  is trivial.  So for example::

    signature A where
        data Bool

    signature A where
        data Bool = True | False

  merges to::

    signature A where
        data Bool = True | False

* The other declarations are simply unioned.

The **home signature** of a unit at some module name ``m``
is the merge of all signatures from unit dependencies which were
instantiated with ``<m>``, as well as the local source signature at ``m``.
This merged signature is what is brought into scope when a module
or source signature writes ``import m``.

Dependencies
~~~~~~~~~~~~

::

    mdecl ::= "dependency" UnitId ModuleRenaming
    ModuleRenaming    ::= ""
                        | "(" entry "," ... "," entry ")"
                        -- TODO: "hiding" "(" ModuleName "," ... "," ModuleName ")"
    entry ::= ModuleName
            | ModuleName "as" ModuleName

A ``dependency`` declaration specifies a dependency on an
external library.  For example::

    dependency p[A=<B>]

...states that we depend on the library ``p``, with its requirement ``A``
instantiated with the ``B`` requirement from our **home library** (the
library the dependency is in).  The unit identity of a dependency
declaration specifies what implementations we feed *in* to the dependency
and provides modules for us to import.  (For example, if ``p`` provides
the module named ``M``, this dependency brings ``M`` into scope.)

More subtly, the dependency identifier feeds *out* required signatures
which get merged to the signature of the home library.  For example,
``p[A=<B>]`` additionally states that, to compute the required signature
``B`` in our home library, we must merge in the requirement ``A`` from
``p``.  The operation of the unit identifier is bidirectional: it
feeds out signatures for us to **merge**, but we feed in the merged
signatures and implementations to **instantiate** it.

Signature merging
'''''''''''''''''''''

When we want to merge all external signatures for a signature in the
home library ``m``, we must find all occurrences of ``<m>`` in the
dependencies of our library.  Let us call the **inherited signatures**
of a library a mapping from a required signature module name to a
set of module identifiers identifying signatures to be merged into that
required signature.

The inherited signatures induced by a ``dependency`` are determined by
the following recursive procedure:

Given a unit identity ``p[S]``:

1. For each entry of the form ``m = <m'>`` in ``S``, add
   a mapping from ``m'`` to ``p[S]:m`` to the in-scope signatures.

2. For each entry of the form ``m = P:m'`` in ``S``, recursively
   process ``P``.

For example, a unit with the declaration::

    dependency p[A=q[B=<H>]:C,D=<H>]:E

specifies that signatures ``q[B=<H>]:B`` and ``p[A=q[B=<H>]:C,D=<H>]:D``
must be merged to form the home signature ``H``.

Dependency instantiation
''''''''''''''''''

A library with requirements can be thought of as a collection of modules
which import some signatures.  The process of *instantiating* such a
unit replaces the imports of these signatures with the modules specified
by the module substitution::

    unit p where          p/A.hs ---imports---> p/H.hsig
      signature H
      module A -- import H

    -- Import on p/H.hsig is replaced with import to q/H.hs
    dependency p[H=q:H]   p/A.hs -\             p/H.hsig <-\
                                   \--imports-> q/H.hs ----/ implements

    -- Import on p/H.hsig is replaced with import of home merged signature H
    dependency p[H=<H>]   p/A.hs -\             p/H.hsig
                                   \--imports-> (home merged signature H)

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

Cabal
---------------------

A library defines a collection of provided modules and
required signatures in an environment that is created by a set of
``backpack-includes`` and ``build-depends``.  Libraries are
specified in the Cabal file and are the user-facing interface for
working with Backpack.  Unlike mixed libraries, the environment of
in-scope modules implies how requirements of the dependent components
are wired up through mix-in linking; no explicit instantiation is
necessary.  A library exports some modules, making them available to
other components; required signatures are always exported.

Libraries may reference each other through ``build-depends``, which
implicitly bring all of the exposed modules of the referenced component
into scope, or an explicit ``backpack-includes``, which can specify
which modules to bring under scope or instantiate a referenced component
multiple times.

Libraries are used for name-space control, which in turn specifies how
mix-in linking is carried out.  Higher-order libraries are out of scope
for this proposal: libraries are parametrized by the requirements they
define or inherit.

Cabal's libraries predate Backpack, but for completeness
we give a full description of it here.

Library structure
~~~~~~~~~~~~~~~~~~~

A library defines a scope containing declarations for modules
and signatures.

::

    library ::=
        "library" ( PackageName )?
            library-fields *

    library-fields ::=
        "exposed-modules:"      exposed-module        ...     exposed-module
      | "other-modules:"        other-module          ...     other-module
      | "signatures:"           signature             ...     signature
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

Entities exported by a library can be brought into scope in
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

    exposed-module  ::= ModuleName
    other-module    ::= ModuleName
    signature       ::= ModuleName

The ``exposed-modules``, ``other-modules`` and ``signatures``
field specify the Haskell modules (``hs``) and signatures (``hsig``)
which are locally defined by this package.  It is NOT required for all
the transitive requirements of a component
to be listed in ``signatures``: only requirements which have
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
