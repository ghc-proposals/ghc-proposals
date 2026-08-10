---
author: Michael Peyton Jones
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0).

# Pure Template Haskell

Template Haskell couples the ability to do useful pure computation with the ability to perform arbitrary IO. 
The usage of IO is poorly specified, difficult for tools to handle, and comparatively rarely used in the wild.

We propose to move towards *pure* Template Haskell, where Template Haskell by *default* does not allow the use of IO, while still leaving that option open for those who need it.

This proposal consists of:

- New syntax for impure splices
- Additions to `template-haskell` to allow more functionality in pure splices
- Two language extensions to 1) enforce purity of splices and 2) explicitly control the presence of impure splices.
- Warnings to support a migration path to pure-by-default

## Motivation

The beauty of Template Haskell is that it allows us to do compile-time work with normal Haskell. 
This makes it easy to support _creating_ IO actions... at which point why not run them?

But allowing Template Haskell code to perform IO makes the compiler as a whole much more impure, and crucially it becomes impure in ways that are impossible to predict, since they depend on user-defined code.

This causes all kinds of headaches.

### Working directories

The popular `file-embed` library allows users to embed the contents of a file into their program at compile-time.

It is typically used like this: `myfile = $(embedFile "dirName/fileName")`

However, the argument is a relative file path. 
So the behaviour of this code will depend on the working directory when the IO action that reads the file is run.

Who determines the working directory? 
Well, whoever runs GHC. 
Which means the build tool, or possibly HLS. 
But these tools *do not agree*!

This is a real problem that has come up in practice: https://github.com/haskell/haskell-language-server/issues/1584

The upshot is that if you use relative paths naively in Template Haskell, it is impossible to make your project compile with both cabal and stack. 
There are workarounds, such as searching for a “project file” and then using a path relative to that, but the underlying issue is that the “working directory when compiling a package” is not specified anywhere. 
And why should it be? It wouldn’t matter... except that it’s visible to the program being compiled.

### Cross-compilation

Using Template Haskell when cross-compiling is already difficult due to the fact that GHC will only compile code for the host system. 
But Template Haskell requires us to actually execute such code, hence the need for the external interpreter which can execute host code.

IO complicates this even further. 
Where does the IO take place? 
Users typically assume they can read files from, say, inside the source repository they are building, so if the external interpreter is running in a container, we need to ensure it can see those files so it can read them. 
What if the system locale is different? 
What if the target container doesn’t even have a filesystem?

Again, the underlying problem is that there is no specification of where the IO is happening. 
And why should there be? It wouldn’t matter... except that it’s visible to the program being compiled.

Pure Template Haskell would by no means deal with all the problems of cross-compiling with Template Haskell, but it would at least remove some of them. Cross-compilers could conceivably refuse to compile impure Template Haskell (although this would likely be too stringent in practice).

### Recompilation avoidance

Despite the fact that Template Haskell can perform IO, GHC currently uses a fairly optimistic recompilation check, which essentially assumes that the results of the IO are the same. 
Users can use `addDependentFile` to declare a dependency on an external file, giving a limited improvement to recompilation checking.

Pure Template Haskell would help in that the current recompilation check would actually be correct for modules that only have Pure Template Haskell. 
We would still have the same difficulty for modules with impure Template Haskell, but since there would be far fewer, we could potentially use a more conservative recompilation check without having a terrible impact on the innocent majority.

### Better motivation

The above motivation is somewhat weak. 
Alone, this proposal is more of a “cleanliness” proposal. However, the author believes it is an important step towards making TH easier for GHC to work with, which in the long run will be quite beneficial. 
The motivation for this proposal is therefore missing some support from an (unwritten) larger manifesto on this topic.

## Proposed Change Specification

### 0. Terminology

A Template Haskell quote is said to be "impure" if it has type `m a` where `m` has or requires a `MonadIO` instance.

A Template Haskell splice is said to be "impure" if the code inside the splice has type `m a` where `m` has or requires a `MonadIO` instance.

Note that today types for quotes and splice code are already inferred with appropriate constraints depending on what they use.

### 1. Syntax

A new syntax for top-level “impure splices” is introduced.

- Impure untyped splice: `$!..`
- Impure typed splice: `$$!..`

We will elide the distinction between a splice being impure in the sense of "needing IO" and a splice being impure in the sense of "using impure splice syntax", so long as this is not confusing.

Splices which do not use the impure splice syntax are called “normal” splices. This includes:
- Splices of all kinds when `EnforceTemplateHaskellPurity` is disabled
- Pure splices when `EnforceTemplateHaskellPurity` is enabled
- Nested (non-top-level) splices

### 2. Additions to `template-haskell`

Introduce a new class `Reify`, containing the `reify` methods from `Quasi`:
```
–- could use MonadError here, but we’re going to impose a Reify m constraint at 
-- the top level, and we probably want users to be able to pick their own 
-- error monad for m if necessary
class Monad m => Reify m where
  reify          :: Name -> ExceptT ReifyError m Info
  reifyFixity    :: Name -> ExceptT ReifyError m (Maybe Fixity)
  reifyType      :: Name -> ExceptT ReifyError m Type
  reifyInstances :: Name -> [Type] -> ExceptT ReifyError m [Dec]
  reifyRoles         :: Name -> ExceptT ReifyError m [Role]
  reifyAnnotations   :: Data a => AnnLookup -> ExceptT ReifyError m [a]
  reifyModule        :: Module -> ExceptT ReifyError m ModuleInfo
  reifyConStrictness :: Name -> ExceptT ReifyError m [DecidedStrictness]
```

Introduce a type `PureQ`, which parallels `Q` but only has access to `Quote` and `Reify`.
```
newtype PureQ a = PureQ { unPureQ :: forall m . (Quote m, Reify m) => m a }
```

`PureQ` does not provide an explicit error-reporting mechanism. For reporting errors to the user, splices should still throw exceptions, which can be caught by the compiler and reported to the user.

### 3. Language extension

Impure splice syntax is permitted only if the language extension `ImpureTemplateHaskell` is enabled.
The code in an impure splice must have type `Q a`, as with normal splices today. 

The behaviour of normal splices is controlled with a new language extension, `EnforceTemplateHaskellPurity`. 
The effect of `EnforceTemplateHaskellPurity` is that the code in a top-level normal splice must have type `PureQ a` instead of `Q a`.

Additionally, there is a new language extension `PureTemplateHaskell`, which implies `NoImpureTemplateHaskell` and `EnforceTemplateHaskellPurity`.

### 4. Changing the defaults and warnings

At some point in the future we would like to turn on `EnforceTemplateHaskellPurity` by default. 
To aid in this, we add a warning `-Wimpure-splices` (added to `-Wcompat`) which will warn if  `EnforceTemplateHaskellPurity` is not enabled, but a top-level splice would be rejected by it. 
That is, if there is a normal splice which cannot be typed as `PureQ a`.

TODO: it’s not entirely clear how we would implement this, we are complaining about a “hypothetical” type check. 

The result of this is that to be `-Wcompat`-free a module with impure TH must a) enable `EnforceTemplateHaskellPurity` and `ImpureTemplateHaskell`, and b) use impure splice syntax. 
In particular, this means that the module will continue to compile when we change the defaults (although `EnforceTemplateHaskellPurity` will become redundant).

### 5. Proposed migration path

The ideal migration path would go like this:

- Implement the technical parts of this proposal
- Users with `-Wcompat` will start to migrate to explicit impure splices.
- Once much of the ecosystem is `Wimpure-splices`-free, switch `EnforceTemplateHaskellPurity` on by default.

In parallel, as soon as there is significant explicit usage of `ImpureTemplateHaskell` in the wild, this will lead to pressure on library authors to purify the Template Haskell functions that they provide (since Haskellers will hate having something saying “impure” at the top of their files).

This in turn will lead to pressure to ensure that `PureQ` has enough capabilities to cover major use cases (see “Supporting sufficiently many use cases” below).

However, this process does not block changing the defaults, and can continue afterwards.

## Examples

Examples of `EnforceTemplateHaskellPurity`. 
We're ignoring the stage restriction here for compactness, since it’s orthogonal to this proposal.

```
-- GOOD: Generating syntax definitions is entirely pure, but typically requires Reify
$makeLenses ‘Foo

-- BAD: reading a file requires IO, ‘embedFile’ has type ‘FilePath -> Q Exp’
myfile1 = $(embedFile "dirName/fileName")
-- GOOD: explicitly uses an impure splice
myfile2 = $!(embedFile “dirName/fileName”)

-- BAD: the inner splice is impure, leading to a MonadIO constraint on the overall splice, 
-- which cannot be satisfied by a top-level pure splice
myLazyfile3 = $(BSL.toLazy $(embedFile “dirName/fileName”))
-- GOOD: explicitly uses an impure splice
myLazyfile3 = $!(BSL.toLazy $(embedFile “dirName/fileName”))

-- this is an impure quote
q :: (MonadIO m, Quote m) => m Exp
q = [| $(liftIO (putStrLn "hello") >> pure (LitE (LitInteger 5))) |]

-- BAD: the quote is impure, so we need an impure splice
myInt1 = $q
-- BAD: use an impure splice
myInt2 = $!q
```

## Effect and Interactions

### Direct effects

The effect of `EnforceTemplateHaskellPurity` is to make the evaluation of top-level normal splices pure. 
We think it is justified to move towards having this as the default, seeing as we expect it to cover the majority of cases (and it fits with Haskell’s general ethos of pure-first).

`ImpureTemplateHaskell` then allows top-level splices that need IO so long as they use the new syntax for impure splices.

The overall effect is that the programmer can statically assert that execution of Template Haskell will not perform IO at compile time (modulo `unsafePerformIO`). 
However it leaves the option for them to opt into this explicitly at key points.

### unsafePerformIO

How can we claim that we are gaining any safety when `unsafePerformIO` exists? 
Surely this allows users to smuggle IO into anywhere! 
So the compiler cannot assume that there is no IO (and nor can the user!).

This is true, but it is true of normal Haskell code as well. 
The approach we usually take is to say “the compiler is entitled to assume that there is no IO if it’s not in the type; if you use unsafePerformIO then unpredictable things may happen and that’s your problem”. 
The situation can be similar here: if you use `unsafePerformIO` in a pure TH splice, then maybe you will see unpredictable working directories, or your recompilation avoidance will be worse. 
But if you really want to use the single-global-variable trick, perhaps that’s fine - it’s the user’s responsibility to be sure it’s safe.

### Supporting sufficiently many usecases

The already-implemented Overloaded Quotation Brackets proposal (https://github.com/ghc-proposals/ghc-proposals/pull/246) changes many functions in the `template-haskell` library to be polymorphic over `Quote`. 
Any Template Haskell quotes that only use these functions would already be considered pure under the rules in this proposal.

However, many syntax-generating uses of Template Haskell (e.g. `makeLenses`) rely heavily on the `reify` functions. 
These currently still require `Quasi`, tying them to the impure `Q` monad. 
This is why we propose breaking the `reify` functions out into a new class `Reify`. 
We believe that this would allow the majority of TH uses in the wild to be considered pure.

We may want to go further than `Reify` in service of this goal. 
For example, `location` is used by some logging frameworks to generate logs that include source locations. 
Perhaps we should consider `location` to be pure. 
However, we can always do such additions later in a backwards-compatible way, by progressively defining more classes and bundling them into `PureQ`. 
This will gradually expand the set of splices that are considered pure.

### Changing defaults

We believe that most uses of TH in the ecosystem will turn out to be pure, so in the long run enabling `EnforceTemplateHaskellPurity` should be a no-op for most packages.

However, the `-Wimpure-splices` warning should function to highlight the modules which will not work with purity-by-default and help users fix them so they will keep working.

We expect that impure splices will never go away, but instead we expect users to turn on `ImpureTemplateHaskell` manually.

### Interactions

This proposal should not interact significantly with other features. 
It largely does not interact with the behaviour of Template Haskell quotes or splices, except to insert some typing constraints at a few points.

## Costs and Drawbacks

This adds new syntax to Haskell, which grows the grammar.

It adds a new concept that users of Template Haskell must understand, although it is one that is hopefully quite intuitive. 

The implementation should be fairly straightforward and orthogonal to other concerns. The most complex part may be the warnings!

## Alternatives

### No syntax

An alternative would be to have no new syntax, and instead to toggle the purity check wholesale for a module with a single `ImpureTemplateHaskell` extension.

This would be significantly simpler, as it avoids adding syntax. 
However, the author finds it less appealing to have a single module-level switch, especially as a language pragma. 
Generally it seems better for a language feature to enable a new syntactic form that provides additional power, rather than just changing the behaviour of an existing feature across a module.

However, this is a woolly preference, and quite possibly the module-level switch is a better approach. 

### Implement Reify etc. as separate proposals

There is no intrinsic need to include defining `Reify` in the proposal, except that it will be hard to achieve the full goals of this proposal without making pure splices more expressive. 
Much less can be done with just `Quote`.

## Unresolved Questions

### What to do about nested splices?

Nested splices behave a bit strangely in this proposal, in that they do not have any syntax when they introduce impurity, rather they simply allow any typeclass constraints to propagate up to the top level where they then get resolved (or not) by the choice of concrete monad, based on the splice type syntax.

An alternative would be to require impure splice syntax for nested splices that need `MonadIO`. It’s less clear how to implement this, however, without fixing the concrete monad for nested splices, which would go against the current flexible design that allows more choice over the monad for quotes.

### Are "impure quotes" a problem?

Today, constraints already propagate out to the type of a quote.
So a quote which contains a splice that uses `MonadIO` will also have a `MonadIO` constraint on its monad.

This proposal does not propose impure quotes should look any different to pure ones.
The reason for this is that since the constraints are all propagated to the top-level, we can control the purity of the entire (nested) TH evaluation by only constraining the top-level monad to be `PureQ` or `Q`.
So we don't actually need to constrain quotes in order to ensure that all TH evaluation is pure.


## Implementation Plan

Currently this proposal lacks an implementor.
