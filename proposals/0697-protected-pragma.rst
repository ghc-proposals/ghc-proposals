# Function Protection Pragma

.. author:: Oleksandr Zhabenko, with assistance from Claude (Anthropic AI)
.. date-accepted:: [To be determined]
.. contents::

## Summary

This proposal introduces a new pragma ``{-# PROTECTED #-}`` that prevents user code from shadowing or substituting critical system functions, enhancing security in environments where untrusted code is evaluated or loaded.

## Motivation

### The Security Problem

Modern Haskell applications increasingly need to evaluate or load untrusted user code in several contexts:

* **Deployment systems** (Keter) load user application bundles that may contain malicious code
* **Plugin architectures** execute user-provided modules with potential for function substitution
* **Code evaluation platforms** run user Haskell code in sandboxed environments
* **Configuration systems** parse user-defined functions that could override system functions

Currently, user code can shadow or redefine any function in scope, including security-critical ones. This creates a significant attack vector where malicious user code can substitute trusted system functions with compromised implementations.

Consider this scenario in a deployment system::

    -- System defines safe decompression with resource limits
    safeDecompress :: ByteString -> IO ByteString
    safeDecompress input = do
        -- Careful bounds checking, resource limits, timeout protection
        if BS.length input > maxCompressedSize
            then throwIO ResourceLimitExceeded
            else performSafeDecompression input

    -- User code can shadow this with a malicious implementation:
    -- let safeDecompress = zipBombDecompress in userApplication
    -- This creates a zip bomb attack vector

### Real-World Impact in Keter

In deployment systems like Keter, this vulnerability manifests during bundle unpacking::

    unpackBundle :: FilePath -> AppId -> KeterM AppStartConfig (FilePath, BundleConfig)  
    unpackBundle bundle aid = do
        AppStartConfig{..} <- ask
        $logInfo $ pack $ "Unpacking bundle '" <> show bundle <> "'"
        -- This could be compromised if user bundle redefines unpackTempTar
        liftIO $ unpackTempTar (fmap snd ascSetuid) ascTempFolder bundle folderName $ \dir -> do
            -- Configuration parsing could also be compromised
            mconfig <- decodeFileRelative configFP
            config <- case mconfig of
                Right config -> return config
                Left e -> throwIO $ InvalidConfigFile e
            return (dir, config)

If a malicious user bundle contains code that redefines ``unpackTempTar`` or ``decodeFileRelative``, they can:

* Bypass security restrictions during extraction
* Execute arbitrary code during configuration parsing  
* Escalate privileges beyond intended sandbox limits
* Launch denial-of-service attacks (zip bombs, resource exhaustion)

## Proposed Change

### Syntax

Functions can be marked as protected against user substitution::

    {-# PROTECTED functionName #-}
    functionName :: Type -> Type  
    functionName = trustedImplementation

### Semantics

Functions marked with ``{-# PROTECTED #-}`` cannot be:

1. **Shadowed** by local definitions in any scope
2. **Redefined** in user modules or loaded code
3. **Overridden** through imports or qualified names
4. **Substituted** via Template Haskell splices
5. **Masked** by FFI imports using the same name

The protection is **global and absolute** - once a function is marked as protected, all references to that name within the compilation unit will resolve to the original protected definition, regardless of scope or import context.

### Example Usage

System/trusted code defines protected functions::

    -- In system security module
    {-# PROTECTED safeDecompress #-}
    safeDecompress :: ByteString -> IO ByteString
    safeDecompress = trustedDecompressionImplementation

    {-# PROTECTED unpackTempTar #-}  
    unpackTempTar :: FilePath -> IO ()
    unpackTempTar = trustedExtractionImplementation

    {-# PROTECTED executeCommand #-}
    executeCommand :: String -> IO ProcessResult
    executeCommand = sandboxedCommandExecution

User code attempts to compromise these functions (all would result in compile errors)::

    -- These would all be rejected at compile time:
    module UserCode where
    
    -- Local shadowing attempt
    let safeDecompress = maliciousZipBomb in userFunction  -- COMPILE ERROR
    
    -- Import shadowing attempt  
    import qualified Evil as safeDecompress                -- COMPILE ERROR
    
    -- Direct redefinition attempt
    safeDecompress x = unsafePerformIO $ launchMissiles    -- COMPILE ERROR
    
    -- Template Haskell substitution attempt
    $(do let safeDecompress = ...; return [...])           -- COMPILE ERROR

## Examples

### Deployment Security (Keter)

::

    -- Protected bundle unpacking ensures trusted extraction
    {-# PROTECTED unpackBundle #-}
    {-# PROTECTED unpackTempTar #-}  
    {-# PROTECTED decodeFileRelative #-}
    
    unpackBundle :: FilePath -> AppId -> KeterM AppStartConfig (FilePath, BundleConfig)
    unpackBundle bundle aid = do
        AppStartConfig{..} <- ask
        $logInfo $ pack $ "Unpacking bundle '" <> show bundle <> "'"
        -- Guaranteed to use trusted implementation even if user bundle
        -- attempts to redefine unpackTempTar
        liftIO $ unpackTempTar (fmap snd ascSetuid) ascTempFolder bundle folderName $ \dir -> do
            -- Safe configuration parsing, immune to user override attempts
            mconfig <- decodeFileRelative configFP
            config <- case mconfig of
                Right config -> return config  
                Left e -> throwIO $ InvalidConfigFile e
            return (dir, config)

### Plugin System Security

::

    -- Core system functions marked as protected
    {-# PROTECTED executeCommand #-}
    {-# PROTECTED readSecretFile #-}
    {-# PROTECTED networkRequest #-}
    {-# PROTECTED allocateMemory #-}
    
    loadUserPlugin :: FilePath -> IO PluginResult
    loadUserPlugin pluginPath = do
        -- Even if plugin code tries to redefine these functions,
        -- our system calls use the original protected implementations
        result <- executeCommand "validate-plugin"
        secrets <- readSecretFile "/etc/app/secrets" 
        response <- networkRequest "https://api.validate.com" payload
        return $ PluginResult result secrets response

### Educational/Evaluation Platform

::

    -- Protect critical evaluation infrastructure
    {-# PROTECTED evaluateUserCode #-}
    {-# PROTECTED checkResourceLimits #-}
    {-# PROTECTED sanitizeOutput #-}
    
    runStudentSubmission :: String -> IO EvalResult  
    runStudentSubmission studentCode = do
        -- Student code cannot override our safety mechanisms
        limits <- checkResourceLimits defaultLimits
        result <- evaluateUserCode studentCode limits
        output <- sanitizeOutput result
        return $ EvalResult output

## Specification

### Scope and Resolution Rules

**Global Protection**: Protection applies across all modules in the compilation unit. Once a function is marked ``{-# PROTECTED #-}``, all references to that name resolve to the protected definition.

**Name Resolution Priority**:
1. Protected functions take absolute precedence
2. Attempts to shadow protected names result in compile-time errors  
3. Qualified imports cannot circumvent protection
4. Local bindings cannot mask protected names

**Module System Integration**:
* Protected status propagates through re-exports
* ``import qualified`` cannot override protected functions
* ``hiding`` clauses cannot hide protected functions
* Module aliases cannot redirect protected function calls

### Error Messages

The compiler provides clear error messages for protection violations::

    • Cannot shadow protected function 'safeDecompress'
      Protected functions cannot be redefined, shadowed, or overridden
    • In the definition: let safeDecompress = maliciousImplementation
      Suggested fix: Use a different name for your local function
    
    • Cannot import function with same name as protected function 'executeCommand'  
      Protected function names are globally reserved
    • In the import: import qualified Malicious as executeCommand
      Suggested fix: Import with a different qualified name

### Interaction with Language Features

**Template Haskell**: TH splices cannot generate code that redefines or shadows protected functions. The protection check occurs after TH expansion.

**Foreign Function Interface**: FFI imports cannot use names that conflict with protected functions.

**Type Classes**: Method names can be protected, preventing malicious instance definitions from overriding critical operations.

**Deriving Mechanisms**: Generated code respects protection constraints.

**Generics and Reflection**: Protected function names are not accessible through generic programming interfaces that might allow runtime substitution.

## Effect and Interactions  

### Backward Compatibility

This feature is **purely additive**:

* Existing code requires no changes
* No modifications to current pragma syntax
* Only affects code that explicitly uses the new pragma
* No runtime behavior changes for unprotected functions

### Performance Impact

**Compile-time**: Minimal overhead for name resolution checking. Protected function lists are small and lookups are efficient.

**Runtime**: No performance impact whatsoever. Protection is purely a compile-time constraint with no runtime representation.

**Optimization**: No interference with GHC's optimization passes. Unlike ``{-# NOINLINE #-}``, this pragma does not affect inlining or other optimizations.

### Module System Integration

**Hierarchical Modules**: Protection works seamlessly with hierarchical module names (``System.Security.Safe.decompress``).

**Package Boundaries**: Protected functions maintain their protection status across package boundaries when re-exported.

**Cabal Integration**: Build systems can statically verify that critical functions are properly protected.

## Costs and Drawbacks

### Implementation Complexity

**Name Resolution Changes**: Requires modifications to GHC's name resolution algorithms to enforce global protection constraints.

**Error Reporting**: New category of compile-time errors with appropriate error messages and suggestions.

**Documentation**: Updates to GHC User's Guide, pragma documentation, and error message catalog.

**Testing**: Comprehensive test suite covering interaction with all language features.

### Potential User Experience Issues

**Learning Curve**: Developers need to understand when and why to use protection.

**Over-protection**: Risk of marking too many functions as protected, reducing flexibility unnecessarily.

**Debugging Challenges**: Protection errors might be confusing for users unfamiliar with the feature.

### Technical Limitations

**Granularity**: Current design only protects individual functions, not entire modules or namespaces.

**Dynamic Loading**: Protection may not extend to dynamically loaded code using the GHC API.

**Cross-Language Boundaries**: Cannot protect against substitution in FFI code or other language interfaces.

## Alternatives

### 1. Module-Level Protection

Protect entire modules rather than individual functions::

    {-# PROTECTED_MODULE System.Security.Safe #-}
    -- All exports from this module are protected

**Pros**: Simpler to use, broader protection  
**Cons**: Less granular control, may be overly restrictive

### 2. Namespace-Based Protection

Protect hierarchical namespaces::

    {-# PROTECTED_NAMESPACE System.Security.* #-}
    -- Protects all functions under System.Security hierarchy

**Pros**: Good middle ground between function and module protection  
**Cons**: More complex implementation, namespace boundary ambiguities

### 3. Import-Time Protection

Specify protection at import sites rather than definition sites::

    import System.Security.Safe (safeDecompress) as protected
    -- Only this import form grants access to the function

**Pros**: More flexible, allows different protection levels per import  
**Cons**: Distributed protection specification, harder to audit

### 4. Capability-Based Security

Use type system to enforce access control::

    safeDecompress :: SecurityCapability -> ByteString -> IO ByteString
    -- Requires explicit capability to call function

**Pros**: Type-safe, composable, fine-grained control  
**Cons**: More invasive changes, requires capability distribution mechanism

### 5. Runtime Sandboxing

Use runtime restrictions rather than compile-time protection::

    -- Enforce restrictions through modified runtime system
    sandboxed :: IO a -> IO a

**Pros**: Can protect against dynamically loaded code  
**Cons**: Runtime overhead, complex implementation, bypassable

## Unresolved Questions

### Technical Design Questions

1. **Cross-Package Protection**: How should protection work across package boundaries? Should protected functions be protected globally across all packages, or only within the defining package?

2. **Versioning Interaction**: How do protected functions interact with package versioning? If package A v1.0 protects function ``f`` but v2.0 doesn't, what happens?

3. **Template Haskell Integration**: Should TH be able to query whether a function is protected? Should it be able to generate protection pragmas?

4. **GHCi Behavior**: How should protection work in the interactive environment? Should users be able to override protected functions at the REPL for debugging?

### Semantic Questions  

5. **Scope of Protection**: Should protection extend to data constructors, type constructors, type classes, and type class methods?

6. **Qualified Name Handling**: How should protection interact with qualified names? Should ``qualified imports`` be allowed to create aliases for protected functions?

7. **Record Field Protection**: Should record field accessors be protectable independently of their containing data types?

### Process and Tooling Questions

8. **Tooling Integration**: How should IDEs, linters, and other tools handle protected functions? Should they provide special highlighting or warnings?

9. **Documentation Generation**: How should Haddock and other documentation tools present protected functions?

10. **Migration Strategy**: For existing codebases that want to adopt protection, what tools or processes should be provided to identify candidates for protection?

## Implementation Plan

### Phase 1: Core Implementation (6-8 weeks)

* Extend GHC's pragma parsing to recognize ``{-# PROTECTED #-}``
* Modify name resolution to enforce protection constraints  
* Implement basic compile-time error reporting
* Create initial test suite covering basic functionality

### Phase 2: Language Feature Integration (4-6 weeks)  

* Template Haskell integration and protection checking
* FFI interaction and conflict detection
* Type class method protection support
* Advanced error messages with suggestions

### Phase 3: Tooling and Documentation (3-4 weeks)

* GHC User's Guide documentation
* Haddock integration for protected function documentation  
* Error message catalog updates
* Migration tooling for existing codebases

### Phase 4: Extended Testing and Refinement (4-6 weeks)

* Comprehensive test suite across all language features
* Performance impact analysis and optimization
* Community feedback integration and refinement
* Real-world testing with projects like Keter

**Total Estimated Timeline**: 4-6 months for full implementation

## Related Work

### Security in Other Languages

**Java Sealed Classes**: Restrict which classes can extend a particular class, providing inheritance-level protection.

**Rust Visibility System**: Fine-grained visibility control with ``pub(crate)``, ``pub(super)``, etc., but focused on access control rather than substitution prevention.

**JavaScript Object.freeze()**: Prevents modification of object properties at runtime, similar in spirit but limited to object mutation.

**Python Import Hooks**: Can control module loading and name resolution, but primarily for customization rather than security.

### Capability-Based Security Research

**Object-Capability Model**: Academic work on capability-based security systems that inspired the design principles.

**E Language**: Pioneered many concepts in capability-based security for distributed systems.

**Joe-E**: Java subset designed for secure programming with capability-based security.

### Compiler Security Features  

**Control Flow Integrity (CFI)**: Hardware and compiler-based techniques to prevent control flow hijacking.

**Stack Canaries**: Compiler-generated protection against buffer overflow attacks.

**Position Independent Executables (PIE)**: Address space layout randomization support in compilers.

## Conclusion

The ``{-# PROTECTED #-}`` pragma addresses a significant security gap in Haskell applications that handle untrusted code. By preventing function substitution attacks at compile time, it enables safer deployment systems, plugin architectures, and code evaluation platforms.

While implementation challenges exist, the feature provides essential security infrastructure for the growing ecosystem of Haskell applications that process untrusted input. The design is conservative, maintaining full backward compatibility while providing strong security guarantees.

This proposal represents an important step toward capability-based security in functional programming languages and establishes Haskell as a leader in secure language design for systems programming.

The security benefits significantly outweigh the implementation costs, and the feature fills a genuine need in the community as evidenced by real-world vulnerabilities in deployment systems like Keter.

## Acknowledgments

This proposal originated from practical security concerns encountered while working with deployment systems like Keter. The core insight about function substitution vulnerabilities in untrusted code evaluation emerged from analyzing real-world attack vectors in Haskell application deployment.

Claude (Anthropic AI) provided significant assistance in structuring this proposal, refining the technical specification, exploring implementation considerations, and ensuring comprehensive coverage of edge cases and interactions with existing language features. The collaboration helped transform practical security observations into a formal language extension proposal.

Thanks also to the GHC development community for providing the foundation and processes that make such proposals possible.

---

**Authors**: Oleksandr Zhabenko, with assistance from Claude (Anthropic AI)  
**Date**: August 2025  
**Status**: Draft
