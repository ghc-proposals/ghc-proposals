Function Protection Pragma
==========================

.. author:: Oleksandr Zhabenko, with assistance from Claude (Anthropic AI) and later GPT-5 (OpenAI)
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/715>`_.
.. sectnum::
.. contents::

This proposal introduces a new pragma ``{-# PROTECTED #-}`` that prevents user code from shadowing or substituting critical system functions, enhancing security in environments where untrusted code is evaluated or loaded. This addresses a significant security vulnerability in deployment systems, plugin architectures, and code evaluation platforms where malicious user code can override trusted system functions.

Motivation
----------

Modern Haskell applications increasingly need to handle untrusted user code in security-critical contexts. Deployment systems like Keter load user application bundles, plugin systems execute user-provided modules, and educational platforms evaluate student code. Currently, user code can shadow or redefine any function in scope, creating severe security vulnerabilities.

Consider this real-world scenario from the Keter deployment system::

    -- System defines safe decompression with resource limits
    safeDecompress :: ByteString -> IO ByteString
    safeDecompress input = do
        -- Careful bounds checking, prevents zip bombs
        if BS.length input > maxCompressedSize
            then throwIO ResourceLimitExceeded
            else performSafeDecompression input

    -- User's malicious bundle can include:
    module UserApp where
    import System.Security.Safe
    
    -- This shadows the safe function with a zip bomb
    safeDecompress = createZipBomb
    
    -- Now all calls to safeDecompress use the malicious version
    processUserData = safeDecompress userInput

This creates attack vectors for:

* **Zip bomb attacks** where malicious decompression functions consume infinite resources
* **Privilege escalation** by overriding security check functions  
* **Data exfiltration** through compromised file I/O functions
* **Denial of service** via resource exhaustion

In Keter's bundle unpacking code, this vulnerability manifests as::

    unpackBundle :: FilePath -> AppId -> KeterM AppStartConfig (FilePath, BundleConfig)
    unpackBundle bundle aid = do
        AppStartConfig{..} <- ask
        $logInfo $ pack $ "Unpacking bundle '" <> show bundle <> "'"
        -- Vulnerable: user bundle could redefine unpackTempTar
        liftIO $ unpackTempTar (fmap snd ascSetuid) ascTempFolder bundle folderName $ \dir -> do
            -- Vulnerable: user could redefine decodeFileRelative
            mconfig <- decodeFileRelative configFP
            config <- case mconfig of
                Right config -> return config
                Left e -> throwIO $ InvalidConfigFile e
            return (dir, config)

If a user bundle redefines ``unpackTempTar`` or ``decodeFileRelative``, they can bypass all security restrictions and execute arbitrary code with elevated privileges.

The status quo provides no mechanism to prevent function shadowing in security-critical contexts. Existing solutions like qualified imports can be circumvented, and there's no way to declare that certain functions must maintain their trusted implementations regardless of user code.

Proposed Change Specification
-----------------------------

This proposal adds a new pragma ``{-# PROTECTED #-}`` that prevents function shadowing and substitution. The pragma is applied at the function definition site and creates a global, immutable binding that cannot be overridden by any user code.

**Syntax**

The pragma follows standard GHC pragma syntax::

    {-# PROTECTED functionName #-}
    functionName :: Type -> Type
    functionName = implementation

**Lexical Analysis**

The lexer is extended to recognize the ``PROTECTED`` keyword in pragma contexts. This follows the existing pattern for pragma keywords like ``INLINE``, ``NOINLINE``, etc.

**Parsing**

The parser grammar is extended with::

    pragma_decl ::= '{-#' 'PROTECTED' qvar '#-}'

**Semantics**

Functions marked with ``{-# PROTECTED #-}`` have the following properties:

1. **Global Resolution Priority**: Protected function names always resolve to their protected definitions, regardless of local scope or imports.

2. **Shadowing Prevention**: Any attempt to define a local binding with the same name as a protected function results in a compile-time error.

3. **Import Restrictions**: Qualified imports cannot create aliases that override protected function names.

4. **Template Haskell Immunity**: TH splices cannot generate code that redefines protected functions.

5. **FFI Conflicts**: Foreign imports cannot use names that conflict with protected functions.

**Name Resolution Algorithm**

The name resolution algorithm is modified as follows:

1. Before normal name resolution, check if the identifier matches any protected function name
2. If it matches, resolve to the protected definition regardless of scope
3. If user code attempts to bind the same name, report a protection violation error
4. Continue with normal resolution for non-protected names

**Error Reporting**

New error messages are introduced::

    • Cannot shadow protected function 'safeDecompress'
      Protected functions cannot be redefined, shadowed, or overridden
    • In the definition: let safeDecompress = maliciousImplementation
      Suggested fix: Use a different name for your local function

**Interaction with Language Features**

*Modules and Imports*: Protected status is preserved across module boundaries. Re-exports maintain protection.

*Template Haskell*: Protection checking occurs after TH expansion. TH cannot generate protected pragma declarations.

*Type Classes*: Type class methods can be protected, preventing malicious instance definitions.

*Generics*: Generic programming interfaces respect protection constraints.

*Foreign Function Interface*: FFI imports conflict with protected names and are rejected.

Proposed Library Change Specification
-------------------------------------

No changes to ``base`` or ``ghc-experimental`` are required for this proposal. The feature is implemented entirely through compiler infrastructure and does not require new library functions or types.

The pragma can be applied to any function, including those in ``base`` and other libraries. However, adding ``{-# PROTECTED #-}`` pragmas to existing ``base`` functions would require separate CLC approval and is not part of this core language feature proposal.

Examples
--------

**Basic Protection**

Protecting a security-critical function::

    module System.Security.Safe where
    
    {-# PROTECTED safeDecompress #-}
    safeDecompress :: ByteString -> IO ByteString
    safeDecompress input = do
        if BS.length input > maxSize
            then throwIO TooBig
            else Z.decompress input

User code attempting to compromise this function fails::

    module UserCode where
    import System.Security.Safe
    
    -- Compile error: Cannot shadow protected function 'safeDecompress'
    safeDecompress = createZipBomb
    
    -- Compile error: Cannot import with same name as protected function
    import qualified Malicious as safeDecompress

**Deployment System Protection**

Securing Keter-style deployment::

    module System.Deploy.Safe where
    
    {-# PROTECTED unpackBundle #-}
    {-# PROTECTED unpackTempTar #-}
    {-# PROTECTED decodeFileRelative #-}
    
    unpackBundle :: FilePath -> AppId -> IO (FilePath, BundleConfig)
    unpackBundle bundle aid = do
        -- These calls are guaranteed to use trusted implementations
        unpackTempTar bundle tempDir $ \dir -> do
            config <- decodeFileRelative (dir </> "config.yaml")
            return (dir, config)

**Plugin System Protection**

Securing a plugin architecture::

    module System.Plugin.Safe where
    
    {-# PROTECTED executeCommand #-}
    {-# PROTECTED readFile #-}
    {-# PROTECTED writeFile #-}
    
    loadPlugin :: FilePath -> IO PluginResult
    loadPlugin path = do
        -- Plugin code cannot override these system calls
        result <- executeCommand ("validate " ++ path)
        content <- readFile path
        writeFile "/tmp/plugin.log" ("Loaded: " ++ path)
        return $ PluginResult result content

**Template Haskell Protection**

Protection extends to TH-generated code::

    {-# PROTECTED criticalFunction #-}
    criticalFunction :: Int -> Int
    criticalFunction x = x + 1
    
    -- This TH splice will fail at compile time
    $(do
        let name = mkName "criticalFunction"
        body = normalB [| \x -> x - 1 |]  -- Different implementation
        return [FunD name [Clause [] body []]])

**Error Message Examples**

Clear error reporting guides users::

    UserCode.hs:15:5: error:
        • Cannot shadow protected function 'safeDecompress'
          Protected functions cannot be redefined or overridden
        • In the binding: safeDecompress = maliciousZipBomb
          Suggested fix: Use a different name like 'myDecompress'
    
    UserCode.hs:8:1: error:
        • Cannot import function with same name as protected function 'executeCommand'
        • In the import: import qualified Evil as executeCommand
          Suggested fix: Import with a different qualified name

Effect and Interactions
-----------------------

**Primary Effect**

This proposal directly addresses the security vulnerability identified in the motivation. Protected functions maintain their trusted implementations regardless of user code, preventing function substitution attacks in deployment systems, plugin architectures, and code evaluation platforms.

**Security Benefits**

* **Prevents zip bomb attacks** by ensuring decompression functions cannot be substituted
* **Stops privilege escalation** by protecting security check functions
* **Prevents data exfiltration** by securing file I/O operations  
* **Eliminates denial of service** through protected resource management functions

**Interaction with Existing Features**

*Name Resolution*: Integrates cleanly with existing name resolution by adding a precedence layer for protected names.

*Module System*: Works seamlessly with qualified imports, re-exports, and hierarchical modules.

*Template Haskell*: Protection checking occurs after TH expansion, ensuring TH cannot circumvent protection.

*Optimization*: No interference with GHC's optimization passes. Unlike ``{-# NOINLINE #-}``, protection is purely a compile-time constraint.

*FFI*: Clear conflict resolution prevents accidental override of protected functions via foreign imports.

*Type Classes*: Enables protection of type class methods, preventing malicious instance definitions.

Costs and Drawbacks
-------------------

**Development Costs**

* **Implementation effort**: Estimated 3-4 months of development time including design, implementation, testing, and documentation
* **Compiler complexity**: Modest increase in name resolution complexity 
* **Maintenance burden**: Ongoing maintenance of protection checking infrastructure

**Learning Curve**

* **New concept**: Developers need to understand when and how to use function protection
* **Error messages**: New category of compile errors that users must learn to interpret
* **Best practices**: Community needs to develop guidelines for appropriate use

**Technical Limitations**

* **Granularity**: Only protects individual functions, not entire modules or namespaces
* **Dynamic loading**: May not extend to dynamically loaded code using the GHC API
* **Performance**: Minimal compile-time overhead for name resolution checking

**Potential Misuse**

* **Over-protection**: Risk of marking too many functions as protected, reducing flexibility
* **False security**: Users might rely on protection instead of proper sandboxing
* **Debugging challenges**: Protection errors might confuse users unfamiliar with the feature

**Remaining Drawbacks**

* **Cannot protect against all attacks**: Does not prevent attacks through other language features like unsafePerformIO
* **Limited scope**: Only prevents name shadowing, not other forms of code injection
* **Compile-time only**: No protection against runtime code modification

Backward Compatibility
----------------------

This proposal achieves **Level 0: No breakage** on the compatibility scale.

**Complete Backward Compatibility**

* **Purely additive feature**: Existing code requires zero changes
* **No semantic changes**: Behavior of existing programs remains identical  
* **Optional adoption**: Only code that explicitly uses the new pragma is affected
* **No performance impact**: Zero runtime overhead for all code

**No Breaking Changes**

* **Existing pragmas unchanged**: All current pragma behavior preserved
* **Name resolution preserved**: Non-protected functions resolve exactly as before
* **Import behavior unchanged**: Existing import statements work identically
* **Template Haskell compatibility**: Existing TH code continues to work

**Migration Path**

For codebases wanting to adopt protection:

1. **Identify security-critical functions** through security audit
2. **Add protection pragmas** to vulnerable functions
3. **Test compilation** to ensure no conflicts with existing code  
4. **Gradual rollout** by protecting functions incrementally

**Future Compatibility**

The design allows for future extensions without breaking changes:

* **Module-level protection** could be added later
* **Namespace protection** could extend the current design
* **Dynamic protection** could be added for runtime scenarios

Alternatives
------------

**Alternative 1: Module-Level Protection**

Protect entire modules rather than individual functions::

    {-# PROTECTED_MODULE System.Security.Safe #-}

*Advantages*: Simpler to use, broader protection coverage
*Disadvantages*: Less granular control, potentially over-restrictive, complex module dependency handling

**Alternative 2: Qualified-Only Imports**

Require specific import syntax for sensitive functions::

    import System.Security.Safe (safeDecompress) as protected qualified

*Advantages*: Uses existing import machinery, fine-grained control
*Disadvantages*: Distributed protection specification, can be circumvented, requires changes to import syntax

**Alternative 3: Capability-Based Security**

Use phantom types to control function access::

    safeDecompress :: SecurityCapability -> ByteString -> IO ByteString

*Advantages*: Type-safe, composable, leverages existing type system
*Disadvantages*: Invasive API changes, requires capability distribution mechanism, complex for simple use cases

**Alternative 4: Runtime Sandboxing**

Implement protection through modified runtime system::

    sandboxed :: IO a -> IO a
    sandboxed action = -- Intercept and validate system calls

*Advantages*: Can protect against dynamically loaded code, comprehensive protection
*Disadvantages*: Significant runtime overhead, complex implementation, platform-specific

**Alternative 5: Lint-Based Approach**

Use external tools to detect function shadowing::

    -- Special comments checked by linter
    -- PROTECTED: safeDecompress
    safeDecompress = implementation

*Advantages*: No compiler changes required, flexible tooling
*Disadvantages*: Not enforced at compile time, requires separate tooling, easy to bypass

**Why the Proposed Design is Superior**

The ``{-# PROTECTED #-}`` pragma approach is chosen because it:

* **Provides strong guarantees**: Compile-time enforcement prevents all shadowing
* **Minimizes complexity**: Simple pragma syntax with clear semantics  
* **Maintains compatibility**: Zero impact on existing code
* **Offers precise control**: Function-level granularity with room for future extension
* **Integrates cleanly**: Works with existing GHC infrastructure and language features

Unresolved Questions
--------------------

**Cross-Package Protection**

Should protection extend across package boundaries? If package A protects function ``f``, should package B be able to define its own ``f``? Current design allows this, but it may reduce security benefits.

**Template Haskell Integration**  

Should Template Haskell be able to query protection status of functions? This could enable more sophisticated metaprogramming but might reveal protection internals.

**GHCi Behavior**

How should protection work in the interactive environment? Should users be able to override protected functions at the REPL for debugging purposes?

**Record Field Protection**

Should record field accessors be protectable independently? This would enable fine-grained protection of data access patterns.

**Performance Optimization**

Can name resolution be optimized to minimize the overhead of protection checking? Hash tables or other data structures might improve lookup performance.

**Tooling Integration**

How should IDEs and other tools present protected functions? Special highlighting, warnings, or documentation features might improve user experience.

Implementation Plan
-------------------

**Phase 1: Core Implementation (6-8 weeks)**

* Extend lexer to recognize ``PROTECTED`` pragma keyword
* Modify parser to handle protection pragma declarations  
* Implement name resolution changes for protection checking
* Add basic error reporting for protection violations
* Create initial test suite covering basic functionality

**Phase 2: Language Integration (4-6 weeks)**

* Integrate protection with Template Haskell checking
* Add FFI conflict detection and error reporting
* Implement type class method protection support  
* Enhance error messages with suggestions and fixes
* Extend test coverage to all language features

**Phase 3: Documentation and Tooling (3-4 weeks)**

* Write GHC User's Guide documentation with examples
* Update pragma documentation and reference materials
* Create migration guide for adopting protection
* Integrate with Haddock for documentation generation

**Phase 4: Testing and Refinement (4-6 weeks)**

* Comprehensive testing across GHC's test suite
* Performance impact analysis and optimization
* Real-world testing with projects like Keter
* Community feedback integration and bug fixes

**Total Timeline**: 4-6 months for complete implementation

**Prerequisites**

* Access to GHC development environment and build system
* Familiarity with GHC's name resolution and pragma handling
* Testing infrastructure for comprehensive validation

**Implementer**

The proposal author is prepared to implement this feature with assistance from the GHC development community for code review and integration guidance.

Endorsements
-------------

**Security Community**

This proposal addresses real security vulnerabilities in production Haskell applications. Systems like Keter that handle untrusted code deployment would benefit significantly from function protection capabilities.

**Educational Platforms**

Organizations running code evaluation platforms (like programming competition sites or educational tools) face similar security challenges where student code could compromise system functions.

**Plugin Architecture Developers**

Applications with plugin systems need mechanisms to prevent plugins from overriding critical system functions while still allowing reasonable extensibility.

**Enterprise Adoption**

The security guarantees provided by function protection could encourage broader enterprise adoption of Haskell for security-critical applications where code injection vulnerabilities are a major concern.
