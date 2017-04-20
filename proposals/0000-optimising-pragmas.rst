.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/51>`_. 

.. contents::

Proposal title
==============

This is a proposal to add various optimisation annotations, such as the most likely branch in a case statement, to help inform the code generator 
with domain knowledge.

Motivation
------------

There are many places where the compiler simply cannot know the intention of the programmer, or the background knowledge. For example, when 
switching on a sum type of messages  coming over the network, what is the most likely message to be received (and therefore, what is the most likely 
branch in the case statement?)

Another example is a long chain of producers, consumers and transformers of data structures. Sometimes, the correct RULES pragma is too difficult to 
determine, or it only applies in a certain special case. One might want to tell the compiler to recursively inline everything  in a certain 
expression, in order to maximise the chances of deforestation and other optimisations.

Yet another is in privacy-sensitive applications - you might want the compiler to automatically zero memory upon both initialisation and release of 
a field on the stack, or a structure in the heap. Another exmaple might be an instruction to the RTS to free an allocated object as soon as it's no 
longer live - for example, a plaintext password field.

In a similar vein, such communications of programmer intent can serve as machine-readable, formal, inline documentation, similar to 
design-by-contract syntax and Hoare triples.

By communicating more information about programmer intent to the compiler, we can enable smarter optimisations and other transformations to our 
code, with minimal effort.


Development and Reasoning
-------------------------
[WIP, may be removed before submission to committee]

One may look to LLVM as a guide - a distinction between attributes, metadata, and intrinsics. Attributes apply to functions, parameters and return 
values that are part of the function definition or callsite itself, and visible to users without examining the function body; metadata can apply 
to anything and is named (and sharable), and can be discarded without affecting correctness; and intrinsics are inline instructions or function 
calls which don't necessarily result in emitted code, but still affect the (intended, generalised (e.g. time-space)) semantics of the 
corresponding basic block.




Proposed Change Specification
-----------------------------
[WIP]

Hereafter, "annotation" shall mean any of attributes, metadata, or intrinsics.

The following are LLVM-supported target-independent annotations which are likely to be of particular interest at the Haskell surface syntax level. 
Each of these would likely be of interest to various GHC IR transformation passes; both to consume and, where possible, emit themselves.

**Specifically supported annotations:**
* Security (MUST preserve):
 - Zero-after-free (expression/value, binding, datatype definition, datatype field, 
 - Free immediately after dead (expression, binding, datatype definition, datatype field)
 - Function annotations:
  - SafeStack
  - Sanitise address/thread
  - Stack smashing protector (Likely used most with FFI)
  
* Semantics (MUST preserve): 
 - FP mode

* Performance (SHOULD preserve):
 - Branch weight metadata (http://llvm.org/docs/BranchWeightMetadata.html). Only applies to case expressions, if-then-else expressions, and pattern matching (including guards). Related is "unpredictable".
 - Function annotations:
   - Garbage collector strategy name (functions)
   - Cold function
   - Minimise size
   - Don't optimise
   - No indirect/mutual recursion
   - Total function
 - Lifetime begin/end (binding)

The following pragmas are to enable placing arbitrary LLVM annotations

**Extendability:**
* {-# LLVM ATTRIBUTES <function name> [<opaque string>] [{(<named parameter>=<opaque string>)+}] [<opaque string>]  #-}
 - Function name, optional attributes of function, optional non-empty dictionary of parameter attributes, optional return value attributes
 - Full list of parameter attributes: http://llvm.org/docs/LangRef.html#parameter-attributes
 - Full list of function attributes: http://llvm.org/docs/LangRef.html#function-attributes
* {-# LLVM METADATA <metadata name> <opaque string> #-}
 - List of metadata nodes: http://llvm.org/docs/LangRef.html#specialized-metadata-nodes
* {-# LLVM USES METADATA <metadata name>+ #-}
  - Immediately prior to expression or binding. Multiple names allowed.
* {-# LLVM INTRINSIC <function name> <llvm intrinsic name> #-}
  - parameter types must match
  - Full list of intrinsics: http://llvm.org/docs/LangRef.html#intrinsic-functions




Effect and Interactions
-----------------------
[WIP]



Detail how the proposed change addresses the original problem raised in the motivation. Detail how the proposed change interacts with existing language or compiler features and provide arguments why this is not going to pose problems.



Costs and Drawbacks
-------------------
[WIP]
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.



Alternatives
------------
[WIP]



List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.



Unresolved questions
--------------------

SYNTAX:
Does every proposed annotation which is semantic-changine belong as a pragma? One could argue that if it's semantic-altering, then it deserves its 
own syntax/keyword. This would make the parser much more complicated, but likely would provide a cleaner interface to the language. Of course, they 
may all start out as pragmas, and be converted into standard syntax/keywords if they become popular.


{-# ANN #-} annotations are a natural choice to provide structured data, but a bit verbose (the 'ANN' is just noise). On the other hand, standard 
pragmas such as {-# INLINE #-} already exist, without the ANN. Choosing which format to go in seems to be purely an aesthetics choice; one could 
argue that one shouldn't have low-level optimisation constructs in the same syntactic namespace as, for example, the LANGUAGE pragma; but there 
already exist pragmas which violate this. On the other hand, one might risk "cluttering" the syntactic namespace.

A possibility might be to have semantic-altering annotations in {-# #-} syntax, and optional/non-semantic-altering annotations in the {-# ANN #-} 
syntax. Again, however, existing pragmas already violate this.

VISIBILITY:

We must consider how "visible" the annotations should be to GHC - should they be viewable throughout the pipeline? Should they be acted upon 
by GHC? Or should they just be handled opaquely and passed off to the code generator? We assume that semantic-changing annotations MUST be 
preserved, whereas non-semantic-changing annotations SHOULD be preserved. Perhaps an optional argument at each use of an annotation choosing this?

IMPLEMENTATION AND STRUCTURE:
This brings us to our next question - how should they be structured? Should they be plain strings, to be parsed by any interested consumer? Should 
they be fully structured ADTs? This seems to be a tradeoff between ease of passing the annotations to the codegen, vs ease of consumption via GHC 
(including plugins, Hoopl, etc).

Further, is the current pragma support in Core sufficient? 

SEMANTICS:
Consider a function that has several SPECIALISE'd versions. How can different (or the same) annotations be specified for each specialised version? 
For example, mapping over a list of bools would likely not particularly care about setting the FP mode or specific FP SIMD instruction sets, 
whereas mapping over a list of doubles would.

It is likely that there would be target-dependent annotations which would be of interest as well. Would it be better to include them as standard 
pragmas, or devolve them to requiring the LLVM ATTRIBUTE/LLVM METADATA/LLVM INTRINSIC pragmas?


Implementation Plan
-------------------
[WIP]
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
