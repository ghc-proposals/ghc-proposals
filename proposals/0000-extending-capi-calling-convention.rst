extending CAPI calling convention
=================================

.. author:: Vu Hoang Hung
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/630>`_.
.. sectnum::
.. contents::   

GHC currently does not support foreign call in which the function returns or takes as parameters aggregate types (structs and unions). This proposal tweaks the capi calling convention to 
support those C functions. 

Motivation
----------
GHC foreign call into C is limited to functions that accept and return only primitives types (int, float, etc) as the procedure of passing an aggregated type is largely unknown to the compiler. 
To call a C function that does not meet the aforementioned criterion, the user is forced to produce a C wrapper function in which the offending types are passed or returned through pointers. 
This is trivial albeit tedious task, one that is labourious if the user is required to interface with many C functions.
Both ccall and capi disallow passing aggregated types directly, but for capi this is not strictly required as it already produces wrapper functions in C. 

Proposed Change Specification
-----------------------------
The proposed changes affect how capi calls are handled. 
The parameters and output of the called C function can be classified into three categories: 
1. primitives and newtypes around primitives (example: CInt, CFloat)
2. newtypes around primitives declared with a CTYPE pragma 
3. data types implementing a Storable instance declared with a CTYPE pragma

A parameter of category 1 is passed and returned as is already implemented. 

For category 2, the parameter is passed by the value of the encase primitive like category 1. This category corrisponds with C structs that possess only one member of a primitive type (example: 
``typedef struct { int value } unique_id;``). In the C wrapper function, the corrisponding parameter has the member type and is encapsulated in a struct before being passed to the wrapped function.
For return value in this category, the value is returned in the primitive type of the singleton member. 

Parameters of category 3 are passed by pointers. The wrapper function shalls dereference these pointers before passing them to the wrapped function.  
If the return value is in this category, the wrapper function shalls have one extra rightmost pointer parameter where the result of the wrapped function will be written to. 
The types of category 3 must implement a Storable instance in order for Haskell to marshal them to and from pointers. 

Examples
--------
Suppose the the C function ``aggregate wrapped(int, aggregate)`` is needed to be called from Haskell. This would be how it is done. 

    data {-# CTYPE "aggregate" #-} Aggregate = ...
    instance Storable Aggregate where ...
 
    foreign unsafe capi "header.h wrapped" ffunc :: CInt -> Aggregate -> Aggregate

Generated C wrapper function

    void wrapper (int _0, aggregate \*_1, aggregate \*_result) { \*_result = wrapped(_0, \*_1); }

Equivalent in ccall convention

    foreign unsafe ccall "wrapper" ffunc_wrapper :: CInt -> Ptr Aggregate -> Ptr Aggregate -> IO ()
    ffunc :: CInt -> Aggregate -> Aggregate
    ffunc _0 _1 = unsafePerformIO (with _1 (\ _1' -> alloca (\ _result -> ffunc_wrapper _0 _1 _result >> peek _result)))


For cases of category 2

    newtype {-# CTYPE "unique_id" #-} UniqueId = UniqueId CInt
    
    foreign unsafe capi "header.h wrapped" ffunc :: UniqueId -> Ptr () -> IO UniqueId 

C wrapper function

    int wrapper (int _0, void \*_1) { unique_id result = wrapped((unique_id){_0}, _1); return *(int*)&result; }

ccall equivalent

    foreign unsafe ccall "wrapper" ffunc_wrapper :: UniqueId -> Ptr () -> IO UniqueId
    ffunc :: UniqueId -> Ptr () -> IO UniqueId
    ffunc _0 _1 = ffunc_wrapper _0 _1

Effect and Interactions
-----------------------
The modification shoud make interfacing with C code easier, especially for external C libraries.

Costs and Drawbacks
-------------------
The cost is limited to the initial implementation. Little maintenance should be required due to the simplicity of the changes.

Backward Compatibility
----------------------
Since this proposal only affects capi calls that previously would have been illegal, it is fully backward compatible.

Alternatives
------------
Another approach is to flatten structs into their primitive members and reconstruct the structs inside the wrapper function. 
This grant minor performance benefit but is more difficult to implement and does not for unions.

Unresolved Questions
--------------------
None.
