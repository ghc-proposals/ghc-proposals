.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Keyword Regions
===============

This is yet another proposal about the syntax, nevertheless it is not about a new syntactic construct.
The proposed change is to update the grammar, not to disallow any already-valid code, 
rather to allow for a more uniform syntactic style across different syntactic entities. 
Specifically, it is to allow declarations for entities like pattern synonyms, types synonyms, or type families 
to follow the same syntactic style as declarations in term bindings where multiple terms and their types annotations 
can be defined under the same keyword, like `let`, or `where`.

Motivation
------------
When programming in Haskell, I often end up having fragments like the following in my code::

  pattern
    P :: ...
  pattern
    Q :: ...
  pattern
    R :: ...

  pattern
    P ... = ...
  pattern
    Q ... = ...
  pattern
    R ... = ...
    
or:: 

  type  
    A ... = ...
  type 
    B ... = ...
  type 
    C ... = ...

also::

  type family
    F ...
  type family
    G ...
  type family
    H ...

  type instance 
    F ... = ...
  type instance
    G ... = ...
  type instance
    H ... = ...
  
or even::

  import 
    Magma  
      (...) 
  import 
    Monoid 
      (...)
  import 
    Group
      (...)

That is when, for terms and their type annotations, I have fragments like::

  where
    f :: ...
    f ... = ...
    
    g :: ...
    g ... = ...
    
    h :: ...
    h ... = ...
    
 
While for a group of terms, and their type annotations, I have to wrote zero (top-level), 
or one (local) keyword/syntactic marker,
why should I write one keyword/syntactic marker per each pattern, type synonym, or the like?   

We all know patterns, types, and modules are second-class citzens (at least for now), but why should we remind them of 
this unpleasant fact every single time? Why not at least letting them live together in the same regions.
We can maybe have::

  pattern
    P :: ...
    P ... = ...
   
    Q :: ...
    Q ... = ...
  
    R :: ...
    R ... = ...
    
or:: 

  type  
    A ... = ...
    B ... = ...
    C ... = ...

also::

  type family
    F ...
    G ...
    H ...

  type instance 
    F ... = ...
    G ... = ...
    H ... = ...
  
or even::

  import 
    Magma  
      (...) 
    Monoid 
      (...)
    Group
      (...)
  
Less ink, and more clear: by looking at the region's heading, we know who's living in the block.

Worried about the issues with intendentation? Or, you love semi-colons anyway? 
A uninform style would allow for the following style (similar to the one for term bindings) as well::

  pattern
    { P :: ...
    ; P ... = ...
    
    ; Q :: ...
    ; Q ... = ...
  
    ; R :: ...
    ; R ... = ... 
    }

Proposed Change Specification
-----------------------------

When one writes the block::

  keyword
    XX
    YY
    ZZ

or equivalently::

  keyword
    { XX
    ; YY
    ; ZZ }
    
one actually means::

  keyword
    XX
  keyword    
    YY
  keyword    
    ZZ
   
For what keywords you ask?
I have personally experimented in my code with keywords `pattern`, `import`, `type`, and `type family`.
Nothing is stopping us from adding, or removing, keywords like these to the set of allowed keyword regions.


Effect and Interactions
-----------------------

I have to add that I am a bit worried about the type families: with keyword regions it may be harder to 
identify openness/closedness of a declaration. 
More input from Depedent Haskell people on the upcoming changes to the syntax is well appreciated. 

Costs and Drawbacks
-------------------

Any changes to the syntax comes at a noticeable cost: all tools, like IDEs, working on Haskell syntax
should be updated to account for the changes to the grammar. 
We will introduce a LANGUAGE pragma for this extension. 

Besides the fact that keyword regions should appear pleasant to the majority 
(we will hopefully hear from the community in the discussion section), I cannot immediately see any drawbacks. 
Though, there are often subtle issues with any syntactic extension discovered once we start to implement.


Alternatives
------------
 
Save for the status quo, nothing yet 

Unresolved questions
--------------------

Nothing yet

Implementation Plan
-------------------

I can do part of the job (expect it to take quite some time), 
and Alan Zimmerman has kindly offered to assisst me.
