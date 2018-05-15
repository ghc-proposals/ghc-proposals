This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/62>`_. 

.. contents::

Let-less 
==============

As `this reddit thread
<https://www.reddit.com/r/haskellquestions/comments/6jdgn1/why_do_we_need_let_inside_do_xallownolet_continued/>`_
describes: the syntax for local declarations in a ``do``-block is awkward, especially when mixing monadic binds (``<-``) and ``let``-declarations.

.. code-block:: haskell

  do 
    line :: String <- getLine
    
    let revLine :: String
        revLine = reverse line
        
    len :: Int <- fmap length getLine
    
    let lines :: [String]
        lines = replicate len revLine
        
    pure lines

The change is to allow users to omit ``let``-s when the bindings are not recursive (and even though this should be its own proposal, allowing type signatures on their own line for variables bound from ``do`` notation)

.. code-block:: haskell

  do 
    line :: String
    line <- getLine
    
    revLine :: String
    revLine = reverse line
    
    len :: Int
    len <- fmap length getLine
    
    lines :: [String]
    lines = replicate len revLine
    
    print lines

Now the code jumps around a lot less! 

Indenting ``let`` declarations is awkward and only gets worse is production code — with this proposal, the type signatures are completely consistent.

Motivation
------------

Haskell code already has an awkward tendancy to trail to the right.

The indentation associated with ``let`` is largely unnecessary (``let`` is awkward to align as well).

Mixing ``let`` and monadic binds leads to uneven and visually displeasing code that is difficult to scan.
It makes ``let`` declarations stick out among monadic binds.

A slightly longer bogus example is 

.. code-block:: haskell

  do 
    let action :: IO ()
        action = times 5 $ do
            day :: Date <- today
            
            let adjustedDay :: Date
                adjustedDay = adjust day ...
                
            token :: SpecialToken <- getRequest adjustedDay
            
            let request :: SpecialRequest
                request = formRequest token ..
                
            sendRequest request
    
    -- ...
    
looks far nicer as (IMO) 

.. code-block:: haskell

  do 
    action :: IO ()
    action = times 5 $ do
    
        day :: Date
        day <- today
          
        adjustedDay :: Date
        adjustedDay = adjust day ...
                
        token :: SpecialToken
        token <- getRequest adjustedDay
            
        request :: SpecialRequest
        request = formRequest token ..
               
        sendRequest request
    
    -- ...

and this is a tame example compared to some I've seen in the wild. The issue is not urgent but this would be good to have.

Proposed Change Specification
-----------------------------

I'm not a precise person, allow when users bind values without a ``let`` pretend that they wrote a ``let`` 

.. code-block:: haskell

  do 
    new, old :: Array Value
    Dict { value = new } = ...
    Dict { value = old } = ...

---> 

.. code-block:: haskell

  do 
    let new, old :: Array Value
        Dict { value = new } = ...
        Dict { value = old } = ...

When they define a group of declarations of the same name, promote them to the same ``let`` binding

.. code-block:: haskell

  do 
    f :: Int -> Int
    f 0 = 0
    f n = 2 * n
    
    str <- replicateM (f 10) getLine

--->

.. code-block:: haskell

  do 
    let f :: Int -> Int
        f 0 = 0
        f n = 2 * n
    
    str <- replicateM (f 10) getLine

And when a type signature appears before a monadic bind declaring the type of a variable being bound,

.. code-block:: haskell

  do 
    str1 :: [String]
    str1 <- replicateM (f 10) getLine
    
    str2 :: [String]
    str2 <- replicateM (f 20) getLine
    ...
    
Treat it as if the user had annotated the bound variable itelf

.. code-block:: haskell

  do 
  
    str1 :: [String] <- replicateM (f 10) getLine
    str2 :: [String] <- replicateM (f 20) getLine
    ...

The same can be argued for ``Monad`` comprehensions, allowing

.. code-block:: haskell

  [ z | x <- [1..5], y = x * x, z = y + x ]
  
to mean

.. code-block:: haskell

  [ z | x <- [1..5], let y = x * x, let z = y + x ]
  
but that is secondary. 

Effect and Interactions
-----------------------

Detail how the proposed change addresses the original problem raised in the motivation.
Detail how the proposed change interacts with existing language or compiler features and provide arguments why this is not going to pose problems.

Costs and Drawbacks
-------------------

If users are willing to enable this extension / flag I imagine they will find this **more consistent** than the current situation.

Most documentation uses the current style however, so that may be confusing depending on the order in which they learn this.

There is always some maintainance cost.

Alternatives
------------

Writing the same code as we do now.

Unresolved questions
--------------------

There are probably some details missing, I haven't put any thought into the subtleties.

Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
