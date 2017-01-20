.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/40>`_.

Context fixes.
==============

I suggest to add a new syntactic form ``context`` which uniformly abstracts over a group of declarations. Consider this example, which does not do anything useful, but shows how the feature can be used to solve the configuration problem:

::

  context fixes progName in
    foo :: Maybe Int -> Either String Int
    foo Nothing  = Left $ progName ++ ": no number given"
    foo (Just i) = bar i
    
    bar :: Int -> Either String Int
    bar 0 = Left $ progName ++ ": zero no good"
    bar n = Right $ n + 1


This is equivalent to 

::

  foo :: String -> Maybe Int -> Either String Int
  foo progName Nothing  = Left $ progName ++ ": no number given"
  foo progName (Just i) = bar progName  i
  
  bar :: String -> Int -> Either String Int
  bar progName  0 = Left $ progName ++ ": zero no good"
  bar progName n = Right $ n + 1


Motivation
------------

Haskell is good at abstracting. If I have a function like::

  foo :: T -> S
  foo t = … 42 …

and I want to make this abstract in the number, I add a new parameter::

  foo :: Int -> T -> S
  foo n t = … n …

Nice and easy. But what if I have a group of declarations like the following::

  foo :: T -> S
  foo t = … 42 … bar s … foo t …
  
  bar :: S -> T
  bar s = … foo t … 42 …

I not only have to add the parameter to every function involved, but I also have to change every function call within the group::

  foo :: Int -> T -> S
  foo n t = … n … bar n s … foo n t …
  
  bar :: Int -> S -> T
  bar n s = … foo n t … n … 

This works, but is clumpsy. Also, it is less easy to understand: As a reader of the new code it is not obvious to me that the `n` parameter is *the same* in all the invocations – after all, one of the calls between the functions could be `foo 42` or `foo (n+1)`.

The suggested new language feature would allow me to express this as follows::

  context fixes n in
    foo :: T -> S
    foo t = … n … bar s … foo t …

    bar :: S -> T
    bar s = … foo t … n …

Note that:

* Besides the indentation and replacing ``42`` by ``n``, I did not have to change any code.
* It is very obvious to the reader that within the indented block, ``n`` is not changed.
* The type of ``foo`` is different within the scope of the ``context`` block: It is ``T -> S`` inside, but ``Int -> T -> S`` outside.

Typical use cases of this might be

* Abstracting some business logic over the configuration, which is typicall read once in the thin IO wrapper, but is static from the point of view of the actual code (the ”configuration problem”).
* Abstracting a lexing ``parsec`` parser over the `Language <http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Language.html>` used.
* *Add your favorite example here.*

See below for some existing ways of approximationg this feature.

Proposed Change Specification
-----------------------------

The grammar would be extended as follows::

  topdecl → …
          | context fixes apat { apat } in decls
          
The names bound in the patterns (the “fixed names”) scope over the *decls*, which forms one recursive group.

In scope at the top level of the module are the names defined by the *decls*, which they type of the fixed names prepended as arguments to the type of the declared function.

Note that the grammar does not allow other kind of *topdecl* things in the scope, i.e. no types, classes, instances…

*It is clear to me what this does at this point, but not so much how and what to write here. So please ask for clarification at the pull request.*

Possible extension
------------------

It might be useful to be able to have a ``where`` clause as in

::

  topdecl → …
          | context fixes apat { apat } in decls where decls

where both groups of *decls* form one recursive group, but only the names from the first group are visible to the outside. This would indicate “internal” functions, just like with a ``where`` clause of a single function.


Effect and Interactions
-----------------------

By being able to abstract over a group of functions, the intent of the programmer is clearer, with more concise code and easier refactoring.

In addition, the ``context`` keyword might be useful for many other language extensions. Therefore the ``fixes`` aspect of it – this is just one way of adding a context.


Costs and Drawbacks
-------------------
Parsing, typing and desugaring seem to be straight-forward (famous last words).

Learnability is not greatly affected. Code using the keywords seems to be understandable even to someone who does not know the feature. The biggest mental hurdle is to predict the type of the abstracted functions outside the scope of the ``context``.


Alternatives
------------

* One alternative was given above: Simply add the parameter to all functions involved.

* Another way of implementing this is to have a “generator function”::

    generator progName = (foo, bar)
     where
      foo :: Maybe Int -> Either String Int
      foo Nothing  = Left $ progName ++ ": no number given"
      foo (Just i) = bar i

      bar :: Int -> Either String Int
      bar 0 = Left $ progName ++ ": zero no good"
      bar n = Right $ n + 1

    foo progName = fst (generator progName)
    bar progName = snd (generator progName)

  This can be automated using Template Haskell, as done in the `seal-module package <https://hackage.haskell.org/package/seal-module>`_, which is motivated in a `blog post <https://www.joachim-breitner.de/blog/443-A_Solution_to_the_Configuration_Problem_in_Haskell>`_.

* A third alternative is using implicit parameters, which works fine as long as one does not want to write type signatures for the functions. With type signatures, the parameter still appears there everywhere.

* If the code is monadic anyways, or by turning it into a monad, the ``Reader`` monad can be used.

* Using mutable references and some hacking with ``unsafePerformIO``…

Related work
------------

Other languages have this feature. For me it is inspired by Isabelle, where you can say

::

  locale withConfig
    fixes theConfig :: Config
  begin
    fun a_fun :: "int => int" where "a_fun n = n + (incSetting config)"
  end

and now have a function ``withConfig.a_fun :: Config => int => int``. One can say ``interpret withConfig theActualConfig`` to get ``a_fun :: int => int`` into scope, where the ``theConfig`` parameter is instantiated with the argument ``theActualConfig``. One can also say ``context withConfig begin … end`` and work within (and extend) the context.

Agda also has this concept in the form of `parametrised modules <http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.Modules#param>`_




Unresolved questions
--------------------

* What should happen with fixity declarations inside a ``context``? (Probably they would be local to the ``context`` block).
* Is the syntax good, and are the keywords well chosen?
* If one wants to give a type signature to the fixed parameters, should there be a way that resembles a type signature of a function? (Right now one can use ``PatternSignatures``, which some may find insufficient). What would that syntax look like?

