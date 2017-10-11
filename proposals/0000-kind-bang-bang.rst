.. proposal-number:: 

.. trac-ticket:: 

.. implemented:: 

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

``:kind!!``
===========

This is a proposal to add a ``:kind!!`` command to *ghci* for expanding both
type families *and* type synonyms.


Motivation
----------
*ghci* currently does not provide a way to expand type synonyms. It accidentally
did, in a single release (GHC 7.8.4), as evidenced by
`this trac ticket <https://ghc.haskell.org/trac/ghc/ticket/13795#comment:3>`_.

This accidental feature turned out to be quite useful in practice, to me and other
haskellers I have talked to, in a variety of contexts. The one I'm the most
capable of talking about is the servant libraries, which requires its users
to define *types* that describe web APIs, at the type-level. Such types are
then usually given a name, using a type synonym:

.. code-block:: haskell

   type API = "cat" :> Get '[JSON] Cat
         :<|> "dog" :> Get '[JSON] Dog

A quick github search shows reveals that the vast majority of APIs in the wild
follow this "convention". *servant* also uses type families a lot, for computing
fine-grained expected types for request handlers that implement such an API,
or for client functions that can query individual endpoints of this API, or
type-safe links, etc.

The existing ghci commands that can help a servant developer or user figure out
what the library's type-level computations do and why they might be going wrong
are ``:kind`` and ``:kind!``. The former simply returns the
*kind* of a term, while the latter also returns the normalized term, which
is usually used for reducing applications of type families, as illustrated
by the following ``ghci`` session:

.. code-block:: haskell

  $ ghci
  GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
  Prelude> :set -XTypeFamilies
  Prelude> type family F a
  Prelude> type instance F Int = Bool
  Prelude> type instance F Bool = Int
  Prelude> type instance F Char = String
  Prelude> :kind F
  F :: * -> *
  Prelude> :kind F Int
  F Int :: *
  Prelude> :kind! F Int
  F Int :: *
  = Bool

If we ask for ``:kind! F Char`` though, we do not get to the "bottom" of
the reduction, in a way, since *ghci* answers back ``String``. That is not
a problem, but it would be nice to have a command readily available that we
can use as a hammer to reduce any mix of type families and type synonyms
as much as possible.

Here is a less contrived example, using *servant*, which does not involve
type families at all. It is often useful to abstract common patterns
in web APIs into "parametrized APIs", which here are simply
type synonyms with type parameters. For instance:

.. code-block:: haskell

   -- POST an 'a' to /, get an identifier of sorts 'i'
   type New i a = ReqBody '[JSON] a :> Post '[JSON] i

   -- GET the 'a' associated to the given 'i' at /<the identifier>
   type View i a = Capture "id" i :> Get '[JSON] a

   -- We now combine both to provide a reusable pair of route
   -- definitions for a common scenario. :<|> separates two
   -- endpoints.
   type CreateView i a = New i a
                    :<|> View i a

   -- Let's use it:
   type MyAPI = CreateView UserId User :<|> CreateView ItemId Item

Those "API types" can however become somewhat complicated rather quickly.
While ``:kind`` allow a user to distinguish between the different kinds involved
in API types (``*``, ``Symbol``, ``[*]`` among others) and ``:kind!`` allows
a user to evaluate servant's type families on API types right from *ghci*,
there is no easy way to find out what type `MyAPI` reduces (or "unrolls") to,
when we expand all the type synonyms involved. Note that even ``Post`` and
``Get`` are type aliases for a general ``Verb`` construct. However, neither
``:kind`` nor ``:kind!`` offer any insight into the structure of ``MyAPI``,
for exploratory or debugging purposes:

.. code-block:: haskell

   Prelude Servant.API λ> :kind MyAPI
   MyAPI :: *
   Prelude Servant.API λ> :kind! MyAPI
   MyAPI :: *
   = MyAPI

Proposed Change Specification
-----------------------------

This proposal is about adding a ``:kind!!`` command to *ghci*, which just like
``:kind`` and ``:kind!`` takes a type as argument, but instead of just reducing
type families involved in the given type, this command would additionally expand
all type synonyms.

``:kind!!`` would behave exactly like ``:kind!`` when no type synonym is
involved in the command's argument. When there is one or more type synonym,
the ``:kind!!`` command would substitute them by the type they are a synonym of,
applying this transformation everywhere until no type synonym is left, yielding
the final answer for the user. The desired behavior can be summarized by the
following *ghci* session, using a fork of GHC implementing this proposal:

.. code-block:: haskell

   Prelude λ> :set -XTypeFamilies
   Prelude λ> :kind!! Maybe Int
   Maybe Int :: *
   = Maybe Int
   Prelude λ> type family F a
   Prelude λ> type instance F Int = Bool
   Prelude λ> type instance F Bool = Char
   Prelude λ> type instance F Char = String
   Prelude λ> :kind!! F Int
   F Int :: *
   = Bool
   Prelude λ> :kind! F Int
   F Int :: *
   = Bool
   Prelude λ> :kind!! F Char
   F Char :: *
   = [Char]
   Prelude λ> :kind! F Char
   F Char :: *
   = String

This would not interact in any way with other commands, as the new functionality
will only be available through the new command, ``:kind!!``.

Effect and Interactions
-----------------------

The command described by this proposal would provide the desired functionality
by expanding all the type synonyms and reveal the complete structure of the
argument type in terms of its core constituents. Applied to the problematic
servant example from the *Motivation* section, the command would yield:

.. code-block:: haskell

   Prelude Servant.API λ> :kind!! MyAPI
   MyAPI :: *
   = ((ReqBody '[JSON] User :> Verb 'POST 200 '[JSON] UserId)
   :<|> (Capture "id" UserId :> Verb 'GET 200 '[JSON] User))
   :<|> ((ReqBody '[JSON] Item :> Verb 'POST 200 '[JSON] ItemId)
   :<|> (Capture "id" ItemId :> Verb 'GET 200 '[JSON] Item))

This would not have any contentious interaction as this would only
be available under a new command without touching the behaviour of
existing ones.

Costs and Drawbacks
-------------------

I already have a very small patch that implements the ``:kind!!`` command. I
expect this to be a very low maintenance feature as it is expressed in terms
of GHC functions that do all the hard work. I am of course willing to keep
an eye on it and update its code if necessary to accomodate for potential
breaking changes.

Regarding the learnability for novice users, if this has any impact I'd definitely
say it would have to be a positive one, in that users aware of this command could
just use ``:kind!!`` (maybe a different name would be better, I'm not sure yet)
to figure out the definition of any type synonym they want and learn more about some
new library or module by using a mix of ``:browse``, ``:info``, ``:kind!!``
and other commands. I am however not entirely convinced that newcomers would
jump on this command even if well-documented, so this is all hypothetical.

I am not aware of any drawback to be solved except the obvious one.
How should that functionality be made available: as a new command named
``:kind!!`` as suggested (but not required) here, under some other name or
folded into ``:kind!``? I personally quite like the thought of having
fine-grained tools for untangling complex type-level expressions, with
the hammer that expands everything being ``:kind!!``. This name is
admittedly a bit bizarre but certainly is consistent with ``:kind!``:
the additional *!* signals that this command also expands type synonyms.

Alternatives
------------

I am aware of two alternative ways to solve the problem. The first would be
to fold that functionality into the ``:kind!`` command and have that one expand
everything. The second is more of a workaround and is mentionned in
`the trac ticket <https://ghc.haskell.org/trac/ghc/ticket/13795#comment:7>`_ as
well.

It defines a helper type family because ``:kind!`` appears to reduce
type synonyms that appear immediately as arguments to a type family.
``:kind!`` fails to expand them if they appear deeper in the expression
or outside of any type family application. This is therefore not
a good enough workaround as its applicability is very limited and setting it up
is nowhere near as simple as typing a *ghci* command.

Unresolved questions
--------------------

Are we willing to have fine-grained ``:kind``, ``:kind!`` and ``:kind!!``?
Assuming reasonable explanations for all 3 commands in the manual, to clarify
the subtle and not-so-subtle differences between them?

Implementation Plan
-------------------

I have a working patch already along with an example
`in this gist <https://gist.github.com/alpmestan/50a0a00ebf4208c8ae898f84d6b41e07>`_.
If this proposal is approved, I would clean it up and submit it for review,
address the feedback, as well as augment the manual to document the new command
and clarify the differences with its two relatives. The command would then hopefully
get merged and be available in an upcoming GHC release.
