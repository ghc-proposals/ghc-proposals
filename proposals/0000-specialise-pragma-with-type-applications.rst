.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Proposal title
==============

GHC's ``SPECIALISE`` pragma allows the user to instruct the compiler specialise
a polymorphic function at a concrete type. However, its current syntax is quite
unwieldy, requiring that the user repeat the entire type of the the specialised
function, including those parts that are shared with its unspecialised type.

Here we exploit the ``TypeApplications`` syntax added in GHC 8.0 to write many
uses of ``SPECIALISE`` more concisely.

Motivation
----------

The ``SPECIALISE`` pragma is a means of guiding the optimizer towards the types
a function will most commonly be used at. For instance, consider this
polymorphic function from the ``postgresql-simple`` database bindings, ::

    foldWithParser :: ( MonadIO m, MonadMask m, ToRow params )
                   => RowParser row -> Connection
                   -> Query -> params
                   -> a -> (a -> row -> m a)
                   -> m a
    foldWithParser parser conn template qs a f = ...

Here we have a function with a non-trivial, polymorphic type. If we know that
this function may often be used at the ``IO`` monad then we might instruct
GHC to derive an appropriate specialisation, ::

    {-# SPECIALISE
        foldsAndParser
            :: RowParser row -> Connection
            -> Query -> params
            -> a
            -> (a -> row -> IO a) -> IO a
        #-}

This is, of course, quite a mouthful! Moreover, if we would at some point like
to change the type of ``foldWithParser``, we would need to update each of the
specialisations.

We would ideally like to use the fact that we are merely instantiating the
function's ``m`` type argument at ``IO``.

Proposed Change
---------------

Here we propose to allow use of the ``TypeApplications`` syntax introduced in
GHC 8.0.1 in place of the type in a ``SPECIALISE`` pragma. For instance, in the
case of ``foldWithParser`` above we might first add an explicit ``forall`` to
make the order of type variables clear, ::


    foldWithParser :: forall m params row a. ( MonadIO m, MonadMask m, ToRow params )
                   => RowParser row -> Connection
                   -> Query -> params
                   -> a -> (a -> row -> m a)
                   -> m a
    foldWithParser parser conn template qs a f = ...

and then write its ``SPECIALISE`` pragma as a unsaturated type application, ::

    {-# SPECIALISE foldsAndParser @IO #-}

Of course, this raises the question of how we might quantify over ``m`` while
instantiating a later variable, for instance ``params ~ ()``. Here we propose a
very restricted use of explicit ``forall`` syntax to introduce type binders
which can then be used to fill in the type arguments of a function which we'd
like to quantify over, ::

    {-# SPECIALISE forall m. foldsAndParser @m @() #-}

Proposed syntax
~~~~~~~~~~~~~~~

The syntax of ``SPECIALISE`` would become,

.. productionlist::
    specialise_pragma: "{-# SPECIALISE" activation specialise_body
    specialise_body: bndr "::" type
                   | ["forall" tybndr+ "." bndr type_app*
    type_app: '@' type

    bndr: (a Haskell value-level binder)
    tybndr: (a Haskell type-level binder)
    type: (A Haskell type)

Drawbacks
---------

This introduces yet another dependency on the often-implicit order of type
variables in a function definition, although we are already quite far down this
road with the introduction of ``TypeApplications``.

It can be hard to see at first glance what the type of a specialisation written
in ``TypeApplications``\-form is.

Alternatives
------------

We could be more liberal in the sorts of types we allow to be written with
``forall``. For instance, we might one to constrain ``forall``\-bound types
(ignore the silly choice of syntax), ::

    {-# SPECIALISE forall params. AClass params => foldsAndParser @IO @params #-}

That being said, it's not clear that this is a solution to a problem that users
actually feel. It seems the simple approach proposed above would give us 90% of
the benefit for a fraction of the complexity.

Unresolved Questions
--------------------

Are there any parts of the design that are still unclear? Hopefully this section
will be empty by the time the proposal is brought up for a final decision.
