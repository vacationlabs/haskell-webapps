.. _a_server_client_architecture

A server-client architecture
============================

In this installment of the series, we'll see:

* how to implement a client-server architecture, with a common package to share
  code and abstractions between the two parts.

* how to use the package ``servant-reflex`` to seamlessy embed server requests
  in the frp network.

* how to use a library to talk about data validation, of the kind done in html
  forms.

The code for today's repo is in: TODO

Let's begin with the simplest matter: how to share data definitions and
abstractions between the backend and the frontend. It seems a very widespread
practice to create three packages: one, let's say ``common``, will contain the
shared abstractions, and will be included by the other two, ``client`` (with the
code for the webapp, to be compiled with ghcjs), and ``server`` (with the code
for the server, to be compiled with ghc). That's all.

Let's also briefly describe here what this application does and the structure of
the server: TODO

Validation
----------

The requisites for validation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When designing a web app there are two kinds of validations that can be run: the
first is the one done on the client, to provide validation against crude error
(think of inputing a well-formed email address); the other one, usually done on
the server, is about validating the data against our knowledge (think of
checking if an email address is in the user database).

Sometimes, for security reasons, the server might want to do again the
validations which happened in the client, and so we need way of easily composing
validations, sharing the common infrastructure, so that code duplication is
reduced.

Another problem that we encouter is that the format in which we report back the
error to the client must be convenient enough to report errors near the UI
element which caused them; for example, when validating a user with a
combination of mail and password, an error message for a wrong password should
be displayed near the password itself.

This brings us to discussing common solution for validation: there is the
``Data.Validation`` approach, in the ``validation`` package, which is
essentially ``Either`` with another applicative instance. Unfortunately this
approach fails us because we have no obvious way of reporting back errors to
their use site.

On the other hand we have the ``digestive-functors`` approach, which
unfortunately is geared towards a server-centric approach, and makes validations
on the client difficult to write (TODO: Check the correctness of this
information with Jasper).

A possible solution
^^^^^^^^^^^^^^^^^^^

So let's think about another solution: let's say I'm implementing a
Mail/Password validation, so the type of my user could be

.. code-block:: haskell

  data User = User Mail Text

Now, if we expand slightly our definition to

.. code-block:: haskell

  data UserShape f = UserShape (f Mail) (f Text)

we gain the possibility of talking about a structure whose fields talk about
operations or data parametrized by ``Mail`` and ``Text``.

For example, some functor that we might want to use are ``Identity`` (and in
fact ``User`` is obiously isomorphic to ``UserShape Identity``), ``Maybe`` or
``Either Text`` to model the presence of errors, or for example

.. code-block:: haskell

  newtype Validation a = Validation { unValidationF :: Compose ((->) a) Maybe a }

so that:

.. code-block:: haskell

  UserShape Validation ~ UserShape (Mail -> Maybe Mail) (Text -> Maybe Text)

Now that we can talk about this "user shaped" objects, we might want to combine
them, for example with something like:

.. code-block:: haskell

  validateUser :: User -> UserShape Validation -> UserShape Maybe

the ``shaped`` library has a generic mechanism of doing this kind of
manipulations (check out the ``validateRecord`` function). The library uses
internally ``generics-sop`` to construct and match the generic representations,
and some Template Haskell to shield the user from the boilerplate instance
declarations.

Now, we can send to the server a tentative ``User`` to check, and get back a
``UserShape Maybe`` that we can easily map back to our input text-boxes.

You can check how that's done in the client for today's installment (TODO link
the correct lines).

How to query the API endpoint
-----------------------------

The common code in this simple case contains only the definition of the user
type and the type for our servant API

The server code is a simple server that serves a mock authentication. I'm not
entering in an in depth discussion on the ``servant`` approach here (if you're
interested check the wonderful `servant documentation
<http://haskell-servant.readthedocs.io/en/stable/tutorial/index.html>`_, but the
gist is that you can create from a description of the api, in this project:

.. code-block:: haskell

  type MockApi = "auth" :> ReqBody '[JSON] User :> Post '[JSON] Text
               :<|> Raw

A server satisfying that api, here:

.. code-block:: haskell

   server :: Server MockApi
   server = authenticate :<|> serveAssets :<|> serveJS

The package ``servant-reflex`` transforms a Servant API in Reflex functions for
querying it, in the same way ``servant-server`` transforms it in a server. The
invocation is very easy:

.. code-block:: haskell
   
   let url = BaseFullUrl Http "localhost" 8081 ""
   (invokeAPI :<|> _ :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)

.. code-block:: haskell

  client :: HasClient t m layout => Proxy layout -> Proxy m -> Dynamic t BaseUrl -> Client t m layout

As you can see, ``client`` is the most important function: it takes proxies for
the API and the monad in which the computation is executed (as it's customary to
run a reflex computation in a (constrained) universally quantified monad, like
our own ``body :: MonadWidget t m => m ()`` (the syntax with ``@`` is due to the
ghc 8's ``TypeApplications`` extension, without it you should have written
``Proxy :: Proxy MockApi`` etc.)

That gives us a mean to call the relevant API endpoint (TODO: detail the type of
the transformed function, detailing how the API call is translated in events.
Also talk about Xhr).

For example in our code we use this feature to like this:
