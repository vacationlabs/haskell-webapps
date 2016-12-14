.. _a_server_client_architecture

A server-client architecture
============================

In this installment of the series, we'll see how to implement a client-server
architecture, with a common package to share code and abstractions between the
two parts.

The code for today's repo is in:

Let's take a brief look at the structure of packages:

From a technical stanpoint, the aim of this project is to see how various
libraries fit together: reflex and reflex-dom for the frontend, a simple servant
server on the backend, the bridge among the two worlds being made with
servant-reflex.

Note the structure of the project: three separate cabal projects are created,
one for the frontend, one for the backend, and one for the API and the shared
datatypes, which is included in the other two.


* A simple server
You can see that the program is structured against a simple server written with servant

* Servant integration

* A project with sharing between backend and frontend

In common we have all the code shared between the client and the frontend, for
example the definition of the API, or the things that will come with authentication.
The server package only contains the things needed to run a server at the specific address.

* Using servant-reflex to generate the 

* Making validation work

* The requisites for validation

* Validation should be done both in the client and in the backend

* Sharing the common infrastructure, so that code duplication is reduced

* The main difficulty is linking back the error to the recipient

* Why a solution with digestive-functors isn't sufficient

* The description of my package

* An example: authentication for a user

* How the various data is being generated

* How it's linked back in the structure

The API
=======

The common code in this simple case contains only the definition of the user type and the type for our servant API

The server code is a simple server that serves a mock authentication.

The client code explained

The usage of `servant-reflex`
=============================

.. code-block:: haskell

  client :: HasClient t m layout => Proxy layout -> Proxy m -> Dynamic t BaseUrl -> Client t m layout

For example in our code we use this feature to like this:
