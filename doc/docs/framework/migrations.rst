.. _migrations:

Migrations: Creating and editing DB models
==========================================

Creating a new model
--------------------

.. code:: sh

   poi migrate new createUsers

This will create a file called ``<projectRoot>/migrations/MYYYYMMDDHHmmSS-createUsers.hs`` (where ``YYYYMMDDHHmmSS`` is the actual timestamp on which you run the command). The file will look like the following:

.. code:: haskell

   module M20170828164533_createUsers where

   import Control.Monad
   import Database.Rivet.V0
   import Text.InterpolatedString.Perl6 (qc)

   migrate :: Migration IO ()
   migrate = sql up down


   up = ([qc|
   -- INSERT YOUR SQL HERE
   |])

   down = ([qc|
   -- INSERT YOUR SQL HERE
   |])

Now edit this file to create your tables, indexes, constraints, triggers, etc. using raw SQL:

.. code:: haskell

   module M20170828164533_createUsers where

   import Control.Monad
   import Database.Rivet.V0
   import Text.InterpolatedString.Perl6 (qc)

   migrate :: Migration IO ()
   migrate = sql up down


   up = ([qc|
   CREATE TABLE users
                (
                  id serial primary key
                  ,created_at timestamp with time zone not null default current_timestamp
                  ,updated_at timestamp with time zone not null default current_timestamp
                  ,username text not null
                  ,password text not null
                  ,first_name text
                  ,last_name text
                  ,status user_status not null default 'inactive'
                  CONSTRAINT chk_status CHECK ((status IN ('active', 'inactive', 'deleted', 'blocked')))
                );
   CREATE INDEX idx_users_created_at on users(created_at);
   CREATE INDEX idx_users_updated_at on users(updated_at);
   CREATE INDEX idx_users_status on users(status);
   CREATE UNIQUE INDEX idx_users_username on users(username);
   |])

   down = ([qc|
   DROP TABLE users;
   |])


.. todo::

   Soemthing comes here...how does thsi get formatted?
