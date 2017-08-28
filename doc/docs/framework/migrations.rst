.. _migrations:

Migrations: Creating and editing DB models
==========================================

Setting up a fresh database
---------------------------

.. code:: sh

   poi migrate prepare

This command will generate the following tables and triggers in your DB, **if they don't already exist:**

#. ``schema_migrations`` table to track which migrations have already been run. This is directly influenced from Rails migrations.
#. ``trg_update_modified_column`` - a trigger to automatically set ``updated_at`` column to ``current_timestamp`` whenever any row is updated in a table which contains this column.

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
   -- INSERT YOUR MIGRATION SQL HERE
   |])

   down = ([qc|
   -- INSERT YOUR ROLLBACK SQL HERE
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

   CREATE TRIGGER trg_modify_updated_at
          BEFORE UPDATE ON users
          FOR EACH ROW EXECUTE PROCEDURE update_modified_column();
   |])

   down = ([qc|
   DROP TABLE users;
   |])


.. tip::

   We should probably have our own quasi-quoter called ``sql`` or something, which allows mixing of raw SQL along with custom helper functions. We can write helper functions to generated indexes, triggers for audit logs, triggers for updating ``updated_at``, triggers for pushing to DB based ``event_log``, etc.


Now, run the migration, with the following command:

.. code:: sh

   poi migrate up

Here is what this will do, under the hood:

#. This will connect to the development database (by default) and execute all pending migrations. The timestamp/version of all migrations in the ``<projectRoot>/migrations/`` directory will be looked-up in the ``schema_migrations`` table. Any migration which is not there in the table will be executed in ascending order of the timestamp/version.
#. Each individual migration will be wrapped within a **single BEGIN/COMMIT** block - which means that if any migration throws an error:

   a. that particular migration will be rolled back,
   b. all previous migrations (which have already executed successful) will persist,
   c. and all migrations which are yet to be executed, will be aborted.

#. Once the migration runs successfully, it will run the model code-generator under the hood, to create/modify/delete any model files that need to be updated as a result of this migration.

Editing an existing models
--------------------------

The worlflow remains pretty much the same as "Creating a new model":

#. Create a migration file
#. Write a bunch of ``ALTER`` statements in the migration
#. Run ``poi migrate up``

Other useful command-line arguments
-----------------------------------

.. code:: sh

   poi migrate [ up | down | redo | prepare | new ]

   --env environmentName
         Explicitly pass an environment to the script. Default value is `development` or `APP_ENV` environment variable (in that order)
   --version regex
          Pass a specific migration version to the script. A fuzzy (or regex) match will be attempted with the given argument.
          If exactly one migration matches, it will be targeted, else all matching migrations will be printed out STDOUT.
