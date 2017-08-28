.. _migrations:

Migrations: Creating and editing DB models
==========================================

Creating a new model
--------------------

  .. code-block::

     poi migrate new createUsers

This will create a file called ``<projectRoot>/migrations/MYYYYMMDDHHmmSS-createUsers.hs`` (where ``YYYYMMDDHHmmSS`` is the actual timestamp on which you run the command). The file will look like the following::

  .. code-block:: haskell

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

TODO

