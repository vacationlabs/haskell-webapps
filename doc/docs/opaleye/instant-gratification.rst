.. _instant_gratification:

Instant Gratification
=====================

Overview
--------

We'll start by quickly running through the following DB operations, which should give you a sense of "instant gratification" (as the title says!) However, **do not** start writing apps with Opaleye just after reading this. As they say, a little knowledge is a dangerous thing! We **strongly encourage** you to read all the chapters in this tutorial before using Opaleye in any serious project.

* Connecting to the Postgres DB
* Selecting multiple rows
* Selecting a row
* Inserting a row
* Updating a row
* Selecting a single row

Preliminaries
-------------

* Install PostgreSQL. Create a database. Run the table creation script given below.

  .. code-block:: sql

    create table users(
       id serial primary key
      ,name text not null
      ,email text not null
    );

    insert into users(name, email) values ('John', 'john@mail.com');
    insert into users(name, email) values ('Bob', 'bob@mail.com');
    insert into users(name, email) values ('Alice', 'alice@mail.com');

* Install ``opaleye`` using your favourite package management tool
* Fire up your favourite text editor and copy-paste the code snippet below, and make sure it compiles without any errors.

  .. literalinclude:: code/instant-gratification.hs


**Now read on to understand what this code is doing...**

Teaching your table schema to Opaleye
-------------------------------------

Let's tackle the cryptic ``userTable`` definition at the very beginning of this code.

  .. code-block:: haskell

    userTable :: Table
      (Column PGInt4, Column PGText, Column PGText)  -- read type
      (Column PGInt4, Column PGText, Column PGText) -- write type
    userTable = Table "users" (p3 (required "id",
                                   required "name",
                                   required "email"))

Here's what it is basically teaching Opaleye:

* We will be reading rows of the type ``(Column PGInt4, Column PGText, Column PGText)`` from the table.
* We will be writing rows of the same type to the table. (Opaleye allows you to read and write rows of *different* types for very valid reasons. Read `Basic DB Mapping`_ for more details on this.)
* The table's name is ``users``
* The first column in the table is called ``id``; it is *required*; and it maps to the first value of the tuple.
* The second column in the table is called ``name``; it is *required*; and it maps to the second value of the tuple.
* The third column in the table is called ``email``; it is *required*; and it maps to the third value of the tuple.

We will need to use ``userTable`` to SELECT, INSERT, UPDATE, or DELETE from the ``users`` table via Opaleye.

To learn more about how to map your DB schema to Opaleye's ``Table`` types, please read :ref:`basic_mapping` and :ref:`advanced-db-mapping` chapters.

Connecting to the Postgresql database
---------------------------------------

Opaleye uses `postgresql-simple <https://hackage.haskell.org/package/postgresql-simple>`_ to actually talk to the database.So, we first start by getting hold of a DB ``Connection`` using postgres-simples's ``connect`` function:

  .. code-block:: haskell

    conn <- connect ConnectInfo{connectHost="localhost"
                               ,connectPort=5432
                               ,connectDatabase="opaleye_tutorial"
                               ,connectPassword="opalaye_tutorial"
                               ,connectUser="opaleye_tutorial"
                               }


  .. warning:: Please take care to change the DB connection settings based on your local system.

Selecting all rows
------------------

Next we fetch and print all the rows from the ``users`` table:

  .. code-block:: haskell

    allRows <- selectAllRows conn
    print allRow

which calls ``selectAllRows``:

  .. code-block:: haskell

    selectAllRows :: Connection -> IO [(Int, String, String)]
    selectAllRows conn = runQuery conn $ queryTable userTable

This uses ``runQuery``, which is basically ``SELECT`` in Opaleye. Please take **special note** of the type signature of this function. It evaluates to ``IO [(Int, String, String)]``, whereas we clearly told Opaleye that we will be reading rows of type ``(Column PGInt4, Column PGText, ColumnPGText)``. So, why doesn't this function evaluate to ``IO [(Column PGInt4, Column PGText, ColumnPGText)]``?

This is because Opaleye knows how to convert most basic data types from DB => Haskell (eg. ``PGInt4`` => ``Int``). And also vice versa. 

However, here's a **gotcha!** Try compiling ths function *without* the type signature. The compiler will fail to infer the types. This is also due to the underlying infrastructure that Opaleye uses to convert DB => Haskell types. To understand this further, please read `Advanced DB mapping`_.

Inserting a row
---------------

  .. code-block:: haskell

    insertRow :: Connection -> (Int, String, String) -> IO ()
    insertRow conn row = do
      runInsertMany conn userTable [(constant row)]
      return ()

  This function uses ``runInsertMany`` which is basically Opaleye's version of ``INSERT``, **but** it only supports inserting *multiple rows*. This is why it is called ``runInsertMany`` instead of ``runInsert`` and the third argument is a *list* of rows.

    .. topic::  So, what does constant row do? 

      It converts Haskell types => DB types, i.e. ``(Int, String, String)`` => ``(Column PGInt4, Column PGText, Column PGText)`` This is because we clearly told Opaleye that we will be writing rows of type ``(Column PGInt4, Column PGText, Column PGText)`` to ``userTable``. However, our program doesn't deal with values of type ``Column PGText`` or ``Column PGInt4`` directly. So, this function - ``insertRow`` - gets a regular ``(Int, String, String)`` tuple and uses ``constant`` to convert it to ``(Column PGInt4, Column PGText, Column PGText)`` before handing it over to Opaleye.

  .. note:: Strangely, while ``runQuery`` converts DB => Haskell types automagically, ``runInsertMany`` and ``runUpdate`` refuse to do Haskell => DB conversions on their own. Hence the need to do it explicitly when using these functions.

Updating a row
--------------

  .. code-block:: haskell

    updateRow :: Connection -> (Int, String, String) -> IO ()
    updateRow conn row@(key, name, email) = do
      runUpdate 
        conn 
        userTable 
        (\_ -> constant row) -- what should the matching row be updated to
        (\ (k, _, _) -> k .== constant key) -- which rows to update?
      return ()

* As you can see from this function, updating rows in Opaleye is not very pretty! The biggest pain is that you cannot define which columns to update. You are forced to update the **entire row**. More about this in :ref:`updating-rows`.
* You already know what ``constant row`` does - it converts a Haskell datatype to its corresponding PG data type, which for some strange reason, Opaleye refuses to do here automagically.
* The comparison operator ``.==`` is what gets translated to equality operator in SQL. We cannot use Haskell's native equality operator because it represents equality in Haskell-land, whereas we need to represent equality when it gets convert to SQL-land. You will come across a lot of such special operators that map to their correspnding SQL parts.

Selecting a single row
----------------------

  .. warning:: **Caution!** Extreme hand-waving lies ahead. This is probably an incorrect explanation, but should work well-enough to serve your intuition for some time.

  .. code-block:: haskell

    selectByEmail :: Connection -> String -> IO [(Int, String, String)]
    selectByEmail conn email = runQuery conn $ proc () ->
        do
          row@(_, _, em) <- queryTable userTable -< ()
          restrict -< (em .== constant email)
          returnA -< row

And finally, the last section of this chapter introduces you to a weird arrow notation ``-<``, which we have absolutely no clue about! All we know is that it works... mostly!

Check the type of ``row@(_, _, em)`` in your editor. It should be ``(Column PGInt4, Column PGText, Column PGText)``, which means that if we do some hand-waving, here's what's happening in this function:

* ``queryTable userTable -< ()`` maps to a ``SELECT`` clause in SQL-land. 
* The columns selected are *conceptually* capurted in ``row@(_, _, em)`` in SQL-land (which is why the row is a PG type instead of a Haskell type).
* ``restrict`` maps to ``WHERE`` in SQL. The condition refers to ``email`` which is a Haskell-type and hence, we need to convert it to a PG type via ``constant email`` before comparing it to ``em``
* Finally ``returnA`` does some magic to return all the *captured* columns back to Haskell-land.

