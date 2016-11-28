Introduction
==============================

Connecting to the PostgreSQL database
---------------------------------------

Haskell Relational Record (HRR) promises to be a very high level abstraction for both SQL query generation as well as correlation
of SQL results with Haskell data types. It sits on top of HDBC, the Haskell Database Connectivity package. So connecting
to your PostgreSQL database entails no more than installing HDBC's PostgresSQL driver, importing its Haskell
module and passing a PostgreSQL connection string as an argument. ::

    import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

    getDataSource :: IO Connection
    getDataSource = connectPostgreSQL "host=some.dbserver.com dbname=mydatabase"

For any project using HRR this is the very first thing you should define - otherwise your project won't even compile! Why's that?
Because HRR generates Haskell record data types from your database schema at compile-time via Template Haskell (TH);
those in turn must then type-check against the queries you've defined in your code. The instant gratification here is that you can't
end up with queries in your project that are inconsistent with your database (e.g. when the underlying schema has changed due to
some migration). Also, you avoid writing a fair amount of boilerplate code.

On the other hand though, a live connection to a database *is* prerequisite to even compiling a HRR project.


Your very first query
---------------------

Let's assume a database table like the following storing user data for some app or service:

  .. code-block:: sql

    create table users(
       id serial primary key
      ,email text not null
      ,name text
    );

What needs to be done to query this table? Well, first of all, HRR needs to derive a Haskell data type for it. It's advised to have a separate
Haskell module (and thus, namespace) for each data type you derive with HRR, since there might be more than one table in the database using
identical column names like e.g. 'id' and 'name'.

So you define a small helper module which will contain a wrapper function for HRR; this function will output some
top-level defintions via HRR's TH magic. ::

    import  Database.HDBC.Query.TH              (defineTableFromDB)
    import  Database.HDBC.Schema.PostgreSQL     (driverPostgreSQL)

    defineTable :: String -> Q [Dec]
    defineTable tableName =
        defineTableFromDB getDataSource driverPostgreSQL "public" tableName [''Show]

As you can see, we needed ``getDataSource`` from above; ``"public"`` (or whatever name) refers to the corresponding PostgreSQL schema.
Now we're good to go to derive our Haskell record type ``Users``: ::

    {-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

    $(defineTable "users")

And that's it. The module now contains the definition of the record type, along with several instances that handle conversion from and to SQL,
and a basic relation to access the underlying database table. Since that's exactly what we wanted to do in the first place, let's do it! We
import that module and define a query that looks up the user's complete record, given their registration e-mail address: ::

    getUserEntry :: Relation String Users
    getUserEntry = relation' . placeholder $ \param -> do
        u       <- query users
        wheres  $ u ! email' .=. param
        return  u

The type signature reads '*... is a relation from String to Users*' in a straightforward way; the rest will be explained in more detail later. For
now you just need to know that all building blocks of this relation are either part of HRR's DSL for describing queries or part of what we've derived
earlier for the record data type - thus the definition is wholly type-safe. What remains is rendering the relation as a SQL query and
executing it on the DB: ::

    import  Database.HDBC.Record.Query      (runQuery)
    import  Database.Relational.Query       (relationalQuery)

    myFirstQuery :: IO [Users]
    myFirstQuery = getDataSource >>= \conn ->
        runQuery conn (relationalQuery getUserEntry) "some.user@mailprovider.com"

And that was your very first query in HRR!


Type works
----------

TODO


Reading into custom data types
------------------------------

TODO
