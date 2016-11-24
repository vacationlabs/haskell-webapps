Introduction
==============================

Connecting to the Postgresql database
---------------------------------------


Opaleye uses a lower level postgresql database client library called postgresql-simple to actually talk to the database.
So we will start by getting a value of type *Connection*, which represents a connection to a postgresql database server
and is defined by the postgresql-simple library. We obtain a 'Connection' by using the 'connect'
function, also provided by the postgresql-simple library. ::

    connect :: ConnectInfo -> IO Connection

As you can see, the 'connect' function accepts a value of type 'ConnectInfo', which is just a record that hold
the usual database connection parameters, host, dbname, password and such.

The postgresql-simple libary also provide us with another function 'defaultConnectInfo', that a value of
type 'ConnectInfo', which is preloaded with default values like 'localhost' for host, and 5432 as port etc.

This means that you can get a connection as follows ::

    do
      connection <- connect defaultConnectInfo

Now that we have a live connection to the database, we can start setting up stuff to ineract with it.
Actually you can start querying the database right now using functions provided by postgresql-simple.
But that would be really low level and we want to use opaleye to communicate with the database at a higher level.

Your very first query.
----------------------

You need some 'setting up' to tell opaleye about properties of your table rows. By that way, you get a bit
of safety from type system. For example, the type system can prevent you from running code that will
create "column cannot be null" error from the database. You won't get these kinds of safety nets if you
are using raw postgresql-simple functions.

Let us get started defining a *Table* value for a table named "users", which has the
following structure,

  .. code-block:: sql

    create table users(
       id serial primary key
      ,email text not null
      ,name text
    );

Here is the a haskell function (getUserRows), that returns all the rows in this table, using Opaleye's api.

  .. literalinclude:: code/opaleye-select-basic.hs


  Warning! The above code will not compile and error out with an "ambiguous type variable" error, if you remove the type annotations of the userTable and getUserRows functions.

Type works
----------

Let us first look at the type of the 'userTable'. ::

    userTable :: Table 
        (Column PGInt4, Column PGText, Column PGText) 
        (Column PGInt4, Column PGText, Column PGText)

The Table is a type constructor that take two concrete types. The first type is the type of the value that opaleye will use to *write* to
the table. Second type is the type of value that opaleye will *read* from table.

In this case the both the write type and the read type are the same *viz* (Column PGInt4, Column PGText, Column PGText).
If we have an autogenerateable field in our table, we might want to make that field a *Maybe* type, so that opaleye can
use a *Nothing* value while writing to the table. In that case, the read type and write type will not be same.

You might have noticied that we are using a strange function called *p3*. You can also see we are calling a *required* function
passing in the column names of our table. The type of *required* function is  ::

    required :: String -> TableProperties (Column a) (Column a)

So this code ::

    (
    required "id",
    required "email",
    required "name"
    )

expands to the tuple ::

  (TableProperties (Column a) (Column a),
    TableProperties (Column a1) (Column a1),
    TableProperties (Column a2) (Column a2))

The function *p3* converts a value of type ::

    (TableProperties a0 b0, TableProperties a1 b1, TableProperties a2 b2)
    
into a value of type ::

    TableProperties (a0, a1, a2) (b0, b1, b2)*

and applying it to the earlier tuple give us a value of the required type  ::

    Table 
      (Column PGInt4, Column PGText, Column PGText) 
      (Column PGInt4, Column PGText, Column PGText)

The function *p3* only works with tuples of 3 items. If your table has 4 columns, then
you have to use *p4* and so on. The Profunctor.Product module defines these functions 
upto *p35*.

If you took at our *getUserRows* function in the earlier code, you will see
that we are getting a tuple of type ::

    (Int, String, String)

instead of type ::
    
    (Column PGInt4, Column PGText, Column PGText)

We can do this because Opaleye has built in conversion methods defined between
common haskell and postgresql types. Let us see how this conversion is defined by
defining our own data type and making opaleye give us data wrapped in it. 

Reading into custom data types
------------------------------

In the below code, we define a new data type *UserId* that just wraps an Int in it.
Instead of using an *Int* for user id, we now use this *UserId* type. Let us see
how we can make opaleye return a *UserId* type from a field that has type of *(Column PGInt4)*
in the read type tuple.

  .. literalinclude:: code/opaleye-select-custom-datatype.hs
     :emphasize-lines: 21-25

The import piece of code here is these ::

    instance FromField UserId where
      fromField field bs = UserId <$> fromField field bs

    instance QueryRunnerColumnDefault PGInt4 UserId where
      queryRunnerColumnDefault = fieldQueryRunnerColumn

Let us comment those lines and try to compile it. You will get the following error. ::
    
    * No instance for (QueryRunnerColumnDefault PGInt4 UserId)

Ok, sure enough, this looks like a typeclass that can handle conversion between PGInt4 and
UserId. Let us see what it expects, ::

    class QueryRunnerColumnDefault pgType haskellType where
      queryRunnerColumnDefault :: QueryRunnerColumn pgType haskellType

Ok. This means that we just needs to define a function that returns a value of type *QueryRunnerColumn PGInt4 UserId*.
if you look at the hackage page for this typeclass here_, you can see that it mentions a function ::

    fieldQueryRunnerColumn :: FromField haskell => QueryRunnerColumn pgType haskell

From the about signature, we can see that this function can give our required type *QueryRunnerColumn PGInt4 UserId* as
soon as we make UserId an instance of the *FromField* typeclass.

Looking up the info for *FromField* we get this ::

  > :info FromField
    class FromField a where
      fromField :: FieldParser a

Ok, what is FieldParser? ::

  > :info FieldParser
  type FieldParser a = Field -> Maybe ByteString -> Conversion a

So, it's just a function that takes a value of *Field* and a *Maybe ByteString* and
returns a value of type *Conversion a*. So if we are to define a FromField instance
for UserId, we just need to implement the function ::
    
    Field -> Maybe ByteString -> Conversion UserId

If you lookup the info for *Conversion*, you will see that it is an instance of
a *Functor* typeclass. We know that a *FromField* instance exists for *Int*. This means
that we have an instance of the *fromField* function that can take a *Field* and a *Maybe ByteString*
and return a value of type *Conversion Int*. If we *fmap* the *UserId* constructor over *Conversion Int* we will get
a *Conversion UserId*, and that is exactly we do in the following lines ::

    instance FromField UserId where
      fromField field bs = UserId <$> fromField field bs

That is it. Now we can recieve values of type *UserId* directly from Opaleye's query functions.


.. _here: https://hackage.haskell.org/package/opaleye-0.5.2.1/docs/Opaleye-Internal-RunQuery.html#t:QueryRunnerColumnDefault
