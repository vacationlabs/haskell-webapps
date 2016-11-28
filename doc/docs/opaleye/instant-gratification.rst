Introduction
=====================

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
      ,name text not null
      ,email text not null
    );

    insert into "users" values (1, 'John', 'john@mail.com');
    insert into "users" values (2, 'Bob', 'bob@mail.com');
    insert into "users" values (3, 'Alice', 'alice@mail.com');


Here is the a haskell function (getUserRows), that returns all the rows in this table, using Opaleye's api.

.. literalinclude:: code/opaleye-select-basic.hs
   :linenos:


Type works
----------

Let us first look at the type of the 'userTable'. ::

    userTable :: Table 
        (Column PGInt4, Column PGText, Column PGText) 
        (Column PGInt4, Column PGText, Column PGText)

The *Table* is a type that opaleye uses to represent a database Table.

The *Table* type constructor takes two concrete types to create the *Table*. The first type is the type of the value that opaleye will use to *write* to
the table. Second type is the type of value that opaleye will *read* from table.


In this case the both the write type and the read type are the same *viz* (Column PGInt4, Column PGText, Column PGText).
If we have an autogenerateable field in our table, we might want to make that field a *Maybe* type, so that opaleye can
use a *Nothing* value while writing to the table. In that case, the read type and write type will not be same.

Let us see how we construct a *Table* value. The *table* type has got two constructors as shown below, ::

    data Table writerColumns viewColumns
      = Table String (TableProperties writerColumns viewColumns)
      | TableWithSchema String
                        String
                        (TableProperties writerColumns viewColumns)

We will use the first one here. ::

  Table String (TableProperties writerColumns viewColumns)

The first argument to the constructor is a *String*, which has to be the name of the table.
The second argument is of type *TableProperties writercolumns viewcolumns* where *writercolumns* and
*viewcolumns* correspond to the write type and read type respectively. Let us see how we are 
making this second argument.

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

(because p3 works with any instance of a *ProductProfunctor* which TableProperties happen to be)

and applying it to the earlier tuple give us a value of the required type  ::

    TableProperties
      (Column PGInt4, Column PGText, Column PGText) 
      (Column PGInt4, Column PGText, Column PGText)

This is passed as the second argument to the *Table* constructor to obtain the required
*Table* value.

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

In the below code, we define a new data type *UserId* that just wraps an *Int* in it.
Instead of using an *Int* for user id, we now use this *UserId* type. Let us see
how we can make opaleye return a *UserId* type from a field that has type of *(Column PGInt4)*
in the read type tuple.

.. literalinclude:: code/opaleye-select-custom-datatype.hs
   :emphasize-lines: 21-25
   :linenos:

The important piece of code here is, ::

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
if you look at the hackage page for this typeclass 
`here <https://hackage.haskell.org/package/opaleye-0.5.2.1/docs/Opaleye-Internal-RunQuery.html#t:QueryRunnerColumnDefault>`_, 
you can see that it mentions a function ::

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

Reading whole rows into a custom data type
------------------------------------------

Just now we saw how we can read a column into a user defined data type. Now let us see how we can do it
for whole rows. Let us make a *User* record that will hold data from one row from the *user* table we saw
earlier. ::

    data User = User { id :: Int, email :: String, name :: String }

If we just change the type signature of the *getUserRows* function from *(Int, String, String)* to *IO [User]*, we
will get this error ::

    No instance for (Default QueryRunner (Column PGInt4, Column PGText, Column PGText) User)
        arising from a use of `runQuery'

Ok, this means that if we want our rows from table *Table a b* to be read as type *c* we need to make
the triplet QueryRunner b c an instance of the *Default* typeclass.

Let us take a look at this *Default* typeclass, ::

    > :info Default
    class Default (p :: * -> * -> *) a b where
      def :: p a b

It just contains a function *def*, and if we implement an instance for  *QueryRunner (Column PGInt4, Column PGText, Column PGText) User*
the *def* function should return a value of type *QueryRunner (Column PGInt4, Column PGText, Column PGText) User*.

How can we get it? We know that the type *QueryRunner (Column PGInt4, Column PGText, Column PGText) (Int, String, String)* is already
an instance of *Default*, so we can use the *def* function to get a value of that type.

When we look up info about *QueryRunner*, ::

    :info QueryRunner
    type role QueryRunner representational nominal
    data QueryRunner columns haskells = Opaleye.Internal.RunQuery.QueryRunner (Unpackspec columns ())
          (columns -> Database.PostgreSQL.Simple.Internal.RowParser haskells) (columns -> Bool)
            -- Defined in `Opaleye.Internal.RunQuery'
    instance Functor (QueryRunner c)
      -- Defined in `Opaleye.Internal.RunQuery'
    instance Applicative (QueryRunner c)
      -- Defined in `Opaleye.Internal.RunQuery'
    instance QueryRunnerColumnDefault a b =>
             Default QueryRunner (Column a) b
      -- Defined in `Opaleye.Internal.RunQuery'

We can see that *QueryRunner* is an instance of a *Functor*. We already have an instance of *def* function
that can return a *QueryRunner (Column PGInt4, Column PGText, Column PGText) (Int, String, String)*. And since
*QueryRunner* is a functor, we can convert it to *QueryRunner (Column PGInt4, Column PGText, Column PGText) User*
if we *fmap* a function of type *(Int, String, String) -> User* over it.

That is just what we are doing in the code below.

.. literalinclude:: code/opaleye-select-custom-datatype-row.hs
   :emphasize-lines: 26-35
   :linenos:


Restricting rows
----------------

We did a *select * from users*. Now let us see how we can do a *select * from user where name = 'John'*.

.. literalinclude:: code/opaleye-select-with-condition.hs
   :emphasize-lines: 47-49
   :linenos:

The highlighted lines shows the code that restrict the rows returned by a query. The strange notation
that code uses is called a *proc* notation and it uses something called *arrows*. If you look at the top,
you can see that we have also enabled the *Arrows* extension (Without which this code will not pass syntax check). ::

      user@(_, pgName, _) <- queryTable userTable -< ()

This line can be thought of as reading a row into a variable. As you can see, the type of the row is the
read type of our table. So it is a three element tuple. We want to select rows with "John" in the name column,
so we just store the name column in a variable, and in the line below ::

      restrict -< (pgName .== (pgStrictText "John"))

We are doing the filtering. *restrict* is a function that you can feed restriction parameters. The *.==* is
a function exported by Opaleye, which is supposed to look like the *==* comparison operator. It also export similar
function for the rest of the operators. You can see more of these 
`here <https://hackage.haskell.org/package/opaleye-0.5.2.1/docs/Opaleye-Operators.html>`_
This syntax means that you can mix and match these just like you would do
in an normal sql select query and use parenthesis to group them.


Parametric records for rows
---------------------------

Even though the previous method of reading entire rows into a record works, it
makes certain things a little difficult. For example, while adding a where clause, we had to do this, ::

      user@(_, pgName, _) <- queryTable userTable -< ()

What if the column has got ten fields? You would have to match on a ten element tuple. That is awkward.
If you have generated lenses for accessing the fields of User record, you cannot use it here
because the read row is not a record at this point. This is because the read type of our table 
is a tuple, and our actual target data structure is a record.

What if we can make the read type of the table, a record? A record of a similar "shape" as that of our
target data structure?

That is what we are going to do in this section. Here comes the code.

.. literalinclude:: code/opaleye-select-basic-with-records.hs
   :emphasize-lines: 4, 11, 20-34
   :linenos:

You can see that we have changed the *User* to be parametrized and defined three type
synonyms to represent our actual *User* and the read/write types. ::

  data UserPoly id name email = User { id :: id, name :: name, email :: email } deriving (Show)

  type User = UserPoly UserId String String
  type UserPGW = UserPoly (Column PGInt4) (Column PGText) (Column PGText)
  type UserPGR = UserPoly (Column PGInt4) (Column PGText) (Column PGText)

Just after that, we are calling a Template Haskell function, ::

  $(makeAdaptorAndInstance "pUser" ''UserPoly)

Which expands into the following code ::

    pUser ::
      forall p a1_0 a2_0 a3_0 a1_1 a2_1 a3_1.
      ProductProfunctor p =>
      UserPoly (p a1_0 a1_1) (p a2_0 a2_1) (p a3_0 a3_1)
      -> p (UserPoly a1_0 a2_0 a3_0) (UserPoly a1_1 a2_1 a3_1)
    pUser
      = (((profunctors-5.2:Data.Profunctor.Unsafe.dimap
             toTuple fromTuple)
          . p3)
         . toTuple)
      where
          toTuple (User a1_ a2_ a3_) = (a1_, a2_, a3_)
          fromTuple (a1_, a2_, a3_) = User a1_ a2_ a3_

    instance (ProductProfunctor p,
              Default p a1_0 a1_1,
              Default p a2_0 a2_1,
              Default p a3_0 a3_1) =>
             Default p (UserPoly a1_0 a2_0 a3_0) (UserPoly a1_1 a2_1 a3_1) where
      def = pUser (User def def def)

As you can see, this code contain two items, one is a function *pUser* and other is an instance
declaration.

If you remember our last section where we were reading whole rows to a record, you will recall
we had to use a function *p3* and had to define an instance of *Default*. That
is exactly what this code do. The *pUser* function is the counterpart of *p3*
that can be used with our polymorphic *UserPoly* records. The instance declaration
enables rows to be read into any type that is generated from *UserPoly* type constructor, as long
as the constituent types conforms with the constraints in the instance declaration. 
See the use of *def* function in this line. ::

      def = pUser (User def def def)

Also, last time we used the concrete type *QueryRunner* in place of type variable *p* here. But here
*p* can take any instance of *ProductProfunctor*, which *QueryRunner* happen to be an instance of.
So everything lines up nicely and we can start writing our queries, as we do in the code below.

.. literalinclude:: code/opaleye-select-with-records-and-restrict.hs
   :emphasize-lines: 49-50
   :linenos:

In the emphasized lines, you can see we pattern match on the *User* record. But
this time, since we are getting records, we were able to pattern match on the field name. This
is much nicer than pattern matching on an arbitrarily large tuple, as we did last time.
This can be much more nicer if we have generated lenses for the records.


Excercise 1: Read an enum type into a sum type record field
----------------------------------------------------------

Suppose our *User* record has a field called that represent the type of the user.
In Haskell we can model this as a sum type. ::

  data UserType = SuperAdmin | Admin | Registered

We will use the following sql to create the custom type and the table

.. code-block:: sql
  :linenos:

    create type user_type as enum('superadmin', 'admin', 'registered');

    create table typed_users(
           id serial primary key
           ,name text not null
           ,email text not null
           ,user_type user_type not null
    );

    insert into "typed_users" values (1, 'John', 'john@mail.com', 'superadmin');
    insert into "typed_users" values (2, 'Bob', 'bob@mail.com', 'registered');
    insert into "typed_users" values (3, 'Alice', 'alice@mail.com', 'admin');

You might be tempted to write FromField instances for this type as follows,
taking advantage of the FromField instance of a *String*

.. code-block:: haskell
  :linenos:

    instance FromField UserType where
      fromField field bs = utConversion $ fromField field bs
        where
          utConversion :: Conversion String -> Conversion UserType
          utConversion cString = do
            typeString <- cString
            case mkUserType typeString of
              Nothing -> returnError ConversionFailed field "Unrecognized user type"
              Just ut -> return ut
          mkUserType :: String -> Maybe UserType
          mkUserType "superadmin" = Just SuperAdmin
          mkUserType "admin" = Just Admin
          mkUserType "registered" = Just Registered
          mkUserType _ = Nothing

This will compile just fine, but will error out at runtime with the following
error. ::

  *** Exception: Incompatible {errSQLType = "user_type",
      errSQLTableOid = Just (Oid 33093), errSQLField = "result4_2", errHaskellType = "Text", 
      errMessage = "types incompatible"}

This error is produced by the *fromField* instance of a string.
If you look at the error, you will see two fields *errSQLType* and *errHaskellType*. This means that
you are trying to read from a postgres column type of "user_type" (our custom enum type that we have created in postgresql), 
into a Haskell value of type *Text*. So it turns out that the FromField instance of String will only
read from columns of types such as `Varchar, Char etc. <https://mail.haskell.org/pipermail/database-devel/2012-June/000019.html>`_
