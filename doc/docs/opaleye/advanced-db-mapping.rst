.. _advanced_mapping:

Advanced DB Mappings
====================

Overview
--------

In this chapter we'll build upon what we did in the last chapter:

- We'll modify the ``tenants`` table, to be a little more typesafe by changing the type of the ``status`` column to a Postgres ``ENUM`` (rather than a ``text``) and mapping it to a Haskell ADT.
- We'll add a new table called ``products`` that will be used to store information of various products in our hypothetical ecommerce store
- We'll change the ``id`` and ``createdAt`` columns to be read-only, for greater type-safety while inserting records.
- We'll change the primary keys, ``tenants.id`` and ``products.id`` to ``TenantId`` and ``ProductId`` respecively. Again, for greater type-safety.


SQL for table creation
----------------------

  .. code-block:: sql
    :emphasize-lines: 5,15,30,39,46,44

    --
    -- Tenants
    --

    create type tenant_status as enum('active', 'inactive', 'new');
    create table tenants(
           id serial primary key
           ,created_at timestamp with time zone not null default current_timestamp
           ,updated_at timestamp with time zone not null default current_timestamp
           ,name text not null
           ,first_name text not null
           ,last_name text not null
           ,email text not null
           ,phone text not null
           ,status tenant_status not null default 'inactive'
           ,owner_id integer
           ,backoffice_domain text not null
           constraint ensure_not_null_owner_id check (status!='active' or owner_id is not null)
    );
    create unique index idx_index_owner_id on tenants(owner_id);
    create index idx_status on tenants(status);
    create index idx_tenants_created_at on tenants(created_at);
    create index idx_tenants_updated_at on tenants(updated_at);
    create unique index idx_unique_tenants_backoffice_domain on tenants(lower(backoffice_domain));

    ---
    --- Products
    ---

    create type product_type as enum('physical', 'digital');
    create table products(
           id serial primary key
           ,created_at timestamp with time zone not null default current_timestamp
           ,updated_at timestamp with time zone not null default current_timestamp
           ,tenant_id integer not null references tenants(id)
           ,name text not null
           ,description text
           ,url_slug text not null
           ,tags text[] not null default '{}'
           ,currency char(3) not null
           ,advertised_price numeric not null
           ,comparison_price numeric not null
           ,cost_price numeric
           ,type product_type not null
           ,is_published boolean not null default false
           ,properties jsonb
    );
    create unique index idx_products_name on products(tenant_id, lower(name));
    create unique index idx_products_url_sluf on products(tenant_id, lower(url_slug));
    create index idx_products_created_at on products(created_at);
    create index idx_products_updated_at on products(updated_at);
    create index idx_products_comparison_price on products(comparison_price);
    create index idx_products_tags on products using gin(tags);
    create index idx_product_type on products(type);
    create index idx_product_is_published on products(is_published);


Code that we'll run through
---------------------------

.. literalinclude:: code/opaleye-tenants-and-products.hs
  :linenos:

.. warning:: In the code given above, we are using ``PGFloat8`` to represent monetary values. This is a **bad idea** and absolutely **not recommended.** We are forced to do this because Opaleye's support for Postgres ``NUMERIC`` datatype `is not really complete. <https://github.com/tomjaguarpaw/haskell-opaleye/issues/230>`_


Core mechanism for mapping custom Haskell types to PG types
-----------------------------------------------------------

There are three typeclasses at play in converting values between Haskell types (like Int, Text and other user defined types)
and PG types (like PGInt4, PGText etc). These are:

* ``FromField``
* ``QueryRunnerColumnDefault``
* ``Default`` (*not* ``Data.Default``)

FromField
*********

This is a typeclass defined by the postgresql-simple library. This typeclass decides how values read from database are
converted to their Haskell counterparts. It is defined as ::

  .. code-block:: haskell

  class FromField a where
    fromField :: FieldParser a

  type FieldParser a = Field -> Maybe ByteString -> Conversion a

The basic idea of this typeclass is simple. It wants you to define a function ``fromField`` which will be passed the following: 

* ``Field`` -  a record holding a lot of metadata about the underlying Postgres column 
* ``Maybe ByteString`` - the raw value of that column

You are expected to return a ``Conversion a`` which is conceptually an *action*, which when evaluated will do the conversion from ``Maybe ByteString`` to your desired type ``a``.

Diligent readers will immediately have the following questions:

**What kind of metadata does ``Field`` have?**

.. code-block:: haskell
  
  name :: Field -> Maybe ByteString
  tableOid :: Field -> Maybe Oid
  tableColumn :: Field -> Int
  format :: Field -> Format
  typeOid :: Field -> Oid
  -- and more




The type *Conversion* is a functor, so you can define instances for custom types in terms of existing *FromField* instances.
For example, if you have a type that wraps an Int, like

  data ProductId = ProductId Int

You can make a field parser instance for *ProductId* as follows ::

  instance FromField ProductId where
    fromField field mb_bytestring = ProductId <$> fromField field mb_bytestring

While doing the above method, you have to make sure that the *FromField* instance that you are depending on
can actually accept data from the underlying database column. This is relavant if you want to do this for
enum types. 

If you depend on the *FromField* instance of a String to read the data coming from an Enum field, it will error out
because the *FromField* instance of String checks if the data is coming from a Varchar or Char field (using the first argument
to the *fromField* function), and errors out if it is not.

Since the second argument to the fromField functon is a *Maybe Bytestring*,
for a data type *TenantStatus* defined as  ::

  data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew

we could do the following ::

  instance FromField TenantStatus where
    fromField field mb_bytestring = makeTenantStatus mb_bytestring
      where
      makeTenantStatus :: Maybe ByteString -> Conversion TenantStatus
      makeTenantStatus (Just "active") = return TenantStatusActive
      makeTenantStatus (Just "inactive") = return TenantStatusInActive
      makeTenantStatus (Just "new") = return TenantStatusNew
      makeTenantStatus (Just _) = returnError ConversionFailed field "Unrecognized tenant status"
      makeTenantStatus Nothing = returnError UnexpectedNull field "Empty tenant status"

With OverloadedStrings extension enabled, we could pattern match on Bystrings using normal String literals, and return the proper value.
You can also see how we are handling unexpected values or a null coming from the column.

2. QueryRunnerColumnDefault
--------------------------
  
This typeclass is used by Opaleye to do the conversion from postgres types defined by Opaleye, into Haskell types. It is defined as ::

  class QueryRunnerColumnDefault pgType haskellType where
    queryRunnerColumnDefault :: QueryRunnerColumn pgType haskellType

Opaleye provides with a function  ::

  fieldQueryRunnerColumn:: FromField haskell => QueryRunnerColumn pgType haskell

As the type signature shows, *fieldQueryRunnerColumn* can return a value of type *QueryRunnerColumn a b* as long as *b* is an instance
of *FromField* typeclass. So once we define an instance of FromField for our type, all we have to do is the following.

For the data type *TenantStatus* that we saw earlier, ::

  instance QueryRunnerColumnDefault PGText TenantStatus where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

3. Default
----------

This is a typeclass that Opaleye uses to convert Haskell values to postgresql values
while writing to the database. It is defined as  ::

  class Default (p :: * -> * -> *) a b where
    def :: p a b

You see a type variable p, that this definition required. Opaleye
provided with a type *Constant* that can be used here. It is defined as ::

  newtype Constant haskells columns
    = Constant {constantExplicit :: haskells -> columns}

So if we are
defining a Default instance for the *TenantStatus* we saw earlier, it 
would be something like this. ::

  instance Default Constant TenantStatus (Column PGText) where
    def = Constant def'
      where
        def' :: TenantStatus -> (Column PGText)
        def' TenantStatusActive = pgStrictText "active"
        def' TenantStatusInActive = pgStrictText "inactive"
        def' TenantStatusNew = pgStrictText "new"

Newtypes for primary keys
-------------------------

Ideally, we would like to represent our primary keys using newtypes that wrap around an Int.
We do it so that we can have some additional type safety while building queries. For example,
if we try to compare two different types, both that wraps an Int, it would be a compiler error.

But it seems that Opaleye's support for this feature is not really ready. So we will skip it for now.

Mapping ENUMs to Haskell ADTs
-----------------------------

For Reading ::

  instance FromField TenantStatus where
    fromField field mb_bytestring = makeTenantStatus mb_bytestring
      where
      makeTenantStatus :: Maybe ByteString -> Conversion TenantStatus
      makeTenantStatus (Just "active") = return TenantStatusActive
      makeTenantStatus (Just "inactive") = return TenantStatusInActive
      makeTenantStatus (Just "new") = return TenantStatusNew
      makeTenantStatus (Just _) = returnError ConversionFailed field "Unrecognized tenant status"
      makeTenantStatus Nothing = returnError UnexpectedNull field "Empty tenant status"

  instance QueryRunnerColumnDefault PGText TenantStatus where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

As we saw in the Typeclasses section, Opaleye requires the QueryRunnerColumnDefault
typeclass instances for converting from data read from Database to Haskell values. the function
*fieldQueryRunnerColumn* can return the value of the required type as long as there is a FromField
instance for the required type.

For Writing ::

  instance Default Constant TenantStatus (Column PGText) where
    def = Constant def'
      where
        def' :: TenantStatus -> (Column PGText)
        def' TenantStatusActive = pgStrictText "active"
        def' TenantStatusInActive = pgStrictText "inactive"
        def' TenantStatusNew = pgStrictText "new"


Handing Postgres Arrays
-----------------------

Postgresql Array column are represented by the PGArray type. It can take
an additional type to represent the kind of the array. So if the column
is text[], the type needs to be *PGArray PGText*.

If you look at the earlier code, you can see that the output contains a
list for the *tag* fields.


Handling JSONB
--------------

The type that represents *jsonb* postgresql columns in Opaleye is PGJsonb.
You can
It will support any type that has a ToJSON/FromJSON instances defined for it.

ToJSON/FromJSON typeclasses are exported by the Aeson json library.

This is how it is done. Let us change the *properties* field of the *Product* type
we saw earlier into a record in see how we can store it in a jsonb field. 

.. literalinclude:: code/opaleye-products-with-json-properties.hs
  :linenos:
  :emphasize-lines: 267-288

In the emphasized lines in code above, we are defining instances to support json conversion.
The binary operators *.:* and *.=* that you see are
stuff exported by the Aeson json library.
The basis of Json decoding/encoding is the aeson's Value type. This type can represent
any json value. It is defined as  ::

  data Value
    = Object !Object
    | Array !Array
    | String !Text
    | Number !Scientific
    | Bool !Bool
    | Null

The Object type is an alias for a HashMap, and Array for a Vector and so on.

The instances are our usual type conversion instances. The *Value* type has the instances built in, so
we will use them for defining instances for ProductProperties.
So when we define a *FromField* instance for ProductProperties, we use the fromField instance of the *Value*
type. We are also handling errors that might occur while parsing and reporting via postgresql's error reporting functions.

In the last instance, we are using the Default instance of the aforementioned *Value* type to implement instance
for *ProductProperties*. The toJSON converts our ProductProperties to *Value* type, and since there are already
built in Default instance for *Value* type, we were able to call the *constant* function on it, to return the
appropriate opaleye's column type.

.. _readonly_columns:


Making columns read-only
------------------------

Sometimes we will want to make a certain column read only, accepting only values generated from the database.
Here is how we can do it.

We have to define a new function *readOnly*, which will make the required field of type (), in the write types
so we won't be able to provide a value for writing.

.. literalinclude:: code/opaleye-readonly.hs
  :linenos:
  :emphasize-lines: 31-32, 56, 317, 142
