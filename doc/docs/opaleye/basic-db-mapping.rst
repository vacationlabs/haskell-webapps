.. Haskell Tutorials documentation master file, created by
   sphinx-quickstart on Thu Nov 24 09:36:10 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _basic_mapping:

Basic DB mappings
=================

Overview
--------

In this chapter we will configure the DB<=>Haskell mapping for the following table:

* ``tenants`` - the master table of "tenants" in a typical multi-tenant SaaS app. You can think of a tenant as a "company account", where no two company accounts share any data.

At the end of the mapping process, we would like to have a schema as close to the following, as possible.

  .. code-block:: sql

    --
    -- Tenants
    --

    create table tenants(
           id serial primary key
           ,created_at timestamp with time zone not null default current_timestamp
           ,updated_at timestamp with time zone not null default current_timestamp
           ,name text not null
           ,first_name text not null
           ,last_name text not null
           ,email text not null
           ,phone text not null
           ,status text not null default 'inactive'
           ,owner_id integer
           ,backoffice_domain text not null
           constraint ensure_not_null_owner_id check (status!='active' or owner_id is not null)
    );
    create unique index idx_index_owner_id on tenants(owner_id);
    create index idx_status on tenants(status);
    create index idx_tenants_created_at on tenants(created_at);
    create index idx_tenants_updated_at on tenants(updated_at);
    create unique index idx_unique_tenants_backoffice_domain on tenants(lower(backoffice_domain));

Further, we will see how Opaleye deals with the following four cases:

* Non-nullable columns without DB-specified defaults
* Non-nullable columns with DB-specified defaults
* Nullable columns without DB-specified defaults
* Nullable columns with DB-specified defaults - TODO: What's a good use-case for such a column?

Creating the DB
--------------

Since Opaleye does not have any support for migrations, setting up the DB schema is done by simply issuing SQL statement directly.

  .. code-block:: sh
  
    $ createdb vacationlabs
    $ psql vacationlabs < includes/db-mappings/schema.sql

Now, to setup the DB<=>Haskell mapping for the ``tenants`` table, we'll walk down the following code: ::

    module DB where

    import Opalaye
    import Data.Text
    import Data.Time (UTCTime)

    data TenantPoly key createdAt updatedAt name status ownerId backofficeDomain = Tenant {
      tenantKey :: key
      ,tenantCreatedAt :: createdAt
      ,tenantUpdatedAt :: updatedAt
      ,tenantName :: name
      ,tenantStatus :: status
      ,tenantOwnerId :: ownerId
      ,tenantBackofficeDomain :: backofficeDomain
      } deriving Show

    type TenantPGWrite = TenantPoly
      (Maybe (Column PGInt8)) -- key
      (Maybe (Column PGTimestamptz)) -- createdAt
      (Column PGTimestamptz) -- updatedAt
      (Column PGText) -- name
      (Column PGText) -- status
      (Column (Nullable PGInt8)) -- ownerId
      (Column PGText) -- backofficeDomain

    type TenantPGRead = TenantPoly
      (Column PGInt8) -- key
      (Column PGTimestamptz) -- createdAt
      (Column PGTimestamptz) -- updatedAt
      (Column PGText) -- name
      (Column PGText) -- status
      (Column (Nullable PGInt8)) -- ownerId
      (Column PGText) -- backofficeDomain

    type Tenant = TenantPoly
      Integer -- key
      UTCTime -- createdAt
      UTCTime -- updatedAt
      Text -- name
      Text -- status
      (Maybe Integer) -- ownerId
      Text -- backofficeDomain

    $(makeAdaptorAndInstance "pTenant" ''TenantPoly)
    $(makeLensesWith abbreviatedFields ''TenantPoly)


    tenantTable :: Table TenantPGWrite TenantPGRead
    tenantTable = Table "tenants" (pTenant Tenant{
                                      tenantKey = optional "id"
                                      ,tenantCreatedAt = optional "created_at"
                                      ,tenantUpdatedAt = required "updated_at"
                                      ,tenantName = required "name"
                                      ,tenantStatus = required "status"
                                      ,tenantOwnerId = required "owner_id"
                                      ,tenantBackofficeDomain = required "backoffice_domain"
                                      })

That's quite a **lot of code** to setup mappings for just one table! Most of it is just boilerplate that can easily be abstracted away using type-families or some TemplateHaskell. In fact there are libraries, such as, SilkOpaleye and dbrecord-opaleye which try to give Opaleye an easier-to-use API.

Strange polymorphic records
---------------------------

Firstly, let's tackle the strangely polymorphic ``TenantPoly``. ::


  data TenantPoly key createdAt updatedAt name status ownerId backofficeDomain = Tenant {
    tenantKey :: key
    ,tenantCreatedAt :: createdAt
    ,tenantUpdatedAt :: updatedAt
    ,tenantName :: name
    ,tenantStatus :: status
    ,tenantOwnerId :: ownerId
    ,tenantBackofficeDomain :: backofficeDomain
    } deriving Show

This is a **base type** which defines the **shape** of a set of related record-types (namely ``TenantPGRead``, ``TenantPGWrite``, and ``Tenant``). ``TenantPoly`` is polymorphic over every single field of the record. This allows us to easily change the type of each field, while ensuring that the *shape* of all these related records is always the same. (*Why* would we want records with similar shapes, but different types, will get clearer in a moment - hang in there!) Generally, ``TenantPoly`` is never used directly in any Opaleye operation. The concrete types - ``TenantPGRead`` ``TenantPGWrite`` and ``Tenant`` - are used instead.

At the the time of writing, Opalaye does **not do any reflection** on the DB schema whatsoever. This is very different from something like Rails (in the Ruby world) and HRR (in the Haskell world), which generate the DB<=>Haskell mappings on the basis of schema reflection). So, Opaleye does not know what data-types to expect for each column when talking to the DB. Therefore, we have to teach it by essentially duplicating the SQL column definitions in Haskell. This is precisely what ``TenantPGRead``,  ``TenantPGWrite``, ``makeAdaptorAndInstance`` and ``tenantTable`` do, and this is what we absolutely hate about Opaleye!


.. note:: We've scratched our own itch here and are working on `Opaleye Helpers <https://github.com/vacationlabs/opaleye-helpers/>`_ to help remove this duplication and boilerplate from Opaleye.


.. code-block:: haskell

  type TenantPGWrite = TenantPoly
    (Maybe (Column PGInt8)) -- key
    (Maybe (Column PGTimestamptz)) -- createdAt
    (Column PGTimestamptz) -- updatedAt
    (Column PGText) -- name
    (Column PGText) -- status
    (Column (Nullable PGInt8)) -- ownerId
    (Column PGText) -- backofficeDomain

  type TenantPGRead = TenantPoly
    (Column PGInt8) -- key
    (Column PGTimestamptz) -- createdAt
    (Column PGTimestamptz) -- updatedAt
    (Column PGText) -- name
    (Column PGText) -- status
    (Column (Nullable PGInt8)) -- ownerId
    (Column PGText) -- backofficeDomain

  $(makeAdaptorAndInstance "pTenant" ''TenantPoly)

  tenantTable :: Table TenantPGWrite TenantPGRead
  tenantTable = Table "tenants" (pTenant Tenant{
                                    tenantKey = optional "id"
                                    ,tenantCreatedAt = optional "created_at"
                                    ,tenantUpdatedAt = optional "updated_at"
                                    ,tenantName = required "name"
                                    ,tenantStatus = required "status"
                                    ,tenantOwnerId = required "owner_id"
                                    ,tenantBackofficeDomain = required "backoffice_domain"
                                    })

Different types for read & write
--------------------------------

With this, we witness another quirk (and power) of Opaleye. It allows us to define different types for the read (SELECT) and write (INSERT/UPDATE) operations. In fact, our guess is that, to achieve type-safety, it is forced to do this. Let us explain. If you're using standard auto-increment integers for the primary key (which most people do), you essentially end-up having two different types for the ``INSERT`` and ``SELECT`` operations. In the ``INSERT`` operation, you *should not* be specifying the ``id`` field/column. Whereas, in the ``SELECT`` operation, you will always be reading it. (Look at Persistent if you want to see another approach of solving this problem.)

One way to avoid having separate types for read & write operations, is to allow the PK field to be ``undefined`` in Haskell, being careful not to evaluate it when dealing with a record that has not yet been saved to the DB. We haven't tried this approach yet, but we're very sure it would require us to teach Opalaye how to map ``undefined`` values to SQL. Nevertheless, depending upon partially defined records for something as common as ``INSERT`` operations does not bode too well for a language that prides itself on type-safety and correctness. 

Therefore, the need for two separate types: ``TenantPGRead`` and ``TenantPGWrite``, with subtle differences. But, before we discuss the differences, we need to understand how Opaleye deals with ``NULL`` values and "omitted columns".

Handling ``NULL`` and database defaults
-------------------------------------

Let's look at the types of a few fields from ``TenantPGWrite`` and how they interact with ``NULL`` values and the ``DEFAULT`` value in the DB:


**The (Column a) types**

* ``updatedAt`` of type ``(Column PGTimestamptz)`` corresponding to ``updated_at timestamp with time zone not null default current_timestamp``
* ``name`` of type ``(Column PGText)`` corresponding to ``name text not null``
* ``status`` of type ``(Column PGText)`` corresponding to ``status text not null default 'inactive'``

In each of these cases you **have to** specify the field's value whenever you are inserting or updating via Opaleye. Moreover, the type ensures that you cannot assign a ``null`` value to any of them at the Haskell-level. **Please note,** ``null`` is NOT the same as ``Nothing``

**The (Maybe (Column a)) types**

* ``key`` of type ``(Maybe (Column PGInt8))`` corresponding to ``id serial primary key``
* ``createdAt`` of type ``(Maybe (Column PGTimestamptz))``  corresponding to ``created_at timestamp with time zone not null default current_timestamp``

In both these cases, during an INSERT, if the value is a ``Nothing``, the entire column itself will be omitted from the INSERT statement and its fate will be left to the DB.

**The (Column (Nullable a)) types**

* ``ownerId`` of type ``(Column (Nullable PGInt8))`` corresponding to ``owner_id integer``

In this case, while you **have to** specify a value at the Haskell level, you can specify a ``null`` as well.

For example, this is a possible INSERT operation:

.. code:: haskell
  
  runInsertMany 
    conn  -- PG Connection
    userTable -- Opaleye table identifer 
    [(
      TenantPGWrite
        {
          tenantKey              = Nothing -- omitted from query; will use DB's DEFAULT
        , tenantCreatedAt        = Just $ pgUTCTime someTime -- NOT omitted from query; will NOT use DB's DEFAULT
        , tenantUpdatedAt        = pgUTCTime someTime
        , tenantName             = pgText "Saurabh"
        , tenantStatus           = pgText "inactive"
        , tenantOwnerId          = null -- specfically store a NULL value
        , tenantBackofficeDomain = pgText "saurabh.vacationlabs.com"
        }
    )]
 

.. note:: Please make sure you understand the difference between ``Maybe (Column a)`` and ``Column (Nullable a)``. And possibly ``Maybe (Column (Nullable a))`` - although we're not sure how useful the last one is!


Different types for read & write - again
----------------------------------------

Now, coming back to the subtle differences in ``TenantPGWrite`` and ``TenantPGRead``:

* While writing, we may **omit** the ``key`` and ``createdAt`` columns (because their type is ``(Maybe (Column x))`` in ``TenantPGWrite`` - as opposed to simply ``Column x``)
* However, we are telling Opaleye, that while reading from the DB, we guarantee that ``key`` and ``createdAt`` will both be ``NOT NULL``. This is because in ``TenantPGRead`` their types are ``(Column x)`` (as opposed to ``Maybe (Column x)``)

.. note:: **Here's a small exercise:** What if ``ownerId`` had the following types. What would it mean?

  * ``TenantPGWrite``: (Maybe (Column (Nullable PGInt8)))
  * ``TenantPGRead``: (Column (Nullable PGInt8))
    
.. note:: **Here's another small exercise:** What if ``ownerId`` had the following types. What would it mean?

  * ``TenantPGWrite``: (Maybe (Column PGInt8))
  * ``TenantPGRead``: (Column (Nullable PGInt8))

    
.. note:: **Here's more to think about:** What if ``ownerId`` had the following types. What would it mean? 

  * ``TenantPGWrite``: (Maybe (Column PGInt8))
  * ``TenantPGRead``: (Maybe (Column PGInt8))

  What does having a ``(Maybe (Column x))`` during ``SELECT`` operations really mean? Does it mean anything in regular ``SELECT`` operations? What about ``LEFT JOIN`` operations?

**Making things even more typesafe:** If you notice, ``TenantPGWrite`` has the ``key`` field as ``(Maybe (Column PGInt8))``, which makes it *omittable*, but it also makes it *definable*. Is there really any use of sending the primary-key's value from Haskell to the DB? In most cases, we think not. So, if we want to make this interface uber typesafe, Opaleye allows us to do the following as well (notice the type of ``key``): ::

  type TenantPGWrite = TenantPoly
    () -- key
    (Maybe (Column PGTimestamptz)) -- createdAt
    (Column PGTimestamptz) -- updatedAt
    (Column PGText) -- name
    (Column PGText) -- status
    (Column (Nullable PGInt8)) -- ownerId
    (Column PGText) -- backofficeDomain

.. seealso:: You'll need to do some special setup for this to work as described in :ref:`readonly_columns`

Wrapping-up
-----------

Coming to the last part of setting up DB<=>Haskell mapping with Opaleye, we need to issue these magic incantations: ::

  $(makeAdaptorAndInstance "pTenant" ''TenantPoly)

  tenantTable :: Table TenantPGWrite TenantPGRead
  tenantTable = Table "tenants" (pTenant Tenant{
                                    tenantKey = optional "id"
                                    ,tenantCreatedAt = optional "created_at"
                                    ,tenantUpdatedAt = optional "updated_at"
                                    ,tenantName = required "name"
                                    ,tenantStatus = required "status"
                                    ,tenantOwnerId = required "owner_id"
                                    ,tenantBackofficeDomain = required "backoffice_domain"
                                    })

The TH splice - ``makeAdaptorAndInstance`` - does two very important things:

* Defines the ``pTenant`` function, which is subsequently used in ``tenantTable``
* Defines the ``Default`` instance for ``TenantPoly`` (this is not ``Data.Default``, but the `poorly named *Data.Profunctor.Product.Default* <https://github.com/tomjaguarpaw/haskell-opaleye/issues/225#issuecomment-258441089>`_

Right now, we don't need to be bothered with the internals of ``pTenant`` and ``Default``, but we *will* need them when we want to do some advanced DB<=>Haskell mapping. Right now, what we need to be bothered about is ``tenantTable``. That is what we've been waiting for! This is what represents the ``tenants`` table in the Haskell land. Every SQL operation on the ``tenants`` table will need to reference ``tenantsTable``. And while defining ``tenantsTable`` we've finally assembled the last piece of the puzzle: field-name <=> column-name mappings AND the name of the table! (did you happen to forget about them?)

.. note:: We're not really clear why we need to specify ``optional`` and ``required`` in the table definition when ``TenantPGWrite`` has already defined which columns are optional and which are required.

And, one last thing. We've been talking about ``PGText``, ``PGTimestamptz``, and ``PGInt8`` till now. These aren't the regular Haskell types that we generally deal with! These are representations of native PG types in Haskell. You would generally not build your app with these types. Instead, you would use something like ``Tenant``, defined below: ::

  type Tenant = TenantPoly
    Integer -- key
    UTCTime -- createdAt
    UTCTime -- updatedAt
    Text -- name
    Text -- status
    (Maybe Integer) -- ownerId
    Text -- backofficeDomain

**Remember these three types and their purpose. We will need them when we're inserting, udpating, and selecting rows.**

* ``TenantPGWrite`` defines the record-type that can be written to the DB in terms of PG types.
* ``TenantPGRead`` defines the record-type that can be read from the DB in terms of PG types.
* ``Tenant`` defines the records that represents rows of the ``tenants`` table, in terms of Haskell types. We haven't yet split this into separate read and write types.

Template Haskell expansion
--------------------------

If you're curious, this is what the TH splice expands to (not literally, but conceptually). You might also want to read the [documentation of Data.Profunctor.Product.TH](https://hackage.haskell.org/package/product-profunctors-0.7.1.0/docs/Data-Profunctor-Product-TH.html) to understand what's going on here. ::

    pTenant :: ProductProfunctor p =>
      TenantPoly 
        (p key0 key1)
        (p createdAt0 createdAt1) 
        (p updatedAt0 updatedAt1)
        (p name0 name1)
        (p status0 status1)
        (p ownerId0 ownerId1)
        (p backofficeDomain0 backofficeDomain1)
      -> p  (TenantPoly key0 createdAt0 updatedAt0 name0 status0 ownerId0 backofficeDomain0) 
            (TenantPoly key1 createdAt1 updatedAt1 name1 status ownerId1 backofficeDomain1)
    pTenant = (((dimap toTuple fromTuple) . Data.Profunctor.Product.p7). toTuple)
      where
          toTuple (Tenant key createdAt updatedAt name status ownerId backofficeDomain)
            = (key, createdAt, updatedAt, name, status, ownerId, backofficeDomain)
          fromTuple (key, createdAt, updatedAt, name, status, ownerId, backofficeDomain)
            = Tenant key createdAt updatedAt name status ownerId backofficeDomain


    instance (ProductProfunctor p,
              Default p key0 key1,
              Default p createdAt0 createdAt1,
              Default p updatedAt0 updatedAt1,
              Default p name0 name1,
              Default p status0 status,
              Default p ownerId0 ownerId1,
              Default p backofficeDomain0 backofficeDomain1) =>
             Default p (TenantPoly key0 createdAt0 updatedAt0 name0 status0 ownerId0 backofficeDomain0) 
                       (TenantPoly key1 createdAt1 updatedAt1 name1 status ownerId1 backofficeDomain1) where
      def = pTenant (Tenant def def def def def def def)
