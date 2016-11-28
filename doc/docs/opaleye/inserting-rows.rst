.. _inserting_rows:

Inserting rows
==============

SQL for table creation
----------------------

We'll stick with the same ``tenants`` table as the previous chapter:

  .. code-block:: sql

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

Inserting rows
--------------

TODO

- Quick example of inserting a new row into the ``tenants`` table using ``runInsertMany``
- Explanation of the code and how it corresponds to the type-signature of ``runInsertMany``


Getting the ID of a newly inserted row
--------------------------------------

TODO

- Quick example of inserting a new row into the ``tenants`` table and getting back the ID
- Explanation of the type-signature of ``runInsertManyReturning`` API call
- Showing the actual SQL queries being executed in the background

Three functions missing from the Opaleye API
--------------------------------------------

TODO: Recommended functions for the following two common operations:

- Inserting a row using Haskell types as input (as against the PG type as input)
- Inserting a single row and getting back the newly inserted ID
- Inserting a single row and getting back the newly inserted row


Dealing with errors
-------------------

TODO: 

- What happens when an insert fails at the DB level, eg. a ``CHECK CONSTRAINT`` prevents insertion?
- Take the example of ``idx_unique_tenants_backoffice_domain``


Using a different record-type for INSERTs
-----------------------------------------

TODO

- Example of defining and using a ``NewTenant`` type for row creation
- Commentary on why this could be useful
- Link-off to a later section which discusses these design decisions in detail - "Designing a domain API using Opaleye"
