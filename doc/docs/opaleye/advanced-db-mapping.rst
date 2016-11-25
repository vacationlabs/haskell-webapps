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
    :emphasize-lines: 5,15,30,39,46

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

TODO:

- Complete code with polymorphic records, etc (complete boilerplate) for ``tenats`` and ``products``

Core mechanism for mapping custom Haskell types to PG types
----------------------

TODO:

- General commentary on how Haskell<=>DB mapping workds
- Details about all the type-classes AND TH functions involved
- Tutorial flow should treat the following sections as example of how the machinery is to be used in practice

Newtypes for primary keys
-------------------------

TODO:

- Code snippet to deal with ``ProductID`` and ``TenantID``
- Example of joining ``tenants`` and ``products`` on ``tenants.ids=products.tenant_id`` thus resulting in correctly compiling code.
- Example of joining ``tenants`` and ``products`` on ``tenants.id=products.id`` thus resulting in code that wont compile.


Mapping ENUMs to Haskell ADTs
-----------------------------

TODO: 

- Code snippet for mapping ``tenant_status`` and its explanation. 
- Fetch a record by primary-key to show that we are indeed getting a Haskell ADT out.
- What happens in failure case, where we have some DB valeus that cannot be parsed to the ADT value?

Handing Postgres Arrays
-----------------------

TODO: 

- Code snippet for handling ``tags text[]`` and its explanation
- Fetch a record by primary-key to show that we are indeed getting an array out. 

Handling JSONB
--------------

TODO:

- Code snippet for handling ``properties`` column
- Some recommendation about using strict vs lazy JSON. Is it possible to force usage of only one kind, while setting up the types for the table?
- Fetch a record by primary-key to show how the JSONB column will look-like in Haskell land.


Making columns read-only
------------------------

TODO:

- Code snippet for making ``id`` and ``createdAt`` readonly and its explanation
- Note about why would you want to make a columnn read-only.
- Quick example of inserting a row with a read-only column.
