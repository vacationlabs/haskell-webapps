.. _updating_rows:

Updating rows
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


Updating rows
--------------

TODO

- Quick example of selecting a single row by PK, changing a field, and updating it back, using ``runUpdate``
- Explanation of the code and how it corresponds to the type-signature of ``runUpdate``


Getting the updated rows back from the DB
-----------------------------------------

TODO

- Quick example of updating multiple rows in the ``products`` table and getting back the updated rows
- Explanation of the type-signature of ``runUpdateReturning`` API call
- Show the actual SQL queries being executed in the background

Commentary on Opaleye's update APIs
-----------------------------------

TODO: 

- Opaleye forces you to update every single column in the row being updated. Why is this?

Multi-table updates (updates with JOINs)
----------------------------------------

TODO: Does Opaleye even support them? If not, what's the escape hatch?
