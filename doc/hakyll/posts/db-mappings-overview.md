---
title: Setting up basic Haskell<=>DB mappings
date: 2016-11-23
---

In this section we will configure the DB<=>Haskell mapping for the following table:

* `tenants` - the master table of "tenants" in a typical multi-tenant SaaS app. You can think of a tenant as a "company account", where no two company accounts share any data.

At the end of the mapping process, we would like to have a schema as close to the following, as possible.

```sql
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
```

Further, we will see how each DB library deals with the following four cases:

* Non-nullable columns without DB-specified defaults
* Non-nullable columns with DB-specified defaults
* Nullable columns without DB-specified defaults
* Nullable columns with DB-specified defaults - TODO: What's a good use-case for such a column?