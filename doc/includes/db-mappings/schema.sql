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

--
-- Users
--

create table users(
       id serial primary key
       ,created_at timestamp with time zone not null default current_timestamp
       ,updated_at timestamp with time zone not null default current_timestamp
       ,tenant_id integer not null references tenants(id)
       ,username text not null
       ,password text not null
       ,first_name text
       ,last_name text
       ,status text not null default 'inactive'
);

create unique index idx_users_username on users(lower(username));
create index idx_users_created_at on users(created_at);
create index idx_users_updated_at on users(updated_at);
create index idx_users_status on users(status);

alter table tenants
      add constraint fk_tenants_owner_id
      foreign key (owner_id)
      references users(id);
