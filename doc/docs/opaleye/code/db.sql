create table users(
       id serial primary key
       ,name text not null
       ,email text not null
);

insert into "users" values (1, 'John', 'john@mail.com');
insert into "users" values (2, 'Bob', 'bob@mail.com');
insert into "users" values (3, 'Alice', 'alice@mail.com');

create table posts(
       id serial primary key
       ,name text not null
       ,user_id integer not null references users(id)
);

insert into "posts" values (1, 'PostJohn', 1);
insert into "posts" values (2, 'PostBob', 2);
insert into "posts" values (3, 'PostJohn2', 1);

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
       ,backoffice_domain text not null
);
create index idx_status on tenants(status);
create index idx_tenants_created_at on tenants(created_at);
create index idx_tenants_updated_at on tenants(updated_at);
create unique index idx_unique_tenants_backoffice_domain on tenants(lower(backoffice_domain));

--insert into "tenants" values (1, default, default, 'Tenant John', 'John', 'Honai', 'john@mail.com', '2255', 'inactive', 'jhonhonai.com');
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

--insert into "products" values (1, default, default, 1, 'Biscuits', 'Biscuits, you know..', 'biscuits', '{bakery, snacks}', 'INR', 40, 55, 34, 'physical', default, '{"weight": "200gm"}'); 
