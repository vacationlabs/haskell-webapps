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

--
-- Users
--

create type user_status as enum('active', 'inactive', 'blocked');
create table users(
       id serial primary key
       ,created_at timestamp with time zone not null default current_timestamp
       ,updated_at timestamp with time zone not null default current_timestamp
       ,tenant_id integer not null references tenants(id)
       ,username text not null
       ,password text not null
       ,first_name text
       ,last_name text
       ,status user_status not null default 'inactive'
);

create unique index idx_users_username on users(lower(username));
create index idx_users_created_at on users(created_at);
create index idx_users_updated_at on users(updated_at);
create index idx_users_status on users(status);

alter table tenants
      add constraint fk_tenants_owner_id
      foreign key (owner_id)
      references users(id);

--
-- Roles
--
-- TODO: Write a CHECK CONSTRAINT that ensures that permissions[] contains only
-- those permissions that the Haskell ADT can recognize

create table roles(
       id serial primary key
       ,tenant_id integer not null references tenants(id)
       ,name text not null
       ,permissions text[] not null constraint at_least_one_permission check (array_length(permissions, 1)>0)
       ,created_at timestamp with time zone not null default current_timestamp
       ,updated_at timestamp with time zone not null default current_timestamp
);
create unique index idx_roles_name on roles(tenant_id, lower(name));
create index idx_roles_created_at on roles(created_at);
create index idx_roles_updated_at on roles(updated_at);

-- 
-- User<>roles
--
-- Join-through table between users and roles
--

create table users_roles(
       user_id integer not null references users(id)
       ,role_id integer not null references roles(id)
);
create unique index idx_users_roles on users_roles(user_id, role_id);


--
-- Audit log
--

create table audit_logs(
       id serial primary key
       ,tenant_id integer not null references tenants(id)
       ,user_id integer references users(id)
       ,changed_by_system boolean not null default false
       ,auditable_id integer not null
       ,auditable_table_name text not null
       ,summary text not null
       ,changes jsonb not null
       ,created_at timestamp with time zone not null default current_timestamp
       constraint ensure_user_id check ((user_id is not null and not changed_by_system) or (user_id is null and changed_by_system))
);
create index idx_audit_logs_auditable_row on audit_logs(auditable_id, auditable_table_name);
create index idx_audit_logs_tenant_user_id on audit_logs(tenant_id, user_id);
create index idx_audit_logs_created_at on audit_logs(created_at);
-- TODO: index on audit_logs(changes)?

--
-- Products
--
-- TODO: Evolve this schema to have a "price on request" feature. Evolve this
-- say whether the comparison_price is computed automatically or manually set by
-- the user.
--
-- TODO: do we need an is_deleted housekeeping column in every table? Is that
-- really required, given that we have an audit log?

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

       -- NOTE: Adding the cost-price as an optional column to make the JSON
       -- responses dependent upon the persmission of the signed-in user.
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

--
-- Variants
--

create type weight_unit as enum('grams', 'kgs', 'pounds');
create table variants(
       id serial primary key
       ,created_at timestamp with time zone not null default current_timestamp
       ,updated_at timestamp with time zone not null default current_timestamp
       ,tenant_id integer not null references tenants(id)
       ,product_id integer not null references products(id)
       ,name text not null
       ,sku text not null
       ,currency char(3) not null
       ,price numeric not null
       ,quantity integer
       ,weight_in_grams integer
       ,weight_display_unit weight_unit
);

-- TODO: Do we need an index on variants(tenant_id) & varianta(product_id)
create index idx_variants_created_at on variants(created_at);
create index idx_variants_updated_at on variants(updated_at);

create function check_weight_reqd_for_physical_products() returns trigger as $$
       declare
            ptype product_type;
       begin
            select type into ptype from products where id=new.product_id;
            if (ptype='physical') and (weight_in_grams is null or weight_display_unit is null) then
               raise exception 'weight_in_grams and weight_display_unit, both, should be set only for physical products';
            end if;

            return new;
       end;
$$ language plpgsql;

create constraint trigger trig_weight_reqd_for_physical_products
       after insert or update on variants
       deferrable initially deferred
       for each row
       -- when ((new.weight_in_grams is not null) or (new.weight_display_unit is not null))
       execute procedure check_weight_reqd_for_physical_products();

-- TODO: Need a trigger-contraint to ensure that, if the product-type is chaged
-- to 'physical' then weights have been added to variants. This raises the
-- question about what is a better approach in DB design?
--
-- 1. Different triggers for every such condition, or
--
-- 2. One unified 'validation' trigger that will be fired anytime a row in
-- products, variants, images, or any other related table is created, updated,
-- or deleted?
--
-- It seem (2) is more in line with the Haskell philosophy, i.e.
-- idempotent/stateless actions.

create table photos(
       id serial primary key
       ,created_at timestamp with time zone not null default current_timestamp
       -- no updated_at on purpose
       ,tenant_id integer not null references tenants(id)
       ,product_id integer references products(id)
       ,variant_id integer references variants(id)
       ,file_size integer not null
       ,file_type integer not null
       ,file_original_path text not null
       ,processed_styles jsonb
       ,fingerprint text not null
       constraint ensure_photo_reference check (product_id is not null or variant_id is not null)
);
create index idx_photos_created_at on photos(created_at);
create index idx_photos_fingerprint on photos(fingerprint);
create index idx_photos_variant_id on photos(variant_id);
create index idx_photos_product_id on photos(product_id);


-- Function to find difference between two jsonb values
create or replace function json_diff(valo JSONB, valn JSONB)
returns jsonb as $$
declare
    v record;
    result jsonb;
    change jsonb;
begin
    result = '{}'::jsonb;
    for v in select * from jsonb_each(valo) loop
        if valn->v.key != valo->v.key
        then
            change = json_build_object('old', valo->v.key, 'new', valn->v.key);
            result = result || json_build_object(v.key, change)::jsonb;
        end if;
    end looP;
    return result;
end;
$$ LANGUAGE plpgsql;


create or replace function insert_audit_row() returns trigger as $$
    declare
      changes jsonb;
      summary varchar;
      audit_user integer;
      audit_tenant integer;
    begin
      if TG_OP = 'UPDATE' then
        changes = json_diff(row_to_json(old)::jsonb, row_to_json(new)::jsonb);
      elseif TG_OP = 'INSERT' then
        changes = row_to_json(new);
      end if;
      select current_setting('audit.summary') into summary;
      select current_setting('audit.currentuser') into audit_user;
      select current_setting('audit.currenttenant') into audit_tenant;
      if audit_user = '0' then
        audit_user = null;
      end if;
      insert into audit_logs (
          tenant_id,
          user_id, 
          changed_by_system,
          auditable_id,
          auditable_table_name, 
          summary,
          changes
          ) values (
            audit_tenant,
            audit_user,
            true,
            new.id,
            TG_ARGV[0],
            summary,
            changes
          );
       return new;
       end;
$$ language plpgsql;

create trigger trig_insert_tenant_audit
    after insert or update on tenants
    for each row execute procedure insert_audit_row('tenant');

create trigger trig_insert_user_audit
    after insert or update on users
    for each row execute procedure insert_audit_row('users');

create trigger trig_insert_user_roles
    after insert or update on roles
    for each row execute procedure insert_audit_row('roles');
