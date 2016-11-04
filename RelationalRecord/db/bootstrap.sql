/*
    Initial setup to create PostgreSQL tables
    and some data.
*/

create table tenant
 (tenant_id serial primary key,
  tenant_firstname varchar(63) not null,
  tenant_lastname varchar(63) not null,
  tenant_email varchar(255) not null,
  tenant_phone varchar(31) not null,
  tenant_status varchar(31) not null,
  tenant_ownerid integer,
  tenant_backofficedomain varchar(255) not null
);

insert into
    tenant (tenant_firstname, tenant_lastname, tenant_email, tenant_phone, tenant_status, tenant_backofficedomain)
    values ('Hans', 'Klingelheller', 'hans@klingelheller.com', '', '', 'klingelheller.com');

insert into
    tenant (tenant_firstname, tenant_lastname, tenant_email, tenant_phone, tenant_status, tenant_backofficedomain)
    values ('Mary', 'Miller', 'mary@miller.co.uk', '', '', 'miller-domain.net');
