/*
    Some sample data
*/

insert into
    tenants (name, first_name, last_name, email, phone, backoffice_domain)
    values ('hansk', 'Hans', 'Klingelheller', 'hans@klingelheller.com', '12345', 'klingelheller.com');

insert into
    tenants (name, first_name, last_name, email, phone, backoffice_domain)
    values ('marym', 'Mary', 'Miller', 'marym@gmailll.com', '9012345', 'mary.domain.info');

insert into
    roles (tenant_id, name, permissions)
    values (1, 'good cop', '{"foo", "bar", "baz"})';

insert into
    roles (tenant_id, name, permissions)
    values (2, 'bad cop', '{"baz"}');
