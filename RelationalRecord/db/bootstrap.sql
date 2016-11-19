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
    tenants (name, first_name, last_name, email, phone, backoffice_domain)
    values ('pikachu', 'Pika', 'Chu', 'pika@pokemon.jp', '2290125', 'pkchu.pokemon.com');

insert into
    roles (tenant_id, name, permissions)
    values (1, 'good cop', 'foo,bar,baz');
insert into
    roles (tenant_id, name, permissions)
    values (2, 'bad cop', 'baz');
insert into
    roles (tenant_id, name, permissions)
    values (2, 'neutral', 'null,nil');

insert into
    users (tenant_id, username, password)
    values (1, 'testuser1', 'testpass1');
insert into
    users (tenant_id, username, password)
    values (1, 'testuser2', 'testpass2');
insert into
    users (tenant_id, username, password)
    values (2, 'testuser3', 'testpass3');
insert into
    users (tenant_id, username, password)
    values (3, 'testuser4', 'testpass4');

insert into users_roles values (1, 1);
insert into users_roles values (1, 2);
insert into users_roles values (1, 3);
insert into users_roles values (2, 2);
insert into users_roles values (2, 3);
insert into users_roles values (3, 1);

update tenants set status = 2, owner_id = 2 where id = 2;
