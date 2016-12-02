create table users(
       id serial primary key
       ,name text not null
       ,email text not null
);

insert into users values (default, 'John', 'john@mail.com');

create table tenants(
       id serial primary key
       ,name text not null
       ,email text not null
       ,owner_id int not null
);
insert into tenants values (default, 'John', 'john@mail.com', 1);
