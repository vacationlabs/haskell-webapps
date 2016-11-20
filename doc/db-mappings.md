# Setting-up basic DB mappings

## Overview

In this section we will configure the DB<=>Haskell mapping for the following table:

* `tenants` - the master table of "tenants" in a typical multi-tenant SaaS app. You can think of a tenant as a "company account", where no two company accounts share any data.

At the end of the mapping process, we would like to have a schema as close to the following, as possible.

{% include includes/db-mappings/schema.sql %}

Further, we will see how each DB library deals with the following four cases:

* Non-nullable columns without DB-specified defaults
  * eg. `tenants.name`, `tenants.first_name`, `tenants.email`, etc.
* Non-nullable columns with DB-specified defaults
  * eg. `tenants.status`, `users.status`, etc.
* Nullable columns without DB-specified defaults
  * eg. `tenants.owner_id`, `users.first_name`, `users.last_name`, etc.
* Nullable columns with DB-specified defaults
  * TODO: What's a good use-case for such a column?

## Opaleye

### Creating the DB

Since Opaleye does not have any support for migrations, setting up the DB schema is done by simply issuing SQL statement directly.

```
$ createdb vacationlabs
$ psql vacationlabs < includes/db-mappings/schema.sql
```

Now, to setup the DB<=>Haskell mapping for `tenants` and `users` tables.

{% include includes/db-mappings/DB.hs %}

That's quite a **lot of code** to setup mappings for just two tables! Most of it is just boilerplate that can easily be abstracted away using typefamilies or some TemplateHaskell. In fact there are libraries, such as, SilkOpaleye and dbrecord-opaleye which try to give Opaleye an easier-to-use API.

### Strange polymorphic records

Firstly, let's tackle the strangely polymorphic `TenantPoly`.

```
data TenantPoly key createdAt updatedAt name status ownerId backofficeDomain = Tenant {
  tenantKey :: key
  ,tenantCreatedAt :: createdAt
  ,tenantUpdatedAt :: updatedAt
  ,tenantName :: name
  ,tenantStatus :: status
  ,tenantOwnerId :: ownerId
  ,tenantBackofficeDomain :: backofficeDomain
  } deriving Show
```

This is a **base type** which defines the **shape** of a set of related reecord-types (namely `TenantPGRead`, `TenantPGWrite`, and `Tenant`). `TenantPoly` is polymorphic over every single field of the record. This allows us to easily change the type of each field, while ensuring that that the *shape* of all these related records is always the same. (*Why* would we want records with similar shapes, but different types, will get clearer in a moment - hang in there!) Generally, `TenantPoly` is never used directly in any Opaleye operation. The concrete types - `TenantPGRead` `TenantPGWrite` and `Tenant` - that are used instead.

Now, it seems that Opalaye does **not do any reflection** on the DB schema whatsoever. This is a completely different approach compared to Rails (in the Ruby world) and HRR (in the Haskell world) which generate the DB<=>Haskell classes/record-types completely on the basis of schema reflection). So, Opaleye does not know what data-types to expect for each column when talking to the DB. Therefore, we have to teach it by duplicating the column definitions in Haskell. This is precisely what `TenantPGRead`,  `TenantPGWrite`, `makeAdaptorAndInstance` and `tenantTable` do, and what we absolutely hate!

```
type TenantPGWrite = TenantPoly
  (Maybe (Column PGInt8)) -- key
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

type TenantPGRead = TenantPoly
  (Column PGInt8) -- key
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)

tenantTable :: Table TenantPGWrite TenantPGRead
tenantTable = Table "tenants" (pTenant Tenant{
                                  tenantKey = optional "id"
                                  ,tenantCreatedAt = optional "created_at"
                                  ,tenantUpdatedAt = optional "updated_at"
                                  ,tenantName = required "name"
                                  ,tenantStatus = required "status"
                                  ,tenantOwnerId = required "owner_id"
                                  ,tenantBackofficeDomain = required "backoffice_domain"
                                  })
```

### Different types for read & write

With this, we witness another quirk (and power) of Opaleye. It allows us to define different types for the read (SELECT) and write (INSERT/UPDATE) operations. In fact, our guess is that, to achieve type-safety, it is forced to do this. If you're using standard auto-increment integers for the primary key (which most people do), you essentially end-up having two different types for the `INSERT` and `SELECT` operations. In the former, you *should not* be specifying the `id` field/column, and in the latter, you will be reading it. 

One way to achieve this is to define only a single type `TenantPG`, let the `id` (or `key`) field be lazy, and depend on it being `undefined` for new records. We haven't tried this approach yet, but we're very sure it would require us to teach Opalaye how to map `undefined` values in Haskell to SQL. Nevertheless, depending upon partially defined records for something as common as `INSERT` operations does not bode too well for a language that prides itself on type-safety and correctness.

```
type TenantPG = TenantPoly
  (Column PGInt8) -- key
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain
```

Therefore, the need for two separate types: `TenantPGRead` and `TenantPGWrite`, with subtle differences. But, before we discuss the differences, we need to understand how Opaleye deals with `NULL` values and "omitted columns".

### Handling `NULL` and database defaults

Let's look at `TenantPGWrite` again:

| Column | Data type | Meaning|
| --- | --- | --- |
| `key` |  `(Maybe (Column PGInt8))` | A PG column of type PGInt8, which may be omitted from the INSERT/UPDATE, thus leaving its fate to the DB. If the DB has a default-value for this column (which it does, it's an auto-increment primary key), it will be used, else it will be `NULL`. |
| `createdAt` | `(Maybe (Column PGTimestamptz))` | A PG column of type PGTimestamptz (`TIME WITH TIME ZONE`), which may be omitted from the INSERT/UPDATE, thus leaving its fate to the DB. If the DB has a default-value for this column (which it does, it is `current_timestamp`), it will be used, else it will be `NULL`. |
| `updatedAt` | `(Column PGTimestamptz)` | A PG column of type PGTimestamptz, which can NOT be omitted from the INSERT/UPDATE statement AND its value must be `NOT NULL` |
| `name` | `(Column PGText)` | A PG column of type PGText (`TEXT`), which can NOT be omitted from the INSERT/UPDATE statement AND its value must be `NOT NULL` |
| `status` | `(Column PGText)`| A PG column of type PGText (`TEXT`), which can NOT be omitted from the INSERT/UPDATE statement AND its value must be `NOT NULL` |
| `ownerId` | `(Column (Nullable PGInt8))` | A PG column of type PGInt8, which can NOT be omitted from the INSERT/UPDATE statement, HOWEVER its value CAN be `NULL` |
| `backofficeDomain` | `(Column PGText)` | A PG column of type PGText (`TEXT`), which can NOT be omitted from the INSERT/UPDATE statement AND its value must be `NOT NULL` |

Take **special note** of `ownerId`. While the column can hold `NULL` values, it cannot be omitted from the INSER/UPDATE statement. This means, that even if you want to set it to `NULL` you need to do so explicitly. Moreover, actual `NULL` values are represented as [`null`](https://hackage.haskell.org/package/opaleye-0.5.1.0/docs/Opaleye-Column.html#v:null) instead of `Nothing` in Opaleye.

### Different types for read & write - again

Now, coming back to the subtle differences in `TenantPGWrite` and `TenantPGRead`:

* While writing, we may **omit** the `key` and `createdAt` columns (because their type is `(Maybe (Column x))` in `TenantPGWrite`)
* However, we are telling Opaleye, that while reading from the DB, we guarantee that `key` and `createdAt` will be of type `TIME WITH TIME ZONE NOT NULL`

**Here's a small exercise:** What if `ownerId` had the following types. What would it mean?

* `TenantPGWrite`: (Maybe (Column (Nullable PGInt8)))
* `TenantPGRead`: (Column (Nullable PGInt8))

**Here's another small exercise:** What if `ownerId` had the following types. What would it mean?

* `TenantPGWrite`: (Maybe (Column PGInt8))
* `TenantPGRead`: (Column (Nullable PGInt8))

**Making things even more typesafe:** If you notice, `TenantPGWrite` has the `key` field as `(Maybe (Column PGInt8))`, which makes it *omittable*, but it also makes it *definable*. Is there really any use of sending across the primary-key value from Haskell to the DB? In most cases, we think not. So, if we want to make this interface ultra typesafe, Opaleye allows us to do the following as well (notice the type of `key`):

```
type TenantPGWrite = TenantPoly
  () -- key
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain
```

### Wrapping-up

Coming to the last part of setting up DB<=>Haskell mapping with Opaleye, we need to issue these magic incantations:

```
$(makeAdaptorAndInstance "pTenant" ''TenantPoly)

tenantTable :: Table TenantPGWrite TenantPGRead
tenantTable = Table "tenants" (pTenant Tenant{
                                  tenantKey = optional "id"
                                  ,tenantCreatedAt = optional "created_at"
                                  ,tenantUpdatedAt = optional "updated_at"
                                  ,tenantName = required "name"
                                  ,tenantStatus = required "status"
                                  ,tenantOwnerId = required "owner_id"
                                  ,tenantBackofficeDomain = required "backoffice_domain"
                                  })
```

The TH splice - `makeAdaptorAndInstance` - does two very important things:

* Defines the `pTenant` function, which is subsequently used in `tenantTable`
* Defines the `Default` instance for `TenantPoly` (this is not `Data.Default`, but the [poorly named `Data.Profunctor.Product.Default`](https://github.com/tomjaguarpaw/haskell-opaleye/issues/225#issuecomment-258441089))

Right now, we don't need to be bothered with the internals of `pTenant` and `Default`, but we *will* need them when we want to do some advanced DB<=>Haskell mapping. Right now, what we need to be bothered about is `tenantTable`. That is what we've been waiting for! This is what represents the `tenants` table in the Haskell land. Every SQL operation on the `tenants` table will need to reference `tenantsTable`. And while defining `tenantsTable` we've finally assembled the last piece of the puzzle: field-name <=> column-name mappings AND the name of the table! (did you happen to forget about them?)

**Note:** We're not really clear why we need to specify `optional` and `required` in the table definition when `TenantPGWrite` has already defined which columns are optional and which are required.

If you're curious, this is what the TH splice expands to (not literally, but conceptually):

```
    pTenant :: ProductProfunctor p =>
      TenantPoly 
        (p key0 key1)
        (p createdAt0 createdAt1) 
        (p updatedAt0 updatedAt1)
        (p name0 name1)
        (p status0 status1)
        (p ownerId0 ownerId1)
        (p backofficeDomain0 backofficeDomain1)
      -> p  (TenantPoly key0 createdAt0 updatedAt0 name0 status0 ownerId0 backofficeDomain0) 
            (TenantPoly key1 createdAt1 updatedAt1 name1 status ownerId1 backofficeDomain1)
    pTenant = (((dimap toTuple fromTuple) . Data.Profunctor.Product.p7). toTuple)
      where
          toTuple (Tenant key createdAt updatedAt name status ownerId backofficeDomain)
            = (key, createdAt, updatedAt, name, status, ownerId, backofficeDomain)
          fromTuple (key, createdAt, updatedAt, name, status, ownerId, backofficeDomain)
            = Tenant key createdAt updatedAt name status ownerId backofficeDomain


    instance (ProductProfunctor p,
              Default p key0 key1,
              Default p createdAt0 createdAt1,
              Default p updatedAt0 updatedAt1,
              Default p name0 name1,
              Default p status0 status,
              Default p ownerId0 ownerId1,
              Default p backofficeDomain0 backofficeDomain1) =>
             Default p (TenantPoly key0 createdAt0 updatedAt0 name0 status0 ownerId0 backofficeDomain0) 
                       (TenantPoly key1 createdAt1 updatedAt1 name1 status ownerId1 backofficeDomain1) where
      def = pTenant (Tenant def def def def def def def)
```

## HRR

## Persistent
