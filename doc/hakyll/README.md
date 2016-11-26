# Content Outline

* Philosophy and guiding principles behind the libraries
  * Opaleye
  * HRR
  * Persistent
* Setting up basic DB mappings
  * Non-nullable columns without DB-specified defaults
  * Non-nullable columns with DB-specified defaults
  * Nullable columns without DB-specified defaults
  * Nullable columns with DB-specified defaults
  * Opalaye
    * Why do we need to specify Haskell AND PG types?
    * Commentary on parameterized record types
    * Ability to represent DB rows as tuples instead of records
  * Persistent
    * Recommended way is Template Haskell
  * HRR
    * Uses TemplateHaskell to fetch the DB definition at compile-time and generate the record types
* Setting up advanced DB mappings
  * ENUMs
  * Postgres arrays
  * JSONB
  * Non-integer primary keys
* DSL support for other SQL stuff
  * Constraints (CHECK & REFERENCES)
  * Triggers
  * Opaleye
    * Doesn't give DSL for DDL (data definition language) commands in SQL
  * Persistent
    * How to run custom SQL in-sync with the automatic migration?
  * HRR
    * Don't know
* Inserting rows
  * Specifying all columns in the row
  * Omitting certain columns and letting DB specify the default
  * INSERT... RETURNING ID
  * INSERT... RETURNING *
  * Insert single row
  * Insert multiple rows
* Updating rows
  * Updating all columns in matching row(s)
  * Updating only specific columns in matching row(s)
  * Updates with JOINs
  * UPDATE... RETURNING *
* Selecting rows
  * Simple select of single row
  * Fetch all columns from rows matching a condition
  * Fetch specific columns from rows matching a condition
  * Ordering
  * Grouping
  * Joins
  * Limit/offset
  * Selecting in batches (or using cursors)
* Database transactions
  * Nested DB transactions
* Managing housekeeping columns (createdAt, updatedAt)
* Support for prepared statements
* Any special/interesting/unique features?