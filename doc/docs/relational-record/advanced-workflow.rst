
Advanced Workflow
==================


DB-side constraints / data validation
------------------------------------

[explain how (sadly) HRR can't capture a DB constraint as a Haskell function for validation; only flat data type derivation]


Joins
-----

[explain examples utilizing various kinds of joins in HRR]


Subqueries
----------

[provide examples of subselects]


Union, Coalesce
---------------

[check if HRR supports these expressions, if so, give examples]


Case ... when
-------------

[check if HRR supports that expression, if so, give an example]


Functions
---------

[Say something about functions like e.g. char_length(), date_part(), ...]


Housekeeping
------------

[how to deal with housekeeping columns, like timestamps]


Fallback to HDBC
----------------

[being based on HDBC, you can have a fallback when doing something
 with the DB that HRRs abstractions don't cover, like e.g. select the
 last inserted PK or do INSERT ... RETURNING *]



Bulk inserting / preparing statements
-------------------------------------

[as before, this stuff has to be done one level below HRRs abstraction; HRR
 does not provide any mechanism for these cases]
