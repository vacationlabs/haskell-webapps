
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



(some more paragraphs that would be of interest here?)
