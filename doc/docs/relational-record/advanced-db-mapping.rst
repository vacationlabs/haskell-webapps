Advanced DB mapping
====================

[HRR is DB agnostic, meaning that user defined types or extensions are not supported, by choice of HRRs authors.
 This chapter should give an overview how to approach that topic]

[This chapter includes a view on how HRR interoperates with postgres additions to the SQL standard]


date/time types
---------------

[how are date/time values mapped by HRR? is there any special way necessary to deal with them?]


JSON(B) type
------------

[Show how a patched HRR library can derive JSON(B) as ByteString but provides
 no abstraction for it in its query syntax (like cf. https://www.postgresql.org/docs/9.5/static/functions-json.html)]


Enums
-----

[Show how with some TH magic you can use HRRs data type derivation to actually
 generate Haskell sum types and HRR projections from postgres enums, so that
 this feature becomes usable quite well]


Arrays
------

[There are yet some experiments to do here on how to best deal with
 postgres arrays, e.g. parse the literals into Haskell lists with
 custom FromSql / ToSql instances - in short, proof of concept still missing]


 Type Alias
 ----------

 [Is it possible to use a type alias on the DB and generate a mapping to a Haskell 
  newtype, for additional type safety?]
