Advanced DB mapping
====================

[HRR is DB agnostic, meaning that user defined types or extensions are not supported, by choice of HRRs authors.
 This chapter should give an overview how to approach that topic]


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



(anything else belonging in the advances section I might have missed?)
