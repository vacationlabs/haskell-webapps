Summary and Conclusion
========================


Why and when you should use HRR in a live system
--------------------------------------------------

[Describe what use scenario HRRs handles best; outline the benefits:
 type safety of queries defined in Haskell and type checking of queries against the DB schema; avoiding boilerplate]


Drawbacks you might be in for
------------------------------

[HRRs limitation to the base cases, as per decision of the authors; sometimes clumsy
 syntax; need for a complete and running DB backend to build Haskell project;
 while HRR entry-level docs are quite good, lack of mid-level documentation
 and reference examples becomes obvious later on]


Integration
-----------

[To live up to the promise of type-safety, a change in the underlying DB schema
 must be reflected when (re)building a Haskell project which uses HRR. Say something
 about how HRR can be integrated with a build system like stack so that data types
 get derived anew in their respective Haskell modules when necessary]
