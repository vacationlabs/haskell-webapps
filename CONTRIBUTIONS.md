# How to contribute

1. Pick a [database library](#database-libraries) that no one is working on.
2. State your goals for the first cut of the domain API. 
3. Pick a limited set of [higher-order design goals](#higher-order-design-goals) that you would like to in the first cut of the domain API.
4. Quickly get a working implementation of **at least** the following APIs: tenant, user, role, permission.
  * Refer to the [Domain module in the skeleton folder](https://github.com/vacationlabs/haskell-webapps/tree/master/skeleton/src/Domain) to get a working idea. The type signatures will need to be modified depending on your DB library and design goals.
  * Refer to the [JSON API spec](https://vacationlabs.github.io/haskell-webapps/json-api-spec.html) to understand how the domain API will finally be used in the web-app
  * Refer to the [motivation behind the spec](https://github.com/vacationlabs/haskell-webapps/blob/master/SPEC.md) to get more context.
5. Raise a PR to get feedback on the direction.
6. If all is good, implement the other parts of the API: product, variants, photos
7. Pick one [higher-order design goals](#higher-order-design-goals) problem, refactor your code, raise a PR, and repeat...
8. Somewhere in this process, you'll be happy with the domain API and can start building the web API on top of it.

The central idea is to NOT deviate from the goals stated at the beginning of a development sprint (unless you hit a brick-wall during implementation!) You'll come across a lot of interesting problems that should be answered, hold on to them. Put them in a backlog. We'll tackle them in a later sprint.

## Formalizing your development sprints

* Open a PR against your branch **before** you start working on the sprint. Tag it with the `sprint` label
* Use the PR description to formally state what you would like to achieve at the end of the sprint. Being detailed helps. Writing down your thoughts will help you structure them.
* Put a timeline to the sprint -- try to keep sprints short enough so that they fit within a week.
* Keep committing to your branch - the PR will keep getting updated automatically.
* Comment on the PR when you feel it's ready to be merged (or you'd like someone to quickly review it).
* Get it merged. 
* Close the PR with a comment that answers the following:
  * What got done in this sprint
  * What got pushed to the next sprint
  * Any interesting learnings/roadblocks/issues/thoughts/comments

## Communication & daily standups

* [Lobby @ Gitter IM](https://gitter.im/haskell-webapps/Lobby?source=orgpage) for general discussions. It's a public room, so even if you're not an active contributor you are welcome to join the discussion.
* [Standup @ Gitter IM](https://gitter.im/haskell-webapps/standup) for daily standups. It's a private room only for [active contributors](#contributors). Every day, we would expect you to answer the following questions at the beginning of *your* work day (in your timezone):
  * Did you work on what you wanted to yesterday? If not, what happened?
  * What will you work on today?
  * What obstacles or issues are impeding your progress?

# Libraries worked on
## Database Libraries

| DB library | Who's working on it |
| --- | --- |
| Persistent | [wz1000](https://github.com/sudhirvkumar) |
| Opaleye | [sras](https://github.com/sras) |
| HDBC | [jfoutz](https://github.com/jfoutz) |
| Haskell Relational Record | no one, yet |
| HASQL | no one, yet |

## Web libraries

A lot of people are excited about building a JSON API in Servant and everyone seems to be picking that. We're actively looking for contributors who can pick other web libraries.

| Web library | Who's working on it |
| --- | --- |
| Servant | [wz1000](https://github.com/sudhirvkumar), [jfoutz](https://github.com/sudhirvkumar) |
| Yesod | no one, yet |
| Snap | no one, yet |
| Happstack | no one, yet |

## UI languages/frameworks

| Library/framework | Who's working on it                     |
| ---               | ---                                     |
| Elm               | No one yet                              |
| Purescript        | No one yet                              |
| Reflex FRP        | [meditans](https://github.com/meditans) |

# Higher order design goals

## General principles
* Make nonsensical states non-representible in the domain model. eg. product can have only two types -- physical and digital. Status can have only few values. Basically a lot of ADT usage.
* Lift more invariants to the type-level.
* Investigate how testing is (or could be) done in the various domains, re-interpreting the type of test commonly used in web development (Unit, Controller, Integration tests) if necessary, to write them in idiomatic ways.
* Create documentation for successful workflows/techniques.

## Database domain

* Best way to deal with housekeeping columns, like `createdAt`, `updatedAt`
* Implementing audit logs
* Ensuring type-safety in the create/edit/update calls for each DB model, such that an accidental write of a "protected" field is not possible. Example of such fields: `createdAt`, `updatedAt`, `id`, `status`, `type`, etc. (fields that need some side-effects or workflow to change)
* How to deal with DB updates? 
  * Should the domain API take the complete record as an argument? Who should be responsible for loading the record from the DB? How many times will we be loading the same record from the DB, if we need to chain/compose different update APIs together?
  * Should the domain API take a *diff* as an argument? How do we represent a diff in a typesafe manner?
* Implementing validations
* Changing response JSON based on incoming request
* DB transactions
* Implementing authorization
* JSONB, ENUM, and Array support in DB library
* Redis caching at object level
* Redis caching at page level

## UI Domain

* Architectural concerns
  - Create a reusable collection of abstractions for most used ui components (forms etc.)
  - Develop a coherent story on how to share data structures with backend.
  - Investigate the tradeoff of doing all the implementation in haskell vs. interface, via haskell, to html templates.
  - Investigate how to integrate with existing jQuery widgets (calendar, accordion, search/sort tables, editable grids, etc)
  - Forms relying only on server side validation or with mixed client/server side validation
  - Investigate the preferred way to architecture an application. How powerful, and how general, is it?
  - Find a simple and elegant way to do client-side routing. Bonus points if it's easily integrable with the server.
  - Analyze, in the case of a language barrier to cross, the tradeoffs involved. In particular if is it possible to automatically reutilize the same structures (like the description of an API) on both frontend and backend.
* Deployment concerns
  - Minification toolchain to reduce the final JS size (closure compiler, specific ghcjs compilation options etc.)
  - Progressive loading of JS files to reduce initial page-load time
  - Server-side rendering of initial page-load
  - Benchmarking how well the generated app fares on mobile

**Please raise a PR against this file to add more higher-order design goals**

# Contributors

## Saurabh Nanda

Been writing code since I was 12 years old. Wrote the same paint program in GwBasic, QBasic, Pascal, C, and C++ (remember BGI libraries?). Fell in love with Lisp at [Cleartrip](https://www.cleartrip.com), but sadly had to pivot to Java + Ruby because of lack of mature DB libraries (at the point in time). Now, building a company - [Vacation Labs](https://www.vacationlabs.com) which uses code to solve real-world problems in the travel space. Looking for something better after having written 250,000+ lines of Rails & AngularJS.

[LinkedIn](https://in.linkedin.com/in/saurabhnanda1) | [Twitter](https://www.twitter.com/saurabhnanda) | [Github](https://www.github.com/saurabhnanda)

## Jason Foutz

TODO

## Zubin Duggal

TODO
