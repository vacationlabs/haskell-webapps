# Motivation

* Too much choice with respect to libraries when building a typical Postgres-backed webapp. Most people don't know enough about advantages/disadvantages to make the right choice.
  ** Web server + routing + cookies + headers
  ** Templating (primarily HTML templates)
  ** DB access (Postgres only)
  ** DB migrations
  ** Redis for caching expensive things -- eg. expensive DB queries or JSON generation
  ** Authentication
  ** Authorization
  ** Audit logs
  ** Validating incoming user-input
  ** Dealing with static assets (i.e. images, JS, CSS) - during development and during production. Integrating non-Haskell tech into the static asset toolchain (eg. Coffeescript, Typescript, LessCSS, Sass, asset combining and minification)
  ** Automated testing - unit testing, controller/functional testing, integration tests using Selenium (at least)
  ** Anything else?
* I believe most of these things are *possible* in Haskell and its rich library ecosystem. However, making all of this work is *not as easy as it ought to be.* This is because the *idiomatic* and "will-work for-80%-of-the-use-cases-with-20%-effort" way of dealing with these things is not documented properly in one place. This gets more complicated due to the library fragmentation. Libraries make very different decision choices and take **very different** approaches for solving the same problem. Transliterating the idiomatic way from one library to the other may not result in the most pragmatic codebase.
* While there are great tutorials (either provided by the project maintainers themselves or in various blog posts), my experience is that most tutorials walk you through implementing the most basic webapp. I wasn't able to find answers easily for most real-life concerns. 
* Scratching my own itch. I'm running a SaaS company, Vacation Labs, which has a Rails+AngularJS codebase that has grown to 250,000+ LoC over the past 4 years. I'm feeling the downsides of using a dynamically typed language on a very large code-base. While automated testing (unit tests & controller tests) helps, it still doesn't give enough correctness guarantees that something like Haskell can give. I wanted to quickly evaluate Haskell for our use-case, but going through the steep learning curve (functors, monads, laziness, purity, Reader, monad transformers, etc.) has taken a lot of time. I'm not left with enough time to evaluate multiple libraries to pick the best. Therefore, I want to crowd-source the effort, generating a valuable community resource in the process.

# The Plan

* Spec out a typical Postgres-backed web-app which covers all the points mentioned in the "Motivation" section. 
  ** Design the DB schema to include the following commonly occuring webapp requirements:
    *** one:one, one:many, and many:many associations
    *** created-at and updated-at housekeeping columns
    *** unified audit-log table with "before" and "after" columns implemented as HSTOREs
    *** using JSONB and Postgres-arrays in the DB schema
  ** Design a JSON API which covers all domain-level operations, which a hypothetical "dumb" UI may consume. Why a "dumb" UI? Because IMO pushing domain-logic to the UI layer isn't a good idea. Generally, UIs are harder to test, and in today's multi-screen/multi-device world you'll end up implementing multiple UIs for the same app. Thus, forcin you to implement the domain-logic multiple times -- for your single-page-app (SPA) in Javascript, your Android app, your iOS app, your Windows app, and your desktop app.
  ** Design an **optional** Bootstrap-3 based UI (with HTML & LessCSS) for testing out how code shapes up with different templating libraries in Haskell.
* However, it won't make sense to implement the entire app (UI **and** API) for every possible combination of libraries. Therefore, implement the spec in three distinct phases. The assumption is that the phases are loosely coupled and the library choices for one phase do not impact the other phase significantly. (This might be not be true in the cases of frameworks like Yesod, but then again, this is not supposed to be a scientifically controlled experiment).
  ** Phase 1: Domain-level API + DB-access + validations
  ** Phase 2.1: JS-powered SPA (single page app)
  ** Phase 2.2: Server-powered HTML UI
  ** Phase 3: Testing

## Phase 1: Common to both UI approaches (ie. JS-powered SPA and server-powered HTML)

* Phase 1: Domain-level API (in Haskell) + DB-access + validations
  ** Opaleye
  ** Groundhog 
  ** Persistent
  ** Postgresql-ORM

## Phase 2.1: JS-powered SPA

* Step 1: Domain-API mentioned in the section above will be reused
* Step 2: Write JSON-based API to access the domain API
  ** Servant + Aeson
  ** Yesod + Aeson
  ** Snap + Aeson
* Step 3: Write SPA in Haskell-powered technologies
  ** GHCJS + Reflex-FRP
  ** Any other?

## Phase 2.2: Server-powered HTML UI

* Step 1: Domain-API mentioned in the section above will be reused
* Step 2: Convert HTML into templates, and wire up the UI as per the spec:
  ** Shakespearean templates
  ** Lucid
  ** Blaze
  ** Heist

## Phase 3: Testing

* Step 1: Unit tests for domain-level API
  ** Quicktest
  ** Hspec
  ** Anything else?
* Step 2: Integration/browser tests using Selenium
  ** Which library?

# The Spec

TODO