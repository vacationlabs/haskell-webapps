module DomainApi where

import qualified Data.Text as T
import Control.Monad.Identity
import Opaleye (Query)

-- TODO: Figure out the right monad-transformed stack for the domain API. We'll have to do the following:
-- * DB operations
-- * Logging
-- * Redis operations, potentitally
type App = Identity

--
-- Simple email+password based authentication
--
-- TODO: Which of `authenticate1` or `authenticate2` are "better"? Definition of
-- "better" is: striking a balance between ease-of-use and
-- type-safety/correctness.
--

authenticate1 :: String -> String -> App Bool
authenticate1 username password = undefined

-- TODO: Should the constructors for Username & Password data-types be exported?
newtype Username = Username T.Text

-- TODO: Should the type for the incoming password be different from the one
-- stored in the DB?
newtype Password = Password T.Text

data AuthenticationError = InvalidUser | InvalidPassword | AccountDisabled | AccountNotVerified
data AuthenticationResult = Authenticated | AuthenticationError

-- TODO: Is is better to use Either as the result-type?
authenticate2 :: Username -> Password -> App AuthenticationResult
authenticate2 username password = undefined


--
-- Get list of products, with search, sort, and pagination
--
-- TODO: Which of `getProductList1`, `getProductList2`, `getProductList3` is
-- better? `getProductList1` hides the DB-layer from higher-level functions,
-- thus allowing the DB-schema to evolve fairly independently. `getProductList2`
-- allows us to expose the full power of SQL to higher-level functions. Neither
-- allows us to define which associated records (eg. images, product varints)
-- should be loaded along with the products. `getProductList3` abstracts out the
-- filtering logic, while still giving the ability to load associated objects as
-- the scenario demands (at some call-site no associated-records may be
-- required, at another images AND variants may be required, at yet another,
-- only the first image may be required). Is there a fourth approach which is
-- even better?
--
-- `getProductList4` allows us to "hide" certain fields from users based on
-- their privileges. However, I'm not sure how the return type can handle
-- missing fields in a type-safe manner. Also, is this a good idea? On one hand
-- it gives a very small, controlled API surface area to ensure that privileged
-- information never leaks out. On the other hand, it might become a problem
-- later. What if a non-privileged user is making the API call, and a privileged
-- field is required for some interim processing, but the said field will NOT be
-- displayed to the user?

data ProductFilter = ProductFilter{}
data Product = Product{}
getProductList1 :: ProductFilter -> App [Product]
getProductList1 filter = undefined


-- TODO: Figure out if Opaleye operations need to be performaed in the App
-- monad-transformer stack
getProductList2 :: App (Query Product)
getProductList2 = undefined

newtype ProductId = ProductId Int -- TODO: Will change to whatever Opaleye needs.
getProductList3 :: ProductFilter -> App (Query ProductId)
getProductList3 filter = undefined

data User = User{}
getProductList4 :: User -> ProductFilter -> App [Product]
getProductList4 user filter = undefined

--
-- Get single product details (fixed/pre-decided JSON response)
--
-- TODO: Design this API based once the `getProductList` is finalized
--

-- 
-- Get single product details with fields specified in the request (variable JSON response, depending upon the request)
--
-- TODO: Figure out how can the response-type handle a different structure/shape
-- based on arguments
--


--
-- Create a new product
--
-- TODO: Figure out the easiest way to do validations with two approaches:
--
-- 1. Errors are essentially returned as flat list of strings. This will allow
-- the UI to display the errors bunched-up together at the top of the form.
--
-- 2. Errors are a returned as a structured list of strings, such that the UI
-- can, if it wants, identify which error maps to which user-input and displays
-- it next to the field on the UI.
--

-- TODO: Figure out the correct way to parameterize a record with the validation
-- status. Should it be a field within the record or something else?
data ValidationState = NotValidated | Validated
data ValidationResult = Invalid | Valid

-- TODO: Product2 needs to be a record obviously. But I don't know how to make a
-- record parameterized over something. Also, how do we represent an invalid
-- user-input in Haskell? Something which may have missing fields? Does this
-- mean that we need THREE records-types to hold essentially the same data?
-- First for user-input (which needs to be most tolerant, missing fields, etc.).
-- Second for validated input with all the constraints checked. And third for
-- something that only the DB layer might require. Seems too much.
--
-- Or, should the user-validation be done by the 'controller' layer, i.e. the
-- JSON parsing code. So, the JSON parsing results in either a valid
-- domain-record, or it results in the error object,
-- i.e. `Either ValidationErrors Product`
data Product2 = Product2 ValidationState

--
-- Create a new discount code
--

--
-- Modify an existing product
--

--
-- Place an order (containing multiple products) as a regular user
--

--
-- Place an order (containing multiple products) as an administrator
--
