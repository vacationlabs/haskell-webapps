module DomainApi where

import qualified Data.Text as T

-- Simple email+password based authentication

authenticate1 :: String -> String -> Bool
authenticate1 username password = undefined

newtype Username = Username T.Text

-- TODO: Should the type for the incoming password be different from the one
-- stored in the DB?
newtype Password = Password T.Text

-- TODO: Is is better to use Either as the result-type?
data AuthenticationError = InvalidUsed | InvalidPassword | AccountDisabled | AccountNotVerified
data AuthenticationResult = Authenticated | AuthenticationError 

authenticate2 :: Username -> Password -> AuthenticationResult
authenticate2 username password = undefined


-- Get list of products, with search, sort, and pagination
-- Get single product details (fixed/pre-decided JSON response)
-- Get single product details with fields specified in the request (variable JSON response, depending upon the request)
-- Create a new product
-- Create a new discount code
-- Modify an existing product
-- Place an order (containing multiple products) as a regular user
-- Place an order (containing multiple products) as an administrator
