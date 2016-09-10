# What are we building

A typical shopping-cart (ecommerce) webapp. But it isn't the entire app with all bells and whistles. Just a few features - enough to uncover a "Minimum Viable Architecture" (that leverages the Haskell type-system to good use).

# Things to cover in the spec

* Siged-out (not logged-in) operations
* Signed-in operations for an unprivileged user (regular user)
* Signed-in operations for two separate types of privileged users (eg. super-admin and limited-admin) -- will help implement POC for authorization
* Domain-level operations that require DB transactions
* A moderately complex web-form (to complete the user-input validation and user-feedback loop)
* Searching the app's core data based on user-input
* Sending out emails? (should this be included in the scope?)

# Domain-Level API to be implemented in Phase 1

* Tenant creation
* User creation
* Tenant activation along with assigning owner
* Simple email+password based authentication
* Get list of products, with search, sort, and pagination
* Get single product details (fixed/pre-decided JSON response)
* Get single product details with fields specified in the request (variable JSON response, depending upon the request)
  * Only privileged users can get access to the following fields: inventory count, cost price
* Create a new product
  * Requires "product admin" privileges
  * Upload & auto-crop images to various geometries
* Create a new discount code
  * Requires "marketing admin" privileges
* Modify an existing product
  * Tags
  * Variants
  * Images
  * Only 
* Place an order (containing multiple products) as a regular user
  * Requires a DB transaction
  * Stock cannot become negative
  * Validations
    * Stock for any product cannot become negative after the order is placed
    * Basic contact detail validations (email, name, phone number)
    * Basic address validations (city+state+country should match the zipcode)
    * All products should be shippable to the zipcode
* Place an order (containing multiple products) as an administrator
  * Requires a DB transaction
  * Validations
    * All end-user validations to be turned to warnings.
    * Admin should be able to override all validations
