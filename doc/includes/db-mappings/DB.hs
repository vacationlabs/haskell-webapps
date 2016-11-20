module DB where

import Opalaye
import Data.Text
import Data.Time (UTCTime)

data TenantPoly key createdAt updatedAt name status ownerId backofficeDomain = Tenant {
  tenantKey :: key
  ,tenantCreatedAt :: createdAt
  ,tenantUpdatedAt :: updatedAt
  ,tenantName :: name
  ,tenantStatus :: status
  ,tenantOwnerId :: ownerId
  ,tenantBackofficeDomain :: backofficeDomain
  } deriving Show

type TenantPGWrite = TenantPoly
  (Maybe (Column PGInt8)) -- key
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

type TenantPGRead = TenantPoly
  (Column PGInt8) -- key
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText) -- name
  (Column PGText) -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

type Tenant = TenantPoly
  Integer -- key
  UTCTime -- createdAt
  UTCTime -- updatedAt
  Text -- name
  Text -- status
  (Maybe UserId) -- ownerId
  Text -- backofficeDomain

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)
$(makeLensesWith abbreviatedFields ''TenantPoly)


tenantTable :: Table TenantPGWrite TenantPGRead
tenantTable = Table "tenants" (pTenant Tenant{
                                  tenantKey = optional "id"
                                  ,tenantCreatedAt = optional "created_at"
                                  ,tenantUpdatedAt = required "updated_at"
                                  ,tenantName = required "name"
                                  ,tenantStatus = required "status"
                                  ,tenantOwnerId = required "owner_id"
                                  ,tenantBackofficeDomain = required "backoffice_domain"
                                  })


--
-- User
--

data UserPoly key createdAt updatedAt tenantId username password firstName lastName status = User {
  userKey :: key
  ,userCreatedAt :: createdAt
  ,userUpdatedAt :: updatedAt
  ,userTenantId :: tenantId
  ,userUsername :: username
  ,userPassword :: password
  ,userFirstName :: firstName
  ,userLastName :: lastName
  ,userStatus :: status
  } deriving Show

type User = UserPoly
  UserId -- key
  UTCTime -- createdAt
  UTCTime -- updatedAt
  TenantId -- tenantId
  Text -- username
  BcryptText -- password
  (Maybe Text) -- firstName
  (Maybe Text) -- lastName
  UserStatus -- status

type UserPGWrite = UserPoly
  (Maybe (Column PGInt8)) -- key
  (Maybe (Column UTCTime)) -- createdAt
  (Maybe (Column UTCTime)) -- updatedAt
  (Column PGInt8) -- tenantId
  (Column PGText) -- username
  (Column PGText) -- password
  (Column (Nullable PGText)) -- firstName
  (Column (Nullable PGText)) -- lastName
  (Column PGText) -- status

type UserPGRead = UserPoly
  (Column PGInt8) -- key
  (Column UTCTime) -- createdAt
  (Column UTCTime) -- updatedAt
  (Column PGInt8) -- tenantId
  (Column PGText) -- usernam
  (Column PGText) -- password
  (Column (Nullable PGText)) -- firstName
  (Column (Nullable PGText)) -- lastName
  (Column PGText) -- status

$(makeAdaptorAndInstance "pUser" ''UserPoly)
$(makeLensesWith abbreviatedFields ''UserPoly)

userTable :: Table UserPGWrite UserPGRead
userTable = Table "users"
  (pUser User{
      userKey = optional "id"
      ,userCreatedAt = optional "created_at"
      ,userUpdatedAt = optional "updated_at"
      ,userTenantId = required "tenant_id"
      ,userUsername = required "username"
      ,userPassword = required "password"
      ,userFirstName = required "first_name"
      ,userLastName = required "last_name"
      ,userStatus = required "status"
      })
