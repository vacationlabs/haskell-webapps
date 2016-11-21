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
  (Maybe Integer) -- ownerId
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