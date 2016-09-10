{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Domain.Tenant where

import Domain.Base
import Opaleye
import Data.Text
import Data.Time

--
-- Tenant creation
--

data TenantCreationError = NonUniqueBackofficeDomainError


-- createTenant1 offers absolutely no type-safety. The caller may pass-in key,
-- createdAt, and updatedAt, and the best that we can do, is to ignore them. Not
-- a good way to build a predictable API.
createTenant1 :: TenantPGWrite -> App (Either TenantCreationError Tenant)
createTenant1 newTenant = undefined

-- Trying to fix the problem with createTenant1, we come up with a new type
-- 'NewTenant' where key, createdAt, updatedAt are forced to be units, i.e. ()
-- Also, we've made the the status field to be () as well, to ensure that a new
-- tenant is always created in an inactive state and one has to call
-- 'activateTenant' to ensure that all side-effects take place. Is there any
-- other way to deal with these side-effects? Can we wrap the Tenant in some
-- sort of monad and ensure that whenever the tenant's status is changed (within
-- the monad) the "context" captures these side-effects and makes sure that they
-- run?

type NewTenant = TenantPoly
  () -- key
  () -- createdAt
  () -- updatedAt
  (Column PGText) -- name
  () -- status
  (Column (Nullable PGInt8)) -- ownerId
  (Column PGText) -- backofficeDomain

createTenant2 :: NewTenant -> App (Either TenantCreationError Tenant)
createTenant2 newTenant = undefined

-- Another varition on createTenant2 makes the tenant's status explicit in the
-- type signature. This also allows us to have functions which work with with a
-- Tenant in a particular status, eg.
-- 
-- fooWithTenant :: (TenantWithStatus 'TenantActive) -> App ()
--
-- However, the GADT constructor does not enforce the value of the tenantStatus
-- field. I think it is possible ot create a (TenantWithStatus 'TenantActive)
-- having tenantStatus='inactive'

data TenantWithStatus (status :: TenantStatus) where
  NewTenant :: TenantPoly () () () Text () () Text -> TenantWithStatus 'TenantInactive
  InactiveTenant :: TenantPoly TenantId UTCTime UTCTime Text TenantStatus (Maybe UserId) Text -> TenantWithStatus 'TenantInactive
  ActiveTenant :: TenantPoly TenantId UTCTime UTCTime Text TenantStatus (Maybe UserId) Text -> TenantWithStatus 'TenantActive

createTenant3 :: TenantWithStatus 'TenantNew -> App (Either TenantCreationError (TenantWithStatus 'TenantInactive))
createTenant3 newTenant = undefined

-- Is there any approach possible with typeclasses?
