module Domain.Tenant where

import Import
import Domain.Types

createTenant :: Tenant -> AppM(Tenant)
createTenant tenant = undefined

activateTenant :: Tenant -> Text -> AppM(Tenant)
activateTenant tenant activationKey = undefined

getTenant :: TenantID -> AppM(Tenant)
getTenant tid = undefined

