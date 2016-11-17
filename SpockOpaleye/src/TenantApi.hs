{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module TenantApi
  ( createTenant
  , readTenants
  , readTenantById
  , readTenantByBackofficedomain
  , removeTenant
  , updateTenant
  , activateTenant
  , deactivateTenant
  ) where

import           ApiBase
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Text
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef
import           Prelude                    hiding (id)
import           RoleApi
import           UserApi

createTenant :: TenantIncoming -> AppM (Auditable Tenant)
createTenant tenant = do
  auditable <$> createRow tenantTable tenant

activateTenant :: (Auditable Tenant) -> AppM (Auditable Tenant)
activateTenant tenant = setTenantStatus tenant TenantStatusActive

deactivateTenant :: (Auditable Tenant) -> AppM (Auditable Tenant)
deactivateTenant tenant = setTenantStatus tenant TenantStatusInActive

setTenantStatus :: (Auditable Tenant) -> TenantStatus -> AppM (Auditable Tenant)
setTenantStatus tenant st = updateTenant (tenant & status .~ st)

updateTenant :: (Auditable Tenant) -> AppM (Auditable Tenant)
updateTenant tenant = do
  updateAuditableRow tenantTable tenant

removeTenant :: Auditable Tenant -> AppM GHC.Int.Int64
removeTenant tenant = do
  tenant_deac <- deactivateTenant tenant
  _ <- updateTenant (tenant_deac & ownerid .~ Nothing)
  usersForTenant <- readUsersForTenant tid
  rolesForTenant <- readRolesForTenant tid
  mapM_ removeRole rolesForTenant
  mapM_ removeUser usersForTenant
  removeRawDbRows tenantTable matchFunc
  where
    tid = tenant ^. id
    matchFunc :: TenantTableR -> Column PGBool
    matchFunc tenant'  = (tenant' ^. id) .== (constant tid)

readTenants :: AppM [Auditable Tenant]
readTenants = wrapAuditable $ readRow tenantQuery

readTenantById :: TenantId -> AppM (Maybe (Auditable Tenant))
readTenantById tenantId = do
  wrapAuditable $ listToMaybe <$> (readRow (tenantQueryById tenantId))

readTenantByBackofficedomain :: Text -> AppM (Maybe (Auditable Tenant))
readTenantByBackofficedomain domain = do
  wrapAuditable $ listToMaybe <$> (readRow (tenantQueryByBackoffocedomain domain))

tenantQuery :: Opaleye.Query TenantTableR
tenantQuery = queryTable tenantTable

tenantQueryById :: TenantId -> Opaleye.Query TenantTableR
tenantQueryById tId = proc () -> do
  tenant <- tenantQuery -< ()
  restrict -< (tenant ^. id) .== (constant tId)
  returnA -< tenant

tenantQueryByBackoffocedomain :: Text -> Opaleye.Query TenantTableR
tenantQueryByBackoffocedomain domain = proc () -> do
  tenant <- tenantQuery -< ()
  restrict -< (tenant ^. backofficedomain) .== (pgStrictText domain)
  returnA -< tenant
