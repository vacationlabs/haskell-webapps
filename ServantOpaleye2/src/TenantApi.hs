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

import           AppCore
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Text
import           GHC.Int
import           Opaleye
import           Prelude              hiding (id)
import           RoleApi
import           UserApi
import           Utils

createTenant :: (DbConnection m) => TenantIncoming -> m Tenant
createTenant tenant = do
  createRow tenantTable tenant

activateTenant :: (DbConnection m, CurrentUser m, CurrentTenant m) => Tenant -> m Tenant
activateTenant tenant = setTenantStatus tenant TenantStatusActive

deactivateTenant :: (DbConnection m, CurrentUser m, CurrentTenant m) => Tenant -> m Tenant
deactivateTenant tenant = setTenantStatus tenant TenantStatusInActive

setTenantStatus :: (DbConnection m, CurrentUser m, CurrentTenant m) => Tenant -> TenantStatus -> m Tenant
setTenantStatus tenant st = updateTenant (tenant & status .~ st)

updateTenant :: (DbConnection m, CurrentUser m, CurrentTenant m) => Tenant -> m Tenant
updateTenant tenant = do
  updateAuditableRow tenantTable tenant

removeTenant :: (DbConnection m, CurrentUser m, CurrentTenant m) => Tenant -> m GHC.Int.Int64
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

readTenants :: (DbConnection m) =>  m [Tenant]
readTenants = readRow tenantQuery

readTenantById :: (DbConnection m) => TenantId -> m Tenant
readTenantById tenantId = do
  (readRow $ tenantQueryById tenantId) >>= returnOneIfNE "Tenant id not found"

readTenantByBackofficedomain :: (DbConnection m) => Text -> m (Maybe Tenant)
readTenantByBackofficedomain domain = do
  listToMaybe <$> (readRow (tenantQueryByBackoffocedomain domain))

tenantQuery :: TenantQuery
tenantQuery = queryTable tenantTable

tenantQueryById :: TenantId -> TenantQuery
tenantQueryById tId = proc () -> do
  tenant <- tenantQuery -< ()
  restrict -< (tenant ^. id) .== (constant tId)
  returnA -< tenant

tenantQueryByBackoffocedomain :: Text -> TenantQuery
tenantQueryByBackoffocedomain domain = proc () -> do
  tenant <- tenantQuery -< ()
  restrict -< (tenant ^. backofficedomain) .== (pgStrictText domain)
  returnA -< tenant
