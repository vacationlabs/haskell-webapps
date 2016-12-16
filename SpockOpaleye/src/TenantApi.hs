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
import           Prelude                    hiding (id)
import           RoleApi
import           UserApi

activateTenant :: Tenant -> AppM Tenant
activateTenant tenant = setTenantStatus tenant TenantStatusActive

deactivateTenant :: Tenant -> AppM Tenant
deactivateTenant tenant = setTenantStatus tenant TenantStatusInActive

setTenantStatus :: Tenant -> TenantStatus -> AppM Tenant
setTenantStatus tenant st = updateTenant (tenant & status .~ st)

updateTenant :: Tenant -> AppM Tenant
updateTenant tenant = do
  updateAuditableRow tenantTable tenant

removeTenant :: Tenant -> AppM GHC.Int.Int64
removeTenant tenant = do
  tenant_deac <- deactivateTenant tenant
  _ <- updateTenant (tenant_deac & ownerid .~ Nothing)
  usersForTenant <- readUsersForTenant tid
  rolesForTenant <- readRolesForTenant tid
  --mapM_ removeRole rolesForTenant
  --mapM_ removeUser usersForTenant
  removeRawDbRows tenantTable matchFunc
  where
    tid = tenant ^. id
    matchFunc :: TenantTableR -> Column PGBool
    matchFunc tenant'  = (tenant' ^. id) .== (constant tid)

readTenants :: AppM [Tenant]
readTenants = wrapAuditable $ readRow tenantQuery

readTenantById :: TenantId -> AppM (Maybe Tenant)
readTenantById tenantId = do
  wrapAuditable $ listToMaybe <$> (readRow (tenantQueryById tenantId))

readTenantByBackofficedomain :: Text -> AppM (Maybe Tenant)
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
