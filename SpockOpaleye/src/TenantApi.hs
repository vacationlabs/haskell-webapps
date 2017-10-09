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
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef
import           Prelude                    hiding (id)
import           RoleApi
import           UserApi

createTenant :: TenantIncoming -> AppM Tenant
createTenant tenant = do
  createRow tenantTable tenant

activateTenant :: Tenant -> AppM Tenant
activateTenant tenant = setTenantStatus tenant TenantStatusActive

deactivateTenant :: Tenant -> AppM Tenant
deactivateTenant tenant = setTenantStatus tenant TenantStatusInActive

setTenantStatus :: Tenant -> TenantStatus -> AppM Tenant
setTenantStatus tenant st = updateTenant (tenant & status .~ st)

updateTenant :: Tenant -> AppM Tenant
updateTenant tenant = do
  updateRow tenantTable tenant

removeTenant :: Tenant -> AppM GHC.Int.Int64
removeTenant tenant = do
    tenant_deac <- deactivateTenant tenant
    _ <- updateTenant (tenant_deac & ownerid .~ Nothing)
    usersForTenant <- readUsersForTenant tid
    rolesForTenant <- readRolesForTenant tid
    mapM_ removeRole rolesForTenant
    mapM_ removeUser usersForTenant
    removeRow tenantTable tenant
  where
    tid = tenant ^. id

readTenants :: Connection -> IO [Tenant]
readTenants conn = runQuery conn tenantQuery

readTenantById :: Connection -> TenantId -> IO (Maybe Tenant)
readTenantById conn tenantId = do
  listToMaybe <$> (runQuery conn $ (tenantQueryById tenantId))

readTenantByBackofficedomain :: Connection -> Text -> IO (Maybe Tenant)
readTenantByBackofficedomain conn domain = do
  listToMaybe <$> (runQuery conn $ (tenantQueryByBackoffocedomain domain))

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
