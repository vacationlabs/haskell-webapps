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
import           Data.Text
import           Data.Time                  (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef
import           Prelude                    hiding (id)
import           RoleApi
import           UserApi

createTenant :: Connection -> TenantIncoming -> IO Tenant
createTenant conn tenant = createRow conn tenantTable tenant

activateTenant :: Connection -> Tenant -> IO Tenant
activateTenant conn tenant = setTenantStatus conn tenant TenantStatusActive

deactivateTenant :: Connection -> Tenant -> IO Tenant
deactivateTenant conn tenant = setTenantStatus conn tenant TenantStatusInActive

setTenantStatus :: Connection -> Tenant -> TenantStatus -> IO Tenant
setTenantStatus conn tenant st = updateTenant conn (tenant & status .~ st)

updateTenant :: Connection -> Tenant -> IO Tenant
updateTenant conn tenant = do
  updateRow conn tenantTable tenant

removeTenant :: Connection -> Tenant -> IO GHC.Int.Int64
removeTenant conn tenant = do
  deactivateTenant conn tenant
  updateTenant conn (tenant & ownerid .~ Nothing)
  usersForTenant <- readUsersForTenant conn tid
  rolesForTenant <- readRolesForTenant conn tid
  mapM_ (removeRole conn) rolesForTenant
  mapM_ (removeUser conn) usersForTenant
  runDelete conn tenantTable matchFunc
  where
    tid = tenant ^. id
    matchFunc :: TenantTableR -> Column PGBool
    matchFunc tenant  = (tenant ^. id) .== (constant tid)

readTenants :: Connection -> IO [Tenant]
readTenants conn = runQuery conn tenantQuery

readTenantById :: Connection -> TenantId -> IO (Maybe Tenant)
readTenantById conn id = do
  r <- runQuery conn $ (tenantQueryById id)
  return $ case r of
    []     -> Nothing
    (x:xs) -> Just x

readTenantByBackofficedomain :: Connection -> Text -> IO (Maybe Tenant)
readTenantByBackofficedomain conn domain = do
  r <- runQuery conn $ (tenantQueryByBackoffocedomain domain)
  return $ case r of
    []     -> Nothing
    (x:xs) -> Just x

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
