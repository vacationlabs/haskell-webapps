{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module TenantApi
  ( create_tenant
  , read_tenants
  , read_tenant_by_id
  , read_tenant_by_backofficedomain
  , remove_tenant
  , update_tenant
  , activate_tenant
  , deactivate_tenant
  ) where

import           Control.Arrow
import           Data.Text
import           Data.Time                  (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef
import           RoleApi
import           UserApi

create_tenant :: Connection -> TenantIncoming -> IO Tenant
create_tenant conn tenant = do
  current_time <- getCurrentTime
  create_item_1 conn tenantTable tenant { tenant_createdat = current_time, tenant_updatedat = current_time }

activate_tenant :: Connection -> Tenant -> IO Tenant
activate_tenant conn tenant = set_tenant_status conn tenant TenantStatusActive

deactivate_tenant :: Connection -> Tenant -> IO Tenant
deactivate_tenant conn tenant = set_tenant_status conn tenant TenantStatusInActive

set_tenant_status :: Connection -> Tenant -> TenantStatus -> IO Tenant
set_tenant_status conn tenant status = update_tenant conn (tenant_id tenant) tenant { tenant_status = status }

update_tenant :: Connection -> TenantId -> Tenant -> IO Tenant
update_tenant conn t_tenantid tenant = do
  current_time <- getCurrentTime
  runUpdate conn tenantTable (update_func current_time) match_func
  return tenant
  where
    match_func :: TenantTableR -> Column PGBool
    match_func Tenant { tenant_id = id } = id .== constant t_tenantid
    update_func :: UTCTime -> TenantTableR -> TenantTableW
    update_func current_time x = constant (tenant { tenant_updatedat = current_time })

remove_tenant :: Connection -> Tenant -> IO GHC.Int.Int64
remove_tenant conn tenant@Tenant {tenant_id = tid} = do
  deactivate_tenant conn tenant
  update_tenant conn (tenant_id tenant) tenant { tenant_ownerid = Nothing }
  users_for_tenant <- read_users_for_tenant conn tid
  roles_for_tenant <- read_roles_for_tenant conn tid
  mapM_ (remove_role conn) roles_for_tenant
  mapM_ (remove_user conn) users_for_tenant
  runDelete conn tenantTable match_func
  where
    match_func :: TenantTableR -> Column PGBool
    match_func Tenant { tenant_id = id } = id .== (constant tid)

read_tenants :: Connection -> IO [Tenant]
read_tenants conn = runQuery conn tenant_query

read_tenant_by_id :: Connection -> TenantId -> IO (Maybe Tenant)
read_tenant_by_id conn id = do
  r <- runQuery conn $ (tenant_query_by_id id)
  return $ case r of
    []     -> Nothing
    (x:xs) -> Just x

read_tenant_by_backofficedomain :: Connection -> Text -> IO (Maybe Tenant)
read_tenant_by_backofficedomain conn domain = do
  r <- runQuery conn $ (tenant_query_by_backoffocedomain domain)
  return $ case r of
    []     -> Nothing
    (x:xs) -> Just x

tenant_query :: Opaleye.Query TenantTableR
tenant_query = queryTable tenantTable

tenant_query_by_id :: TenantId -> Opaleye.Query TenantTableR
tenant_query_by_id t_id = proc () -> do
  row@Tenant {tenant_id = id} <- tenant_query -< ()
  restrict -< id .== (constant t_id)
  returnA -< row

tenant_query_by_backoffocedomain :: Text -> Opaleye.Query TenantTableR
tenant_query_by_backoffocedomain domain = proc () -> do
  row@Tenant { tenant_backofficedomain = bo_domain } <- tenant_query -< ()
  restrict -< bo_domain .== (pgStrictText domain)
  returnA -< row
