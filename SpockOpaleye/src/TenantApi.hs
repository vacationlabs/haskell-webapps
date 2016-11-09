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
import           Control.Lens
import           Data.Text
import           Data.Time                  (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef
import           Prelude                    hiding (id)
import           ApiBase
import           RoleApi
import           UserApi

create_tenant :: Connection -> TenantIncoming -> IO Tenant
create_tenant conn tenant = create_item conn tenantTable tenant

activate_tenant :: Connection -> Tenant -> IO Tenant
activate_tenant conn tenant = set_tenant_status conn tenant TenantStatusActive

deactivate_tenant :: Connection -> Tenant -> IO Tenant
deactivate_tenant conn tenant = set_tenant_status conn tenant TenantStatusInActive

set_tenant_status :: Connection -> Tenant -> TenantStatus -> IO Tenant
set_tenant_status conn tenant st = update_tenant conn (tenant ^. id) (tenant & status .~ st)

update_tenant :: Connection -> TenantId -> Tenant -> IO Tenant
update_tenant conn t_tenantid tenant = do
  update_item conn tenantTable t_tenantid tenant

remove_tenant :: Connection -> Tenant -> IO GHC.Int.Int64
remove_tenant conn tenant = do
  deactivate_tenant conn tenant
  update_tenant conn tid (tenant & ownerid .~ Nothing)
  users_for_tenant <- read_users_for_tenant conn tid
  roles_for_tenant <- read_roles_for_tenant conn tid
  mapM_ (remove_role conn) roles_for_tenant
  mapM_ (remove_user conn) users_for_tenant
  runDelete conn tenantTable match_func
  where
    tid = tenant ^. id
    match_func :: TenantTableR -> Column PGBool
    match_func tenant  = (tenant ^. id) .== (constant tid)

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
  tenant <- tenant_query -< ()
  restrict -< (tenant ^. id) .== (constant t_id)
  returnA -< tenant

tenant_query_by_backoffocedomain :: Text -> Opaleye.Query TenantTableR
tenant_query_by_backoffocedomain domain = proc () -> do
  tenant <- tenant_query -< ()
  restrict -< (tenant ^. backofficedomain) .== (pgStrictText domain)
  returnA -< tenant
