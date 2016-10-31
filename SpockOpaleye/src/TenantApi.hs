{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module TenantApi
  ( create_tenant
  , read_tenants
  , read_tenant_by_id
  , make_tenant
  , remove_tenant
  , update_tenant
  , activate_tenant
  , deactivate_tenant
  ) where

import Control.Arrow
import Data.Text
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import GHC.Int
import Opaleye
import OpaleyeDef
import RoleApi
import UserApi

create_tenant :: Connection -> Tenant -> IO [Int]
create_tenant conn Tenant {tenant_id = id
                          ,tenant_name = name
                          ,tenant_firstname = first_name
                          ,tenant_lastname = last_name
                          ,tenant_email = email
                          ,tenant_phone = phone
                          ,tenant_status = status
                          ,tenant_ownerid = owner_id
                          ,tenant_backofficedomain = bo_domain} =
  runInsertManyReturning
    conn
    tenantTable
    (return
       ( Nothing
       , pgStrictText name
       , pgStrictText first_name
       , pgStrictText last_name
       , pgStrictText email
       , pgStrictText phone
       , constant status
       , toNullable . constant <$> owner_id
       , pgStrictText bo_domain))
    (\(id, _, _, _, _, _, _, _, _) -> id)

activate_tenant :: Connection -> TenantId -> IO GHC.Int.Int64
activate_tenant conn tenant_id = set_tenant_status conn tenant_id TenantStatusActive

deactivate_tenant :: Connection -> TenantId -> IO GHC.Int.Int64
deactivate_tenant conn tenant_id = set_tenant_status conn tenant_id TenantStatusInActive

set_tenant_status :: Connection -> TenantId -> TenantStatus -> IO GHC.Int.Int64
set_tenant_status conn tenant_id status = update_tenant conn tenant_id update_func
  where
    update_func :: TenantTableR -> TenantTableW
    update_func (id, name, fn, ln, em, ph, st, oid, bod) =
      (Just id, name, fn, ln, em, ph, constant status, Just oid, bod)

update_tenant :: Connection
              -> TenantId
              -> (TenantTableR -> TenantTableW)
              -> IO GHC.Int.Int64
update_tenant conn t_tenantid update_func =
  runUpdate
    conn
    tenantTable
    update_func
    (\(id, _, _, _, _, _, _, _, _) -> id .== (constant t_tenantid))

remove_tenant :: Connection -> Tenant -> IO GHC.Int.Int64
remove_tenant conn Tenant {tenant_id = tid} = do
  users_for_tenant <- read_users_for_tenant conn tid
  roles_for_tenant <- read_roles_for_tenant conn tid
  mapM_ (remove_role conn) roles_for_tenant
  mapM_ (remove_user conn) users_for_tenant
  runDelete
    conn
    tenantTable
    (\(id, _, _, _, _, _, _, _, _) -> id .== (constant tid))

read_tenants :: Connection -> IO (Maybe [Tenant])
read_tenants conn = do
  r <- runQuery conn $ tenant_query
  return $
    case r of
      [] -> Nothing
      rows -> Just $ fmap make_tenant rows

read_tenant_by_id :: Connection -> TenantId -> IO (Maybe Tenant)
read_tenant_by_id conn id = do
  r <- runQuery conn $ (tenant_query_by_id id)
  return $
    case r of
      [] -> Nothing
      rows -> Just $ Prelude.head $ fmap make_tenant rows

make_tenant :: (TenantId, Text, Text, Text, Text, Text, TenantStatus, Maybe UserId, Text)
            -> Tenant
make_tenant (id, name, first_name, last_name, email, phone, status, owner_id, bo_domain) =
  Tenant
  { tenant_id = id
  , tenant_name = name
  , tenant_firstname = first_name
  , tenant_lastname = last_name
  , tenant_email = email
  , tenant_phone = phone
  , tenant_status = status
  , tenant_ownerid = owner_id
  , tenant_backofficedomain = bo_domain
  }

tenant_query :: Opaleye.Query TenantTableR
tenant_query = queryTable tenantTable

tenant_query_by_id :: TenantId -> Opaleye.Query TenantTableR
tenant_query_by_id t_id =
  proc () ->
  do row@(id, _, _, _, _, _, _, _, _) <- tenant_query -< ()
     restrict -< id .== (constant t_id)
     returnA -< row
