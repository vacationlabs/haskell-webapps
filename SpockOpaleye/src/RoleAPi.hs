{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module RoleApi
  ( create_role
  , remove_role
  , read_roles_for_tenant
  ) where

import           Control.Arrow
import           Data.List.NonEmpty
import           Data.Text
import           Data.Time                  (getCurrentTime)
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef

create_role :: Connection -> Role -> IO Role
create_role conn role = do
  current_time <- getCurrentTime
  fmap Prelude.head $ runInsertManyReturning conn roleTable [constant (role {
      role_createdat = current_time,
      role_updatedat = current_time
    })
    ] id

remove_role :: Connection -> Role -> IO GHC.Int.Int64
remove_role conn Role {role_id = t_id} = do
  runDelete conn userRolePivotTable (\(_, role_id) -> role_id .== constant t_id)
  runDelete conn roleTable match_func
    where
    match_func Role {role_id = id} = id .== constant t_id

read_roles_for_tenant :: Connection -> TenantId -> IO [Role]
read_roles_for_tenant conn t_id = do
  runQuery conn $ role_query_for_tenant t_id

role_query :: Query RoleTableR
role_query = queryTable roleTable

role_query_for_tenant :: TenantId -> Query RoleTableR
role_query_for_tenant t_tenantid =
  proc () ->
  do row@ Role {role_tenantid = tenant_id } <- role_query -< ()
     restrict -< tenant_id .== (constant t_tenantid)
     returnA -< row
