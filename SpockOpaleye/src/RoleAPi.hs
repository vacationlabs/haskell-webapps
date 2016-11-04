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
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef

create_role :: Connection -> Role -> IO (Maybe Role)
create_role conn role@Role { role_tenantid = tenant_id , role_name = name , role_permission = rp } = do
  ids <-
    runInsertManyReturning
      conn roleTable (return Role {
          role_id = Nothing,
          role_tenantid = constant tenant_id,
          role_name = pgStrictText name,
          role_permission = constant rp
      }) id
  return $ case ids of
    []     -> Nothing
    (x:xs) -> Just x

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
