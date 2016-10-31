{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module RoleApi
  ( create_role
  , remove_role
  , read_roles_for_tenant
  ) where

import Control.Arrow
import Data.List.NonEmpty
import Data.Text
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import GHC.Int
import Opaleye
import OpaleyeDef

create_role :: Connection -> Role -> IO [Int]
create_role conn Role {role_tenantid = tenant_id
                      ,role_name = name
                      ,role_permission = rp} =
  runInsertManyReturning
    conn
    roleTable
    (return (Nothing, constant tenant_id, pgStrictText name, constant rp))
    (\(id, _, _, _) -> id)

remove_role :: Connection -> Role -> IO GHC.Int.Int64
remove_role conn Role {role_id = t_id} =
  runDelete conn roleTable (\(id, _, _, _) -> id .== constant t_id)

read_roles_for_tenant :: Connection -> Int -> IO [Role]
read_roles_for_tenant conn t_id = do
  rows <- runQuery conn $ role_query_for_tenant t_id
  return $ makeRole <$> rows

makeRole :: (Int, Int, Text, [Permission]) -> Role
makeRole (id, tenant_id, name, (h:t)) =
  Role
  { role_id = id
  , role_tenantid = tenant_id
  , role_name = name
  , role_permission = h :| t
  }
makeRole (id, tenant_id, name, []) =
  error "cannot create role without a permission"

role_query :: Query RoleTableR
role_query = queryTable roleTable

role_query_for_tenant :: Int -> Query RoleTableR
role_query_for_tenant t_tenantid =
  proc () ->
  do row@(_, tenant_id, _, _) <- role_query -< ()
     restrict -< tenant_id .== (constant t_tenantid)
     returnA -< row
