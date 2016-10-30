{-# LANGUAGE Arrows, FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module RoleApi
  (
   create_role,
   remove_role,
   read_roles_for_tenant
  )
  where

import Control.Arrow (returnA, (<<<))
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, restrict, (.==), (.<=), (.&&), (.<),
       (.===), (.++), Nullable,
        Query, PGInt4, runInsertMany, runDelete, runInsertManyReturning, queryTable, constant,
        pgStrictText, runQuery)
import Opaleye.PGTypes
import GHC.Int
import Data.Text
import Data.List.NonEmpty

create_role
  :: Connection -> Role -> IO [Int]
create_role conn Role {role_tenantid=tenant_id, role_name=name, role_permission=rp} = 
  runInsertManyReturning conn roleTable
  (return (
           Nothing
          ,constant tenant_id
          ,pgStrictText name
          ,constant rp
          )) (\(id, _, _, _) -> id)

remove_role :: Connection -> Role -> IO GHC.Int.Int64
remove_role conn Role {role_id = t_id} = runDelete conn roleTable
  (\(id, _, _, _) -> id .== constant t_id)

read_roles_for_tenant :: Connection -> Int -> IO [Role]
read_roles_for_tenant conn t_id = do
  rows <- runQuery conn $ role_query_for_tenant t_id
  return $ makeRole <$> rows

makeRole :: (Int, Int, Text, [Permission]) -> Role
makeRole (id, tenant_id, name, (h:t)) = Role {
  role_id = id,
  role_tenantid = tenant_id,
  role_name = name,
  role_permission = h:|t
}

role_query :: Query (Column PGInt4, Column PGInt4, Column PGText, Column (PGArray PGText))
role_query = queryTable roleTable

role_query_for_tenant :: Int -> Query (Column PGInt4, Column PGInt4, Column PGText, Column (PGArray PGText))
role_query_for_tenant t_tenantid = proc () -> do
  row@ (_, tenant_id, _, _) <- role_query -< ()
  restrict -< tenant_id .== (constant t_tenantid)
  returnA -< row
  
