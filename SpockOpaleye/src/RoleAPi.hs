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

import           Control.Lens
import           Prelude                    hiding (id)

create_role :: Connection -> RoleIncoming -> IO Role
create_role conn role = create_item conn roleTable role

update_role :: Connection -> RoleId -> Role -> IO Role
update_role conn role_id role = update_item conn roleTable role_id role

remove_role :: Connection -> Role -> IO GHC.Int.Int64
remove_role conn role = do
  runDelete conn userRolePivotTable (\(_, role_id) -> role_id .== constant (role ^. id))
  runDelete conn roleTable match_func
    where
    t_id = role ^. id
    match_func role = (role ^. id).== constant t_id

read_roles_for_tenant :: Connection -> TenantId -> IO [Role]
read_roles_for_tenant conn t_id = do
  runQuery conn $ role_query_for_tenant t_id

role_query :: Query RoleTableR
role_query = queryTable roleTable

role_query_for_tenant :: TenantId -> Query RoleTableR
role_query_for_tenant t_tenantid =
  proc () ->
  do role <- role_query -< ()
     restrict -< (role ^. tenantid) .== (constant t_tenantid)
     returnA -< role
