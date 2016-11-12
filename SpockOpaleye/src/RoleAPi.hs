{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module RoleApi
  ( 
   removeRole
  , updateRole
  , readRolesForTenant
  ) where

import           Control.Arrow
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef

import           ApiBase
import           Control.Lens
import           Prelude                    hiding (id)

--createRole :: Connection -> RoleIncoming -> IO Role
--createRole conn role = createRow conn roleTable role

updateRole :: Connection -> Role -> IO Role
updateRole conn role = updateRow conn roleTable role

removeRole :: Connection -> Role -> IO GHC.Int.Int64
removeRole conn role = do
  _ <- runDelete conn userRolePivotTable (\(_, roleId) -> roleId .== constant (role ^. id))
  runDelete conn roleTable matchFunc
    where
    tId = role ^. id
    matchFunc role' = (role' ^. id).== constant tId

readRolesForTenant :: Connection -> TenantId -> IO [Role]
readRolesForTenant conn tId = do
  runQuery conn $ roleQueryForTenant tId

roleQuery :: Query RoleTableR
roleQuery = queryTable roleTable

roleQueryForTenant :: TenantId -> Query RoleTableR
roleQueryForTenant tTenantid =
  proc () ->
  do role <- roleQuery -< ()
     restrict -< (role ^. tenantid) .== (constant tTenantid)
     returnA -< role
