{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module RoleApi
  ( 
   updateRole
  , removeRole
  , readRolesForTenant
  ) where

import           AppCore
import           Control.Arrow
import           GHC.Int
import           Opaleye
import           Control.Lens
import           Prelude                    hiding (id)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM


updateRole :: Role -> AppM Role
updateRole role = updateAuditableRow roleTable role

removeRole :: Role -> AppM GHC.Int.Int64
removeRole role = do
  _ <- removeRawDbRows userRolePivotTable (\(_, roleId) -> roleId .== constant (role ^. id))
  removeRawDbRows roleTable matchFunc
    where
    tId = role ^. id
    matchFunc role' = (role' ^. id).== constant tId

readRolesForTenant :: TenantId -> AppM [Role]
readRolesForTenant tId = do
  wrapAuditable $ readRow $ roleQueryForTenant tId

roleQuery :: Query RoleTableR
roleQuery = queryTable roleTable

roleQueryForTenant :: TenantId -> Query RoleTableR
roleQueryForTenant tTenantid =
  proc () ->
  do role <- roleQuery -< ()
     restrict -< (role ^. tenantid) .== (constant tTenantid)
     returnA -< role
