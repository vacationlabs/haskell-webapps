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
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM

createRole :: (MonadIO m, DbConnection m) => RoleIncoming -> m Role
createRole role = createRow roleTable role

updateRole :: (MonadIO m, DbConnection m, CurrentUser m, CurrentTenant m) => Role -> m Role
updateRole role = updateAuditableRow roleTable role

removeRole :: (MonadIO m, DbConnection m) => Role -> m GHC.Int.Int64
removeRole role = do
  _ <- removeRawDbRows userRolePivotTable (\(_, roleId) -> roleId .== constant (role ^. id))
  removeRawDbRows roleTable matchFunc
    where
    tId = role ^. id
    matchFunc role' = (role' ^. id).== constant tId

readRolesForTenant :: (DbConnection m, MonadIO m) => TenantId -> m [Role]
readRolesForTenant tId = do
  readRow $ roleQueryForTenant tId

roleQuery :: RoleQuery
roleQuery = queryTable roleTable

roleQueryForTenant :: TenantId -> RoleQuery
roleQueryForTenant tTenantid =
  proc () ->
  do role <- roleQuery -< ()
     restrict -< (role ^. tenantid) .== (constant tTenantid)
     returnA -< role
