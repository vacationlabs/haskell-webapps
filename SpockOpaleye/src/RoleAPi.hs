{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module RoleApi
  ( createRole
  , removeRole
  , updateRole
  , readRolesForTenant
  ) where

import           Control.Arrow
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef
import           RoleDefs

import           ApiBase
import           Control.Lens
import           Prelude                    hiding (id)
import TenantId
import TH
import Lenses
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

auditable :: a -> Auditable a
auditable a = Auditable {_data = a, _log = Object HM.empty}

wrapAuditable :: (Functor a, Functor b) => a (b c) -> a (b (Auditable c))
wrapAuditable a = (fmap auditable) <$> a

createRole :: RoleIncoming -> AppM Role
createRole role = auditable <$> createRow roleTable role

updateRole :: Role -> AppM Role
updateRole role = updateAuditableRow roleTable role

removeRole :: Role -> AppM GHC.Int.Int64
removeRole Auditable {_data = role} = do
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
