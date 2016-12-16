{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module UserApi
  ( createUser
  , readUsers
  , readUserById
  , readUsersForTenant
  , addRoleToUser
  , removeRoleFromUser
  , updateUser
  , removeUser
  , activateUser
  , deactivateUser
  ) where

import           ApiBase
import           Control.Arrow
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef

import           CryptoDef
import           Prelude                    hiding (id)
import           RoleDefs
import           UserDefs
import           UserId
import           TenantId

import Auditable
import Lenses

createUser :: UserIncoming -> AppM User
createUser user = do
  Just hash <- liftIO $ bcryptPassword $ user ^. password
  let fullUser = user { _userpolyPassword = hash }
  auditable <$> (createRow userTable fullUser)

updateUser :: User -> AppM User
updateUser user = updateAuditableRow userTable user

activateUser :: User -> AppM User
activateUser user = setUserStatus user UserStatusActive

deactivateUser :: User -> AppM User
deactivateUser user = setUserStatus user UserStatusInActive

setUserStatus :: User -> UserStatus -> AppM User
setUserStatus user newStatus = updateUser $ user & status .~ newStatus

removeUser :: User -> AppM GHC.Int.Int64
removeUser Auditable { _data = rUser} = removeRow userTable rUser

readUsers :: AppM [User]
readUsers = wrapAuditable $ readRow userQuery

readUsersForTenant :: TenantId -> AppM [User]
readUsersForTenant tenantId = wrapAuditable $ readRow $ userQueryByTenantid tenantId

readUserById :: UserId -> AppM (Maybe User)
readUserById id' = do
  wrapAuditable $ listToMaybe <$> (readRow $ userQueryById id')

addRoleToUser :: UserId -> RoleId -> AppM [(UserId, RoleId)]
addRoleToUser userId roleId =
  createDbRows userRolePivotTable [(constant (userId, roleId))]

removeRoleFromUser :: UserId -> RoleId -> AppM GHC.Int.Int64
removeRoleFromUser tUserId tRoleId = removeRawDbRows userRolePivotTable
    (\(userId, roleId) -> (userId .== constant tUserId) .&& (roleId .== constant tRoleId))

userQuery :: Query UserTableR
userQuery = queryTable userTable

userQueryById :: UserId -> Query UserTableR
userQueryById tId = proc () -> do
  user <- userQuery -< ()
  restrict -< (user ^. id) .== (constant tId)
  returnA -< user

userQueryByTenantid :: TenantId -> Query UserTableR
userQueryByTenantid tTenantid = proc () -> do
  user <- userQuery -< ()
  restrict -< (user ^. tenantid) .== (constant tTenantid)
  returnA -< user
