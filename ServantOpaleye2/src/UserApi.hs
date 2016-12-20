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

import           AppCore
import           AppM
import           Control.Arrow
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe
import           GHC.Int
import           Opaleye
import           Prelude                hiding (id)
import           Utils

createUser :: UserIncoming -> AppM User
createUser user = do
  Just hash <- liftIO $ bcryptPassword $ user ^. password
  createRow userTable (fillPassword user hash)

updateUser :: User -> AppM User
updateUser user = updateAuditableRow userTable user

activateUser :: User -> AppM User
activateUser user = setUserStatus user UserStatusActive

deactivateUser :: User -> AppM User
deactivateUser user = setUserStatus user UserStatusInActive

setUserStatus :: User -> UserStatus -> AppM User
setUserStatus user newStatus = updateUser $ user & status .~ newStatus

removeUser :: User -> AppM GHC.Int.Int64
removeUser user = removeAuditableRow userTable user

readUsers :: AppM [User]
readUsers = readRow userQuery

readUsersForTenant :: TenantId -> AppM [User]
readUsersForTenant tenantId = readRow $ userQueryByTenantid tenantId

readUserById :: UserId -> AppM User
readUserById id' = do
  (readRow $ userQueryById id') >>= returnOneIfNE "User id not found"

addRoleToUser :: UserId -> RoleId -> AppM [(UserId, RoleId)]
addRoleToUser userId roleId =
  createDbRows userRolePivotTable [(constant (userId, roleId))]

removeRoleFromUser :: UserId -> RoleId -> AppM GHC.Int.Int64
removeRoleFromUser tUserId tRoleId = removeRawDbRows userRolePivotTable
    (\(userId, roleId) -> (userId .== constant tUserId) .&& (roleId .== constant tRoleId))

userQuery :: UserQuery
userQuery = queryTable userTable

userQueryById :: UserId -> UserQuery
userQueryById tId = proc () -> do
  user <- userQuery -< ()
  restrict -< (user ^. id) .== (constant tId)
  returnA -< user

userQueryByTenantid :: TenantId -> UserQuery
userQueryByTenantid tTenantid = proc () -> do
  user <- userQuery -< ()
  restrict -< (user ^. tenantid) .== (constant tTenantid)
  returnA -< user
