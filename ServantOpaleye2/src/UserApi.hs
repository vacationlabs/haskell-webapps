{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module UserApi
  ( createUser
  , readUsers
  , readUserById
  , readUserByName
  , readUsersForTenant
  , addRoleToUser
  , removeRoleFromUser
  , updateUser
  , removeUser
  , activateUser
  , deactivateUser
  , authenticateUser
  ) where

import           AppCore
import           Control.Arrow
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text
import           GHC.Int
import           Opaleye
import           Prelude                hiding (id)
import           Utils

createUser :: (DbConnection m) => UserIncoming -> m User
createUser user = do
  Just hash <- liftIO $ bcryptPassword $ user ^. password
  createRow userTable (fillPassword user hash)

updateUser :: (DbConnection m, CurrentTenant m, CurrentUser m) => User -> m User
updateUser user = updateAuditableRow userTable user

activateUser :: (DbConnection m, CurrentUser m, CurrentTenant m) => User -> m User
activateUser user = setUserStatus user UserStatusActive

deactivateUser :: (DbConnection m, CurrentUser m, CurrentTenant m) => User -> m User
deactivateUser user = setUserStatus user UserStatusInActive

setUserStatus :: (DbConnection m, CurrentTenant m, CurrentUser m) => User -> UserStatus -> m User
setUserStatus user newStatus = updateUser $ user & status .~ newStatus

removeUser :: (DbConnection m) => User -> m GHC.Int.Int64
removeUser user = removeAuditableRow userTable user

readUsers :: (DbConnection m) => m [User]
readUsers = readRow userQuery

readUsersForTenant :: (DbConnection m) => TenantId -> m [User]
readUsersForTenant tenantId = readRow $ userQueryByTenantid tenantId

readUserById :: (DbConnection m) => UserId -> m User
readUserById id' = do
  (readRow $ userQueryById id') >>= returnOneIfNE "User id not found"

readUserByName :: (DbConnection m) => Text -> m [User]
readUserByName uname = readRow $ userQueryByUsername uname 

addRoleToUser :: (DbConnection m) => UserId -> RoleId -> m [(UserId, RoleId)]
addRoleToUser userId roleId =
  createDbRows userRolePivotTable [(constant (userId, roleId))]

removeRoleFromUser :: (DbConnection m) => UserId -> RoleId -> m GHC.Int.Int64
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

userQueryByUsername :: Text -> UserQuery
userQueryByUsername uname = proc () -> do
  user <- userQuery -< ()
  restrict -< ((user ^. username) .== (constant uname)) 
  returnA -< user

authenticateUser :: (DbConnection m) => Text -> Text -> m (Either String User)
authenticateUser username pass = do
  users <- readUserByName username
  return (checkPassword users) 
  where
    -- FIXME: do this in constant time 
    checkPassword :: [User] -> Either String User
    checkPassword (u:_) = if verifyPassword pass (u ^. password) 
      then (Right u)
      else fail
    checkPassword [] = fail
    fail = Left "Authentication fail"
