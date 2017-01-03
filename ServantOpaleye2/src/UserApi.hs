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
  , getTenantForUser
  ) where

import           AppCore
import           Control.Arrow
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text              hiding (filter, null)
import           GHC.Int
import           Opaleye
import           Prelude                hiding (id, null)
import           Utils
import           Control.Monad.Catch

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

readUserAndRoles :: (DbConnection m) => Text -> m (Maybe (User, [RoleId]))
readUserAndRoles uname = groupRows <$> (readRow query) 
  where
    groupRows :: [(User, Maybe RoleId)] -> Maybe (User, [RoleId])
    groupRows [] = Nothing
    groupRows x = groupRows' x
      where
        groupRows' :: [(User, Maybe RoleId)] -> Maybe (User, [RoleId])
        groupRows' ur@((u,_):_) = Just (u, fromJust <$> (filter isNothing $ snd <$> ur))
    userTenant :: Query (UserTableR, (Column PGInt4))
    userTenant = joinF 
      (\u t -> (u, t ^. key))
      (\u t -> u ^. tenantid .== t ^. key)
      (userQueryByUsername uname)
      tenantQuery
    query :: Query (UserTableR, Column (Nullable PGInt4))
    query = leftJoinF
      (\(u, tid) r -> (u, toNullable (r ^. key)))
      (\(u, tid) -> (u, null))
      (\(u, tid) r -> tid .== r ^. tenantid)
      userTenant
      roleQuery

addRoleToUser :: (DbConnection m) => UserId -> RoleId -> m [(UserId, RoleId)]
addRoleToUser userId roleId =
  createDbRows userRolePivotTable [(constant (userId, roleId))]

removeRoleFromUser :: (DbConnection m) => UserId -> RoleId -> m GHC.Int.Int64
removeRoleFromUser tUserId tRoleId = removeRawDbRows userRolePivotTable
    (\(userId, roleId) -> (userId .== constant tUserId) .&& (roleId .== constant tRoleId))

userQueryById :: UserId -> UserQuery
userQueryById tId = proc () -> do
  user <- userQuery -< ()
  restrict -< (user ^. key) .== (constant tId)
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

authenticateUser :: (DbConnection m) => Text -> Text -> m (Either String (User, [RoleId]))
authenticateUser username pass = do
  users <- readUserAndRoles username
  return (checkPassword users) 
  where
    -- FIXME: do this in constant time 
    checkPassword :: Maybe (User, [RoleId])  -> Either String (User, [RoleId])
    checkPassword (Just (u, r)) = if verifyPassword pass (u ^. password) 
      then Right (u, r)
      else fail
    checkPassword Nothing = fail
    fail = Left "Authentication fail"

getTenantForUser :: (DbConnection m, Monad m, MonadThrow m) => UserId -> m Tenant
getTenantForUser userid = readRow query >>= returnOneIfNE "Tenant for user id not found"
  where
    query :: Query TenantTableR
    query = joinF
      (\tenant user -> tenant)
      (\tenant user -> tenant ^. key .== user ^. tenantid)
      tenantQuery
      (userQueryById userid)
