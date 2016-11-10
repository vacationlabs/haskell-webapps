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
import           Database.PostgreSQL.Simple (Connection)
import           DataTypes
import           GHC.Int
import           Opaleye
import           OpaleyeDef

import           CryptoDef
import           Prelude                    hiding (id)

createUser :: Connection -> UserIncoming -> IO User
createUser conn user = do
  Just hash <- bcryptPassword $ user ^. password
  let fullUser = user { _userpolyPassword = hash }
  createRow conn userTable fullUser

updateUser :: Connection -> User -> IO User
updateUser conn user = updateRow conn userTable user

activateUser :: Connection -> User -> IO User
activateUser conn user = setUserStatus conn user UserStatusActive

deactivateUser :: Connection -> User -> IO User
deactivateUser conn user = setUserStatus conn user UserStatusInActive

setUserStatus :: Connection -> User -> UserStatus -> IO User
setUserStatus conn user newStatus = updateUser conn $ user & status .~ newStatus

removeUser :: Connection -> User -> IO GHC.Int.Int64
removeUser conn rUser =
  runDelete conn userTable matchFunction
    where
    matchFunction user = (user ^. id).== constant (rUser ^. id)

readUsers :: Connection -> IO [User]
readUsers conn = runQuery conn userQuery

readUsersForTenant :: Connection -> TenantId -> IO [User]
readUsersForTenant conn tenantId = runQuery conn $ userQueryByTenantid tenantId

readUserById :: Connection -> UserId -> IO (Maybe User)
readUserById conn id' = do
  r <- runQuery conn $ userQueryById id'
  return $ case r of
    []     -> Nothing
    (x:_) -> Just x

addRoleToUser :: Connection -> UserId -> RoleId -> IO GHC.Int.Int64
addRoleToUser conn userId roleId =
  runInsertMany conn userRolePivotTable (return (constant userId, constant roleId))

removeRoleFromUser :: Connection -> UserId -> RoleId -> IO GHC.Int.Int64
removeRoleFromUser conn tUserId tRoleId = runDelete conn userRolePivotTable
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
