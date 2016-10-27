{-# LANGUAGE OverloadedStrings #-}
module Domain.User
    where

import Control.Lens
import Data.Time
import Database.Persist
import Control.Monad.IO.Class
import Control.Monad.Except
import Models
import Types
import Updater
import DBTypes
import Domain.Tenant
import Operation

createUser :: UserInput -> App (Either UserCreationError UserID)
createUser u = runExceptT $ do
    time <- liftIO getCurrentTime
    id <- ExceptT $ maybe (Left $ TenantDoesn'tExist (_tenantBackofficeDomain u))
                          Right
                      <$> dbGetTenantByBackofficeDomain (_tenantBackofficeDomain u)
    let dbu = DBUser { _dBUserFirstName = _firstName u
                     , _dBUserLastName = _lastName u
                     , _dBUserTenantID = id
                     , _dBUserUsername = _username u
                     , _dBUserPassword = _password u
                     , _dBUserEmail = _email u
                     , _dBUserPhone = _phone u
                     , _dBUserStatus = InactiveU
                     , _dBUserCreatedAt = time
                     , _dBUserUpdatedAt = time
                     }
    result <- runDb $ insertUnique dbu
    case result of
         Just a -> return a
         Nothing -> throwError $ UserExists (_username u)

updateUser :: UserID -> UserUpdater -> OperationT App (Either DBError ())
updateUser id uu = requirePermission (EditUser id) >> (runDb $ do
    time <- liftIO $ getCurrentTime
    oldUser' <- get id
    case oldUser' of
         Nothing -> return $ Left $ UserNotFound id
         Just oldUser -> Right <$> replace id (set updatedAt time $ runUpdate uu oldUser))

