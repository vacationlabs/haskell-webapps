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
import Operation

dbCreateUser :: UserInput -> App (Either UserCreationError UserID)
dbCreateUser u = runExceptT $ do
    time <- liftIO getCurrentTime
    let dbu = DBUser { _dBUserFirstName = view firstName u
                     , _dBUserLastName  = view lastName u
                     , _dBUserTenantID  = view userTenantID u
                     , _dBUserUsername  = view username u
                     , _dBUserPassword  = view password u
                     , _dBUserEmail     = view email u
                     , _dBUserPhone     = view phone u
                     , _dBUserStatus    = InactiveU
                     , _dBUserCreatedAt = time
                     , _dBUserUpdatedAt = time
                     }
    result <- runDb $ insertUnique dbu
    case result of
         Just a -> return a
         Nothing -> throwError $ UserExists (view username u)

dbUpdateUser :: UserID -> UserUpdater -> OperationT App (Either DBError ())
dbUpdateUser id uu = requirePermission (EditUser id) >> (runDb $ do
    time <- liftIO $ getCurrentTime
    oldUser' <- get id
    case oldUser' of
         Nothing -> return $ Left $ UserNotFound id
         Just oldUser -> Right <$> replace id (set updatedAt time $ runUpdate uu oldUser))

dbGetUser :: UserID -> OperationT App (Either DBError User)
dbGetUser uid = runExceptT $ do
    time <- liftIO getCurrentTime
    dbu <- ExceptT $ maybe (Left $ UserNotFound uid)
                           Right
                       <$> (runDb $ get uid)
    let u = UserB { _userFirstName = view firstName dbu
                  , _userLastName  = view lastName dbu
                  , _userTenantID  = view dBUserTenantID dbu
                  , _userUsername  = view username dbu
                  , _userPassword  = ()
                  , _userEmail     = view email dbu
                  , _userPhone     = view phone dbu
                  , _userStatus    = InactiveU
                  }
    return u
