{-# LANGUAGE RecordWildCards #-}
module Domain.User
    where

import           Control.Lens
import           Control.Monad.Except


import           Data.Default
import           Data.Text              (Text)
import           Data.Time
import           Database.Persist
import           DBTypes
import           Models
import           Operation
import           Types
import           Updater

dbCreateUser :: DBMonad m => UserInput -> m (Either UserCreationError UserID)
dbCreateUser u = runExceptT $ do
    time <- liftIO getCurrentTime
    let dbu = DBUser { _dBUserFirstName = view firstName u
                     , _dBUserLastName  = view lastName u
                     , _dBUserTenantID  = view tenantID u
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
         Just a  -> return a
         Nothing -> throwError $ UserExists (view username u)

dbUpdateUser :: DBMonad m => UserID -> UserUpdater -> OperationT m (Either DBError ())
dbUpdateUser uid upd = requirePermission (EditUser uid) $ runDb $ do
    oldUser' <- get uid
    case oldUser' of
         Nothing -> return $ Left $ UserNotFound uid
         Just oldUser ->
           Right <$> (replace uid =<< applyUpdate upd oldUser)

dbGetUser :: DBMonad m => UserID -> OperationT m (Either DBError User)
dbGetUser uid = requirePermission (ViewUser uid) $ runExceptT $ do
    dbu <- ExceptT $ maybe (Left $ UserNotFound uid)
                           Right
                       <$> (runDb $ get uid)
    let u = UserB { _userFirstName = view firstName dbu
                  , _userLastName  = view lastName dbu
                  , _userTenantID  = view tenantID dbu
                  , _userUsername  = view username dbu
                  , _userPassword  = ()
                  , _userEmail     = view email dbu
                  , _userPhone     = view phone dbu
                  , _userStatus    = InactiveU
                  , _userRole      = def
                  , _userUserID    = uid
                  }
    return u

dbActivateUser :: DBMonad m => Text -> m (Either ActivationError ())
dbActivateUser key = runDb $ runExceptT $ do
                       r <- lift $ getBy (UniqueUserKey key)
                       time <- liftIO getCurrentTime
                       case r of
                         Nothing -> throwError ActivationError
                         Just Entity{..} -> do
                           let uid = view dBUserActivationUserID entityVal
                           lift $ update uid [ DBUserStatus =. ActiveU
                                             , DBUserUpdatedAt =. time
                                             ]
                           lift $ delete entityKey
                       return ()

