{-# LANGUAGE RecordWildCards #-}
module Domain.User
    where

import           Control.Lens
import           Control.Monad.Except
import Domain.Roles

import           Data.Text              (Text)
import           Data.Time
import           Database.Persist
import           DBTypes
import           Models
import           Operation
import           Types
import           Updater

dbCreateUser :: MonadIO m => UserInput -> TransactionT m (Either UserCreationError UserID)
dbCreateUser u = runExceptT $ do
    time <- liftIO getCurrentTime
    roleId <- lift $  either entityKey id
                   <$> insertBy (Role "Default Role" [] $ u ^. tenantID)
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
                     , _dBUserRoleId      = roleId
                     }
    result <- lift $ insertUnique dbu
    case result of
         Just a  -> return a
         Nothing -> throwError $ UserExists (view username u)

dbUpdateUser :: MonadIO m => UserID -> UserUpdater -> OperationT (TransactionT m) (Either DBError ())
dbUpdateUser uid upd = requirePermission (EditUser uid) $ lift $ do
    oldUser' <- get uid
    case oldUser' of
         Nothing -> return $ Left $ UserNotFound uid
         Just oldUser ->
           Right <$> (replace uid =<< applyUpdate upd oldUser)

dbGetUser :: MonadIO m => UserID -> OperationT (TransactionT m) (Either DBError User)
dbGetUser uid = requirePermission (ViewUser uid) $  lift $ runExceptT $ do
    dbu <- ExceptT $ maybe (Left $ UserNotFound uid)
                           Right
                       <$> get uid
    let roleId = view dBUserRoleId dbu
    role <- ExceptT $ maybe (Left $ RoleNotFound  $ Left roleId)
                            Right
                        <$> get roleId
    let u = UserB { _userFirstName = view firstName dbu
                  , _userLastName  = view lastName dbu
                  , _userTenantID  = view tenantID dbu
                  , _userUsername  = view username dbu
                  , _userPassword  = ()
                  , _userEmail     = view email dbu
                  , _userPhone     = view phone dbu
                  , _userStatus    = InactiveU
                  , _userRole      = role
                  , _userUserID    = uid
                  }
    return u

dbActivateUser :: MonadIO m => Text -> TransactionT m (Either ActivationError ())
dbActivateUser key = runExceptT $ do
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

dbUserAssignRole :: MonadIO m
                 => Text
                 -> UserID
                 -> OperationT (TransactionT m) (Either DBError ())
dbUserAssignRole name uid =
  runExceptT $ do
    time <- liftIO getCurrentTime
    u <- ExceptT $ dbGetUser uid
    let tid = view tenantID u
    roleE <- ExceptT $ maybe (Left $ RoleNotFound $ Right name)
                       Right
                   <$> lift (dbGetRoleByName name tid)
    lift . lift $ update uid [ DBUserRoleId =. entityKey roleE
                             , DBUserUpdatedAt =. time
                             ]
    return ()
