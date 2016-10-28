module Domain.User
    where

import Control.Lens
import Data.Time
import Database.Persist
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.ByteString
import Data.Default
import Models
import Types
import Updater
import DBTypes
import Operation

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
         Just a -> return a
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

dbActivateUser :: ByteString -> m ()
dbActivateUser = undefined
