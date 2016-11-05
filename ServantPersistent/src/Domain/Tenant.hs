{-# LANGUAGE RecordWildCards #-}
module Domain.Tenant
    where

import           Control.Lens
import           Control.Monad.Except

import           Data.Time
import           Database.Persist
import           DBTypes
import           Models
import           Operation
import           Types
import           Updater
import Data.Text (Text)

dbCreateTenant :: DBMonad m => TenantInput -> m (Maybe TenantID)
dbCreateTenant ti = runDb $ do
    time <- liftIO getCurrentTime
    let dbt = DBTenant { _dBTenantName = ti ^. name
                       , _dBTenantBackofficeDomain = ti ^. backofficeDomain
                       , _dBTenantOwnerId = Nothing
                       , _dBTenantStatus = NewT
                       , _dBTenantCreatedAt = time
                       , _dBTenantUpdatedAt = time
                       }
    insertUnique dbt


dbGetTenant :: DBMonad m => TenantID -> m (Maybe Tenant)
dbGetTenant = runDb . get

dbUpdateTenant :: DBMonad m =>
                  TenantUpdater -> TenantID -> OperationT m (Either DBError ())
dbUpdateTenant upd tid = requirePermission (EditTenant tid) $ runDb $ do
    oldTenant' <- get tid
    case oldTenant' of
         Nothing -> return $ Left $ TenantNotFound tid
         Just oldTenant ->
           maybe (return ())
                 (Left . ViolatesTenantUniqueness)
             <$> (replaceUnique tid =<< applyUpdate upd oldTenant)

activateTenant :: DBMonad m => UserID -> Text -> m (Either ActivationError ())
activateTenant owner key = runDb $ runExceptT $ do
                       r <- lift $ getBy (UniqueTenantKey key)
                       time <- liftIO getCurrentTime
                       case r of
                         Nothing -> throwError ActivationError
                         Just Entity{..} -> do
                           let tid = view dBTenantActivationTenantID entityVal
                           lift $ update tid [ DBTenantStatus =. ActiveT
                                             , DBTenantOwnerId =. Just owner
                                             , DBTenantUpdatedAt =. time
                                             ]
                           lift $ delete entityKey
                       return ()

