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


dbCreateTenant :: MonadIO m => TenantInput -> TransactionT m (Maybe (TenantId, TenantActivationId))
dbCreateTenant ti = do
    time <- liftIO getCurrentTime
    let dbt = DBTenant { _dBTenantName = ti ^. name
                       , _dBTenantBackofficeDomain = ti ^. backofficeDomain
                       , _dBTenantOwnerId = Nothing
                       , _dBTenantStatus = NewT
                       , _dBTenantCreatedAt = time
                       , _dBTenantUpdatedAt = time
                       }
    mtid <- insertUnique dbt
    case mtid of
      Nothing -> return Nothing
      Just tid -> do
                key <- insert (DBTenantActivation tid time)
                return $ Just (tid,key)


dbGetTenant :: MonadIO m => TenantId -> TransactionT m (Maybe Tenant)
dbGetTenant = get

dbUpdateTenant :: MonadIO m =>
                  TenantUpdater -> TenantId -> OperationT (TransactionT m) (Either DBError ())
dbUpdateTenant upd tid = requirePermission (EditTenant tid) $ lift $ do
    oldTenant' <- get tid
    case oldTenant' of
         Nothing -> return $ Left $ TenantNotFound tid
         Just oldTenant ->
           maybe (return ())
                 (Left . ViolatesTenantUniqueness)
             <$> (replaceUnique tid =<< applyUpdate upd oldTenant)

activateTenant :: MonadIO m
               => UserId
               -> TenantActivationId
               -> TransactionT m (Either ActivationError ())
activateTenant owner key = runExceptT $ do
                       r <- lift $ get key
                       time <- liftIO getCurrentTime
                       case r of
                         Nothing -> throwError ActivationError
                         Just (DBTenantActivation tid _) -> do
                           lift $ update tid [ DBTenantStatus =. ActiveT
                                             , DBTenantOwnerId =. Just owner
                                             , DBTenantUpdatedAt =. time
                                             ]
                           lift $ delete key
                       return ()

