{-# LANGUAGE OverloadedStrings #-}
module Domain.Tenant
    where

import Control.Lens
import Data.Time
import Database.Persist
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Except
import Data.ByteString (ByteString)
import Models
import Types
import Updater
import DBTypes
import Operation

dbCreateTenant :: DBMonad m => TenantInput -> m (Maybe TenantID)
dbCreateTenant ti = runDb $ do
    time <- liftIO $ getCurrentTime
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
           Right <$> (replace tid =<< applyUpdate upd oldTenant)

encode = undefined

decode = undefined

activateTenant :: DBMonad m => UserID -> ByteString -> m (Either ActivationError Tenant)
activateTenant owner actkey = runDb $ runExceptT $ do
    let (Activation tid _) = decode actkey
    time <- liftIO $ getCurrentTime
    lift $ updateGet
        tid
        [ DBTenantOwnerId   =. Just owner
        , DBTenantStatus    =. ActiveT
        , DBTenantUpdatedAt =. time
        ]
