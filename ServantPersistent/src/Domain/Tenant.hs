{-# LANGUAGE OverloadedStrings #-}
module Domain.Tenant
    where

import Control.Lens
import Data.Time
import Database.Persist
import Control.Monad.IO.Class
import Data.Text (Text)
import Models
import Types
import Updater
import DBTypes
import Operation

dbCreateTenant :: TenantInput -> App (Maybe TenantID)
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


dbGetTenant :: TenantID -> App (Maybe Tenant)
dbGetTenant = runDb . get

dbGetTenantByBackofficeDomain :: Text -> App (Maybe TenantID)
dbGetTenantByBackofficeDomain t = runDb $ do
    ti <- getBy (UniqueBackofficeDomain t)
    return (entityKey <$> ti)


dbUpdateTenant :: TenantUpdater -> TenantID -> OperationT App (Either DBError ())
dbUpdateTenant tu id = requirePermission (EditTenant id) >> (runDb $ do
    time <- liftIO $ getCurrentTime
    oldTenant' <- get id
    case oldTenant' of
         Nothing -> return $ Left $ TenantNotFound id
         Just oldTenant -> Right <$> replace id (set updatedAt time $ runUpdate tu oldTenant))
