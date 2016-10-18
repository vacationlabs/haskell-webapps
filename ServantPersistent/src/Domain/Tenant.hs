{-# LANGUAGE OverloadedStrings #-}
module Domain.Tenant
    where

import Control.Lens
import Data.Monoid
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Control.Monad.IO.Class
import Models
import Types

newtype TenantUpdater = TU { runUpdate :: Tenant -> Tenant }

instance Monoid TenantUpdater where
    mempty = TU id
    (TU a) `mappend` (TU b) = TU $ (a . b)

parseUpdater :: FromJSON a => Object -> Text -> (a -> Tenant -> Tenant) -> Parser TenantUpdater
parseUpdater v t setter = do
    a <- v .:? t
    case a of
         Nothing -> return mempty
         Just x -> return $ TU (setter x)

instance FromJSON TenantUpdater where
    parseJSON (Object v) = 
        mconcat <$> f
            where f = sequence [ parseUpdater v "name" (set dBTenantName)
                               , parseUpdater v "backoffice_domain" (set dBTenantBackofficeDomain)
                               ]

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

dbUpdateTenant :: TenantUpdater -> TenantID -> App ()
dbUpdateTenant tu id = runDb $ do
    (Just oldTenant) <- get id
    time <- liftIO $ getCurrentTime
    let tu' = tu <> TU (set dBTenantUpdatedAt time)
    replace id (runUpdate tu' oldTenant)
