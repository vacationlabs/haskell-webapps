{-# LANGUAGE OverloadedStrings #-}

module JsonInstances where

import DataTypes
import Data.Aeson
import Control.Monad
import Data.Text

instance FromJSON UserId where
  parseJSON j@(Object v) = UserId <$> (parseJSON j)

instance FromJSON TenantId where
  parseJSON j@(Object v) = TenantId <$> (parseJSON j)

instance FromJSON TenantStatus where
  parseJSON j@(Object v) = t_status <$> (parseJSON j)
    where
      t_status :: Text -> TenantStatus
      t_status "active" = TenantStatusActive
      t_status "inactive" = TenantStatusInActive
      t_status "new" = TenantStatusNew

instance FromJSON Tenant where
  parseJSON (Object v) = Tenant <$>
    v .: "id" <*>
    v .: "name" <*>
    v .: "firstname" <*>
    v .: "lastname" <*>
    v .: "email" <*>
    v .: "phone" <*>
    v .: "status" <*>
    v .: "userId" <*>
    v .: "backofficeDomain"
