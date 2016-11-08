{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JsonInstances where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           DataTypes

instance FromJSON UserId where
  parseJSON j@(Number v) = UserId <$> (parseJSON j)
  parseJSON invalid      = typeMismatch "UserId" invalid

instance FromJSON TenantId where
  parseJSON j@(Number v) = TenantId <$> (parseJSON j)
  parseJSON invalid      = typeMismatch "TenantId" invalid

instance FromJSON TenantStatus where
  parseJSON j@(String v) = t_status <$> (parseJSON j)
    where
      t_status :: Text -> TenantStatus
      t_status "active"   = TenantStatusActive
      t_status "inactive" = TenantStatusInActive
      t_status "new"      = TenantStatusNew
  parseJSON invalid = typeMismatch "TenantStatus" invalid

instance FromJSON TenantIncoming where
  parseJSON (Object v) =
    (Tenant () Nothing Nothing) <$> v .: "name" <*> v .: "firstname" <*> v .: "lastname" <*>
    v .: "email" <*>
    v .: "phone" <*>
    (pure ()) <*>
    v .: "userId" <*>
    v .: "backofficeDomain"

instance ToJSON TenantStatus where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = tg_modify }
    where
      tg_modify :: String -> String
      tg_modify "TenantStatusActive"   = "active"
      tg_modify "TenantStatusInActive" = "inactive"
      tg_modify "TenantStatusNew"      = "new"

instance ToJSON Tenant where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = remove_prefix }
    where
      remove_prefix = Prelude.drop 7

instance ToJSON UserId where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance ToJSON TenantId where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
