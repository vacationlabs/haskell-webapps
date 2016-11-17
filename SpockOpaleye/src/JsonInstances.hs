{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JsonInstances where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text
import           DataTypes

instance FromJSON UserId where
  parseJSON j@(Number _) = UserId <$> (parseJSON j)
  parseJSON invalid      = typeMismatch "UserId" invalid

instance FromJSON TenantId where
  parseJSON j@(Number _) = TenantId <$> (parseJSON j)
  parseJSON invalid      = typeMismatch "TenantId" invalid

instance FromJSON TenantStatus where
  parseJSON j@(String _) = tStatus <$> (parseJSON j)
    where
      tStatus :: Text -> TenantStatus
      tStatus "active"   = TenantStatusActive
      tStatus "inactive" = TenantStatusInActive
      tStatus "new"      = TenantStatusNew
      tStatus _      = error "Unknown status name while parsing TenantStatus field"
  parseJSON invalid = typeMismatch "TenantStatus" invalid

instance FromJSON TenantIncoming where
  parseJSON (Object v) =
    (Tenant () () ()) <$> v .: "name" <*> v .: "firstname" <*> v .: "lastname" <*>
    v .: "email" <*>
    v .: "phone" <*>
    (pure ()) <*>
    v .: "userId" <*>
    v .: "backofficeDomain"
  parseJSON invalid = typeMismatch "Unexpected type while paring TenantIncoming" invalid

instance ToJSON TenantStatus where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = tgModify }
    where
      tgModify :: String -> String
      tgModify "TenantStatusActive"   = "active"
      tgModify "TenantStatusInActive" = "inactive"
      tgModify "TenantStatusNew"      = "new"
      tgModify _                      = error "Unknown status name for tenant"

instance ToJSON Tenant where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = (fmap Data.Char.toLower).removePrefix }
    where
      removePrefix = Prelude.drop 11

instance ToJSON UserId where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance ToJSON TenantId where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance ToJSON UserStatus where
  toJSON x = String $ Data.Text.pack $ show x

instance ToJSON RoleId where
  toJSON (RoleId x) = toJSON x

instance ToJSON Permission where
  toJSON x = toJSON $ show x
