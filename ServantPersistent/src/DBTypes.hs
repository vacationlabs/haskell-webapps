{-# LANGUAGE DeriveGeneric   #-}
module DBTypes where

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics
import Control.Lens
import Types
import Models

type TenantID = Key DBTenant
type Tenant = DBTenant
type TenantOutput = DBTenant
type UserID = Key DBUser

data DBError = TenantNotFound TenantID
             | UserNotFound UserID deriving (Eq, Show)
data UserCreationError = UserExists Text
                       | TenantDoesn'tExist Text
                        deriving (Eq, Show)

data Role = SiteAdmin
          | TenantAdmin TenantID
          | NormalUser

data TenantIdent =
    TI { _name :: Text
       , _backofficeDomain :: Text
       } deriving (Generic)
instance FromJSON TenantIdent where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = Prelude.drop 1})
instance ToJSON TenantIdent where
    toEncoding = genericToEncoding (defaultOptions { fieldLabelModifier = Prelude.drop 1})
    toJSON = genericToJSON (defaultOptions { fieldLabelModifier = Prelude.drop 1})

instance HasName TenantIdent where
    name = lens _name (\ti n -> ti { _name = n } )
instance HasBackofficeDomain TenantIdent where
    backofficeDomain = lens _backofficeDomain (\ti bd -> ti { _backofficeDomain = bd } )

type TenantInput = TenantIdent

data UserInput =
    UserI { _firstName :: Text
          , _lastName :: Text
          , _email :: Text
          , _phone :: Text
          , _username :: Text
          , _password :: Text
          , _tenantName :: Text
          }

data User =
    User { userFirstName :: Text
         , userLastName :: Text
         , userEmail :: Text
         , userPhone :: Text
         , userUsername :: Text
         , userTenantId :: TenantID
         , userRole :: Role
         , userUserID :: UserID
    }
