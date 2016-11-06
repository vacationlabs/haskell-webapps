{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
module DBTypes where

import Data.Aeson
import Data.Aeson.TH
import Data.Text

import Data.Time.Clock
import GHC.Generics
import Control.Lens
import Types
import Price
import Models

import Database.Persist
import Database.Persist.Sql
import Data.Serialize
import GHC.TypeLits


type TenantID = Key DBTenant
type Tenant = DBTenant
type TenantOutput = DBTenant
type UserID = Key DBUser
type ProductID = Key DBProduct
type RoleID = Key Role


data Product = Product { getProduct :: Entity DBProduct
                       , getVariants :: [Entity DBVariant]}
-- Dummy instance

instance ToJSON Product where
  toJSON = undefined
data ProductInput =
    ProductI { piName :: Text
             , piDescription :: Text
             , piCurrency :: Text
             , piType :: ProductType
             , piVariants :: [VariantInput]
             , piProperties :: AppJSON
             , piCostPrice :: Maybe Price
             , piComparisonPrice :: Maybe Price
             , piAdvertisedPrice :: Maybe Price
             , piURLSlug :: Maybe Text
             }
data VariantInput =
    VariantI { viName :: Text
             , viSKU :: Text
             , viWeightInGrams :: Maybe Double
             , viWeightDisplayUnit :: Maybe Text
             , viPrice :: Price
             }

instance Serialize UserID where
  get = DBUserKey . SqlBackendKey <$> Data.Serialize.get
  put x = put (unSqlBackendKey $ unDBUserKey x)

data Session = Session { sessionUserID :: UserID
                       } deriving (Show, Generic, Serialize, FromJSON , ToJSON)

data DBError = TenantNotFound TenantID
             | UserNotFound UserID 
             | ProductNotFound ProductID
             | RoleNotFound (Either RoleID Text)
             | ViolatesTenantUniqueness (Unique Tenant)
                deriving (Eq, Show)

data UserCreationError = UserExists Text
                       | TenantDoesn'tExist Text
                        deriving (Eq, Show)
data ActivationError = ActivationError


data Activation =
    Activation { activationTenantID :: TenantID
               , activationTime :: UTCTime
               } deriving (Generic)

data TenantIdent =
    TenantI { _name :: Text
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


data UserType = Input
              | Regular

type family Omittable (state :: UserType) (s :: Symbol) a where
  Omittable Input "password" a = a
  Omittable Input _ a = ()
  Omittable Regular "password" a = ()
  Omittable Regular _ a = a

class HasTenantID s where
    tenantID :: Lens' s TenantID

class HasUserID s where
    userID :: Lens' s UserID

instance HasTenantID DBUser where
    tenantID = dBUserTenantID

data UserBase (userType :: UserType)=
    UserB { _userFirstName :: Text
          , _userLastName :: Text
          , _userEmail :: Text
          , _userPhone :: Text
          , _userUsername :: Text
          , _userTenantID :: TenantID
          , _userPassword :: Omittable userType "password" Text
          , _userStatus :: Omittable userType "status" UserStatus
          , _userRole :: Omittable userType "role" Role
          , _userUserID :: Omittable userType "userID" UserID
          } deriving (Generic)

makeLenses ''UserBase

instance HasHumanName (UserBase a) where
    firstName = userFirstName
    lastName = userLastName
instance HasContactDetails (UserBase a) where
    email = userEmail
    phone = userPhone
instance HasUsername (UserBase a) where
    username = userUsername
instance HasPassword (UserBase Input) where
    password = userPassword
instance HasTenantID (UserBase a) where
    tenantID = userTenantID
instance HasUserID (UserBase Regular) where
    userID = userUserID

deriving instance (Show (Omittable a "password" Text),
                   Show (Omittable a "status" UserStatus),
                   Show (Omittable a "role" Role),
                   Show (Omittable a "userID" UserID))
                   => Show (UserBase a)

type UserInput = UserBase Input
type User = UserBase Regular

