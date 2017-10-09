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


type Tenant = DBTenant
type TenantOutput = DBTenant
type TenantId = Key DBTenant
type UserId = Key DBUser
type ProductId = Key DBProduct
type UserActivationId = Key DBUserActivation
type TenantActivationId = Key DBTenantActivation


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

instance Serialize UserId where
  get = DBUserKey . SqlBackendKey <$> Data.Serialize.get
  put x = put (unSqlBackendKey $ unDBUserKey x)

instance Serialize UTCTime where
  get = read <$> Data.Serialize.get
  put x = put (show x)


data Session = Session { sessionUserID :: UserId
                       , startTime :: UTCTime
                       } deriving (Show, Generic, Serialize, FromJSON , ToJSON)

data DBError = TenantNotFound TenantId
             | UserNotFound UserId
             | ProductNotFound ProductId
             | RoleNotFound (Either RoleId Text)
             | ViolatesTenantUniqueness (Unique Tenant)
             | UserAlreadyActive UserId
                deriving (Eq, Show)

data UserCreationError = UserExists Text
                       | TenantDoesn'tExist Text
                        deriving (Eq, Show)
data ActivationError = ActivationError


data TenantInput =
    TenantI { _name :: Text
            , _backofficeDomain :: Text
            } deriving (Generic)
instance FromJSON TenantInput where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = Prelude.drop 1})
instance ToJSON TenantInput where
    toEncoding = genericToEncoding (defaultOptions { fieldLabelModifier = Prelude.drop 1})
    toJSON = genericToJSON (defaultOptions { fieldLabelModifier = Prelude.drop 1})

instance HasName TenantInput where
    name = lens _name (\ti n -> ti { _name = n } )
instance HasBackofficeDomain TenantInput where
    backofficeDomain = lens _backofficeDomain (\ti bd -> ti { _backofficeDomain = bd } )

data UserType = Input
              | Regular

type family Omittable (state :: UserType) (s :: Symbol) a where
  Omittable Input "password" a = a
  Omittable Input _ a = ()
  Omittable Regular "password" a = ()
  Omittable Regular _ a = a

class HasTenantID s where
    tenantID :: Lens' s TenantId

class HasUserID s where
    userID :: Lens' s UserId

instance HasTenantID DBUser where
    tenantID = dBUserTenantID

data UserBase (userType :: UserType)=
    UserB { _userFirstName :: Text
          , _userLastName :: Text
          , _userEmail :: Text
          , _userPhone :: Text
          , _userUsername :: Text
          , _userTenantID :: TenantId
          , _userPassword :: Omittable userType "password" Text
          , _userStatus :: Omittable userType "status" UserStatus
          , _userRole :: Omittable userType "role" Role
          , _userUserID :: Omittable userType "userID" UserId
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
                   Show (Omittable a "userID" UserId))
                   => Show (UserBase a)

type UserInput = UserBase Input
type User = UserBase Regular

