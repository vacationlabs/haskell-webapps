{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module TenantDefs where

import           Auditable
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Profunctor.Product.Default      as D
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text
import           Data.Time
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           GHC.Generics
import           Ids
import           InternalUtils
import           Opaleye
import           OpaleyeDef
import           Prelude                              hiding (id)

data TenantPoly key created_at updated_at name fname lname email phone status owner_id b_domain = Tenant {
    _tenantpolyId               :: key
  , _tenantpolyCreatedat        :: created_at
  , _tenantpolyUpdatedat        :: updated_at
  , _tenantpolyName             :: name
  , _tenantpolyFirstname        :: fname
  , _tenantpolyLastname         :: lname
  , _tenantpolyEmail            :: email
  , _tenantpolyPhone            :: phone
  , _tenantpolyStatus           :: status
  , _tenantpolyOwnerid          :: owner_id
  , _tenantpolyBackofficedomain :: b_domain
} deriving (Show, Generic)


data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew
  deriving (Show, Generic)

type InternalTenant = TenantPoly TenantId UTCTime UTCTime Text Text Text Text Text TenantStatus (Maybe UserId) Text

type Tenant = Auditable InternalTenant

type TenantTableW = TenantPoly
  ()
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Maybe (Column PGText))
  (Maybe (Column (Nullable PGInt4)))
  (Column PGText)

type TenantTableR = TenantPoly
  (Column PGInt4)
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column (Nullable PGInt4))
  (Column PGText)

type TenantQuery = Query TenantTableR

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)

tenantTable :: Table TenantTableW TenantTableR
tenantTable = Table "tenants" (pTenant
   Tenant {
     _tenantpolyId = (readOnly "id"),
     _tenantpolyCreatedat = (optional "created_at"),
     _tenantpolyUpdatedat = (required "updated_at"),
     _tenantpolyName = (required "name"),
     _tenantpolyFirstname = (required "first_name"),
     _tenantpolyLastname = (required "last_name"),
     _tenantpolyEmail = (required "email"),
     _tenantpolyPhone = (required "phone"),
     _tenantpolyStatus = (optional "status"),
     _tenantpolyOwnerid = (optional "owner_id"),
     _tenantpolyBackofficedomain = (required "backoffice_domain")
   }
 )

getTestTenant :: Tenant
getTestTenant = auditable $ Tenant (TenantId 1) tz tz "tjhon" "John" "Jacob" "john@gmail.com" "2342424" TenantStatusNew Nothing "Bo domain"
  where
      tz = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }

type TenantIncoming = TenantPoly () () () Text Text Text Text Text () (Maybe UserId) Text

instance D.Default Constant TenantStatus (Maybe (Column PGText)) where
  def = Constant def'
    where
      def' :: TenantStatus -> (Maybe (Column PGText))
      def' TenantStatusInActive = Just $ pgStrictText "inactive"
      def' TenantStatusActive   = Just $ pgStrictText "active"
      def' TenantStatusNew      = Just $ pgStrictText "new"

instance FromField TenantStatus where
  fromField _ mdata = return tStatus
    where
      tStatus =
        case mdata of
          Just "active"   -> TenantStatusActive
          Just "inactive" -> TenantStatusInActive
          Just "new"      -> TenantStatusNew
          _               -> error "Bad value read for user status"

instance QueryRunnerColumnDefault PGText TenantStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

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

instance ToJSON InternalTenant where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = (fmap Data.Char.toLower).removePrefix }
    where
      removePrefix = Prelude.drop 11

