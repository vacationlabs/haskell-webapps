{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}

module UserDefs where

import           Auditable
import           Control.Lens
import           CryptoDef
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Profunctor.Product.Default      as D
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text
import           Data.Time
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics
import           Ids
import           InternalUtils
import           Opaleye
import           OpaleyeDef
import           Prelude                              hiding (id)
import           Data.Scientific
import           Data.Ratio

import qualified Text.Digestive as DIG
import qualified Text.Digestive.Aeson as DIGA

import Control.Monad.Identity

data UserPoly key created_at updated_at tenant_id username password firstname lastname status  = User {
    _userpolyKey        :: key
  , _userpolyCreatedat :: created_at
  , _userpolyUpdatedat :: updated_at
  , _userpolyTenantid  :: tenant_id
  , _userpolyUsername  :: username
  , _userpolyPassword  :: password
  , _userpolyFirstname :: firstname
  , _userpolyLastname  :: lastname
  , _userpolyStatus    :: status
} deriving (Show, Generic)

type UserTableW = UserPoly
  ()
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGInt4)
  (Column PGText)
  (Column PGBytea)
  (Maybe (Column (Nullable PGText)))
  (Maybe (Column (Nullable PGText)))
  (Maybe (Column PGText))

type UserTableR = UserPoly
  (Column PGInt4)
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt
  (Column PGInt4)
  (Column PGText)
  (Column PGBytea)
  (Column (Nullable PGText))
  (Column (Nullable PGText))
  (Column PGText)


$(makeAdaptorAndInstance "pUser" ''UserPoly)

userTable :: Table UserTableW UserTableR
userTable = Table "users" (pUser
  User {
    _userpolyKey = (readOnly "id")
  , _userpolyCreatedat = (optional "created_at")
  , _userpolyUpdatedat = (required "updated_at")
  , _userpolyTenantid = required "tenant_id"
  , _userpolyUsername = required "username"
  , _userpolyPassword = required "password"
  , _userpolyFirstname = optional "first_name"
  , _userpolyLastname = optional "last_name"
  , _userpolyStatus = optional "status"
 })

data UserStatus = UserStatusActive | UserStatusInActive | UserStatusBlocked
  deriving (Show)

type InternalUser = UserPoly UserId UTCTime UTCTime TenantId Text BcryptPassword (Maybe Text) (Maybe Text) UserStatus
type User = Auditable InternalUser

getTestUser :: IO User
getTestUser = do
  Just password_ <- bcryptPassword "adsasda"
  return $ auditable $ User (UserId 1) tz tz (TenantId 1) "John" password_  (Just "2342424") (Just "asdada") UserStatusActive
  where
      tz = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }

type UserIncoming = UserPoly () () () TenantId Text Text (Maybe Text) (Maybe Text) ()

instance D.Default Constant UserStatus (Maybe (Column PGText)) where
  def = Constant def'
    where
      def' :: UserStatus -> Maybe (Column PGText)
      def' UserStatusInActive = Just $ pgStrictText "inactive"
      def' UserStatusActive   = Just $ pgStrictText "active"
      def' UserStatusBlocked  = Just $ pgStrictText "blocked"

instance FromField (UserStatus) where
  fromField _ mdata = return gender
    where
      gender =
        case mdata of
          Just "active"   -> UserStatusActive
          Just "inactive" -> UserStatusInActive
          Just "blocked"  -> UserStatusBlocked
          _               -> error "Bad value read for user status"

instance QueryRunnerColumnDefault PGText UserStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance ToJSON UserStatus where
  toJSON x = String $ Data.Text.pack $ show x

instance ToJSON InternalUser where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = (fmap Data.Char.toLower).removePrefix }
    where
      removePrefix = Prelude.drop 11

instance FromJSON UserIncoming where
  parseJSON (Object v) = User () () () <$> v .: "Tenantid"
                              <*> v .: "username"
                              <*> v .: "password"
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure ()

data Abc = Abc Text Int deriving (Show)

data Dfg = Def Abc Text deriving (Show)

defForm :: (Monad m) => DIG.Form Text m Dfg
defForm = Def <$> "Abc" DIG..: abcForm
              <*> "SomeName" DIG..: nonEmptyText

abcForm :: (Monad m) => DIG.Form Text m Abc
abcForm = Abc <$> "name" DIG..: nonEmptyText
              <*> "number" DIG..: gtZero

nonEmptyText :: (Monad m) => DIG.Form Text m Text
nonEmptyText = DIG.check "name shall not be empty" ((>0).Data.Text.length) (DIG.text Nothing)

gtZero :: (Monad m) => DIG.Form Text m Int
gtZero = floor <$> DIG.check "number should be greater than zero" (>0) (DIG.stringRead "yyy" Nothing)

validate' = let Just j = jsonv in runIdentity $ DIGA.digestJSON abcForm j

validate2 = let Just j = jsonv2 in runIdentity $ DIGA.digestJSON defForm j

jsonv :: Maybe Value
jsonv = decode "{\"name\":\"max\", \"number\": 25}"

jsonv2 :: Maybe Value
jsonv2 = decode "{\"Abc\": {\"name\":\"max\", \"number\": 25}, \"SomName\": \"Blah\"}"


--validateInteger :: Num a => Scientific -> DIG.Result Text a
--validateInteger x =
--  let xRat = toRational x
--  in if denominator xRat /= 1
--       then DIG.Error "Number must be an integer"
--       else return (fromInteger $ numerator xRat)
--
----------------------------------------------------------------------------------
--parseInteger :: (Monad m, Num a) => DIG.Form Text m a
--parseInteger =
--  DIG.validate validateInteger (DIG.stringRead "Could not parse number" Nothing)
