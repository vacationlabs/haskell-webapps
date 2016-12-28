{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
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

data UserPoly key created_at updated_at tenant_id username password firstname lastname status  = User {
    _userpolyId        :: key
  , _userpolyCreatedat :: created_at
  , _userpolyUpdatedat :: updated_at
  , _userpolyTenantid  :: tenant_id
  , _userpolyUsername  :: username
  , _userpolyPassword  :: password
  , _userpolyFirstname :: firstname
  , _userpolyLastname  :: lastname
  , _userpolyStatus    :: status
} deriving (Show)

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
    _userpolyId = (readOnly "id")
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
