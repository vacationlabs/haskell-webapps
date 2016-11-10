{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module OpaleyeDef where

import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Profunctor.Product
import qualified Data.Profunctor.Product.Default      as D
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           Database.PostgreSQL.Simple           (Connection)
import           Database.PostgreSQL.Simple.FromField
import           Opaleye

import           Control.Lens
import           Data.Vector
import           DataTypes
import           GHC.Int

type TenantTableW = TenantPoly
  (Maybe (Column PGInt4))
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

type UserTableW = UserPoly
  (Maybe (Column PGInt4))
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

type RoleTableW = RolePoly
  (Maybe (Column PGInt4))
  (Column PGInt4)
  (Column PGText)
  (Column (PGArray PGText))
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt

type RoleTableR = RolePoly
  (Column PGInt4)
  (Column PGInt4)
  (Column PGText)
  (Column (PGArray PGText))
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)

tenantTable :: Table TenantTableW TenantTableR
tenantTable = Table "tenants" (pTenant
   Tenant {
     _tenantpolyId = (optional "id"),
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

$(makeAdaptorAndInstance "pUser" ''UserPoly)

userTable :: Table UserTableW UserTableR
userTable = Table "users" (pUser
  User {
    _userpolyId = optional "id"
  , _userpolyCreatedat = (optional "created_at")
  , _userpolyUpdatedat = (required "updated_at")
  , _userpolyTenantid = required "tenant_id"
  , _userpolyUsername = required "username"
  , _userpolyPassword = required "password"
  , _userpolyFirstname = optional "first_name"
  , _userpolyLastname = optional "last_name"
  , _userpolyStatus = optional "status"
 })

$(makeAdaptorAndInstance "pRole" ''RolePoly)

roleTable :: Table RoleTableW RoleTableR
roleTable = Table "roles" (pRole Role {
  _rolepolyId = optional "id",
  _rolepolyTenantid = required "tenant_id",
  _rolepolyName = required "name",
  _rolepolyPermission = required "permissions",
  _rolepolyCreatedat = optional "created_at",
  _rolepolyUpdatedat = required "updated_at"
  })

userRolePivotTable :: Table (Column PGInt4, Column PGInt4) (Column PGInt4, Column PGInt4)
userRolePivotTable = Table "users_roles" (p2 (required "user_id", required "role_id"))

instance D.Default Constant TenantStatus (Maybe (Column PGText)) where
  def = Constant def'
    where
      def' :: TenantStatus -> (Maybe (Column PGText))
      def' TenantStatusInActive = Just $ pgStrictText "inactive"
      def' TenantStatusActive   = Just $ pgStrictText "active"
      def' TenantStatusNew      = Just $ pgStrictText "new"

instance FromField TenantStatus where
  fromField f mdata = return tStatus
    where
      tStatus =
        case mdata of
          Just "active"   -> TenantStatusActive
          Just "inactive" -> TenantStatusInActive
          Just "new"      -> TenantStatusNew
          _               -> error "Bad value read for user status"

instance QueryRunnerColumnDefault PGText TenantStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant UserStatus (Maybe (Column PGText)) where
  def = Constant def'
    where
      def' :: UserStatus -> Maybe (Column PGText)
      def' UserStatusInActive = Just $ pgStrictText "inactive"
      def' UserStatusActive   = Just $ pgStrictText "active"
      def' UserStatusBlocked  = Just $ pgStrictText "blocked"

instance FromField (UserStatus) where
  fromField f mdata = return gender
    where
      gender =
        case mdata of
          Just "active"   -> UserStatusActive
          Just "inactive" -> UserStatusInActive
          Just "blocked"  -> UserStatusBlocked
          _               -> error "Bad value read for user status"

instance QueryRunnerColumnDefault PGText UserStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant (NonEmpty Permission) (Column (PGArray PGText)) where
  def = Constant def'
    where
      def' :: (NonEmpty Permission) -> (Column (PGArray PGText))
      def' (ph :| pl) = pgArray pgStrictText $ to_text <$> (ph : pl)
        where
          to_text :: Permission -> Text
          to_text Read   = "Read"
          to_text Create = "Create"
          to_text Update = "Update"
          to_text Delete = "Delete"

instance QueryRunnerColumnDefault (PGArray PGText) (NonEmpty Permission) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField Permission where
  fromField f mdata = return $ makePermission mdata
    where
      makePermission (Just x) = toPermission $ decodeUtf8 x
      makePermission Nothing  = error "No data read from db"

toPermission :: Text -> Permission
toPermission "Read"   = Read
toPermission "Create" = Create
toPermission "Update" = Update
toPermission "Delete" = Delete
toPermission _        = error "Unrecognized permission"

instance FromField [Permission] where
  fromField field mdata =  (fmap toPermission) <$> Data.Vector.toList <$> fromField field mdata

instance FromField (NonEmpty Permission) where
  fromField field mdata = (fromJust.nonEmpty) <$> (fromField field mdata)

instance QueryRunnerColumnDefault PGText Permission where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant (UserId) (Column PGInt4) where
  def = Constant def'
    where
      def' :: UserId -> (Column PGInt4)
      def' (UserId id) = pgInt4 id

instance D.Default Constant (UserId) (Column (Nullable PGInt4)) where
  def = Constant def'
    where
      def' :: UserId -> (Column (Nullable PGInt4))
      def' (UserId id) = (toNullable.pgInt4) id

instance D.Default Constant (UserId) (Maybe (Column PGInt4)) where
  def = Constant def'
    where
      def' :: UserId -> Maybe (Column PGInt4)
      def' (UserId id) = Just $ pgInt4 id

instance FromField UserId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ UserId x

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

--
instance D.Default Constant RoleId (Column PGInt4) where
  def = Constant def'
    where
      def' :: RoleId -> (Column PGInt4)
      def' (RoleId id) = pgInt4 id

instance D.Default Constant RoleId (Maybe (Column PGInt4)) where
  def = Constant def'
    where
      def' :: RoleId -> Maybe (Column PGInt4)
      def' (RoleId id) = Just $ pgInt4 id

instance FromField RoleId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ RoleId x

instance QueryRunnerColumnDefault PGInt4 RoleId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

--
instance D.Default Constant TenantId (Column PGInt4) where
  def = Constant def'
    where
      def' :: TenantId -> (Column PGInt4)
      def' (TenantId id) = pgInt4 id

instance D.Default Constant TenantId (Maybe (Column PGInt4)) where
  def = Constant def'
    where
      def' :: TenantId -> Maybe (Column PGInt4)
      def' (TenantId id) = Just $ pgInt4 id

instance FromField TenantId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ TenantId x

instance QueryRunnerColumnDefault PGInt4 TenantId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

--

instance D.Default Constant () (Maybe (Column PGInt4)) where
  def = Constant (\_ -> Nothing)

instance D.Default Constant () (Maybe (Column PGText)) where
  def = Constant (\_ -> Nothing)

instance D.Default Constant Text (Column (Nullable PGText)) where
  def = Constant (toNullable.pgStrictText)

instance D.Default Constant () (Maybe (Column PGTimestamptz)) where
  def = Constant (\() -> Nothing)

instance D.Default Constant () (Column PGTimestamptz) where
  def = Constant (\() -> pgUTCTime defaultutc)
    where
      defaultutc = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }

instance D.Default Constant UTCTime (Maybe (Column PGTimestamptz)) where
  def = Constant (\time -> Just $ pgUTCTime time)

instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
