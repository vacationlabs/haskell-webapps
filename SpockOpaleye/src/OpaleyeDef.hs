{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module OpaleyeDef where

import           Data.List.NonEmpty
import           Data.Profunctor.Product
import qualified Data.Profunctor.Product.Default      as D
import           Data.Text
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple.FromField
import           Opaleye

import           DataTypes

type TenantTableW = (Maybe (Column PGInt4), Column PGText, Column PGText, Column PGText, Column PGText, Column PGText, Column PGText, Maybe (Column (Nullable PGInt4)), Column PGText)

type TenantTableR = (Column PGInt4, Column PGText, Column PGText, Column PGText, Column PGText, Column PGText, Column PGText, Column (Nullable PGInt4), Column PGText)

tenantTable :: Table TenantTableW TenantTableR
tenantTable =
  Table "tenants"
    (p9
       (optional "id", required "name", required "first_name", required "last_name", required
        "email", required "phone", required "status", optional "owner_id", required
        "backoffice_domain"))

type UserTableW = (Maybe (Column PGInt4), Column PGInt4, Column PGText, Column PGBytea, Maybe (Column (Nullable PGText)), Maybe (Column (Nullable PGText)), Column PGText)

type UserTableR = (Column PGInt4, Column PGInt4, Column PGText, Column PGBytea, (Column (Nullable PGText)), (Column (Nullable PGText)), Column PGText)

userTable :: Table UserTableW UserTableR
userTable =
  Table "users"
    (p7
       (optional "id", required "tenant_id", required "username", 
        required "password", optional "first_name", optional "last_name", required "status"))

type RoleTableW = (Maybe (Column PGInt4), Column PGInt4, Column PGText, Column (PGArray PGText))

type RoleTableR = (Column PGInt4, Column PGInt4, Column PGText, Column (PGArray PGText))

roleTable :: Table RoleTableW RoleTableR
roleTable =
  Table "roles" (p4 (optional "id", required "tenant_id", required "name", required "permissions"))

userRolePivotTable :: Table (Column PGInt4, Column PGInt4) (Column PGInt4, Column PGInt4)
userRolePivotTable =
  Table "users_roles" (p2 (required "user_id", required "role_id"))

instance D.Default Constant TenantStatus (Column PGText) where
  def = Constant def'
    where
      def' :: TenantStatus -> (Column PGText)
      def' TenantStatusInActive = pgStrictText "inactive"
      def' TenantStatusActive = pgStrictText "active"
      def' TenantStatusNew = pgStrictText "new"

instance FromField (TenantStatus) where
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

instance D.Default Constant UserStatus (Column PGText) where
  def = Constant def'
    where
      def' :: UserStatus -> (Column PGText)
      def' UserStatusInActive = pgStrictText "inactive"
      def' UserStatusActive = pgStrictText "active"
      def' UserStatusBlocked = pgStrictText "blocked"

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
          to_text Read = "Read"
          to_text Create = "Create"
          to_text Update = "Update"
          to_text Delete = "Delete"

instance FromField Permission where
  fromField f mdata = return $ makePermission mdata
    where
      makePermission (Just x) = toPermission $ decodeUtf8 x
      makePermission Nothing = error "No data read from db"
      toPermission :: Text -> Permission
      toPermission "Read" = Read
      toPermission "Create" = Create
      toPermission "Update" = Update
      toPermission "Delete" = Delete
      toPermission _ = error "Unrecognized permission"

instance QueryRunnerColumnDefault PGText Permission where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant (UserId) (Column PGInt4) where
  def = Constant def'
    where
      def' :: UserId -> (Column PGInt4)
      def' (UserId id) = pgInt4 id

instance FromField UserId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ UserId x

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

--
instance D.Default Constant (RoleId) (Column PGInt4) where
  def = Constant def'
    where
      def' :: RoleId -> (Column PGInt4)
      def' (RoleId id) = pgInt4 id

instance FromField RoleId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ RoleId x

instance QueryRunnerColumnDefault PGInt4 RoleId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

--
instance D.Default Constant (TenantId) (Column PGInt4) where
  def = Constant def'
    where
      def' :: TenantId -> (Column PGInt4)
      def' (TenantId id) = pgInt4 id

instance FromField TenantId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ TenantId x

instance QueryRunnerColumnDefault PGInt4 TenantId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
