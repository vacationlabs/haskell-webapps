{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
  OverloadedStrings #-}

module OpaleyeDef
  (
    tenantTable
   ,userTable
   ,roleTable
   ,userRolePivotTable
  ) where

import Data.Text.Encoding
import Database.PostgreSQL.Simple.FromField
import qualified Data.Profunctor.Product.Default as D
import           Opaleye 
import           Data.Profunctor.Product
import Data.List.NonEmpty
import Data.Text

import DataTypes

tenantTable :: Table
  (
   Maybe (Column PGInt4),
   Column PGText,
   Column PGText,
   Column PGText,
   Column PGText,
   Column PGText,
   Column PGText,
   Maybe (Column (Nullable PGInt4)),
   Column PGText
   )
  (
   Column PGInt4,
   Column PGText,
   Column PGText,
   Column PGText,
   Column PGText,
   Column PGText,
   Column PGText,
   Column (Nullable PGInt4),
   Column PGText
   )
tenantTable = Table "tenants" (p9 ( 
  optional "id",
  required "name",
  required "first_name",
  required "last_name",
  required "email",
  required "phone",
  required "status",
  optional "owner_id",
  required "backoffice_domain"))

userTable :: Table
  (
   Maybe (Column PGInt4),
   Column PGInt4,
   Column PGText,
   Column PGText,
   Maybe (Column (Nullable PGText)),
   Maybe (Column (Nullable PGText)),
   Column PGText
  )
  (
   Column PGInt4,
   Column PGInt4,
   Column PGText,
   Column PGText,
   (Column (Nullable PGText)),
   (Column (Nullable PGText)),
   Column PGText
  )
userTable = Table "users" (p7 (
  optional "id",
  required "tenant_id",
  required "username",
  required "password",
  optional "first_name",
  optional "last_name",
  required "status"))


roleTable :: Table
  (
   Maybe(Column PGInt4),
   Column PGInt4,
   Column PGText,
   Column (PGArray PGText)
  )
  (
   Column PGInt4,
   Column PGInt4,
   Column PGText,
   Column (PGArray PGText)
  )
roleTable = Table "roles" (p4 (
  optional "id",
  required "tenant_id",
  required "name",
  required "permissions"
  ))

userRolePivotTable :: Table
  (
   Column PGInt4,
   Column PGInt4)
  (
   Column PGInt4,
   Column PGInt4)
userRolePivotTable = Table "users_roles" (p2 (
      required "user_id",
      required "role_id"))

instance D.Default Constant TenantStatus (Column PGText) where
  def = Constant def'
    where
      def' :: TenantStatus -> (Column PGText)
      def' TenantStatusInActive = pgStrictText "inactive"
      def' TenantStatusActive = pgStrictText "active"
      def' TenantStatusNew = pgStrictText "new"

instance FromField (TenantStatus) where
  fromField f mdata = return gender
    where
      gender = 
        case mdata of
          Just "active" -> TenantStatusActive
          Just "inactive" -> TenantStatusInActive
          Just "new" -> TenantStatusNew
          _ -> error "Bad value read for user status"

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
          Just "active" -> UserStatusActive
          Just "inactive" -> UserStatusInActive
          Just "blocked" -> UserStatusBlocked
          _ -> error "Bad value read for user status"

instance QueryRunnerColumnDefault PGText UserStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant (NonEmpty Permission) (Column (PGArray PGText)) where
  def = Constant def'
    where
    def' :: (NonEmpty Permission) -> (Column (PGArray PGText))
    def' (ph :| pl) = pgArray pgStrictText $ to_text <$> (ph:pl)
      where
      to_text :: Permission -> Text
      to_text (Permission name) = name

instance FromField Permission where
  fromField f mdata = return $ makePermission mdata
    where
      makePermission (Just x) = Permission $ decodeUtf8 x
      makePermission Nothing = error "No data read from db"

instance QueryRunnerColumnDefault PGText Permission where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
