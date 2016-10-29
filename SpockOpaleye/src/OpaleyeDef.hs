{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
  OverloadedStrings #-}

module OpaleyeDef
  (
    tenantTable
   ,userTable
  ) where

import Database.PostgreSQL.Simple.FromField
import qualified Data.Profunctor.Product.Default as D
import           Opaleye (Column, Table(Table), Nullable,
                 PGText, Constant(..), constant, pgStrictText,
                 required, optional, (.==), (.<),
                 arrangeDeleteSql, arrangeInsertManySql,
                 arrangeUpdateSql, arrangeInsertManyReturningSql,
                 fieldQueryRunnerColumn,
                 QueryRunnerColumnDefault(queryRunnerColumnDefault),
                 PGInt4, PGFloat8)
import           Data.Profunctor.Product (p6, p7, p8, p9)

import DataTypes

tenantTable :: Table
  (
   Column PGInt4,
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
  required "id",
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
   Column PGInt4,
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
  required "id",
  required "tenant_id",
  required "username",
  required "password",
  optional "first_name",
  optional "last_name",
  required "status"))

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

instance QueryRunnerColumnDefault PGText UserStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
