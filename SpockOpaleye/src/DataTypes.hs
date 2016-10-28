{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
  OverloadedStrings #-}

module DataTypes
  (Tenant(..)
  ,User(..)
  ,UserStatus(..)
  ,TenantStatus(..))
  where

import Data.Text
import qualified Data.Profunctor.Product.Default as D
import Database.PostgreSQL.Simple.FromField
import Opaleye
       (Constant(Constant), PGText, Column, pgStrictText,
        QueryRunnerColumnDefault(queryRunnerColumnDefault),
        fieldQueryRunnerColumn)

data TenantStatus
    = TenantStatusActive 
    | TenantStatusInActive 
    | TenantStatusNew 
    deriving ((((Show))))

data Tenant = Tenant
    { tenant_id :: Int
    , tenant_name :: Text
    , tenant_firstname :: Text
    , tenant_lastname :: Text
    , tenant_email :: Text
    , tenant_phone :: Text
    , tenant_status :: TenantStatus
    , tenant_ownerid :: Maybe Int
    , tenant_backofficedomain :: Text
    } deriving ((((Show))))

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

data UserStatus = UserStatusActive | UserStatusInActive | UserStatusBlocked

data User = User {
  user_id :: Int,
  user_tenantid :: Int,
  user_username :: Text,
  user_password :: Text,
  user_firstname :: Maybe Text,
  user_lastname :: Maybe Text,
  user_status :: UserStatus
}

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
  
