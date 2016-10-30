module DataTypes
  (Tenant(..)
  ,User(..)
  ,Role(..)
  ,Permission(..)
  ,UserStatus(..)
  ,TenantStatus(..))
  where

import Data.Text
import Data.List.NonEmpty
import qualified Data.Profunctor.Product.Default as D
import Database.PostgreSQL.Simple.FromField
import Opaleye
       (Constant(Constant), PGText, Column, pgStrictText,
        QueryRunnerColumnDefault(queryRunnerColumnDefault))

data TenantStatus
    = TenantStatusActive 
    | TenantStatusInActive 
    | TenantStatusNew 
    deriving (((((((((Show)))))))))

data Tenant = Tenant
    { tenant_id :: Maybe Int
    , tenant_name :: Text
    , tenant_firstname :: Text
    , tenant_lastname :: Text
    , tenant_email :: Text
    , tenant_phone :: Text
    , tenant_status :: TenantStatus
    , tenant_ownerid :: Maybe Int
    , tenant_backofficedomain :: Text
    } deriving (((((((((Show)))))))))

data UserStatus
    = UserStatusActive 
    | UserStatusInActive 
    | UserStatusBlocked 

data User = User
    { user_id :: Maybe Int
    , user_tenantid :: Int
    , user_username :: Text
    , user_password :: Text
    , user_firstname :: Maybe Text
    , user_lastname :: Maybe Text
    , user_status :: UserStatus
    } 

newtype Permission =
    Permission Text

data Role = Role
    { role_id :: Int
    , role_tenantid :: Int
    , role_name :: Text
    , role_permission :: NonEmpty Permission
    } 
