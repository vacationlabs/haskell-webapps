module DataTypes where

import Data.List.NonEmpty
import Data.Text

newtype TenantId =
  TenantId Int
  deriving (Show)

data TenantStatus
  = TenantStatusActive
  | TenantStatusInActive
  | TenantStatusNew
  deriving (Show)

data Tenant = Tenant
  { tenant_id :: TenantId
  , tenant_name :: Text
  , tenant_firstname :: Text
  , tenant_lastname :: Text
  , tenant_email :: Text
  , tenant_phone :: Text
  , tenant_status :: TenantStatus
  , tenant_ownerid :: Maybe UserId
  , tenant_backofficedomain :: Text
  } deriving (Show)

data UserStatus
  = UserStatusActive
  | UserStatusInActive
  | UserStatusBlocked
  deriving (Show)

newtype UserId =
  UserId Int
  deriving (Show)

data User = User
  { user_id :: UserId
  , user_tenantid :: TenantId
  , user_username :: Text
  , user_password :: Text
  , user_firstname :: Maybe Text
  , user_lastname :: Maybe Text
  , user_status :: UserStatus
  } deriving (Show)

data Permission = Read|Create|Update|Delete
  deriving (Show)

newtype RoleId =
  RoleId Int
  deriving (Show)

data Role = Role
  { role_id :: RoleId
  , role_tenantid :: TenantId
  , role_name :: Text
  , role_permission :: NonEmpty Permission
  } deriving (Show)
