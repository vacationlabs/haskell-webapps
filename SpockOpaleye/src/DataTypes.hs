module DataTypes where

import Data.List.NonEmpty
import Data.Text
import CryptoDef

data ValidationResult = Valid | Invalid

newtype TenantId =
  TenantId Int
  deriving (Show)

data TenantStatus
  = TenantStatusActive
  | TenantStatusInActive
  | TenantStatusNew
  deriving (Show)

data TenantPoly key name fname lname email phone status owner_id b_domain = Tenant
  { tenant_id :: key
  , tenant_name :: name
  , tenant_firstname :: fname
  , tenant_lastname :: lname
  , tenant_email :: email
  , tenant_phone :: phone
  , tenant_status :: status
  , tenant_ownerid :: owner_id
  , tenant_backofficedomain :: b_domain
  } deriving (Show)

type Tenant = TenantPoly TenantId Text Text Text Text Text TenantStatus (Maybe UserId) Text

type TenantIncoming = TenantPoly () Text Text Text Text Text () (Maybe UserId) Text

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
  , user_password :: BcryptPassword
  , user_firstname :: Maybe Text
  , user_lastname :: Maybe Text
  , user_status :: UserStatus
  } deriving (Show)

data Permission
  = Read
  | Create
  | Update
  | Delete
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
