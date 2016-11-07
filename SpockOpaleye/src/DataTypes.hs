{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import           CryptoDef
import           Data.List.NonEmpty
import           Data.Text
import           GHC.Generics
import           Data.Time(UTCTime)

data ValidationResult = Valid | Invalid
  deriving (Eq, Show)

newtype TenantId = TenantId Int
  deriving (Show, Generic)

data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew
  deriving (Show, Generic)

data TenantPoly key created_at updated_at name fname lname email phone status owner_id b_domain = Tenant
  { tenant_id               :: key
  , tenant_createdat        :: created_at
  , tenant_updatedat        :: updated_at
  , tenant_name             :: name
  , tenant_firstname        :: fname
  , tenant_lastname         :: lname
  , tenant_email            :: email
  , tenant_phone            :: phone
  , tenant_status           :: status
  , tenant_ownerid          :: owner_id
  , tenant_backofficedomain :: b_domain
  } deriving (Show, Generic)

type Tenant = TenantPoly TenantId  UTCTime UTCTime Text Text Text Text Text TenantStatus (Maybe UserId) Text

type TenantIncoming = TenantPoly () () () Text Text Text Text Text () (Maybe UserId) Text

data UserStatus = UserStatusActive | UserStatusInActive | UserStatusBlocked
  deriving (Show)

newtype UserId = UserId Int
  deriving (Show, Generic)

data UserPoly key tenant_id username password firstname lastname status  = User {
  user_id        :: key,
  user_tenantid  :: tenant_id,
  user_username  :: username,
  user_password  :: password,
  user_firstname :: firstname,
  user_lastname  :: lastname,
  user_status    :: status
}

type User = UserPoly UserId TenantId Text BcryptPassword (Maybe Text) (Maybe Text) UserStatus

data Permission = Read | Create | Update | Delete
  deriving (Show)

newtype RoleId = RoleId Int
  deriving (Show)

data RolePoly key tenant_id name permission = Role
  { role_id         :: key
  , role_tenantid   :: tenant_id
  , role_name       :: name
  , role_permission :: permission
  } deriving (Show)

type Role = RolePoly RoleId TenantId Text (NonEmpty Permission)
