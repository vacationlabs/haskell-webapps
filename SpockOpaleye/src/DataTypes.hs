{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module DataTypes where

import           CryptoDef
import           Data.List.NonEmpty
import           Data.Text
import           Data.Time          (UTCTime)
import           GHC.Generics
import Control.Lens

data ValidationResult = Valid | Invalid
  deriving (Eq, Show)

newtype TenantId = TenantId Int
  deriving (Show, Generic)

data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew
  deriving (Show, Generic)

data TenantPoly key created_at updated_at name fname lname email phone status owner_id b_domain = Tenant {
    tenant_id               :: key
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

data UserPoly key created_at updated_at tenant_id username password firstname lastname status  = User {
    user_id        :: key
  , user_createdat :: created_at
  , user_updatedat :: updated_at
  , user_tenantid  :: tenant_id
  , user_username  :: username
  , user_password  :: password
  , user_firstname :: firstname
  , user_lastname  :: lastname
  , user_status    :: status
}

type User = UserPoly UserId UTCTime UTCTime TenantId Text BcryptPassword (Maybe Text) (Maybe Text) UserStatus

type UserIncoming = UserPoly () () () TenantId Text Text (Maybe Text) (Maybe Text) ()

data Permission = Read | Create | Update | Delete
  deriving (Show)

newtype RoleId = RoleId Int
  deriving (Show)

data RolePoly key tenant_id name permission created_at updated_at  = Role {
    _rolepolyId         :: key
  , _rolepolyTenantid   :: tenant_id
  , _rolepolyName       :: name
  , _rolepolyPermission :: permission
  , _rolepolyCreatedat  :: created_at
  , _rolepolyUpdatedat  :: updated_at
} deriving (Show)

type Role = RolePoly RoleId TenantId Text (NonEmpty Permission) UTCTime UTCTime 

makeFields ''RolePoly
