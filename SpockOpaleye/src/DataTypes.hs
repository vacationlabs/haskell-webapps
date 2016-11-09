{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataTypes where

import           Control.Lens
import           CryptoDef
import           Data.List.NonEmpty
import           Data.Text
import           Data.Time          (UTCTime)
import           GHC.Generics

data ValidationResult = Valid | Invalid
  deriving (Eq, Show)

newtype TenantId = TenantId Int
  deriving (Show, Generic)

data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew
  deriving (Show, Generic)

data TenantPoly key created_at updated_at name fname lname email phone status owner_id b_domain = Tenant {
    _tenantpolyId               :: key
  , _tenantpolyCreatedat        :: created_at
  , _tenantpolyUpdatedat        :: updated_at
  , _tenantpolyName             :: name
  , _tenantpolyFirstname        :: fname
  , _tenantpolyLastname         :: lname
  , _tenantpolyEmail            :: email
  , _tenantpolyPhone            :: phone
  , _tenantpolyStatus           :: status
  , _tenantpolyOwnerid          :: owner_id
  , _tenantpolyBackofficedomain :: b_domain
} deriving (Show, Generic)


type Tenant = TenantPoly TenantId (Maybe UTCTime) (Maybe UTCTime) Text Text Text Text Text TenantStatus (Maybe UserId) Text

type TenantIncoming = TenantPoly () (Maybe UTCTime) (Maybe UTCTime) Text Text Text Text Text () (Maybe UserId) Text

data UserStatus = UserStatusActive | UserStatusInActive | UserStatusBlocked
  deriving (Show)

newtype UserId = UserId Int
  deriving (Show, Generic)

data UserPoly key created_at updated_at tenant_id username password firstname lastname status  = User {
    _userpolyId        :: key
  , _userpolyCreatedat :: created_at
  , _userpolyUpdatedat :: updated_at
  , _userpolyTenantid  :: tenant_id
  , _userpolyUsername  :: username
  , _userpolyPassword  :: password
  , _userpolyFirstname :: firstname
  , _userpolyLastname  :: lastname
  , _userpolyStatus    :: status
}

type User = UserPoly UserId (Maybe UTCTime) (Maybe UTCTime) TenantId Text BcryptPassword (Maybe Text) (Maybe Text) UserStatus

type UserIncoming = UserPoly () (Maybe UTCTime) (Maybe UTCTime) TenantId Text Text (Maybe Text) (Maybe Text) ()

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

type Role = RolePoly RoleId TenantId Text (NonEmpty Permission) (Maybe UTCTime) (Maybe UTCTime)
type RoleIncoming = RolePoly () TenantId Text (NonEmpty Permission) (Maybe UTCTime) (Maybe UTCTime)

makeLensesWith abbreviatedFields ''RolePoly
makeLensesWith abbreviatedFields ''TenantPoly
makeLensesWith abbreviatedFields ''UserPoly
