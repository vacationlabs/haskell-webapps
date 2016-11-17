{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module DataTypes where

import           Control.Lens
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           CryptoDef
import           Data.List.NonEmpty
import           Data.Text
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM

type AppM a = WriterT String (ReaderT (Connection, Maybe (Auditable Tenant), Maybe (Auditable User)) IO) a

getConnection :: AppM Connection
getConnection = do
  (conn, _, _) <- R.ask
  return conn

getCurrentTenant :: AppM (Maybe (Auditable Tenant))
getCurrentTenant = do
  (_, tenant, _) <- R.ask
  return tenant

getCurrentUser :: AppM (Maybe (Auditable User))
getCurrentUser = do
  (_, _, user) <- R.ask
  return user

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


type Tenant = TenantPoly TenantId UTCTime UTCTime Text Text Text Text Text TenantStatus (Maybe UserId) Text

type TenantIncoming = TenantPoly () () () Text Text Text Text Text () (Maybe UserId) Text
type TenantIncomingCreatable = TenantPoly () UTCTime UTCTime Text Text Text Text Text () (Maybe UserId) Text

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
} deriving (Show)

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
type RoleIncoming = RolePoly () TenantId Text (NonEmpty Permission) () ()

data Auditable a = Auditable { _data:: a, _log:: Value }  deriving (Show)

auditable :: a -> Auditable a
auditable a = Auditable {_data = a, _log = Object HM.empty}

type TenantA = Auditable Tenant
type RoleA = Auditable Role
type UserA = Auditable User

wrapAuditable :: (Functor a, Functor b) => a (b c) -> a (b (Auditable c))
wrapAuditable a = (fmap auditable) <$> a

makeLensesWith abbreviatedFields ''RolePoly
makeLensesWith abbreviatedFields ''TenantPoly
makeLensesWith abbreviatedFields ''UserPoly
