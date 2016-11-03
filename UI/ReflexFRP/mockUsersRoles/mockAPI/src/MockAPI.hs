{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MockAPI where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API
import Control.Lens
import Control.Lens.Wrapped

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Permissions

type Email = Text

data User = User
  { userMail :: Email
  } deriving (Show, Eq, Ord, Generic)

instance FromHttpApiData User where
  parseUrlPiece mail = Right (User mail)

instance ToHttpApiData User where
  toUrlPiece (User mail) = mail

instance ToJSON User
instance FromJSON User

type RoleName = Text

data RoleAttributes = RoleAttributes
  { _rolePermission      :: Set Permission
  , _roleAssociatedUsers :: Set User
  } deriving (Show, Eq, Ord, Generic)

makeLenses ''RoleAttributes

instance ToJSON RoleAttributes
instance FromJSON RoleAttributes

newtype Roles = Roles { unRoles :: Map RoleName RoleAttributes } deriving (Show, Eq, Ord, Generic)

instance Wrapped Roles where
  type Unwrapped Roles = Map RoleName RoleAttributes
  _Wrapped' = iso unRoles Roles

instance ToJSON Roles
instance FromJSON Roles

type MockApi = "delete" :> Capture "role" RoleName :> Capture "user" User :> Delete '[JSON] NoContent
          :<|> "roles" :> Get '[JSON] Roles
          :<|> "assets" :> Raw
          :<|> Raw
