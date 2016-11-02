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

data User = User
  { userMail :: Text
  } deriving (Show, Eq, Ord, Generic)

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

exRoles = Roles $ Map.singleton "Account administrator" (RoleAttributes undefined users)
  where users = Set.fromList [ User "admin@mydomain.com"
                             , User "otheradmin@mydomain.com"
                             , User "yetanotheradmin@mydomain.com"
                             ]

instance ToJSON Roles
instance FromJSON Roles

type MockApi = "deleteUserRole" :> ReqBody '[JSON] RoleName :> "user" :> ReqBody '[JSON] User :> Delete '[JSON] ()
          :<|> "assets" :> Raw
          :<|> Raw
