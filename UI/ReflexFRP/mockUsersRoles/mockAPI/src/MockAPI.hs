{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module MockAPI where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API

import Permissions

data User = User
  { userMail     :: Text
  , userPassword :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

data Role = Role
  { roleName :: Text
  , rolePermission :: [Permission]
  , roleAssociatedUsers :: [User]
  } deriving (Show, Eq, Generic)

instance ToJSON Role
instance FromJSON Role

type MockApi = "auth" :> ReqBody '[JSON] User :> Post '[JSON] Text
          :<|> "assets" :> Raw
          :<|> Raw
