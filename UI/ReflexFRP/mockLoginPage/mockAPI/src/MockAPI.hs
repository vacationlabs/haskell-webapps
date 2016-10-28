{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module MockAPI where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API

data User = User
  { userMail     :: Text
  , userPassword :: Text
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

type MockApi = "auth" :> ReqBody '[JSON] User :> Post '[JSON] Text
          :<|> "assets" :> Raw
          :<|> Raw
