{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Auth
    where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Data.ByteString
import Data.Proxy
import GHC.Generics
import Data.Serialize
import Data.Text

type AppAuth = AuthProtect "cookie-auth"

data Session = Session { username :: String
                       , email :: String
                       } deriving (Show, Generic)

instance Serialize Session
instance FromJSON Session
instance ToJSON Session



