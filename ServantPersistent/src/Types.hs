{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Types
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
import Control.Monad.Except
import Control.Monad.Reader
import Data.Serialize
import Data.Text

data Environment = Test | Devel | Production deriving (Eq, Show)

data CookieError = NotPresent | AuthError AuthCookieException deriving (Eq, Show)

data Config = Config
    { authSettings :: AuthCookieSettings
    , randomSource :: RandomSource
    , serverKey :: ServerKey
    , environment :: Environment
    }

type App = ReaderT Config (ExceptT ServantErr IO)

data LoginForm = Login { username :: String
                       , password :: String
                       } deriving (Show, Generic)

instance Serialize LoginForm
instance FromJSON LoginForm
instance ToJSON LoginForm

data Session = Session { sessionUser :: String
                       } deriving (Show, Generic)

instance Serialize Session
instance FromJSON Session
instance ToJSON Session



