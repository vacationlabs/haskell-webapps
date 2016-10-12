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

data Config = Config
    { authSettings :: AuthCookieSettings
    , randomSource :: RandomSource
    , serverKey :: ServerKey
    }

type App = ReaderT Config (ExceptT ServantErr IO)

