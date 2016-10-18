{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module API
    where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.ByteString
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Reader
import Servant.Server.Experimental.Auth.Cookie
import Auth
import Types
import Models

type ProductID = Int
type ActivationRequest = ()
type ActivationResponse = ()
type Product = ()

type TenantAPI = 
      "new" :> ReqBody '[JSON] TenantInput 
                :> Post '[JSON] (Headers '[Header "location" String] TenantID)
 :<|> Capture "id" TenantID  :> Get '[JSON] TenantOutput
--      :<|> Capture "id" TenantID  :> "activate" :> ReqBody '[JSON] ActivationRequest :> Post '[JSON] ActivationResponse

type SessionAPI = 
      "new" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "set-cookie" ByteString] ())
-- :<|> "refresh" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "set-cookie" ByteString] ())
-- :<|> "destroy" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "set-cookie" ByteString] ())

type ProductAPI = 
      Capture "id" ProductID :> Get '[JSON] Product
 :<|> Get '[JSON] [Product]

type API = "tenants"  :> TenantAPI
      :<|> "session"  :> SessionAPI
      :<|> "products" :> ProtectEndpoints ProductAPI

api :: Proxy API
api = Proxy

