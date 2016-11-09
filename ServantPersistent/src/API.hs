{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module API
    where

import Servant
import Data.ByteString

import Auth
import DBTypes
import Types
import ProductQuery

type ActivationRequest = ()
type ActivationResponse = ()

type TenantAPI = 
      "new" :> ReqBody '[JSON] TenantInput 
                :> Post '[JSON] (Headers '[Header "location" String] TenantId)
 :<|> Capture "id" TenantId  :> Get '[JSON] TenantOutput
--      :<|> Capture "id" TenantID  :> "activate" :> ReqBody '[JSON] ActivationRequest :> Post '[JSON] ActivationResponse

type SessionAPI = 
      "new" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "set-cookie" ByteString] ())
-- :<|> "refresh" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "set-cookie" ByteString] ())
-- :<|> "destroy" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Headers '[Header "set-cookie" ByteString] ())

type ProductAPI =
      Capture "id" ProductId :> Get '[JSON] Product
 :<|> QueryParams "filter" ProductFilter :> QueryParams "field" ProductView :> Get '[JSON] [Product]

type API = "tenants"  :> TenantAPI
      :<|> "session"  :> SessionAPI
      :<|> "products" :> ProtectEndpoints ProductAPI

api :: Proxy API
api = Proxy

