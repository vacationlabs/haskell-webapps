{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


-- Tenants

type Tenant = String
type TenantAPI = "tenants" :> 
                         ("new" :> Post '[JSON] Tenant
                          :<|> Capture "x" Int :> Get '[JSON] Tenant
                          :<|> Capture "x" Int :> "activate" :> Post '[JSON] Tenant)

tenantNew :: ExceptT ServantErr IO Tenant
tenantNew = return "new tenant"

tenantGet :: Int -> ExceptT ServantErr IO Tenant 
tenantGet x = return $ "get tenant " ++ show x

tenantActivate :: Int -> ExceptT ServantErr IO Tenant
tenantActivate x = return $ "activating " ++ show x

tenantHandlers = tenantNew :<|> tenantGet :<|> tenantActivate

-- Authentication

type Auth = String
type AuthAPI = "sessions" :>
                         ("new" :> Post '[JSON] Auth
                          :<|> "refresh" :> Post '[JSON] Auth
                          :<|> "destroy" :> Post '[JSON] Auth)

authNew :: ExceptT ServantErr IO Auth
authNew = return "should create a cookie"

authRefresh :: ExceptT ServantErr IO Auth
authRefresh = return "should refresh session timeout"

authDestroy :: ExceptT ServantErr IO Auth
authDestroy = return "should destroy the session"

authHandlers = authNew :<|> authRefresh :<|> authDestroy


type ShoppingCart = TenantAPI :<|> AuthAPI

server :: Server ShoppingCart
server =  tenantHandlers :<|> authHandlers

shoppingCart :: Proxy ShoppingCart
shoppingCart = Proxy

application :: Application
application = serve shoppingCart server

main :: IO ()
main = do
  putStrLn "starting ServantHDBC"
  run 8000 application
