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

tenantNew :: ExceptT ServantErr IO String
tenantNew = return "new tenant"

tenantGet :: Int -> ExceptT ServantErr IO String
tenantGet x = return $ "get tenant " ++ show x

tenantActivate :: Int -> ExceptT ServantErr IO String
tenantActivate x = return $ "activating " ++ show x

-- Authentication




type ShoppingCart = TenantAPI

server :: Server ShoppingCart
server = tenantNew :<|> tenantGet :<|> tenantActivate

shoppingCart :: Proxy ShoppingCart
shoppingCart = Proxy

application :: Application
application = serve shoppingCart server

main :: IO ()
main = do
  putStrLn "starting ServantHDBC"
  run 8000 application
