{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api.Authentication
import Api.Photo
import Api.Product
import Api.Tenant
import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


-- Photos


-- ShoppingCart

type ShoppingCart = 
                 TenantAPI
            :<|> AuthAPI
            :<|> ProductAPI 
            :<|> PhotoAPI

server :: Server ShoppingCart
server =  
             tenantHandlers 
        :<|> authHandlers
        :<|> productHandlers 
        :<|> photoHandlers

shoppingCart :: Proxy ShoppingCart
shoppingCart = Proxy

application :: Application
application = serve shoppingCart server

main :: IO ()
main = do
  putStrLn "starting ServantHDBC"
  run 8000 application
