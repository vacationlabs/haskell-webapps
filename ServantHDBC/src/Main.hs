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
import Control.Monad.Trans.Reader
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Database.HDBC as DB
import Database.HDBC.PostgreSQL


-- ShoppingCart

type ShoppingCart = 
                 TenantAPI
            :<|> AuthAPI
            :<|> ProductAPI 
            :<|> PhotoAPI

serverT :: ServerT ShoppingCart (Reader DB.IConnection Connection)
serverT =  
             tenantHandlers 
        :<|> authHandlers
        :<|> productHandlers 
        :<|> photoHandlers

foo = undefined

server :: Server ShoppingCart
server = enter foo serverT

shoppingCart :: Proxy ShoppingCart
shoppingCart = Proxy

application :: Application
application = serve shoppingCart server


main :: IO ()
main = do
  putStrLn "starting ServantHDBC"
  conn <- connectPostgreSQL "host=localhost dbname=cart user=cart password=cart"
  select <- DB.prepare conn "select * from tenants"
  DB.execute select []
  results <- DB.fetchAllRows select
  putStrLn $ show results
  putStrLn "connected"
  run 8000 application
