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

-- Products

data Product = Product
    { ids               :: String
    , q                 :: String
    , title             :: String
    , sku               :: String
    , prod_type         :: String
    , tags              :: String
    , created_at_min    :: String
    , created_at_max    :: String
    , updated_at_min    :: String
    , updated_at_max    :: String
    , limit             :: String
    , limit_count       :: Int
    , offset            :: Int
    , orderby           :: String
    , fields            :: String
    } deriving (Generic)

instance FromJSON Product -- where 
 --   parseJSON (Object v) = Product "" "" "" "" "" "" "" "" "" "" "" 0 0 "" ""


type ProductResult = String
type ProductAPI = "products" :>
                (Capture "product_id" Int :>
                    QueryParam "fields" String :>
                    QueryParam "photo_sizes" String :>
                    QueryParam "varants.photo_sizes" ProductResult :>
                        Get '[JSON] String 
            :<|> QueryParam "ids" String :>
                 QueryParam "q" String :>
                 QueryParam "title" String :>
                 QueryParam "sku" String :>
                 QueryParam "type" String :>
                 QueryParam "tags" String :>
                 QueryParam "created_at_min" String :>
                 QueryParam "created_at_max" String :>
                 QueryParam "updated_at_min" String :>
                 QueryParam "updated_at_max" String :>
                 QueryParam "limit" String :>
                 QueryParam "limit" Int :>
                 QueryParam "offset" Int :>
                 QueryParam "orderby" String :>
                 QueryParam "fields" String :>
                    Get '[JSON] ProductResult 
            :<|> "new" :>
                 ReqBody '[JSON] Product :>
                    Post '[JSON] ProductResult)

productGet id fields photo_sizes variants  = return $ "get a product from id " ++ show id

productList ids q title sku item_type tags created_at_mix created_at_max
    updated_at_min updated_at_max limit limit_count offset orderby fields = return "list products by query params"

productNew :: Product -> ExceptT ServantErr IO String
productNew product = return "create a new product"

productHandlers = productGet :<|> productList :<|> productNew 

-- Photos

type PhotoAPI = "photos" :>
                (Capture "path_segment_1" String :>
                 Capture "path_segment_2" String :>
                 Capture "geometry_or_style" String :>
                 Capture "original_filename" String :>
                    Get '[JSON] String)

photoGet path1 path2 geometry filename = return "return a photo"

photoHandlers = photoGet

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
