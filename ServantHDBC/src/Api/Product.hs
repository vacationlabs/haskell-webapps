{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Api.Product (ProductAPI, productHandlers) where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Servant


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

instance FromJSON Product 

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

productGet :: Int -> Maybe String -> Maybe String -> Maybe String -> ExceptT ServantErr IO String
productGet id fields photo_sizes variants  = return $ "get a product from id " ++ show id

productList
  :: Maybe String
     -> Maybe String
     -> Maybe String
     -> Maybe String 
     -> Maybe String 
     -> Maybe String
     -> Maybe String
     -> Maybe String
     -> Maybe String
     -> Maybe String
     -> Maybe String
     -> Maybe Int
     -> Maybe Int
     -> Maybe String
     -> Maybe String
     -> ExceptT ServantErr IO String

productList ids q title sku item_type tags created_at_mix created_at_max
    updated_at_min updated_at_max limit limit_count offset orderby fields = return "list products by query params"

productNew :: Product -> ExceptT ServantErr IO String
productNew product = return "create a new product"

productHandlers = productGet :<|> productList :<|> productNew 

