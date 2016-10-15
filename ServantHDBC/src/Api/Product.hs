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
import Data.Text
import Data.Time.Clock
import GHC.Generics
import Servant
import Web.HttpApiData



data Product = Product
    { id                :: String
    , tenant_id         :: String
    , name              :: String
    , description       :: String
    , url_slug          :: String
    , tags              :: String
    , currency          :: String
    , advertised_price  :: Int
    , comparison_price  :: Int
    , cost_price        :: Int
    , product_type      :: String
    , is_published      :: Bool
    , properties        :: String
    } deriving (Generic)

instance FromJSON Product 

data Field = Field String

instance FromHttpApiData Field where
    parseUrlPiece x = fmap Field (parseUrlPiece x :: Either Text String)


data ProductId = ProductId Int 

instance FromHttpApiData ProductId where
    parseUrlPiece x = fmap ProductId (parseUrlPiece x :: Either Text Int)

data Sku = Sku String

instance FromHttpApiData Sku where
    parseUrlPiece x = fmap Sku (parseUrlPiece x :: Either Text String)

data ProductType = ProductType String

instance FromHttpApiData ProductType where
    parseUrlPiece x = fmap ProductType (parseUrlPiece x :: Either Text String)

data ProductOrdering = ProductOrdering Int 

instance FromHttpApiData ProductOrdering where
    parseUrlPiece x = fmap ProductOrdering (parseUrlPiece x :: Either Text Int)

type ProductAPI = "products" :>
                (Capture "product_id" Int :>
                    QueryParams "fields" Field :>
                    QueryParam "photo_sizes" String :>
                    QueryParam "varants.photo_sizes" String :>
                        Get '[JSON] String 
            :<|> QueryParams "ids" ProductId :>
                 QueryParam "q" String :>
                 QueryParam "title" String :> 
                 QueryParam "sku" Sku :>
                 QueryParam "type" ProductType :>
                 QueryParam "tags" String :>
                 QueryParam "created_at_min" UTCTime :>
                 QueryParam "created_at_max" UTCTime :>
                 QueryParam "updated_at_min" UTCTime :>
                 QueryParam "updated_at_max" UTCTime :>
                 QueryParam "limit" Int :>
                 QueryParam "offset" Int :>
                 QueryParam "orderby" ProductOrdering :>
                 QueryParams "fields" Field :>
                    Get '[JSON] String
            :<|> "new" :>
                 ReqBody '[JSON] Product :>
                    Post '[JSON] String )

productGet :: Int -> [Field] -> Maybe String -> Maybe String -> ExceptT ServantErr IO String
productGet id fields photo_sizes variants  = return $ "get a product from id " ++ show id

productList
  :: [ProductId]
     -> Maybe String
     -> Maybe String
     -> Maybe Sku
     -> Maybe ProductType
     -> Maybe String
     -> Maybe UTCTime
     -> Maybe UTCTime
     -> Maybe UTCTime
     -> Maybe UTCTime
     -> Maybe Int
     -> Maybe Int
     -> Maybe ProductOrdering 
     -> [Field]
     -> ExceptT ServantErr IO String

productList ids q title sku item_type tags created_at_mix created_at_max
    updated_at_min updated_at_max limit offset orderby fields = return "list products by query params"

productNew :: Product -> ExceptT ServantErr IO String
productNew product = return "create a new product"

productHandlers = productGet :<|> productList :<|> productNew 

