{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module ProductQuery where

import           Control.Lens
import           Data.Aeson
import           Data.Function
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Database.Persist
import           Models
import           Servant.API
import           Types

data ProductFilter =
    ProductF { getProductFilter :: [Filter DBProduct]
             , otherFilters     :: DBTenant -> All
             }

instance Monoid ProductFilter where
    mempty = ProductF [] mempty
    (ProductF a f) `mappend` (ProductF c g) = ProductF (a++c) (f <> g)

data ProductView =
    ProductView { getProductFields :: DBProduct -> AppJSON
                , getVariantFields :: DBVariant -> AppJSON
                }

instance Monoid ProductView where
    mempty = ProductView mempty mempty
    (ProductView a b) `mappend` (ProductView c d) =
        ProductView (a <> c) (b <> d)

newtype ProductComparator =
    ProductC { compareProducts :: DBProduct -> DBProduct -> Ordering } deriving (Monoid)


textSplit :: Char -> Text -> (Text, Text)
textSplit c s = (x, T.tail y)
    where (x, y) = T.break (==c) s

-- Query Format /products?filter=title:sometitle&filter=type:physical

postgreAny s = "ANY (" ++ s ++ ")"

instance FromHttpApiData ProductFilter where
    parseQueryParam s =
        case textSplit ':' s of
            ("title",r) -> Right $ ProductF [DBProductName ==. r] mempty
            --("sku"  ,r) -> Right $ ProductF [DBProductVariantSkus ==. postgreAny r] mempty
            ("createdAtMin"  ,r) -> do
                t <- parseQueryParam r
                return $ ProductF [DBProductCreatedAt >=. t] mempty
            ("updatedAtMin"  ,r) -> do
                t <- parseQueryParam r
                return $ ProductF [DBProductUpdatedAt >=. t] mempty
            ("createdAtMax"  ,r) -> do
                t <- parseQueryParam r
                return $ ProductF [DBProductCreatedAt <=. t] mempty
            ("updatedAtMax"  ,r) -> do
                t <- parseQueryParam r
                return $ ProductF [DBProductUpdatedAt <=. t] mempty
            _ -> Left "Couldn't parse product filters"

makeViewProd :: ToJSON a => Text -> (DBProduct -> a) -> ProductView
makeViewProd field psel = flip ProductView mempty $ \prod ->
    JSON $ object [(field, toJSON (psel prod))]

makeViewVar :: ToJSON a => Text -> (DBVariant -> a) -> ProductView
makeViewVar field vsel = ProductView mempty $ \var ->
    JSON $ object [(field, toJSON (vsel var))]

-- Query Format /products?field=name&field=type

instance FromHttpApiData ProductView where
    parseQueryParam s =
        case s of
            "name" -> Right $ makeViewProd s (view dBProductName)
            "description" -> Right $ makeViewProd s (view dBProductDescription)
            "currency" -> Right $ makeViewProd s (view dBProductCurrency)
            "advertised_price" -> Right $ makeViewProd s (view dBProductAdvertisedPrice)
            "comparision_price" -> Right $ makeViewProd s (view dBProductComparisionPrice)
            "cost_price" -> Right $ makeViewProd s (view dBProductCostPrice)
            "type" -> Right $ makeViewProd s (view dBProductProductType)
            "properties" -> Right $ makeViewProd s (view dBProductProperties)
            "url_slug" -> Right $ makeViewProd s (view dBProductUrlSlug)
            "tenant_id" -> Right $ makeViewProd s (view dBProductTenantID)
            "created_at" -> Right $ makeViewProd s (view dBProductCreatedAt)
            "updated_at" -> Right $ makeViewProd s (view dBProductUpdatedAt)
            "variant.name" -> Right $ makeViewVar s (view dBVariantName)
            "variant.sku" -> Right $ makeViewVar s (view dBVariantSku)
            "variant.price" -> Right $ makeViewVar s (view dBVariantPrice)
            "variant.weight_in_grams" -> Right $ makeViewVar s (view dBVariantWeightInGrams)
            "variant.weight_display_unit" -> Right $ makeViewVar s (view dBVariantWeightDisplayUnit)
            _ -> Left "Couldn't parse fields"

instance FromHttpApiData ProductComparator where
    parseQueryParam s =
        case s of
            "name" -> Right $ ProductC (compare `on` view dBProductName)
            "description" -> Right $ ProductC (compare `on` view dBProductDescription)
            "currency" -> Right $ ProductC (compare `on` view dBProductCurrency)
            "advertised_price" -> Right $ ProductC (compare `on` view dBProductAdvertisedPrice)
            "comparision_price" -> Right $ ProductC (compare `on` view dBProductComparisionPrice)
            "cost_price" -> Right $ ProductC (compare `on` view dBProductCostPrice)
            "url_slug" -> Right $ ProductC (compare `on` view dBProductUrlSlug)
            "tenant_id" -> Right $ ProductC (compare `on` view dBProductTenantID)
            "created_at" -> Right $ ProductC (compare `on` view dBProductCreatedAt)
            "updated_at" -> Right $ ProductC (compare `on` view dBProductUpdatedAt)
            _ -> Left "Couldn't parse fields"


