{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Scientific
import           Data.ByteString hiding (putStrLn)
import           Data.Text
import           Data.Time
import           Opaleye

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (Conversion,
                                                       FromField (..),
                                                       ResultError (..),
                                                       returnError)

import           Control.Arrow
import           Prelude                              hiding (id)


readOnly :: String -> TableProperties () (Column a)
readOnly = lmap (const Nothing) . optional

-- Tenant stuff

newtype TenantId = TenantId Int deriving(Show)

data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew
  deriving (Show)

data TenantPoly key name fname lname email phone status b_domain = Tenant
  { tenant_id               :: key
  , tenant_name             :: name
  , tenant_firstname        :: fname
  , tenant_lastname         :: lname
  , tenant_email            :: email
  , tenant_phone            :: phone
  , tenant_status           :: status
  , tenant_backofficedomain :: b_domain
  } deriving (Show)

type Tenant = TenantPoly TenantId Text Text Text Text Text TenantStatus Text
type TenantIncoming = TenantPoly () Text Text Text Text Text TenantStatus Text

type TenantTableW = TenantPoly
  ()
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)

type TenantTableR = TenantPoly
  (Column PGInt4)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)
  (Column PGText)

-- Product stuff

newtype ProductId = ProductId Int deriving (Show)

data ProductType = ProductPhysical | ProductDigital deriving (Show)

data ProductProperties = ProductProperties { product_color :: String, product_weight :: String} deriving (Show)

data ProductPoly id created_at updated_at tenant_id name description url_slug tags currency advertised_price comparison_price cost_price product_type is_published properties = Product {
      product_id               :: id
    , product_created_at       :: created_at
    , product_updated_at       :: updated_at
    , product_tenant_id        :: tenant_id
    , product_name             :: name
    , product_description      :: description
    , product_url_slug         :: url_slug
    , product_tags             :: tags
    , product_currency         :: currency
    , product_advertised_price :: advertised_price
    , product_comparison_price :: comparison_price
    , product_cost_price       :: cost_price
    , product_product_type     :: product_type
    , product_is_published     :: is_published
    , product_properties       :: properties
  } deriving (Show)

type Product = ProductPoly ProductId UTCTime UTCTime TenantId Text (Maybe Text) Text [Text] Text Scientific Scientific (Maybe Scientific) ProductType Bool ProductProperties
type ProductTableW = ProductPoly
  (Maybe (Column PGInt4))
  (Maybe (Column PGTimestamptz))
  (Maybe (Column PGTimestamptz))
  (Column PGInt4)
  (Column PGText)
  (Maybe (Column (Nullable PGText)))
  (Column PGText)
  (Column (PGArray PGText))
  (Column PGText)
  (Column PGFloat8)
  (Column PGFloat8)
  (Maybe (Column (Nullable PGFloat8)))
  (Column PGText)
  (Column PGBool)
  (Column PGJsonb)

type ProductTableR = ProductPoly
  (Column PGInt4)
  (Column PGTimestamptz)
  (Column PGTimestamptz)
  (Column PGInt4)
  (Column PGText)
  (Column (Nullable PGText))
  (Column PGText)
  (Column (PGArray PGText))
  (Column PGText)
  (Column PGFloat8)
  (Column PGFloat8)
  (Column (Nullable PGFloat8))
  (Column PGText)
  (Column PGBool)
  (Column PGJsonb)

-- Table defs

$(makeAdaptorAndInstance "pTenant" ''TenantPoly)
tenantTable :: Table TenantTableW TenantTableR
tenantTable = Table "tenants" (pTenant
   Tenant {
     tenant_id = (readOnly "id"),
     tenant_name = (required "name"),
     tenant_firstname = (required "first_name"),
     tenant_lastname = (required "last_name"),
     tenant_email = (required "email"),
     tenant_phone = (required "phone"),
     tenant_status = (required "status"),
     tenant_backofficedomain = (required "backoffice_domain")
   }
 )

$(makeAdaptorAndInstance "pProduct" ''ProductPoly)

productTable :: Table ProductTableW ProductTableR
productTable = Table "products" (pProduct
    Product {
      product_id = (optional "id"),
      product_created_at = (optional "created_at"),
      product_updated_at = (optional "updated_at"),
      product_tenant_id = (required "tenant_id"),
      product_name = (required "name"),
      product_description = (optional "description"),
      product_url_slug = (required "url_slug"),
      product_tags = (required "tags"),
      product_currency = (required "currency"),
      product_advertised_price = (required "advertised_price"),
      product_comparison_price = (required "comparison_price"),
      product_cost_price = (optional "cost_price"),
      product_product_type = (required "type"),
      product_is_published = (required "is_published"),
      product_properties = (required "properties") })

-- Instance declarations for custom types
-- For TenantStatus

instance FromField TenantStatus where
  fromField field mb_bytestring = makeTenantStatus mb_bytestring
    where
    makeTenantStatus :: Maybe ByteString -> Conversion TenantStatus
    makeTenantStatus (Just "active") = return TenantStatusActive
    makeTenantStatus (Just "inactive") = return TenantStatusInActive
    makeTenantStatus (Just "new") = return TenantStatusNew
    makeTenantStatus (Just _) = returnError ConversionFailed field "Unrecognized tenant status"
    makeTenantStatus Nothing = returnError UnexpectedNull field "Empty tenant status"

instance QueryRunnerColumnDefault PGText TenantStatus where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- For ProductType

instance FromField ProductType where
  fromField field mb_bytestring = makeProductType mb_bytestring
    where
    makeProductType :: Maybe ByteString -> Conversion ProductType
    makeProductType (Just "physical") = return ProductPhysical
    makeProductType (Just "digital") = return ProductDigital
    makeProductType (Just _) = returnError ConversionFailed field "Unrecognized product type"
    makeTenantStatus Nothing = returnError UnexpectedNull field "Empty product type"

instance QueryRunnerColumnDefault PGText ProductType where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- For productId

instance FromField ProductId where
  fromField field mb_bytestring = ProductId <$> fromField field mb_bytestring

instance QueryRunnerColumnDefault PGInt4 ProductId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
-- For TenantId
instance FromField TenantId where
  fromField field mb_bytestring = TenantId <$> fromField field mb_bytestring

instance QueryRunnerColumnDefault PGInt4 TenantId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- For Scientific we didn't have to implement instance of fromField
-- because it is already defined in postgresql-simple

instance QueryRunnerColumnDefault PGFloat8 Scientific where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- Default instance definitions for custom datatypes for converison to
-- PG types while writing into tables

-- For Tenant stuff

instance Default Constant TenantStatus (Column PGText) where
  def = Constant def'
    where
      def' :: TenantStatus -> (Column PGText)
      def' TenantStatusActive = pgStrictText "active"
      def' TenantStatusInActive = pgStrictText "inactive"
      def' TenantStatusNew = pgStrictText "new"

instance Default Constant TenantId (Maybe (Column PGInt4)) where
  def = Constant (\(TenantId x) -> Just $ pgInt4 x)

-- For Product stuff

instance Default Constant ProductType (Column PGText) where
  def = Constant def'
    where
      def' :: ProductType -> (Column PGText)
      def' ProductDigital = pgStrictText "digital"
      def' ProductPhysical = pgStrictText "physical"

instance Default Constant ProductId (Maybe (Column PGInt4)) where
  def = Constant (\(ProductId x) -> Just $ constant x)

instance Default Constant Scientific (Column PGFloat8) where
  def = Constant (pgDouble.toRealFloat)

instance Default Constant Scientific (Column (Nullable PGFloat8)) where
  def = Constant (toNullable.constant)

instance Default Constant Text (Column (Nullable PGText)) where
  def = Constant (toNullable.pgStrictText)

instance Default Constant UTCTime (Maybe (Column PGTimestamptz)) where
  def = Constant ((Just).pgUTCTime)

instance Default Constant TenantId (Column PGInt4) where
  def = Constant (\(TenantId x) -> constant x)

-- FromJSON/ToJSON instances for properties

instance FromJSON ProductProperties where
  parseJSON (Object v) = ProductProperties <$> v .: "color" <*> v .: "weight"
  parseJSON invalid = typeMismatch "Unrecognized format for product properties" invalid

instance ToJSON ProductProperties where
  toJSON ProductProperties {product_color = color, product_weight = weight} = object ["color" .= color, "weight" .= weight]

instance FromField ProductProperties where
  fromField field mb = do
    v <-  fromField field mb
    valueToProductProperties v
    where
      valueToProductProperties :: Value -> Conversion ProductProperties
      valueToProductProperties v = case fromJSON v of
        Success a -> return a
        Error err -> returnError ConversionFailed field "Cannot parse product properties"

instance QueryRunnerColumnDefault PGJsonb ProductProperties where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant ProductProperties (Column PGJsonb) where
  def = Constant (\pp -> pgValueJSONB $ toJSON pp)

getProducts :: IO [Product]
getProducts = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ queryTable productTable

getTenants :: IO [Tenant]
getTenants = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ queryTable tenantTable

insertTenant :: IO ()
insertTenant = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runInsertManyReturning conn tenantTable [constant getTestTenant] (\x -> x) :: IO [Tenant]
  return ()

insertProduct :: IO ()
insertProduct = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  product <- getTestProduct
  runInsertManyReturning conn productTable [constant product] (\x -> x) :: IO [Product]
  return ()
  
getTestTenant :: TenantIncoming
getTestTenant = Tenant {
  tenant_id = (),
  tenant_name = "Tenant Bob",
  tenant_firstname = "Bobby",
  tenant_lastname = "Bob",
  tenant_email = "bob@gmail.com",
  tenant_phone = "2255",
  tenant_status = TenantStatusInActive,
  tenant_backofficedomain = "bob.com"
}

getTestProduct :: IO Product
getTestProduct = do
  time <- getCurrentTime
  let properties =  ProductProperties { product_color = "red", product_weight = "200gm"}
  return $ Product {
    product_id = (ProductId 5),
    product_created_at = time,
    product_updated_at = time,
    product_tenant_id = (TenantId 5),
    product_name = "snacks",
    product_description = Just "",
    product_url_slug = "",
    product_tags = ["tag1", "tag2"],
    product_currency = "INR",
    product_advertised_price = 30,
    product_comparison_price = 45,
    product_cost_price = Nothing,
    product_product_type = ProductPhysical,
    product_is_published = False,
    product_properties = properties
  }

main :: IO ()
main = do
  insertTenant
  insertProduct
  tenants <- getTenants
  products <- getProducts
  putStrLn $ show tenants
  putStrLn $ show products

-- Output
-- 
-- 
-- [Tenant {tenant_id = TenantId 1, tenant_name = "Tenant John", tenant_firstname = "John", tenant_lastname = "Honai", te
-- nant_email = "john@mail.com", tenant_phone = "2255", tenant_status = TenantStatusInActive, tenant_backofficedomain = "
-- jhonhonai.com"},Tenant {tenant_id = TenantId 5, tenant_name = "Tenant Bob", tenant_firstname = "Bobby", tenant_lastnam
-- e = "Bob", tenant_email = "bob@mail.com", tenant_phone = "2255", tenant_status = TenantStatusInActive, tenant_backoffi
-- cedomain = "bob.com"}]
-- [Product {product_id = ProductId 5, product_created_at = 2016-11-28 12:31:40.085634 UTC, product_updated_at = 2016-11-
-- 28 12:31:40.085634 UTC, product_tenant_id = TenantId 5, product_name = "snacks", product_description = Just "", produc
-- t_url_slug = "", product_tags = ["tag1","tag2"], product_currency = "INR", product_advertised_price = 30.0, product_co
-- mparison_price = 45.0, product_cost_price = Nothing, product_product_type = ProductPhysical, product_is_published = Fa
-- lse, product_properties = ProductProperties {product_color = "red", product_weight = "200gm"}}]
