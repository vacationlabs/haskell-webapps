{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Domain.Product where

import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Time
import           Database.Persist
import           DBTypes
import           Models
import           Operation
import           ProductQuery
import           System.Random
import           Types

data ProductCreationError = EmptyVariantList
                          | PhysicalProductFieldsMissing
                          | DigitalProductExtraFields
                          | ProductUniquenessViolated

sluggify :: T.Text -> T.Text
sluggify = T.intercalate "-"
         . fmap T.toLower
         . T.words

makeUnique :: MonadIO m => T.Text -> TransactionT m T.Text
makeUnique s = do
  r <- getBy (UniqueSlug s)
  case r of
    Nothing -> return s
    Just _ -> do
           (n :: Int) <- liftIO $ randomRIO (1000,100000)
           makeUnique (s `T.append` T.pack (show n))

dbCreateProduct :: MonadIO m
                => TenantId
                -> ProductInput
                -> OperationT (TransactionT m) (Either ProductCreationError ProductId)
dbCreateProduct tid ProductI{..} =
  requirePermission (CreateProduct tid) $
  lift $ runExceptT $ do
     time <- liftIO getCurrentTime
     when (null piVariants) $
          throwError EmptyVariantList
     case piType of
       Phys -> when (any (\VariantI{..} -> isNothing viWeightInGrams
                                        || isNothing viWeightDisplayUnit)
                         piVariants) $
                 throwError PhysicalProductFieldsMissing
       Dig -> when (any (\VariantI{..} -> isJust viWeightInGrams
                                       || isJust viWeightDisplayUnit)
                         piVariants) $
                 throwError DigitalProductExtraFields
     let advertisedPrice = fromMaybe (minimum $ map viPrice piVariants)
                                     piAdvertisedPrice
     let comparisionPrice = fromMaybe advertisedPrice
                                      piComparisonPrice
     let urlSlug' = fromMaybe (sluggify piName)
                              piURLSlug
     urlSlug <- lift $ makeUnique urlSlug'
     let dbProd = DBProduct { _dBProductAdvertisedPrice = advertisedPrice
                            , _dBProductComparisionPrice = comparisionPrice
                            , _dBProductCostPrice = piCostPrice
                            , _dBProductCurrency = piCurrency
                            , _dBProductDescription = piDescription
                            , _dBProductName = piName
                            , _dBProductProductType = piType
                            , _dBProductTenantID = tid
                            , _dBProductProperties = piProperties
                            , _dBProductUrlSlug = urlSlug
                            , _dBProductCreatedAt = time
                            , _dBProductUpdatedAt = time
                            }
     let mkDBVar pid VariantI{..} =
           DBVariant { _dBVariantName = viName
                     , _dBVariantPrice = viPrice
                     , _dBVariantProductID = pid
                     , _dBVariantSku = viSKU
                     , _dBVariantWeightDisplayUnit = viWeightDisplayUnit
                     , _dBVariantWeightInGrams = viWeightInGrams
                     , _dBVariantCreatedAt = time
                     , _dBVariantUpdatedAt = time}
     r <- lift $ insertUnique dbProd
     case r of
       Nothing -> throwError ProductUniquenessViolated
       Just pid -> do lift $ insertMany_ $ map (mkDBVar pid) piVariants
                      return pid

dbGetProduct :: MonadIO m => ProductId -> OperationT (TransactionT m) (Either DBError Product)
dbGetProduct pid =
  lift $ runExceptT $ do
    prod <- ExceptT $
               maybe (Left $ ProductNotFound pid)
                     Right
                 <$> get pid
    variants <- lift $ selectList [DBVariantProductID ==. pid] []
    return $ Product (Entity pid prod) variants

dbGetProductList :: MonadIO m => ProductFilter -> OperationT (TransactionT m) [Product]
dbGetProductList pf =
  lift $ do
    products <- selectList (getProductFilter pf) []
    variants <- forM (map entityKey products) $ \pid ->
                  selectList [DBVariantProductID ==. pid] []
    let pfilter = filter (not . null . getVariants)
    return $ pfilter $ zipWith Product products variants
