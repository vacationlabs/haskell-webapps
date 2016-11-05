{-# LANGUAGE RecordWildCards #-}
module Domain.Product where

import           DBTypes
import           Models
import           ProductQuery

import           Control.Monad.Except
import           Data.Maybe
import           Data.Time
import           Database.Persist
import           Operation
import           Types

data ProductCreationError = EmptyVariantList
                          | PhysicalProductFieldsMissing
                          | DigitalProductExtraFields
                          | ProductUniquenessViolated

sluggify = undefined

dbCreateProduct :: DBMonad m => TenantID -> ProductInput -> OperationT m (Either ProductCreationError ProductID)
dbCreateProduct tid ProductI{..} = runDb $ runExceptT $ do
                          time <- liftIO getCurrentTime
                          when (null piVariants) $
                               throwError EmptyVariantList
                          case piType of
                            Phys -> when (any (\VariantI{..} -> isNothing viWeightInGrams
                                                             || isNothing viWeightDisplayUnit)
                                              piVariants) $
                                      throwError $ PhysicalProductFieldsMissing
                            Dig -> when (any (\VariantI{..} -> isJust viWeightInGrams
                                                             || isJust viWeightDisplayUnit)
                                              piVariants) $
                                      throwError $ DigitalProductExtraFields
                          let advertisedPrice = fromMaybe (minimum $ map viPrice piVariants)
                                                          piAdvertisedPrice
                          let comparisionPrice = fromMaybe (advertisedPrice)
                                                           piComparisonPrice
                          let urlSlug = fromMaybe (sluggify piName)
                                                  piURLSlug
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

dbGetProduct :: DBMonad m => ProductID -> OperationT m (Either DBError Product)
dbGetProduct pid = runDb $ runExceptT $ do
                   product <- ExceptT $
                              maybe (Left $ ProductNotFound pid)
                                    Right
                                <$> get pid
                   variants <- lift $ selectList [DBVariantProductID ==. pid] []
                   return $ Product (Entity pid product) variants

dbGetProductList :: DBMonad m => ProductFilter -> OperationT m [Product]
dbGetProductList pf = runDb $ do
                   products <- selectList (getProductFilter pf) []
                   variants <- forM (map entityKey products) $ \pid ->
                                 selectList (getVariantFilter pf ++ [DBVariantProductID ==. pid]) []
                   let pfilter = filter (not . null . getVariants)
                   return $ pfilter $ zipWith Product products variants
