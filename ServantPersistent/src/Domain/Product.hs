module Domain.Product where

import DBTypes
import ProductQuery
import Models

import Database.Persist
import Types
import Operation
import Control.Monad.Except

data ProductCreationError = Something

dbCreateProduct :: DBMonad m => ProductInput -> OperationT m (Either ProductCreationError ProductID)
dbCreateProduct s = runDb $ runExceptT $ undefined

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
