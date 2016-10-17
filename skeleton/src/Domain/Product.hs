module Domain.Product where

import Import
import Domain.Types

createProduct :: Product -> AppM(Product)
createProduct product = undefined

editProduct :: Product -> AppM(Product)
editProduct product = undefined

getProduct :: ProductID -> AppM(Product)
getProduct pid = undefined


data ProductFilter = ProductFilter { ids :: [ProductID]
                                   , q :: Text
                                   , title :: Text
                                   -- and more such filters can come here
                                   }
filterProducts :: ProductFilter -> AppM([Product])
filterProducts pfilter = undefined
