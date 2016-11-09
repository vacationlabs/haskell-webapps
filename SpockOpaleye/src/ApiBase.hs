{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module ApiBase where

import qualified Data.Profunctor.Product.Default      as D
import           Data.Time                            (UTCTime, getCurrentTime)
import           Control.Lens
import           DataTypes
import           Database.PostgreSQL.Simple
import Opaleye
import OpaleyeDef
import Prelude hiding (id)

create_item :: (
      HasCreatedat haskells (Maybe UTCTime)
    , D.Default Constant haskells columnsW
    , D.Default QueryRunner returned b)
    => Connection -> Table columnsW returned -> haskells -> IO b
create_item conn table item = do
  current_time <- getCurrentTime
  let cl = createdat .~ Just current_time
  fmap head $ runInsertManyReturning conn table [constant $ cl item] (\x -> x)

update_item :: (
    HasUpdatedat haskells (Maybe UTCTime)
    , D.Default Constant haskells columnsW
    , D.Default Constant item_id (Column PGInt4)
    , HasId haskells item_id
    , HasId columnsR (Column PGInt4)
    )
    => Connection -> Table columnsW columnsR -> item_id -> haskells -> IO haskells
update_item conn table it_id item = do
  current_time <- getCurrentTime
  let updated_item = (put_updated_timestamp current_time) item
  runUpdate conn table (\_ -> constant updated_item) match_func
  return updated_item
  where
    put_updated_timestamp :: (HasUpdatedat item (Maybe UTCTime)) => UTCTime -> item -> item
    put_updated_timestamp timestamp  = updatedat .~ Just timestamp
    match_func :: (HasId cmR (Column PGInt4)) => (cmR -> Column PGBool)
    match_func item = (item ^. id) .== (constant it_id)
